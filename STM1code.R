# Rémi code for shark papers grouping STM1 (17 K)
# created: 11 Nov 2020

library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(vegan)
library(readxl)
library(quanteda)
library(tidyverse)
library(tidyr)
library(tidytext)
library(topicmodels)
library(tm)
library(stm)
library(ldatuning)
library(stopwords)

## =================
# load data for shark papers
## =================
setwd("~/Documents/1 - Projets en cours/Requins")
papersall <- read_excel("shark_data.xlsx", sheet="papers", col_names = TRUE)
head(papersall)
length(unique(papersall$abstract))

## =================
# basic string cleaning
## =================

papers <- papersall

#remove rights reserved
papers$abstract <- gsub("\\(C\\).+", " ", papers$abstract)
length(unique(papers$abstract))

#upper cases to lower cases
papers$abstract <- tolower(papers$abstract)

#remove URLs
papers$abstract <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", papers$abstract) 
                        
#remove emojis
papers$abstract <- gsub("[^\x01-\x7F]", "", papers$abstract)

#remove numbers
papers$abstract <- gsub("[0-9]+", "", papers$abstract)

#remove lists with / and replace with " " (e.g., wife/phd/friend)
papers$abstract <- gsub("\\/", " ", papers$abstract)

# forward slash -- escape with a double backslash
sub("\\/", " ", "Peace/Love")
#[1] "Peace Love"

length(unique(papers$abstract))

papers <- papers %>% 
  filter(!grepl('null', abstract))

papers <- papers %>% 
  filter(!is.na(abstract))

#remove punctuations
punct <- '[]\\?!\"\'$%&(){}+*/:;,._`|~\\[<=>\\^-]'
papers$abstract <- gsub(punct, "", papers$abstract)

#convert plural to singular
#papers$abstract <- gsub("\\bmodels\\b", "model", papers$abstract)
#papers$abstract <- gsub("\\btemperatures\\b", "temperature", papers$abstract)
#papers$abstract <- gsub("\\badaptations\\b", "adaptation", papers$abstract)
#papers$abstract <- gsub("\\bwetlands\\b", "wetland", papers$abstract)
#papers$abstract <- gsub("\\gases\\b", "gas", papers$abstract)
#papers$abstract <- gsub("\\bimpacts\\b", "impact", papers$abstract)
#papers$abstract <- gsub("\\beffects\\b", "effect", papers$abstract)
#papers$abstract <- gsub("\\bemissions\\b", "emission", papers$abstract)
#papers$abstract <- gsub("\\bdiseases\\b", "disease", papers$abstract)
#papers$abstract <- gsub("\\bforests\\b", "forest", papers$abstract)
#papers$abstract <- gsub("\\bservices\\b", "service", papers$abstract)
#papers$abstract <- gsub("\\bscenarios\\b", "scenario", papers$abstract)
#papers$abstract <- gsub("\\bprojections\\b", "projection", papers$abstract)
#papers$abstract <- gsub("\\bevents\\b", "event", papers$abstract)
#papers$abstract <- gsub("\\bpopulations\\b", "population", papers$abstract)
#papers$abstract <- gsub("\\byields\\b", "yield", papers$abstract)
#papers$abstract <- gsub("\\bnations\\b", "nation", papers$abstract)
#papers$abstract <- gsub("\\bstrategies\\b", "strategy", papers$abstract)
#papers$abstract <- gsub("\\bshifts\\b", "shift", papers$abstract)
#papers$abstract <- gsub("\\bresponses\\b", "response", papers$abstract)
#papers$abstract <- gsub("\\bfarmers\\b", "farmer", papers$abstract)
#papers$abstract <- gsub("\\bpolicies\\b", "policy", papers$abstract)
#papers$abstract <- gsub("\\bbeliefs\\b", "belief", papers$abstract)
#papers$abstract <- gsub("\\bscientists\\b", "scientist", papers$abstract)
#papers$abstract <- gsub("\\battitudes\\b", "attitude", papers$abstract)
#papers$abstract <- gsub("\\bdecisionmakers\\b", "decisionmaker", papers$abstract)
#papers$abstract <- gsub("\\bperceptions\\b", "perception", papers$abstract)
#papers$abstract <- gsub("\\bconditions\\b", "condition", papers$abstract)
#papers$abstract <- gsub("\\brisks\\b", "risk", papers$abstract)
#papers$abstract <- gsub("\\bsimulations\\b", "simulation", papers$abstract)
#papers$abstract <- gsub("\\blakes\\b", "lake", papers$abstract)
#papers$abstract <- gsub("\\brivers\\b", "river", papers$abstract)
#papers$abstract <- gsub("\\binteractions\\b", "interaction", papers$abstract)
#papers$abstract <- gsub("\\boceanss\\b", "ocean", papers$abstract)
#papers$abstract <- gsub("\\btraits\\b", "trait", papers$abstract)
#papers$abstract <- gsub("\\bcommunities\\b", "community", papers$abstract)
#papers$abstract <- gsub("\\bplants\\b", "plant", papers$abstract)
#papers$abstract <- gsub("\\becosystems\\b", "ecosystem", papers$abstract)
#papers$abstract <- gsub("\\bghg\\b", "greenhouse gas", papers$abstract)
#papers$abstract <- gsub("\\bdistributions\\b", "distribution", papers$abstract)
#papers$abstract <- gsub("\\bregions\\b", "region", papers$abstract)
#papers$abstract <- gsub("\\bfactors\\b", "factor", papers$abstract)

#remove stop words
data("stop_words")
#stop_words <- stop_words %>%
#  add_row(word = "climate", lexicon = "SMART") %>%
#  add_row(word = "change", lexicon = "SMART") %>%
#  add_row(word = "global", lexicon = "SMART") %>%
#  add_row(word = "warming", lexicon = "SMART") %>%
#  add_row(word = "results", lexicon = "SMART") %>%
#  add_row(word = "climatic", lexicon = "SMART") %>%
#  add_row(word = "research", lexicon = "SMART") %>%
#  add_row(word = "paper", lexicon = "SMART") %>%
#  add_row(word = "article", lexicon = "SMART") %>%
#  add_row(word = "st", lexicon = "SMART") %>%
#  add_row(word = "future", lexicon = "SMART") %>%
#  add_row(word = "study", lexicon = "SMART") %>%
#  add_row(word = "analysis", lexicon = "SMART") %>%
#  add_row(word = "impact", lexicon = "SMART") %>%
#  add_row(word = "data", lexicon = "SMART") %>%
#  add_row(word = "effect", lexicon = "SMART") %>%
#  add_row(word = "environmental", lexicon = "SMART") %>%
#  add_row(word = "increase", lexicon = "SMART") %>%
#  add_row(word = "similar", lexicon = "SMART") %>%
#  add_row(word = "temperature", lexicon = "SMART") %>%
#  add_row(word = "species", lexicon = "SMART") %>%
#  add_row(word = "response", lexicon = "SMART") %>%
#  add_row(word = "potential", lexicon = "SMART") %>%
#  add_row(word = "ip", lexicon = "SMART") %>%
#  add_row(word = "gw", lexicon = "SMART") %>%
#  add_row(word = "condition", lexicon = "SMART") %>%
#  add_row(word = "scenario", lexicon = "SMART") %>%
#  add_row(word = "based", lexicon = "SMART") %>%
#  add_row(word = "studies", lexicon = "SMART") %>%
#  add_row(word = "studied", lexicon = "SMART") %>%
#  add_row(word = "current", lexicon = "SMART") %>%
#  add_row(word = "understanding", lexicon = "SMART") %>%
#  add_row(word = "range", lexicon = "SMART") %>%
#  add_row(word = "recent", lexicon = "SMART") %>%
#  add_row(word = "due", lexicon = "SMART") %>%
#  add_row(word = "approach", lexicon = "SMART") %>%
#  add_row(word = "significant", lexicon = "SMART") %>%
#  add_row(word = "community", lexicon = "SMART") %>%
#  add_row(word = "increased", lexicon = "SMART") %>%
#  add_row(word = "increasing", lexicon = "SMART") %>%
#  add_row(word = "shift", lexicon = "SMART") %>%
#  add_row(word = "local", lexicon = "SMART") %>%
#  add_row(word = "time", lexicon = "SMART") %>%
#  add_row(word = "projected", lexicon = "SMART") %>%
#  add_row(word = "influence", lexicon = "SMART") %>%
#  add_row(word = "precipitation", lexicon = "SMART") %>%
#  add_row(word = "regional", lexicon = "SMART") %>%
#  add_row(word = "factors", lexicon = "SMART") %>%
#  add_row(word = "factor", lexicon = "SMART") %>%
#  add_row(word = "assess", lexicon = "SMART") %>%
#  add_row(word = "role", lexicon = "SMART") %>%
#  add_row(word = "human", lexicon = "SMART") %>%
#  add_row(word = "regions", lexicon = "SMART") %>%
#  add_row(word = "region", lexicon = "SMART") %>%
#  add_row(word = "found", lexicon = "SMART") %>%
#  add_row(word = "projection", lexicon = "SMART") %>%
#  add_row(word = "period", lexicon = "SMART") %>%
#  add_row(word = "variability", lexicon = "SMART") %>%
#  add_row(word = "provide", lexicon = "SMART") %>%
#  add_row(word = "major", lexicon = "SMART") %>%
#  add_row(word = "natural", lexicon = "SMART") %>%
#  add_row(word = "suggest", lexicon = "SMART") %>%
#  add_row(word = "key", lexicon = "SMART") %>%
#  add_row(word = "patterns", lexicon = "SMART") %>%
#  add_row(word = "changing", lexicon = "SMART") %>%
#  add_row(word = "ecological", lexicon = "SMART") %>%
#  add_row(word = "observed", lexicon = "SMART") %>%
#  add_row(word = "including", lexicon = "SMART") %>%
#  add_row(word = "expected", lexicon = "SMART") %>%
#  add_row(word = "developed", lexicon = "SMART") %>%
#  add_row(word = "processes", lexicon = "SMART") %>%
#  add_row(word = "policy", lexicon = "SMART") %>%
#  add_row(word = "degrees", lexicon = "SMART") %>%
#  add_row(word = "carbon", lexicon = "SMART") %>%
#  add_row(word = "emission", lexicon = "SMART") %>%
#  add_row(word = "greenhouse", lexicon = "SMART") %>%
#  add_row(word = "gas", lexicon = "SMART") %>%
#  add_row(word = "water", lexicon = "SMART") %>%
#  add_row(word = "forest", lexicon = "SMART") %>%
#  add_row(word = "lake", lexicon = "SMART") %>%
#  add_row(word = "adaptation", lexicon = "SMART") %>%
#  add_row(word = "population", lexicon = "SMART") %>%
#  add_row(word = "distribution", lexicon = "SMART") %>%
#  add_row(word = "ecosystem", lexicon = "SMART") %>%
#  add_row(word = "model", lexicon ="SMART")

stopwordsfr <- stopwords(language = "fr")
stopwordsfr

#unnest tokens
abstract_df <- papers %>% 
  unnest_tokens(word, abstract)

#remove NA tokens
abstract_sep <- abstract_df[complete.cases(abstract_df[, 26]),]

#remove stopwords
abstract_fil <- abstract_sep %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% stopwordsfr)

#count word per document
abstract_count <- abstract_fil %>% 
  count(ut, word, sort = TRUE)

#tf_idf
abstract_count <- abstract_count %>%
  bind_tf_idf(word, ut, n)

abstract_count <- abstract_count %>%
  filter(tf_idf >= 0.034)

abstract_count

abstract_dfm <- abstract_count %>%
  cast_dfm(ut, word, n)

# ntopics <- FindTopicsNumber(
#  abstract_dtm,
#  topics = seq(from = 2, to = 100, by = 1),
#  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#  method = "Gibbs",
#  control = list(seed = 77),
#  mc.cores = 2L,
#  verbose = TRUE
#)
# FindTopicsNumber_plot(ntopics)

abstractstm <- stm(abstract_dfm, K = 17, verbose = TRUE, init.type = "Spectral")
abstopics <- tidy(abstractstm, matrix = "beta")
absdocuments <- tidy(abstractstm, document_names = papers$ut, matrix = "gamma")
abstopics
absdocuments

length(unique(absdocuments$document))

colnames(absdocuments)[colnames(absdocuments) == "document"] <- "ut"

abstopterms <- abstopics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

abstopterms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  labs(title = "Topic modeling of tweeted papers abstract", x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_x_reordered()

absdocuments$id_art <- as.character(as.numeric(absdocuments$id_art))

users <- left_join(x = users, y = absdocuments, by = "id_art")

absdocuments %>%
  ggplot() +
  geom_bar(aes(x = topic, y = gamma, fill = topic), stat = "identity", position = "dodge") +
  facet_wrap(~ document, scales = "free")

absdifterms <- abstopics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

absdifterms

