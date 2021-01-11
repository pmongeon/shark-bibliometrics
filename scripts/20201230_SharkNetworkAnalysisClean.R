## =================
# Rémi Toupin
# Projet : Shark papers topics - network
# created: 17 Nov 2020
# updated: 16 Dec 2020
# Encoding : UTF-8
# main objects =  networkpapers (graph of all papers)
#                 mainnetwork (graph of the main component)
#                 finalnetwork (graph with vertices topic attributes)
#                 df$vertices (dataframe of vertices attributes)

library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(vegan)
library(readxl)
library(quanteda)
library(tidyr)
library(tidytext)
library(tidyverse)
library(igraph)
library(pracma)
library(topicmodels)
library(tm)
library(stm)
library(ldatuning)
library(stopwords)
library(gt)

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================
setwd("~/Documents/1 - Projets en cours/Requins")
dataCCBC <- read.csv("CCBC_network_articles.csv", header = TRUE)
dataCC <- read.csv("CC_network_articles.csv", header = TRUE)
dataBC <- read.csv("BC_network_article.csv", header = TRUE)
Articles <- read_excel("shark_data.xlsx", sheet="papers", col_names = TRUE)

##=================
# create igraph object with edges and vertices
##==================
networkpapers <- graph_from_data_frame(dataCCBC, directed = F) %>% simplify(remove.loops = TRUE)

##==================
#keep only the main component
##==================
V(networkpapers)$comp <- components(networkpapers)$membership
mainnetwork <- induced_subgraph(networkpapers, V(networkpapers)$comp==1)

# Identify macro-clusters
lv <- cluster_louvain(mainnetwork, weights = c(E(mainnetwork)$Weight))
V(mainnetwork)$community <- lv$membership
modularity(lv)

# Add macro cluster to the articles table
Articles <- Articles %>% 
  left_join(select(as_data_frame(mainnetwork, "both")$vertices, name, macro_cluster = community), by=c("ut" = "name"))

##=================
#  Identify meso-clusters
##=================
j = 0
for (i in 1:max(filter(Articles, !is.na(macro_cluster))$macro_cluster)) {
  
  # create igraph object
  network <- dataCCBC %>% 
    inner_join(select(filter(Articles, macro_cluster == i),ut), by=c("Source" = "ut")) %>% 
    inner_join(select(filter(Articles, macro_cluster == i),ut), by=c("Target" = "ut")) %>% 
    graph_from_data_frame(directed = F) %>% 
    simplify(remove.loops = TRUE)
  
  #keep only the main component
  V(network)$comp <- components(network)$membership
  network <- induced_subgraph(network, V(network)$comp==1)
  
  # Identify communities
  V(network)$community <- cluster_louvain(network, weights = c(E(network)$Weight))$membership
  
  # List of articles with their community
  if (i==1){
    x <-  select(as_data_frame(network, "both")$vertices, name, meso_cluster = community) %>% 
      mutate(meso_cluster = meso_cluster+j)
    meso_cluster <- select(Articles,ut) %>% 
      inner_join(x, by=c("ut" = "name"))
  }
  if (i>1){
    x <-  select(as_data_frame(network, "both")$vertices, name, meso_cluster = community) %>% 
      mutate(meso_cluster = meso_cluster+j)
    meso_cluster <- rbind(meso_cluster, select(Articles,ut) %>% 
                            inner_join(x, by=c("ut" = "name")))
  }
  j = max(meso_cluster$meso_cluster)
}

# Add meso clusters to Articles

Articles <- Articles %>% 
  left_join(meso_cluster, by="ut")

##==================
# Identify micro-clusters
##==================
j=0
for (i in 1:max(filter(Articles, !is.na(meso_cluster))$meso_cluster)) {
  
  # create igraph object
  network <- dataCCBC %>% 
    inner_join(select(filter(Articles, meso_cluster == i),ut), by=c("Source" = "ut")) %>% 
    inner_join(select(filter(Articles, meso_cluster == i),ut), by=c("Target" = "ut")) %>% 
    graph_from_data_frame(directed = F) %>% 
    simplify(remove.loops = TRUE)
  
  #keep only the main component
  V(network)$comp <- components(network)$membership
  network <- induced_subgraph(network, V(network)$comp==1)
  
  # Identify communities
  V(network)$community <- cluster_louvain(network, weights = c(E(network)$Weight))$membership
  
  # List of articles with their community
  if (i==1){
    x <-  select(as_data_frame(network, "both")$vertices, name, micro_cluster = community) %>% 
      mutate(micro_cluster = micro_cluster+j)
    
    micro_cluster <- select(Articles,ut) %>% 
      inner_join(x, by=c("ut" = "name"))
  }
  if (i>1){
    x <-  select(as_data_frame(network, "both")$vertices, name, micro_cluster = community) %>% 
      mutate(micro_cluster = micro_cluster+j)
    micro_cluster <- rbind(micro_cluster, select(Articles,ut) %>% 
                             inner_join(x, by=c("ut" = "name")))
  }
  j = max(micro_cluster$micro_cluster)
}

Articles <- Articles %>% 
  left_join(micro_cluster, by="ut")

### ABSTRACT ANALYSIS - WORDS PER COMMUNITY
## =================
# basic string cleaning
## =================

papers <- Articles

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
papers$abstract <- gsub("\\bsharks\\b", "shark", papers$abstract)

#remove stop words
data("stop_words")
stop_words <- stop_words %>%
  add_row(word = "shark", lexicon = "SMART") %>%
  add_row(word = "species", lexicon = "SMART")

##=================
#append vertices topics attributes 
#==================
#unnest tokens
abstract_df <- papers %>% 
  unnest_tokens(word, abstract)

#remove NA tokens
abstract_sep <- abstract_df[complete.cases(abstract_df[, 29]),]

#remove stopwords
abstract_fil <- abstract_sep %>%
  filter(!word %in% stop_words$word)

#count word per document - tf-idf
abstract_count_mac <- abstract_fil %>%
  group_by(macro_cluster) %>%
  count(macro_cluster, npapers = n_distinct(ut), word, sort = TRUE) %>%
  bind_tf_idf(word, macro_cluster, n) %>%
  arrange(-n) %>%
  top_n(10) %>%
  ungroup()

abstract_count_mes <- abstract_fil %>%
  group_by(meso_cluster) %>%
  count(macro_cluster, meso_cluster, npapers = n_distinct(ut), word, sort = TRUE) %>%
  bind_tf_idf(word, meso_cluster, n) %>%
  arrange(-n) %>%
  top_n(10) %>%
  ungroup()

abstract_count_mic <- abstract_fil %>%
  group_by(micro_cluster) %>%
  count(macro_cluster, meso_cluster, micro_cluster, npapers = n_distinct(ut), word, sort = TRUE) %>%
  bind_tf_idf(word, micro_cluster, n) %>%
  arrange(-n) %>%
  top_n(10) %>%
  ungroup()

##=================
#create summary table of top words by clusters
##=================
summaryword <- abstract_count_mic %>%
  group_by(micro_cluster) %>%
  summarise(macro_cluster,
            meso_cluster,
            npapers,
            words = paste(word, collapse=", "),
  ) %>%
  slice(1)

summarytable <- summaryword %>%
  select(macro_cluster, meso_cluster, micro_cluster, npapers, words) %>%
  ungroup()

gt_table <- summarytable %>%
  gt(rowname_col = "micro_cluster") %>%
  tab_header(
    title = "Communities of shark research (co-citations and bibliographic coupling)"
  ) %>%
  tab_stubhead(label = "Micro-community") %>%
  cols_label(
    macro_cluster = "Macro-community",
    meso_cluster = "Meso-community",
    npapers = "N papers in community",
    words = "Top 10 words used in papers abstracts",
  )

gt_table <- gt_table %>% 
  tab_style(style = list(
    cell_text(font = "Times", size = px(14))
  ),
  locations = list(cells_body(), cells_stub(), cells_stubhead(), cells_column_labels(1:4))
  ) %>%
  tab_style(style = list(
    cell_text(align = "center", font = "Times", size = px(16), weight = "bolder")
  ),
  locations = list(cells_column_labels(1:4), cells_stubhead(), cells_title())
  ) %>%
  tab_style(style = list(
    cell_text(align = "left")
  ),
  locations = list(cells_body(1:4))
  ) %>%
  tab_style(style = list(
    cell_text(align = "right")
  ),
  locations = list(cells_body(5))
  )

gt_table