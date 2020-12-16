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
### CHANGE NETWORK FILE ON LINE BELOW
data <- read.csv("CCBC_network_articles.csv", header = TRUE)
head(data)

##=================
# create igraph object with edges and vertices
##==================
networkpapers <- graph_from_data_frame(data, directed = F) %>% simplify(remove.loops = TRUE)

##==================
#keep only the main component
##==================
V(networkpapers)$comp <- components(networkpapers)$membership
mainnetwork <- induced_subgraph(networkpapers, V(networkpapers)$comp==1)

##==================
#compute centrality measures
##==================
edge_density(mainnetwork)
deg_cent <- data.frame(attributes(degree(mainnetwork)),
                       enframe(degree(mainnetwork),name=NULL)) %>%
  arrange(desc(value))
clos_cent <- data.frame("c" = closeness(mainnetwork)) %>% rownames_to_column("nom") %>%
  arrange(desc(c))
betw_cent <- data.frame("c" = betweenness(mainnetwork)) %>% rownames_to_column("nom") %>%
  arrange(desc(c))
eigen_cent <- data.frame("c" = eigen_centrality(mainnetwork)$vector) %>% rownames_to_column("nom") %>%
  arrange(desc(c))

##================
#test louvain community detection
##================
lv <- cluster_louvain(mainnetwork) 
V(mainnetwork)$community <- lv$membership
unique(V(mainnetwork)$community)
c <- palette(rainbow(max(V(mainnetwork)$community)))
c <- palette(rainbow(max(V(mainnetwork)$community)))
plot(mainnetwork,vertex.label=NA,vertex.color = c[V(mainnetwork)$community],vertex.size = 1,layout = layout.fruchterman.reingold)
dev.off()

modularity(lv)

### ABSTRACT ANALYSIS - WORDS PER COMMUNITY
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
papers$abstract <- gsub("\\bsharks\\b", "shark", papers$abstract)
papers$abstract <- gsub("\\bspecies\\b", "shark", papers$abstract)

#remove stop words
data("stop_words")
#stop_words <- stop_words %>%
add_row(word = "shark", lexicon = "SMART")

papers <- papers %>% rename(name = ut)

##=================
#append vertices topics attributes 
#==================
df <- igraph::as_data_frame(mainnetwork, "both")
df$vertices <- df$vertices %>%
  left_join(papers, by = "name")

#unnest tokens
abstract_df <- df$vertices %>% 
  unnest_tokens(word, abstract)

#remove NA tokens
abstract_sep <- abstract_df[complete.cases(abstract_df[, 28]),]

#remove stopwords
abstract_fil <- abstract_sep %>%
  filter(!word %in% stop_words$word)

#count word per document - tf-idf
abstract_count <- abstract_fil %>%
  group_by(community) %>%
  count(community, npapers = n_distinct(name), word, sort = TRUE) %>%
  bind_tf_idf(word, community, n) %>%
  arrange(-tf_idf) %>%
  top_n(10) %>%
  ungroup

abstract_count %>%
  mutate(word = reorder_within(word, n, community)) %>%
  ggplot(aes(word, n, fill = community)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ community, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=11)) +
  labs(x = NULL, y = "n",
       title = "Highest n words in shark research communities",
       subtitle = "")

finalnetwork <- graph_from_data_frame(df$edges, directed = F, vertices = df$vertices)

##=================
#create summary table of topics by community
#gamma is the fitting coefficient of a document to a topic
#IMPORTANT : check for weighted measures of topic to community
##=================
summaryword <- abstract_count %>%
  group_by(community) %>%
  summarise(npapers,
            words = paste(word, collapse=", "),
            ) %>%
  slice(1)

summarytable <- summaryword %>%
  select(community, npapers, words) %>%
  ungroup()

gt_table <- summarytable %>%
  gt(rowname_col = "community") %>%
  tab_header(
    title = "Communities of shark research (co-citations and bibliographic coupling)"
  ) %>%
  tab_stubhead(label = "Community") %>%
  cols_label(
    npapers = "N papers in community",
    words = "Top 10 words used in papers abstracts",
  )

gt_table <- gt_table %>% 
  tab_style(style = list(
    cell_text(font = "Times", size = px(14))
  ),
  locations = list(cells_body(), cells_stub(), cells_stubhead(), cells_column_labels(1:2))
  ) %>%
  tab_style(style = list(
    cell_text(align = "center", font = "Times", size = px(16), weight = "bolder")
  ),
  locations = list(cells_column_labels(1:2), cells_stubhead(), cells_title())
  ) %>%
  tab_style(style = list(
    cell_text(align = "left")
  ),
  locations = list(cells_body(2))
  ) %>%
  tab_style(style = list(
    cell_text(align = "right")
  ),
  locations = list(cells_body(3))
  )
  
gt_table