## =================
# Rémi Toupin
# Projet : Shark papers topics - network
# created: 17 Nov 2020
# Encoding : UTF-8

library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(vegan)
library(readxl)
library(quanteda)
library(tidyr)
library(tidytext)
library(tidyverse)
library(igraph)

## =================
# load nodes and edges info
## =================
setwd("~/Documents/1 - Projets en cours/Requins")
data <- read_excel("shark_data.xlsx", sheet = "citations",col_names = TRUE)
head(data)

colnames(papers)[colnames(papers) == "cited_ut"] <- "ut"
papersid <- papers %>%
  select(ut)

colnames(papersid)[colnames(papersid) == "ut"] <- "citing_ut"
data <- inner_join(data, papersid, by = "citing_ut")
colnames(papersid)[colnames(papersid) == "citing_ut"] <- "cited_ut"
data <- inner_join(data, papersid, by = "cited_ut")

## =================
# create igraph object (network object with edges and vertices attributes)
## =================
network <- igraph::graph_from_data_frame(data, directed = TRUE, vertices = papers)

network <- delete.vertices(network, igraph::V(network)[degree(network)==0])

## =================
# create network visualisation
## =================
graph <- plot(simplify(network, remove.loops = TRUE), edge.arrow.size=.05, 
     vertex.color=igraph::V(network)$topic,
     vertex.label=NA,
     vertex.size=2,
     layout_with_lgl(
       network
     )
)

