## =================
# Rémi code for Users ID
# created: 25 Nov 2019

library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(ggrepel)
library(ggforce)
library(ggvenn)
library(RColorBrewer)
library(vegan)
library(readxl)
library(quanteda)
library(tidyr)
library(tidytext)
library(tidyverse)
library(UpSetR)
library(igraph)
library(ForceAtlas2)

## =================
# load users info
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

network <- igraph::graph_from_data_frame(data, directed = TRUE, vertices = papers)

network <- delete.vertices(network, igraph::V(network)[degree(network)==0])

graph <- plot(simplify(network, remove.loops = TRUE), edge.arrow.size=.05, 
     vertex.color=igraph::V(network)$topic,
     vertex.label=NA,
     vertex.size=2,
     layout_with_lgl(
       network,
       maxiter = 150,
       maxdelta = vcount(graph),
       area = vcount(graph)^2,
       coolexp = 1.5,
       repulserad = area * vcount(graph),
       cellsize = sqrt(sqrt(area)),
       root = NULL
     )
)

