## =================
# Rémi Toupin
# Projet : Shark papers topics - network
# created: 17 Nov 2020
# updated: 18 Non 2020
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

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================
setwd("~/Documents/1 - Projets en cours/Requins")
data <- read.csv("CC_network_articles.csv", header = TRUE)
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
#test leading eigenvector community detection
##================
le <- leading.eigenvector.community(mainnetwork) 
V(mainnetwork)$community <- le$membership
unique(V(mainnetwork)$community)
c <- palette(rainbow(max(V(mainnetwork)$community)))
c <- palette(rainbow(max(V(mainnetwork)$community)))
plot(mainnetwork,vertex.label=NA,vertex.color = c[V(mainnetwork)$community],vertex.size = 1,layout = layout.fruchterman.reingold)
dev.off()

##================
#test fastgreedy community detection
##================
fg <- fastgreedy.community(mainnetwork) 
V(mainnetwork)$community <- fg$membership
unique(V(mainnetwork)$community)
c <- palette(rainbow(max(V(mainnetwork)$community)))
c <- palette(rainbow(max(V(mainnetwork)$community)))
plot(mainnetwork,vertex.label=NA,vertex.color = c[V(mainnetwork)$community],vertex.size = 1,layout = layout.fruchterman.reingold)
dev.off()

##================
#test multilevel community detection
##================
ml <- multilevel.community(mainnetwork) 
V(mainnetwork)$community <- ml$membership
unique(V(mainnetwork)$community)
c <- palette(rainbow(max(V(mainnetwork)$community)))
c <- palette(rainbow(max(V(mainnetwork)$community)))
plot(mainnetwork,vertex.label=NA,vertex.color = c[V(mainnetwork)$community],vertex.size = 1,layout = layout.fruchterman.reingold)
dev.off()

##================
#test walktrap community detection
##================
wt <- walktrap.community(mainnetwork) 
V(mainnetwork)$community <- wt$membership
unique(V(mainnetwork)$community)
c <- palette(rainbow(max(V(mainnetwork)$community)))
c <- palette(rainbow(max(V(mainnetwork)$community)))
plot(mainnetwork,vertex.label=NA,vertex.color = c[V(mainnetwork)$community],vertex.size = 1,layout = layout.fruchterman.reingold)
dev.off()

##================
#test label propagation community detection
##================
lp <- label.propagation.community(mainnetwork) 
V(mainnetwork)$community <- lp$membership
unique(V(mainnetwork)$community)
c <- palette(rainbow(max(V(mainnetwork)$community)))
c <- palette(rainbow(max(V(mainnetwork)$community)))
plot(mainnetwork,vertex.label=NA,vertex.color = c[V(mainnetwork)$community],vertex.size = 1,layout = layout.fruchterman.reingold)
dev.off()

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

##================
#compute modularity measurements by algorithms
##================
modularity(le)
modularity(fg)
modularity(ml)
modularity(lv)
modularity(wt)
modularity(lp)

##=================
#append vertices topics attributes (from STM1_topicmodel.R)
#==================
df <- igraph::as_data_frame(mainnetwork, "both")
df$vertices <- df$vertices %>%
  left_join(topicmain, by = "name")
finalnetwork <- graph_from_data_frame(df$edges, directed = F, vertices = df$vertices)

##=================
#create summary table of topics by community
#gamma is the fitting coefficient of a document to a topic
#IMPORTANT : check for weighted measures of topic to community
##=================
summarynodes <- df$vertices %>%
  group_by(community) %>%
  summarise(npapers = n_distinct(name),
            ntopic = n_distinct(topic, na.rm = T),
            most_topic = pracma::Mode(topic),
            most_topic_median = median(gamma, na.rm = TRUE),
            gamma1 = median(gamma1[gamma1>=0.1], na.rm= T),
            gamma2 = median(gamma2[gamma2>=0.1], na.rm= T),
            gamma3 = median(gamma3[gamma3>=0.1], na.rm= T),
            gamma4 = median(gamma4[gamma4>=0.1], na.rm= T),
            gamma5 = median(gamma5[gamma5>=0.1], na.rm= T),
            gamma6 = median(gamma6[gamma6>=0.1], na.rm= T),
            gamma7 = median(gamma7[gamma7>=0.1], na.rm= T),
            gamma8 = median(gamma8[gamma8>=0.1], na.rm= T),
            gamma9 = median(gamma9[gamma9>=0.1], na.rm= T),
            gamma10 = median(gamma10[gamma10>=0.1], na.rm= T),
            gamma11 = median(gamma11[gamma11>=0.1], na.rm= T),
            gamma12 = median(gamma12[gamma12>=0.1], na.rm= T),
            gamma13 = median(gamma13[gamma13>=0.1], na.rm= T),
            gamma14 = median(gamma14[gamma14>=0.1], na.rm= T),
            gamma15 = median(gamma15[gamma15>=0.1], na.rm= T),
            gamma16 = median(gamma16[gamma16>=0.1], na.rm= T),
            gamma17 = median(gamma17[gamma17>=0.1], na.rm= T),
            gamma18 = median(gamma18[gamma18>=0.1], na.rm= T),
            gamma19 = median(gamma19[gamma19>=0.1], na.rm= T),
            )

write_csv(df$vertices, "CC_Louvain_vertices.csv")
write_csv(summarynodes, "CC_Louvain_vertices_summary.csv")

df$vertices %>%
  filter(community == 1) %>%
  count(topic)
