#***********************
# Project: Mapping LIS
# Description: This code generates the network files for Gephi
# Author: Philippe Mongeon
# Date: 2020-07-31
#***********************


#install.packages(c("ggplot2", "e1071", "caret", "quanteda", 
 #                  "irlba", "randomForest"))

# Load needed packages ----
library(sqldf) # for running SQL in R
library(rio) # for the import function
library(stringr)

library(data.table) # For the parsing
library(tm) # For the stemming

library(textstem)
library(tidyverse)
library(tidytext)
library(readxl)
library(lsa) # For the cosine function
library(irlba)


# Load data ----
Articles <- read_excel("data/shark_data.xlsx", sheet ="papers")
Citations <- read_excel("data/shark_data.xlsx", sheet ="citations")
  
# parser les titres + abstracts pour faire la table id_art | mot
text <- Articles %>% 
  unite(text, c("title", "abstract")) %>% # Concatene le titre et l'abstract dans la colonne text
  select(ut, text) %>%  # selectionne seulement les colonnes id_art et text
  unique()

text <- text %>% 
  unnest_tokens(word, text, token = "ngrams", n = 1) %>% 
  select(ut, word) %>% 
  drop_na(word) %>% 
  mutate(stemmed = stem_words(word))

# Remove stop words

text <- anti_join(text, 
                  mutate(get_stopwords(),stemmed = stem_words(word)), 
                  by = ("stemmed"))

# TF IDF ----

text <- text %>% 
  group_by(ut, word) %>% 
  summarise(freq = n()) %>% 
  ungroup()

text <- bind_tf_idf(text,word,ut,freq)


# 

ls("package:lsa")
help(textmatrix)


# bibliographic coupling ----
BCNetwork <- sqldf("
      SELECT DISTINCT
        a.id_art as Source,
        b.id_art as Target,
        sum((a.tf_idf+b.tf_idf)/2) as Weight,
        'undirected' as Type
      FROM [table] a
      JOIN [table] b on b.word = a.word and b.id_art != a.id_art
      GROUP BY a.id_art, b.id_art
      ORDER BY sum((a.tf_idf+b.tf_idf)/2)
      ")

BCPeopleNetwork <- sqldf("
      SELECT DISTINCT
        b.name as Source,
        c.name as Target,
        sum(Weight) as Weight,
        'undirected' as Type
      FROM BCNetwork a
      JOIN data b on b.id_art = a.source
      JOIN data c on c.id_art = a.target  and c.name > b.name
      GROUP BY b.name, c.name
      ORDER BY sum(Weight)
      ")


remove(text, idf, n_doc, table)

# Export network files ----
# Because Gephi works best with CSV files, I export the data frame in CSV
# by default write.csv will include the row name (by default the row numbers), so I include row.names = FALSE because I don't want those).

write.csv(BCNetwork,"Networks/hybrid_network_sigmet.csv", row.names = FALSE)

# Nodes ----

# affiliations


nodes <- sqldf("select distinct id_art, Affiliation from data where Affiliation != ''")

nodes <- data.table(nodes)

nodes <- nodes[,c(Affiliation=strsplit(Affiliation, " ", fixed = TRUE)), by = id_art] %>% 
  filter(Affiliation != "")





nodes <- sqldf("
               select a.id_art as id, a.Title as label, a.Year, 
               case when b.id_art is not null then 1 else 0 end as McGill,
               case when c.id_art is not null then 1 else 0 end as Montreal,
               case when d.id_art is not null then 1 else 0 end as Toronto,
               case when e.id_art is not null then 1 else 0 end as UBC,
               case when f.id_art is not null then 1 else 0 end as Alberta,
               case when g.id_art is not null then 1 else 0 end as Ottawa,
               case when h.id_art is not null then 1 else 0 end as Western,
               case when i.id_art is not null then 1 else 0 end as Dalhousie
               from data a
               left join nodes b on b.id_art = a.id_art and b.Affiliation = 'MCGILL-UNIV'
               left join nodes c on c.id_art = a.id_art and c.Affiliation = 'UNIV-MONTREAL'
               left join nodes d on d.id_art = a.id_art and d.Affiliation = 'UNIV-TORONTO'
               left join nodes e on e.id_art = a.id_art and e.Affiliation = 'UNIV-BRITISH-COLUMBIA'
               left join nodes f on f.id_art = a.id_art and f.Affiliation = 'UNIV-ALBERTA'
               left join nodes g on g.id_art = a.id_art and g.Affiliation = 'UNIV-OTTAWA'
               left join nodes h on h.id_art = a.id_art and h.Affiliation = 'UNIV-WESTERN-ONTARIO'
               left join nodes i on i.id_art = a.id_art and i.Affiliation = 'DALHOUSIE-UNIV'
               ")

write.csv(nodes,"Networks/hybrid_network_sigmet_nodes.csv", row.names = FALSE) 

remove(nodes)
# Clustering ----

# Libraries --------------------------------------------------------------


library(igraph)


# Data Preparation -------------------------------------------------------

#Load dataset
edges <- BCNetwork %>% 
  select(Source,Target,Weight) %>% 
  filter(Weight >= 0.5)
  
colnames(edges) <- c("from", "to", "weight")

#Create graph for the algorithms
g <- graph_from_data_frame(edges, directed = FALSE)

remove(edges)

# Community Detection ----------------------------------------------------

# Louvain
lc <- cluster_louvain(g)
membership(lc)
communities(lc)

clusters <- communities(lc)

clustering <- as.data.frame(clusters[[1]]) %>% 
  mutate(cluster = 1) %>% 
  rename("id_art" = "clusters[[1]]")

for (i in 2:length(clusters)){
  x <- as.data.frame(clusters[[i]]) %>% 
          mutate(cluster = i)
  colnames(x) <- c("id_art", "cluster")
  clustering <- rbind(clustering,x)
}

remove(x)

clustering <- clustering %>% 
  mutate(id_art = as.integer(as.character(id_art)))

# Infomap
# imc <- cluster_infomap(g)
# membership(imc)
# communities(imc)
# plot(lc, g)

# Visualizing ----

gephi_cluster <- read.csv("data/gephi_export.csv") %>% 
  rename("id_art"="Id", "cluster"="modularity_class")

gephi_cluster[gephi_cluster$cluster == "28"] <- "20"

x <- sqldf("select distinct cluster, count(distinct id_art) as n from gephi_cluster group by cluster order by count(distinct id_art) desc") %>% 
  filter(n >= 10)


data_final <- data %>% 
  left_join(nodes, by = "id_art") %>% 
  inner_join(gephi_cluster, by="id_art") %>%
  inner_join(x, by="cluster") %>% 
  select(PersonID, id_art, Title, Affiliation = Affiliation.x, Year, cluster) %>% 
  unique()

data_person <- data_final %>% 
  select(PersonID, id_art) %>% 
  unique() %>% 
  group_by(PersonID) %>% 
  mutate(person_total = n()) %>%
  ungroup() %>% 
  mutate(pct_total = 1/person_total) %>% 
  left_join(gephi_cluster, by="id_art") %>% 
  group_by(PersonID,cluster) %>% 
  mutate(cluster_total = n()) %>% 
  mutate(pct_cluster = sum(pct_total)) %>% 
  ungroup() %>% 
  select(PersonID, cluster, pct_cluster) %>% 
  unique()

sqldf("select * from data_person where PersonID = 227")

data_person <- data_final %>% 
  select(PersonID, Affiliation, Year, cluster) %>% 
  unique() %>% 
  left_join(data_person, by=c("PersonID","cluster"))
  
data_univ <- data_person %>% 
  select(Affiliation, cluster, pct_cluster) %>% 
  unique() %>% 
  group_by(Affiliation, cluster) %>% 
  mutate(total = sum(pct_cluster)) %>% 
  ungroup %>% 
  select(Affiliation, cluster, total) %>% 
  unique()


write_delim(data_person,"data/data_person.txt", delim="\t")

write_delim(data_final,"data/data_sigmet.txt", delim="\t") 
write_delim(data_univ,"data/data_univ_sigmet.txt", delim="\t")



x <- sqldf("SELECT DISTINCT b.cluster, a.word, count(distinct a.id_art) as n
           from text_stemmed a
           join gephi_cluster b on b.id_art = a.id_art
           where tf_idf >= 0.1
           group by b.cluster, a.word
           ")
x <- sqldf("Select distinct a.*, row_number() over (partition by a.cluster order by a.n desc) as rk
            from x a 
           ")

y <- 20
filter(x, cluster == y, rk <= 20)
  






