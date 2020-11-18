#***********************
# Project: Mapping LIS
# Description: This code generates the network files for Gephi
# Author: Philippe Mongeon
# Date: 2020-07-31
#***********************

# Load needed packages ----
library(sqldf)
library(rio) # for the import function

# Load data ----
Articles <- import("data/lis_articles.txt")
Keywords <- import("data/lis_keyword.txt")
Citations <- import("data/lis_citations.txt")
Abstract <- import("data/lis_abstracts.txt")

# keyword co-occurence ----

CONetwork <- sqldf("
      SELECT DISTINCT
        a.keyword as tource,
        b.keyword as target,
        count(distinct a.id_art) as weight,
        'undirected' as type
      FROM Keywords a
      JOIN keywords b on b.id_art = a.id_art and b.keyword > a.keyword 
      WHERE a.keyword != 'NULL' and b.keyword != 'NULL'
      GROUP BY a.keyword, b.keyword
      ORDER BY count(distinct a.id_art) desc
      ")

# bibliographic coupling ----
BCNetwork <- sqldf("
      SELECT DISTINCT
        a.id_art as Source,
        d.id_art as Target,
        count(distinct b.id_art_cite) as Weight,
        'undirected' as Type
      FROM Articles a
      JOIN Citations b on b.id_art_citant = a.id_art
      JOIN Citations c on c.id_art_cite = b.id_art_cite
      JOIN Articles d on d.id_art = c.id_art_citant
      WHERE d.id_art > a.id_art
      GROUP BY a.id_art, d.id_art
      ORDER BY count(distinct b.id_art_cite) desc
      ")

# Co-citation ----
CCNetwork <- sqldf("
      SELECT DISTINCT
        a.id_art as Source,
        d.id_art as Target,
        count(distinct b.id_art_citant) as Weight,
        'undirected' as Type
      FROM Articles a
      JOIN Citations b on b.id_art_cite = a.id_art
      JOIN Citations c on c.id_art_citant = b.id_art_citant
      JOIN Articles d on d.id_art = c.id_art_cite
      WHERE d.id_art > a.id_art
      GROUP BY a.id_art, d.id_art
      ORDER BY count(distinct b.id_art_cite) desc
      ")

# Direct citation ----
DCNetwork <- sqldf("
      SELECT DISTINCT
        a.id_art as Source,
        c.id_art as Target,
        1 as Weight,
        'directed' as Type
      FROM Articles a
      JOIN Citations b on b.id_art_citant = a.id_art
      JOIN Articles c on c.id_art = b.id_art_cite
      ")

# Export network files ----
# Because Gephi works best with CSV files, I export the data frame in CSV
# by default write.csv will include the row name (by default the row numbers), so I include row.names = FALSE because I don't want those).

write.csv(BCNetwork,"Networks/BC_network_article.csv", row.names = FALSE) 
write.csv(DCNetwork,"Networks/DC_network_article.csv", row.names = FALSE) 
write.csv(CCNetwork,"Networks/CC_netwrok_articles.csv", row.names = FALSE) 
write.csv(CONetwork,"Networks/CO_network_keywords.csv", row.names = FALSE) 

# Clustering ----

# Visualizing ----






