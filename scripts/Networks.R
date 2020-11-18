#***********************
# Project: Mapping LIS
# Description: This code generates the network files for Gephi
# Author: Philippe Mongeon
# Date: 2020-07-31
#***********************

# Load needed packages ----
library(sqldf)
library(tidyverse)
library(readxl)

# Working directory
setwd("C:/Users/Philippe/Documents/GitHub/shark-project")

# Load data ----
Articles <- read_excel("shark_data.xlsx", sheet ="papers")
Citations <- read_excel("shark_data.xlsx", sheet ="citations")



# keyword co-occurence ---- NOT CURRENTLY OPERATIONAL

# CONetwork <- sqldf("
#      SELECT DISTINCT
#        a.keyword as Source,
#        b.keyword as Target,
#        count(distinct a.ut) as Weight,
#        'undirected' as Type
#      FROM Keywords a
#      JOIN keywords b on b.ut = a.ut and b.keyword > a.keyword 
#      WHERE a.keyword != 'NULL' and b.keyword != 'NULL'
#      GROUP BY a.keyword, b.keyword
#      ORDER BY count(distinct a.ut) desc
#      ")

# bibliographic coupling ----
BCNetwork <- sqldf("
      SELECT DISTINCT
        a.ut as Source,
        d.ut as Target,
        count(distinct b.cited_ut) as Weight,
        'undirected' as Type
      FROM Articles a
      JOIN Citations b on b.citing_ut = a.ut
      JOIN Citations c on c.cited_ut = b.cited_ut
      JOIN Articles d on d.ut = c.citing_ut
      WHERE d.ut > a.ut
      GROUP BY a.ut, d.ut
      ORDER BY count(distinct b.cited_ut) desc
      ")

# Co-citation ----
CCNetwork <- sqldf("
      SELECT DISTINCT
        a.ut as Source,
        d.ut as Target,
        count(distinct b.citing_ut) as Weight,
        'undirected' as Type
      FROM Articles a
      JOIN Citations b on b.cited_ut = a.ut
      JOIN Citations c on c.citing_ut = b.citing_ut
      JOIN Articles d on d.ut = c.cited_ut
      WHERE d.ut > a.ut
      GROUP BY a.ut, d.ut
      ORDER BY count(distinct b.cited_ut) desc
      ")

# Direct citation ----
DCNetwork <- sqldf("
      SELECT DISTINCT
        a.ut as Source,
        c.ut as Target,
        1 as Weight,
        'directed' as Type
      FROM Articles a
      JOIN Citations b on b.citing_ut = a.ut
      JOIN Articles c on c.ut = b.cited_ut
      ")

# Export network files ----
# Because Gephi works best with CSV files, I export the data frame in CSV
# by default write.csv will include the row name (by default the row numbers), so I include row.names = FALSE because I don't want those).

write.csv(BCNetwork,"networks/BC_network_article.csv", row.names = FALSE) 
write.csv(DCNetwork,"networks/DC_network_article.csv", row.names = FALSE) 
write.csv(CCNetwork,"networks/CC_network_articles.csv", row.names = FALSE) 
# write.csv(CONetwork,"networks/CO_network_keywords.csv", row.names = FALSE) # CO_Network not currently operational

# Clustering ----

# Visualizing ----






