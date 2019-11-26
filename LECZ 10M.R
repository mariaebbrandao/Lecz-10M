# set workspace
setwd("C:/Users/mb084896/Desktop/LECZ 10M/script")


install.packages("ggraph")
install.packages("data.tree")
install.packages("treemap")

library("ggraph")
library("igraph") 
library("dplyr")
library("ggplot2")
library("data.tree")
library("treemap")
library("tidyverse")
library("tidygraph")
library("viridis")

# Filter by elevation type - Merit
lecztable<-read_csv("C:/Users/mb084896/Desktop/LECZ 10M/Data/merit_smod_statistics_8_13_19_update_v2.csv")%>%
  filter(elevations=="merit") 

names(lecztable)

# Get all categories in column
categories <- unique(lecztablecircle$smodClasses) 
categories <- unique(lecztable$lecz) 

# Get unique, length of unique, and frequency
table(lecztable$elevations)
table(lecztable$lecz)

# 1990 GHSPOP - Select needed columns. Create new column with lecz and smodclasses grouping, filter out N/A in CountryName
# Lecz group 0 to 5 and 6 to 10, into 0 to 10 meters
# Smodclasses group Water and Rural into Rural
lecztablecircle<- lecztable %>% mutate(lecznew=case_when(lecz=="0 to 5 Meters"~" 0 to 10 Meters",
                                                  lecz=="6 to 10 Meters"~"0 to 10 Meters",
                                                  lecz=="Over 10 Meters"~"Over 10 Meters"))%>%
  mutate(smodClassesnew=case_when(smodClasses=="Water"~"Rural",
                                         smodClasses=="Rural"~"Rural", 
                                         smodClasses=="Urban Center"~"Urban Center",
                                         smodClasses=="Urban Cluster"~"Urban Cluster"))%>% 
  filter(!is.na(CountryName))%>% 
  select(smodClassesnew,lecznew,SUM_ghspop_1990) %>% select_all(toupper) 

glimpse(lecztablecircle)

# Add groups together - 0 to 10 meters Rural, Urban Center, Urban Cluster####################
summarise_at(group_by(lecztablecircle,to_2),vars(SUM_GHSPOP_1990),funs(sum(.,na.rm=TRUE)))

# Create levels 

lecztablecircle$from_1 = "SMOD"
lecztablecircle$to_1= paste(lecztablecircle$from_1, lecztablecircle$SMODCLASSESNEW,sep = ".")
lecztablecircle$from_2 = lecztablecircle$to_1
lecztablecircle$to_2 = paste(lecztablecircle$from_1,lecztablecircle$SMODCLASSESNEW,lecztablecircle$LECZNEW,sep = ".")

# Create vertice to include population size
table_vertices <- data.frame(NAME = lecztablecircle$to_2, SHORTNAME = lecztablecircle$LECZNEW, POP = lecztablecircle$SUM_GHSPOP_1990)

# Create new data frame called edgelist
edgelist1<- data.frame(from = lecztablecircle$from_1, to = lecztablecircle$to_1)
edgelist2<- data.frame(from = lecztablecircle$from_2, to = lecztablecircle$to_2)

# Create a new data frame edgelist, appending the two lists above
edgelist<- rbind(edgelist1, edgelist2)
duplicated(edgelist, incomparables = FALSE)
edgelist %>% distinct()

# Drop duplicates from edgelist dataframe
edgelistnoduplicate<-distinct(edgelist)

