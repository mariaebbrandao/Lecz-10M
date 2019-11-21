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

# Selecting needed columns and filtering rows
lecztable<-read_csv("C:/Users/mb084896/Desktop/LECZ 10M/Data/merit_smod_statistics_8_13_19_update_v2.csv")%>%
  filter(elevations=="merit") 
  
  
testdata <- lecztable %>% mutate(newcol=case_when(lecz=="0 to 5 Meters"~" 0 to 10 Meters",
                                                  lecz=="6 to 10 Meters"~"0 to 10 Meters",
                                                  lecz=="Over 10 Meters"~"Over 10 Meters"))

names(lecztable)

# Get all categories in column
categories <- unique(lecztable$elevations) 
categories <- unique(lecztable$lecz) 

# Get unique, length of unique, and frequency
table(lecztable$elevations)
table(lecztable$lecz)


