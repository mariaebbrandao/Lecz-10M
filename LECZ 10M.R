# set workspace
setwd("P:/WRI_LECZ2019/LECZ_CircularGraph/Analysis")


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
lecztable<-read_csv("P:/WRI_LECZ2019/LECZ_CircularGraph/Data/merit_smod_statistics_8_13_19_update_v2.csv")%>%
  filter(elevations=="merit") 

names(lecztable)

# 1990 GHSPOP - Select needed columns. Create new column with lecz and smodclasses grouping, filter out N/A in CountryName
# Lecz group 0 to 5 and 6 to 10, into 0 to 10 meters
# Smodclasses group Water and Rural into Rural
lecztablecircle<- lecztable %>% mutate(lecznew=case_when(lecz=="0 to 5 Meters"~"0to10 Meters",
                                                         lecz=="6 to 10 Meters"~"0to10 Meters",
                                                         lecz=="Over 10 Meters"~"Over10Meters"))%>%
  mutate(smodClassesnew=case_when(smodClasses=="Water"~"Rural",
                                  smodClasses=="Rural"~"Rural", 
                                  smodClasses=="Urban Center"~"UrbanCenter",
                                  smodClasses=="Urban Cluster"~"UrbanCluster"))%>% 
  filter(!is.na(CountryName))%>% 
  select(smodClasses,smodClassesnew,lecz,lecznew,SUM_ghspop_2015) %>% select_all(toupper) 


# Create levels
lecztablecircle$SMOD = "SMOD"
lecztablecircle$from_1 = "SMOD"
lecztablecircle$to_1= paste(lecztablecircle$from_1, lecztablecircle$SMODCLASSESNEW,sep = ".")
lecztablecircle$from_2 = lecztablecircle$to_1
lecztablecircle$to_2 = paste(lecztablecircle$from_1,lecztablecircle$SMODCLASSESNEW,lecztablecircle$LECZNEW,sep = ".")

# Create vertice to include population size  
lecz_vertices<- data.frame(name = lecztablecircle$to_2, shortname = lecztablecircle$LECZNEW, SUM_GHSPOP_2015 = lecztablecircle$SUM_GHSPOP_2015)

# Create new data frame called edgelist
edgelist1<- data.frame(from = lecztablecircle$from_1, to = lecztablecircle$to_1)
edgelist2<- data.frame(from = lecztablecircle$from_2, to = lecztablecircle$to_2)

# Create a new data frame edgelist, appending the two lists above
edgelist<- rbind(edgelist1, edgelist2)
duplicated(edgelist, incomparables = FALSE)
edgelist %>% distinct()

# Drop duplicates from edgelist dataframe
edgelistnoduplicate<-distinct(edgelist)

# Aggregate at to_1
lecztablecircleto_1<- summarise_at(group_by(lecztablecircle,to_1),vars(SUM_GHSPOP_2015),funs(sum(.,na.rm=TRUE)))

# Separate variable to_1 into two variables: into and shortname
lecztablecircleto_1<-separate(lecztablecircleto_1, to_1, c("into", "shortname"), remove = FALSE)

# Rename column inlecztablecircleto_1
lecztablecircleto_1<-rename(lecztablecircleto_1, name = to_1)

# Drop one variables:into
lecztablecircleto_1<-select (lecztablecircleto_1,-c(into))

# Aggregate at lecztablecircleto_2
lecztablecircleto_2<- summarise_at(group_by(lecztablecircle,to_2),
                                   vars(SUM_GHSPOP_2015),
                                   funs(sum(.,na.rm=TRUE)))

# Separate variable to_2 into three variables: into, intointo,shortname
lecztablecircleto_2<-separate(lecztablecircleto_2, to_2,
                              c("into", "intointo", "shortname"),
                              remove = FALSE)

# Rename column in lecztablecircleto_2
lecztablecircleto_2<-rename(lecztablecircleto_2, name = to_2)

# Drop two variables:into, intointo
lecztablecircleto_2<-select (lecztablecircleto_2,-c("into", "intointo"))

# Aggregate - sum up POP to get total population for total SMOD representation
lecztablecircle_from1<- summarise_at(group_by(lecztablecircle,from_1),
                                     vars(SUM_GHSPOP_2015),
                                     funs(sum(.,na.rm=TRUE)))
# Rename variable from_1 to name
lecztablecircle_from1<-rename(lecztablecircle_from1, name = from_1)

# Include third variable: shortname
lecztablecircle_from1$shortname<- lecztablecircle_from1$name

# Append lecztablecircleto_1 and _2 to lecz_vertices
lecz_vertices<- rbind(lecztablecircleto_1,lecztablecircleto_2, lecztablecircle_from1)%>% distinct()
typeof(lecz_vertices$SUM_GHSPOP_2015)

# Then we have to make a 'graph' object using the igraph library:
mygraph <- graph_from_data_frame(edgelistnoduplicate, vertices=lecz_vertices)
mygraph

# Make the graph
ggraph(mygraph, layout = 'circlepack', weight = SUM_GHSPOP_2015) + 
  geom_node_circle(aes(fill=depth)) +
  geom_node_text( aes(label=shortname,filter=shortname != "SMOD", color=depth, size=SUM_GHSPOP_2015))+
  coord_fixed()+
  theme(legend.position = "False") + 
  scale_fill_viridis()+
  ggtitle("LECZ
          Smod classes: Urban Center, Urban Cluster, Rural
          Elevations: 0 to 10 Meters & Over 10 Meters
          GHSPOP in 2015")

