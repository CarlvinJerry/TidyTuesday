setwd("J:\\Personalprojects\\TidyTuesday\\26-Aug-2019 Simpsons Guest Stars")

#Packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggraph)
library(igraph)
library(RColorBrewer)

#get the data
simpsons <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv")

#Episode with most number of guests
epRank <- simpsons %>% count(episode_title, season) %>% filter(season=="27")

#Season with most guests

#Percentage plying themselves etc

# data: edge list
d1 <- data.frame(from="origin", to=paste("group", seq(1,7), sep = ""))

dv <- data.frame(from = epRank$season, to = epRank$episode_title)

d2 <- data.frame(from=rep(d1$to, each=7), to=paste("subgroup", seq(1,49), sep="_"))

dc <-  data.frame(from=epRank$episode_title, to=  as.character( epRank$n))


edges <- rbind(d1, d2)

edge <- rbind(dv,dc)


# We can add a second data frame with information for each node!
name <- unique(c(as.character(edges$from), as.character(edges$to)))
vertices <- data.frame(
  name=name,
  group=c( rep(NA,8) ,  rep( paste("group", seq(1,7), sep=""), each=7)),
  cluster=sample(letters[1:4], length(name), replace=T),
  value=sample(seq(10,30), length(name), replace=T)
)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices)















# create a data frame
data <- data.frame(
  level1="season",
  level2=c( rep("boss1",4), rep("boss2",4)),
  level3=paste0("mister_", letters[1:8])
)

# transform it to a edge list!
edges_level1_2 <- epRank %>% select(season, episode_title) %>% unique %>% rename(from=season, to=episode_title)
edges_level2_3 <- epRank %>% select(episode_title, n) %>% unique %>% rename(from=episode_title, to=n)
edge_list = rbind(edges_level1_2, edges_level2_3)

# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()
