library(tidyverse)
library(xml2)
library(rvest)
library(dplyr)
library(readr)
library(data.table)

options(scipen = 100)

#source("~/Documents/R/House Scraping/openSecretsScrape.R")

scraped_data <- read_csv("allHouseData.csv")

#Assemble a one column data frame of unique nodes
nodes_base <- scraped_data

#nodes_senators <- data.frame(nodes_base$Senator)
#colnames(nodes_senators)[1] <- "Name"
#nodes_senators <- cbind(nodes_senators, firstName=NA)
#nodes_senators <- cbind(nodes_senators, lastName=NA)
#nodes_senators <- cbind(nodes_senators, Elected=NA)

nodes_senators <- read_csv("nodes_senators.csv")


nodes_orgs <- data.frame(nodes_base$Contributor)
colnames(nodes_orgs)[1] <- "Name"
nodes_orgs <- cbind(nodes_orgs,Elected=FALSE)
nodes <- rbind(nodes_senators,nodes_orgs)
nodes <- unique(nodes)

#Fill in total amounts for every entry in nodes
nodes <- cbind(nodes, Amount=NA)

source("~/Documents/R/House Scraping/sumAmounts.R")

amountLimit <- 1000
requiredEdges <- 0

nodes <- nodes %>% filter(Amount != 0)

colnames(nodes)[1] <- "Id"
nodes <- cbind(nodes, Label=NA)
nodes$Label <- nodes$Id
nodes <- cbind(nodes, Size=NA)
nodes$Size <- (nodes$Amount)^(1/10)
nodes <- nodes %>% filter(Amount >= amountLimit)

house_data %>% select(name,party) -> house_data_to_join
colnames(house_data_to_join)[1] <- "Id"

house_data_to_join <- cbind(house_data_to_join, First=NA)
house_data_to_join <- cbind(house_data_to_join, Last=NA)
for (i in 1:length(house_data_to_join$Id)) {
  house_data_to_join$First[i] <- (strsplit(house_data_to_join$Id," ")[[i]])[1]
  house_data_to_join$Last[i] <- (strsplit(house_data_to_join$Id," ")[[i]])[length((strsplit(house_data_to_join$Id," ")[[i]]))]
}
house_data_to_join$Id <- paste(house_data_to_join$First,house_data_to_join$Last)
house_data_to_join <- house_data_to_join %>% select(!First) %>% select(!Last)

nodes <- left_join(nodes,house_data_to_join,by="Id")
nodes[is.na(nodes$party),]$party <- "black"
nodes <- nodes %>% 
  mutate(party=ifelse(party=="republican","red",party)) %>%
  mutate(party=ifelse(party=="democrat","blue",party)) %>%
  mutate(party=ifelse(party=="independent","yellow",party)) %>%
  rename("Color"="party")

nodes$Id <- tolower(nodes$Id)
nodes <- unique(nodes)

edges <- scraped_data
edges <- edges %>% select(!PACs)
edges <- edges %>% select(!Individuals)
colnames(edges)[1] <- "Target"
colnames(edges)[2] <- "Label"
colnames(edges)[3] <- "Source"
edges <- cbind(edges, Type="Undirected")
edges <- cbind(edges, Weight=NA)
edges$Weight <- parse_number(edges$Label)
edges$Weight <- log2(edges$Weight)
edges <- cbind(edges, Amount=NA)
edges$Amount <- parse_number(edges$Label)
edges <- edges %>% filter(Amount >= amountLimit) 
edges <- edges %>% select(!Amount)

message("counting for duplicates")
nodes <- cbind(nodes, Count=NA)
for (i in 1:length(nodes$Id)) {
  if ((length(which(nodes$Id == nodes$Id[i]))) > 1) {
    nodes <- nodes[-c(i),]
  }
  message(i)
}

edges <- edges %>% filter(Weight > 0)
nodes <- nodes %>% select(!Count)
edges$Source <- tolower(edges$Source)
edges$Target <- tolower(edges$Target)

message("cleaning nodes")

nodes <- cbind(nodes, Edged=NA)

for (i in 1:length(nodes$Id)) {
  nodes$Edged[i] <- length(which(edges$Source == nodes$Id[i])) + length(which(edges$Target == nodes$Id[i]))
  message(i)
}

nodes <- nodes %>% filter(Edged > requiredEdges)
nodes <- nodes %>% select(!Edged)

edges <- edges %>% filter(Source %in% nodes$Id & Target %in% nodes$Id)

edges <- unique(edges)

for (i in 1:length(nodes$Elected)) {
  message(i)
  if (nodes[i,]$Elected == TRUE) {
    nodes$ElectColor[i] <- nodes$Color[i]
  } else {
    nodes$ElectColor[i] <- "grey"
  }
}

message("writing csv")

write.csv(nodes,"elected_nodes2022.csv", row.names = FALSE)
write.csv(edges,"elected_edges2022.csv", row.names = FALSE)








