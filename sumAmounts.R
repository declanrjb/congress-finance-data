library(tidyverse)
library(xml2)
library(rvest)
library(dplyr)
library(readr)
library(data.table)

scraped_data <- cbind(scraped_data, First=NA)
scraped_data <- cbind(scraped_data, Last=NA)

for (i in 1:length(scraped_data$Senator)) {
  scraped_data$First[i] <- (strsplit(scraped_data$Senator," ")[[i]])[1]
  scraped_data$Last[i] <- (strsplit(scraped_data$Senator," ")[[i]])[length((strsplit(scraped_data$Senator," ")[[i]]))]
  message(i)
}
scraped_data$Senator <- paste(scraped_data$First,scraped_data$Last)
scraped_data <- scraped_data %>% select(!First) %>% select(!Last)

sum_amount <- function(i,nodes) {
  myRow <- nodes[i,]
  myRow$Amount <- unlist(data.frame(scraped_data %>% filter_all(any_vars(. %in% c(myRow$Name)))) %>%
                           summarize(total=sum(parse_number(Total))))
  message(i)
  return(myRow)
}

nodes_list <- lapply(c(1:nrow(nodes)),sum_amount,nodes)
rbindlist(nodes_list) -> nodes