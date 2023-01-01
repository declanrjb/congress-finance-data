library(tidyverse)
library(xml2)
library(rvest)
library(dplyr)
library(readr)
library(data.table)
library(jsonlite)

candID <- read_csv("CandidateCIDs.csv")
candID <- candID %>% select(!DistIDRunFor & !FECCandID)
candID <- cbind(candID,Name=NA)
colnames(candID) <- c("opensecrets","CRPName","party","name")
testDf <- as.data.frame(strsplit(candID$CRPName,", "))
transpose <- t(testDf)
transpose <- as.data.frame(transpose)
testDf <- rev(transpose)
colnames(testDf) <- c("firstName","lastName")
candID$name <- paste(testDf$firstName,testDf$lastName)
candID <- candID %>% select(!CRPName)
candID$party <- gsub("D","democrat",candID$party)
candID$party <- gsub("R","republican",candID$party)
candID$party <- gsub("I","independent",candID$party)

house_data <- candID

#house_data <- read_csv("senateOpenSecrets.csv")

tableByCid <- function(cid) {
  opensec_root <- stringr::str_c("https://", "www.opensecrets.org/api/")
  opensec_candContrib <- stringr::str_c(opensec_root, "?method=candContrib")
  opensec_candContrib22 <- stringr::str_c(opensec_candContrib, "&cycle=2022")
  opensec_candContrib22JSON <- stringr::str_c(
    opensec_candContrib22,
    "&output=json"
  )
  opensec_candContrib22JSONapi <- stringr::str_c(
    opensec_candContrib22JSON,
    "&apikey=",
    "c29f82f9616469f855440f37e205296c"
  )
  opensecrets_candContribCall <- stringr::str_c(
    opensec_candContrib22JSONapi,
    "&cid=",cid
  )
  message(httr::http_status(httr::GET(opensecrets_candContribCall))$category == "Success")
  if (httr::http_status(httr::GET(opensecrets_candContribCall))$category == "Success") {
    opensec_json_query <- jsonlite::fromJSON(txt = opensecrets_candContribCall)
    BookContribs <- tibble::as_tibble(opensec_json_query$response$contributors$contributor$`@attributes`)
    colnames(BookContribs) <- c("Contributor","Total","PACs","Individuals")
    BookContribs$Total <- paste("$",BookContribs$Total,sep="")
    BookContribs$Individuals <- paste("$",BookContribs$Individuals,sep="")
    BookContribs$PACs <- paste("$",BookContribs$PACs,sep="")
    return(BookContribs)
  }
  
}

#genDf <- data.frame(matrix(ncol = 5, nrow = 0))
#colnames(genDf) <- c('Senator', 'Contributor', 'Total', 'Individuals', 'PACs')

for(i in 1315:1333) {
  if (!is.na(house_data$opensecrets[i])) {
    message(i/(length(house_data$opensecrets)))
    if (is.data.frame(tableByCid(house_data$opensecrets[i]))) {
      sf <- tableByCid(house_data$opensecrets[i])
      tempRow <- house_data %>% filter_all(any_vars(. %in% c(house_data$opensecrets[i])))
      sf <- cbind(sf,Senator=NA)
      message(house_data$name[i])
      sf$Senator <- house_data$name[i]
      genDf <- rbind(genDf,sf)
    }
  }
}

write.csv(genDf,"allHouseData.csv", row.names = FALSE)
