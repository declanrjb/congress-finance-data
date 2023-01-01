senCands <- read_csv("senateCands2022.csv")
senCands <- cbind(senCands,opensecrets=NA)
for (i in 1:length(senCands$Name)) {
  if (senCands[i,]$Name %in% candID$name) {
    candIdRow <- which(candID$name == senCands[i,]$Name)
    senCands[i,]$opensecrets <- candID[candIdRow,]$opensecrets
  }
}

repsByState <- function(state_code) {
  opensec_root <- stringr::str_c("https://", "www.opensecrets.org/api/")
  opensec_candContrib <- stringr::str_c(opensec_root, "?method=getLegislators")
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