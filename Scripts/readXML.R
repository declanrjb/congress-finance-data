library(rvest)
library(dplyr)
library(purrr)
library(xml2)

pg <- read_xml("https://www.opensecrets.org/api/?method=candContrib&cid=N00047923&cycle=2022&apikey=1a3ee8b0f5a96321e1289ca4eed83c26")

pg <- html_elements(pg, xpath="//contributor") %>% xml_text()

#pg <- html_elements(pg, xpath="//name[@primary='true' and @sortindex=1]") %>% xml_text()

pg <- html_nodes(pg, xpath=".//ranks/rank") %>% 
  xml_attrs() %>% 
  map(~as_data_frame(as.list(.))) %>% 
  bind_rows()