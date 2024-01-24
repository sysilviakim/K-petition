## crawl 국민신문고 public petitions list
## note: the list contains petitions filed within one year
## date: "Tue Jan 23 14:56:46 2024"
## BK Kim
library(rvest)
library(stringr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(xml2)

## 공개제안
url <- "https://www.epeople.go.kr/nep/prpsl/opnPrpl/opnpblPrpslList.npaid"
pages <- "?pageIndex="

pub.complaint <- NULL
for(p in 1:287){
  url.p <- paste0(url,pages,p)
  html <- read_html(url.p)
  tables <- html %>%
    html_element(".brd1") %>%
    html_table()
  
  pub.complaint <- rbind(pub.complaint,tables)
  
  if(p %% 50 ==0) cat("p = ",p,"\n")
}

write.csv(file="data/국민신문고_공개제안.csv",pub.complaint,row.names=FALSE)

## 실시제안
url <- "https://www.epeople.go.kr/nep/prpsl/realize/selectXclncPrpslList.npaid"
pages <- "?pageIndex="

rel.complaint <- NULL
for(p in 1:287){
  url.p <- paste0(url,pages,p)
  html <- read_html(url.p)
  tables <- html %>%
    html_element(".brd1") %>%
    html_table()
  
  rel.complaint <- rbind(rel.complaint,tables)
  
  if(p %% 50 ==0) cat("p = ",p,"\n")
}

write.csv(file="data/국민신문고_실시제안.csv",rel.complaint,row.names=FALSE)

