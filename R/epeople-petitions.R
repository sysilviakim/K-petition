## Data collection: crawl 국민신문고 petitions list
## Note: the list contains petitions filed within one year
here::here("R", "utilities.R")

# Description of data source ===================================================
## 국민권익위원회가 운영하는 국민신문고

## The Anti-Corruption & Civil Rights Commission (ACRC) was
## launched on February 29, 2008 by the integration of the Ombudsman of Korea,
## the Korea independent Commission against Corruption,
## and the Administrative Appeals Commission.

## e-People (www.epeople.go.kr) is: A representative online communication
## channel for conveniently filing civil petitions, proposals, participation,
## and reports of budget wastage through the Internet, providing one-stop
## service through close integration with all administrative agencies (central,
## local government, education offices, overseas agencies), Ministry of Justice,
## and major public institutions.

## See https://www.acrc.go.kr/menu.es?mid=a20102000000 for ACRC's history
## and https://www.epeople.go.kr/petition/pps/pps.npaid for the e-people page

# Types of petitions ===========================================================
## 일반 제안(general petitions) ---> private, not available for viewing/scraping
## 공모 제안(invited petitions) ---> discontinued Jan 2, 2024
## 공개 제안(public petitions) ---> available for viewing/scraping
## 실시 제안(realized petitions) ---> available for viewing/scraping

# Data collection ==============================================================
## 공개제안
url <- "https://www.epeople.go.kr/nep/prpsl/opnPrpl/opnpblPrpslList.npaid"
pages <- "?pageIndex="

pub.complaint <- NULL
for (p in 1:287) {
  url.p <- paste0(url, pages, p)
  html <- read_html(url.p)
  tables <- html %>%
    html_element(".brd1") %>%
    html_table()

  pub.complaint <- rbind(pub.complaint, tables)

  if (p %% 50 == 0) cat("p = ", p, "\n")
}

write.csv(file = "data/국민신문고_공개제안.csv", pub.complaint, row.names = FALSE)

## 실시제안
url <- "https://www.epeople.go.kr/nep/prpsl/realize/selectXclncPrpslList.npaid"
pages <- "?pageIndex="

rel.complaint <- NULL
for (p in 1:287) {
  url.p <- paste0(url, pages, p)
  html <- read_html(url.p)
  tables <- html %>%
    html_element(".brd1") %>%
    html_table()

  rel.complaint <- rbind(rel.complaint, tables)

  if (p %% 50 == 0) cat("p = ", p, "\n")
}

write.csv(file = "data/국민신문고_실시제안.csv", rel.complaint, row.names = FALSE)
