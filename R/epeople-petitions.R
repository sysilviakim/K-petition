## Data collection: crawl 국민신문고 petitions list
## Note: the list contains petitions filed within one year
source(here::here("R", "utilities.R"))

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

# Data collection: 공개 제안(public petitions) =================================
url <- "https://www.epeople.go.kr/nep/prpsl/opnPrpl/opnpblPrpslList.npaid"
pages <- "?pageIndex="

## Total page number -----------------------------------------------------------
max_pages <- read_html(url) %>%
  html_nodes(".page_list") %>%
  html_nodes("a") %>% 
  html_text() %>% 
  as.numeric() %>%
  max(na.rm = TRUE)

## Create list of titles per page ----------------------------------------------
pub_petition <- vector("list", max_pages)

for (p in 1:max_pages) {
  html <- read_html(paste0(url, pages, p))
  pub_petition[[p]] <- html %>%
    html_element(".brd1") %>%
    html_table()
  Sys.sleep(3)

  ## Save mid-process ----------------------------------------------------------
  if (p %% 50 == 0 | p == max_pages) {
    cat("p = ", p, "\n")
    save(pub_petition, file = here("output", "pub_petition.Rda"))
  }
}

## Bind rows and save to CSV ---------------------------------------------------
pub_petition <- do.call(rbind, pub_petition)
write.csv(
  file = here("data", "epeople-public-petition-titles.csv"), 
  pub_petition, row.names = FALSE
)

## 실시제안
url <- "https://www.epeople.go.kr/nep/prpsl/realize/selectXclncPrpslList.npaid"
pages <- "?pageIndex="

rel_petition <- NULL
for (p in 1:max_pages) {
  url.p <- paste0(url, pages, p)
  html <- read_html(url.p)
  tables <- html %>%
    html_element(".brd1") %>%
    html_table()

  rel_petition <- rbind(rel_petition, tables)

  if (p %% 50 == 0) cat("p = ", p, "\n")
}

write.csv(file = "data/국민신문고_실시제안.csv", rel_petition, row.names = FALSE)
