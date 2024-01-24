## Data collection: crawl 국민신문고 public petitions list
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
##   -- Also, uploaded by local govt and not by citizens, so not of our interest
## 공개 제안(public petitions) ---> available for viewing/scraping
## 실시 제안(realized petitions) ---> available for viewing/scraping

# 공개 제안(public petitions) titles ===========================================
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
## Initialize empty list
pub_petition_titles <- vector("list", max_pages)

## Loop over page numbers
for (p in 1:max_pages) {
  html <- read_html(paste0(url, pages, p))
  pub_petition_titles[[p]] <- html %>%
    html_element(".brd1") %>%
    html_table()
  Sys.sleep(5)

  ## Save mid-process
  if (p %% 50 == 0 | p == max_pages) {
    cat("Page", p, "finished.\n")
    save(
      pub_petition_titles,
      file = here(
        "data", "raw", 
        paste0("pub_petition_title_list_", format(Sys.Date(), "%Y%m%d"), ".Rda")
      )
    )
  }
}

## Bind rows and save to CSV ---------------------------------------------------
pub_petition_titles <- do.call(rbind, pub_petition_titles)
write.csv(
  file = here(
    "data", "raw", 
    paste0(
      "pub_petition_titles_", format(Sys.Date(), "%Y%m%d"), ".csv"
    )
  ),
  pub_petition_titles, row.names = FALSE
)

# Public petitions: content ====================================================
## Using RSelenium for JavaScript-rendered pages
rd <- rsDriver(browser = "firefox", chromever = NULL, port = 5555L)
remDr <- rd$client

## Initialize petition content list
## Nested list: page number -> petition number -> content
pub_petition_content <- vector("list", max_pages)

for (p in 1:max_pages) {
  remDr$navigate(paste0(url, pages, p))
  Sys.sleep(5)
  
  ## First, find the table on the URL
  tab <- remDr$findElement(using = "css selector", ".brd1")
  tab <- tab$getPageSource()[[1]] %>% read_html() %>% html_table() %>% .[[1]]
  
  ## List clickable elements with javascript void
  ## (i.e., the petition titles)
  ## Find clickable links using a class
  petitions <- remDr$findElements(using = "css selector", ".left a")
  
  ## Initialize nested list
  pub_petition_content[[p]] <- vector("list", length(petitions))
  
  ## Loop over petitions
  for (i in 1:length(petitions)) {
    title <- petitions[[i]]$getElementText()
    ## Click petition title
    petitions[[i]]$clickElement()

    ## Scrape content: deal with elements later
    pub_petition_content[[p]] <- list(
      title = title,
      source = remDr$getPageSource()[[1]],
      page_meta = tab
    )
    Sys.sleep(5)
    
    ## Go back to the parent page
    remDr$navigate(paste0(url, pages, p))
    petitions <- remDr$findElements(using = "css selector", ".left a")
    
    ## Save mid-process (10 petitions at maximum per page)
    if (i == length(petitions)) {
      save(
        pub_petition_content,
        file = here(
          "data", "raw", 
          paste0(
            "pub_petition_content_list_", format(Sys.Date(), "%Y%m%d"), ".Rda"
          )
        )
      )
    }
  }
  
  cat("Page", p, "finished.\n")
  save(
    pub_petition_content,
    file = here(
      "data", "raw", 
      paste0(
        "pub_petition_content_list_", format(Sys.Date(), "%Y%m%d"), ".Rda"
      )
    )
  )
}

## Make sure to deduplicate, given the speed at which new petitions come up
