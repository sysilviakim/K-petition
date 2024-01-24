## Data collection: crawl 국민신문고 realized petitions list
source(here::here("R", "utilities.R"))

# 실시 제안(realized petitions) titles =========================================
url <- "https://www.epeople.go.kr/nep/prpsl/realize/selectXclncPrpslList.npaid"
pages <- "?pageIndex="

## Total page number -----------------------------------------------------------
max_pages <- read_html(url) %>%
  html_nodes(".page_list") %>%
  html_nodes("a") %>%
  html_text() %>%
  as.numeric() %>%
  max(na.rm = TRUE)

## Not entirely sure title-only scraping is worth keeping right now

# Realized petitions: content ==================================================
## Using RSelenium for JavaScript-rendered pages
rd <- rsDriver(browser = "firefox", chromever = NULL, port = 5554L)
remDr <- rd$client
remDr$navigate(url)

## Initialize petition content list
## Nested list: page number -> petition number -> content
rel_petition_content <- vector("list", max_pages)

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
  rel_petition_content[[p]] <- vector("list", length(petitions))
  
  ## Loop over petitions
  for (i in 1:length(petitions)) {
    title <- petitions[[i]]$getElementText()
    ## Click petition title
    petitions[[i]]$clickElement()
    
    ## Scrape content: deal with elements later
    rel_petition_content[[p]][[i]] <- list(
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
        rel_petition_content,
        file = here(
          "data", "raw", 
          paste0(
            "rel_petition_content_list_", format(Sys.Date(), "%Y%m%d"), ".Rda"
          )
        )
      )
    }
  }
  
  cat("Page", p, "finished.\n")
  save(
    rel_petition_content,
    file = here(
      "data", "raw", 
      paste0(
        "rel_petition_content_list_", format(Sys.Date(), "%Y%m%d"), ".Rda"
      )
    )
  )
}

## Make sure to deduplicate
