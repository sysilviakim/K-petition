## Data collection: crawl 국민신문고 realized petitions list
source(here::here("R", "utilities.R"))

# 실시 제안(realized petitions) titles =========================================
# Setup ========================================================================
rd <- rsDriver(browser = "firefox", chromever = NULL, port = free_port())
remDr <- rd$client
url <- "https://www.epeople.go.kr/nep/prpsl/realize/selectXclncPrpslList.npaid"
remDr$open()
remDr$navigate(url)

# Realized petitions: content ==================================================
## Initialize petition content list
## Nested list: page number -> petition number -> content
rel_petition_content <- vector("list", max_pages)

for (yr in seq(2024, 2013)) {
  cat("Begin year", yr, "\n")
  
  ## Set #rqstStDt and #rqstEndDt ----------------------------------------------
  remDr$executeScript(
    paste0(
      "document.getElementById('rqstStDt').value = '",
      paste0(yr, "-01-01"), "';",
      "document.getElementById('rqstEndDt').value = '",
      paste0(yr, "-12-31"), "';"
    )
  )
  
  ## Search button. Not sure why it requires [[2]] and not [[1]]
  remDr$findElements(using = "css selector", ".black")[[2]]$clickElement()
  Sys.sleep(5)
  
  ## Total page number ---------------------------------------------------------
  max_pages <- read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(".page_list") %>%
    html_nodes("a") %>%
    html_text() %>%
    as.numeric() %>%
    max(na.rm = TRUE)
  
  ## Initialize petition content list
  ## Nested list: page number -> petition number -> content
  rel_petition_content <- vector("list", max_pages)

  ## Loop over pages that match the year ---------------------------------------
  for (p in 1:max_pages) {
    ## remDr$navigate(paste0(url, pages, p))
    ## this will recent to most recent 3 months
    
    ## First, find the table on the URL
    tab <- remDr$findElement(using = "css selector", ".brd1")
    tab <- tab$getPageSource()[[1]] %>%
      read_html() %>%
      html_table() %>%
      .[[1]]
    
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
      
      ## Go back to the parent page ---> this will also reset to recent 3 months
      remDr$goBack()
      petitions <- remDr$findElements(using = "css selector", ".left a")
      
      ## Save mid-process (10 petitions at maximum per page)
      if (i == length(petitions)) {
        save(
          rel_petition_content,
          file = here(
            "data", "raw",
            paste0(
              "rel_petition_content_list_", yr, ".Rda"
            )
          )
        )
      }
    }
    
    ## After a full iteration within a page, must click on an ... img... to
    ## progress to the next page
    
    if (p < max_pages) {
      ## Click on the next page button
      img_buttons <- remDr$findElements(using = "css selector", "img")
      ## Not a great approach, but button location is hardcoded
      img_buttons[[length(img_buttons) - 3]]$highlightElement()
      img_buttons[[length(img_buttons) - 3]]$clickElement()
      Sys.sleep(5)
    }
    
    cat("Page", p, "finished.\n")
    save(
      rel_petition_content,
      file = here(
        "data", "raw",
        paste0(
          "rel_petition_content_list_", yr, ".Rda"
        )
      )
    )
  }
  
  cat("Year ", yr, "finished.\n")
}

## Make sure to deduplicate
