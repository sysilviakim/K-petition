## Data collection: crawl 국민신문고 public petitions list
source(here::here("R", "utilities.R"))

# Setup ========================================================================
rd <- rsDriver(browser = "firefox", chromever = NULL, port = 5555L)
remDr <- rd$client
url <- "https://www.epeople.go.kr/nep/prpsl/opnPrpl/opnpblPrpslList.npaid"
remDr$navigate(url)

## Generate weeks within year, from 2013--2023
## Otherwise, too many iterations for Selenium to handle in one go
week_list <- week_list_fxn(2013, 2024)

# 공개 제안(public petitions) content, weekly ==================================
## Loop ------------------------------------------------------------------------
for (wk in week_list) {
  wk <- as.Date(wk, origin = "1970-01-01")
  cat("Begin week", format(wk, "%Y%m%d"), "\n")

  ## Set #rqstStDt and #rqstEndDt ----------------------------------------------
  remDr$executeScript(
    paste0(
      "document.getElementById('rqstStDt').value = '", wk, "';",
      ## Add 6 days to the start date
      "document.getElementById('rqstEndDt').value = '",
      min(wk + 6, ceiling_date(wk, "year") - 1),
      "';"
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

  ## Uh... not sure how to go about this
  ## remDr$findElements(using = "css selector", ".page_list")
  ## remDr$findElements(using = "css selector", ".ds_number")

  ## Initialize petition content list
  ## Nested list: page number -> petition number -> content
  pub_petition_content <- vector("list", max_pages)

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
    pub_petition_content[[p]] <- vector("list", length(petitions))

    ## Loop over petitions
    for (i in 1:length(petitions)) {
      title <- petitions[[i]]$getElementText()
      ## Click petition title
      petitions[[i]]$clickElement()

      ## Scrape content: deal with elements later
      pub_petition_content[[p]][[i]] <- list(
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
          pub_petition_content,
          file = here(
            "data", "raw",
            paste0(
              "pub_petition_content_list_week_", format(wk, "%Y%m%d"), ".Rda"
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
      pub_petition_content,
      file = here(
        "data", "raw",
        paste0(
          "pub_petition_content_list_week_", format(wk, "%Y%m%d"), ".Rda"
        )
      )
    )
  }

  cat("Week", format(wk, "%Y%m%d"), "finished.\n")
}

## Make sure to deduplicate, given the speed at which new petitions come up
