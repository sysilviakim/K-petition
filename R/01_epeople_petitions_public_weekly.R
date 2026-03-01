## Data collection: crawl 국민신문고 public petitions list
source(here::here("R", "utilities.R"))

# Setup ========================================================================
## Run first in terminal:
## drivers/chromedriver-win64/chromedriver.exe --port=5554
library(selenium)

client <- SeleniumSession$new(
  browser = "chrome", port = 5554L
)
client$navigate(
  "https://www.epeople.go.kr/nep/prpsl/opnPrpl/opnpblPrpslList.npaid"
)

## Generate weeks within year, from 2013--2025
## Otherwise, too many iterations for Selenium to handle in one go
week_list <- week_list_fxn(2025, 2025)

# 공개 제안(public petitions) content, weekly ==================================
## Loop ------------------------------------------------------------------------
for (wk in week_list) {
  wk <- as.Date(wk, origin = "1970-01-01")
  cat("Begin week", format(wk, "%Y%m%d"), "\n")

  ## Set #rqstStDt and #rqstEndDt ----------------------------------------------
  client$execute_script(
    paste0(
      "document.getElementById('rqstStDt').value = '", wk, "';",
      ## Add 6 days to the start date
      "document.getElementById('rqstEndDt').value = '",
      min(wk + 6, ceiling_date(wk, "year") - 1),
      "';"
    )
  )

  ## Search button. Not sure why it requires [[2]] and not [[1]]
  client$execute_script(
    "document.querySelectorAll('.black')[1].click();"
  )
  Sys.sleep(5)

  ## Total page number ---------------------------------------------------------
  max_pages <- client$get_page_source() %>%
    read_html() %>%
    html_nodes(".page_list") %>%
    html_nodes("a") %>%
    html_text() %>%
    as.numeric() %>%
    max(na.rm = TRUE)

  ## Initialize petition content list
  ## Nested list: page number -> petition number -> content
  pub_petition_content <- vector("list", max_pages)

  ## Loop over pages that match the year ---------------------------------------
  for (p in 1:max_pages) {
    ## First, get page table metadata
    tab <- client$get_page_source() %>%
      read_html() %>%
      html_table() %>%
      .[[1]]

    ## List clickable elements with javascript void
    ## (i.e., the petition titles)
    ## Find clickable links using a class
    petitions <- client$find_elements("css selector", ".left a")

    ## Initialize nested list
    pub_petition_content[[p]] <- vector("list", length(petitions))

    ## Loop over petitions
    for (i in 1:length(petitions)) {
      title <- petitions[[i]]$get_text()
      ## Scroll into center of viewport, then JS-click to bypass overlays
      client$execute_script(sprintf(
        "document.querySelectorAll('.left a')[%d]
          .scrollIntoView({block:'center'});", i - 1L
      ))
      Sys.sleep(0.5)
      client$execute_script(sprintf(
        "document.querySelectorAll('.left a')[%d].click();", i - 1L
      ))

      ## Scrape content: deal with elements later
      pub_petition_content[[p]][[i]] <- list(
        title     = title,
        source    = client$get_page_source(),
        page_meta = tab
      )
      Sys.sleep(5)

      ## Go back to the parent page -> this resets to recent 3 months
      client$back()
      petitions <- client$find_elements("css selector", ".left a")

      ## Save mid-process (10 petitions at maximum per page)
      if (i == length(petitions)) {
        save(
          pub_petition_content,
          file = here::here(
            "data", "raw",
            paste0(
              "pub_petition_content_list_week_",
              format(wk, "%Y%m%d"), ".Rda"
            )
          )
        )
      }
    }

    ## After a full page iteration, click img button to go to next page
    if (p < max_pages) {
      img_buttons <- client$find_elements("css selector", "img")
      ## Not a great approach, but button location is hardcoded
      ## 0-based index for JS: length - 3 (R) → length - 4 (JS)
      client$execute_script(sprintf(
        "document.querySelectorAll('img')[%d].click();",
        length(img_buttons) - 3L
      ))
      Sys.sleep(5)
    }

    cat("Page", p, "finished.\n")
    save(
      pub_petition_content,
      file = here::here(
        "data", "raw",
        paste0(
          "pub_petition_content_list_week_",
          format(wk, "%Y%m%d"), ".Rda"
        )
      )
    )
  }

  cat("Week", format(wk, "%Y%m%d"), "finished.\n")
}

## Make sure to deduplicate, given the speed at which new petitions come up
