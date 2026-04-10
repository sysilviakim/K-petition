## Data collection: crawl 국민신문고 public petitions list
source(here::here("R", "utilities.R"))

# Setup ========================================================================
## Run first in terminal:
## drivers/chromedriver-win64/chromedriver.exe --port=5554
library(selenium)

## Angular shell is ~4504 chars; rendered petition pages are >10 000 chars
PAGE_CONTENT_THRESHOLD <- 10000L
PAGE_WAIT_TIMEOUT_S    <- 15L
PAGE_POLL_INTERVAL_S   <- 0.5

## Poll get_page_source() until the SPA has rendered content or timeout
wait_for_page_content <- function(
    client,
    threshold  = PAGE_CONTENT_THRESHOLD,
    timeout_s  = PAGE_WAIT_TIMEOUT_S,
    interval_s = PAGE_POLL_INTERVAL_S) {
  deadline <- proc.time()[["elapsed"]] + timeout_s
  repeat {
    src <- client$get_page_source()
    if (nchar(src) > threshold) return(src)
    if (proc.time()[["elapsed"]] >= deadline) {
      message(
        "wait_for_page_content: timed out after ", timeout_s,
        "s (", nchar(src), " chars)"
      )
      return(src)
    }
    Sys.sleep(interval_s)
  }
}

## Poll until .left a list elements appear (listing page re-rendered)
wait_for_list_elements <- function(
    client,
    timeout_s  = PAGE_WAIT_TIMEOUT_S,
    interval_s = PAGE_POLL_INTERVAL_S) {
  deadline <- proc.time()[["elapsed"]] + timeout_s
  repeat {
    elems <- client$find_elements("css selector", ".left a")
    if (length(elems) > 0L) return(elems)
    if (proc.time()[["elapsed"]] >= deadline) {
      message(
        "wait_for_list_elements: timed out after ", timeout_s,
        "s — no .left a elements found"
      )
      return(elems)
    }
    Sys.sleep(interval_s)
  }
}

client <- SeleniumSession$new(
  browser = "chrome", port = 5554L
)
on.exit(client$close(), add = TRUE)
client$navigate(
  "https://www.epeople.go.kr/nep/prpsl/opnPrpl/opnpblPrpslList.npaid"
)

## Generate weeks within year, from 2025--2025
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

  ## Search button — wait for list to render instead of bare Sys.sleep
  client$execute_script(
    "document.querySelectorAll('.black')[1].click();"
  )
  wait_for_list_elements(client)

  ## Total page number ---------------------------------------------------------
  max_pages <- client$get_page_source() %>%
    read_html() %>%
    html_nodes(".page_list") %>%
    html_nodes("a") %>%
    html_text() %>%
    as.numeric() %>%
    max(na.rm = TRUE)

  ## No pagination rendered means 0 or 1 page of results
  if (!is.finite(max_pages)) max_pages <- 1L

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

    ## Empty week: save marker and skip
    if (length(petitions) == 0) {
      pub_petition_content <- data.frame()
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
      break
    }

    ## Initialize nested list
    pub_petition_content[[p]] <- vector("list", length(petitions))

    ## Loop over petitions
    for (i in seq_len(length(petitions))) {
      title <- petitions[[i]]$get_text()
      ## Scroll into center of viewport, then JS-click to bypass overlays
      client$execute_script(sprintf(
        paste0(
          "document.querySelectorAll('.left a')[%d]",
          ".scrollIntoView({block:'center'});"
        ),
        i - 1L
      ))
      Sys.sleep(0.5)
      client$execute_script(sprintf(
        "document.querySelectorAll('.left a')[%d].click();",
        i - 1L
      ))

      ## Wait for SPA to render petition detail before capturing source
      src <- wait_for_page_content(client)

      pub_petition_content[[p]][[i]] <- list(
        title     = title,
        source    = src,
        page_meta = tab
      )

      ## Go back to the parent page -> this resets to recent 3 months
      client$back()
      ## Wait for listing to re-render before re-finding petition links
      petitions <- wait_for_list_elements(client)

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
      ## Wait for next page list to render
      wait_for_list_elements(client)
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
