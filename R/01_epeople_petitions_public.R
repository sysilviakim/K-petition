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

# 공개 제안(public petitions): number over the years ===========================
url <- "https://www.epeople.go.kr/nep/prpsl/opnPrpl/opnpblPrpslList.npaid"
pages <- "?pageIndex="

## Using RSelenium for JavaScript-rendered pages -------------------------------
rd <- rsDriver(browser = "firefox", chromever = NULL, port = free_port())
remDr <- rd$client
remDr$navigate(url)

## Set years to scrape ---------------------------------------------------------
## Note that the default is only the last three months
## So to properly scrape from the beginning, the dates must be specified
## Manually checked that the data starts from 2002
years <- seq(2002, 2012)

## One trouble with this approach is that, for years with large data,
## if Selenium stops in the middle, it's hard to reset it
## e.g., 2012 stopped at 429; I must manually find 429 to enable it to restart

total_list <- vector("list", length(years))
names(total_list) <- paste0("year", years)
for (yr in years) {
  cat("Begin", yr, "\n")

  ## Set #rqstStDt and #rqstEndDt
  remDr$executeScript(
    paste0(
      "document.getElementById('rqstStDt').value = '", yr, "-01-01';",
      "document.getElementById('rqstEndDt').value = '", yr, "-12-31';"
    )
  )

  remDr$findElements(using = "css selector", ".black")[[2]]$clickElement()
  Sys.sleep(5)
  source <- remDr$getPageSource()[[1]]

  ## Scrape the total number of petitions filed
  total_list[[paste0("year", yr)]] <- read_html(source) %>%
    html_nodes(".total span") %>%
    html_text()

  ## No need to go back for this one
}

pub_total <- total_list %>% map_dbl(parse_number)
save(pub_total, file = here("data", "raw", "pub_petition_num_total.Rda"))

## Quick figure
p <- pub_total %>%
  enframe(name = "year", value = "total") %>%
  ggplot(aes(x = gsub("year", "", year), y = total)) +
  ## colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(5)
  geom_col(colour = "#C6DBEF", fill = "#C6DBEF") +
  ## Use ggrepel so that text will not overlap ---> bad idea, looks horrid
  geom_text(
    aes(label = formatC(total, format = "d", big.mark = ",")),
    family = "CM Roman", vjust = -0.5, size = 3
  ) +
  labs(
    ## Left align title and subtitle
    title = "Number of Public Petitions Filed",
    subtitle = "2002-2024",
    x = "Year",
    y = "Number of petitions"
  ) +
  scale_y_continuous(labels = scales::comma)
Kmisc::pdf_default(p) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(here("fig", "pub_petition_num_total.pdf"), width = 8, height = 5)

# 공개 제안(public petitions) content ==========================================
## Loop ------------------------------------------------------------------------
for (yr in years) {
  cat("Begin", yr, "\n")

  ## Set #rqstStDt and #rqstEndDt ----------------------------------------------
  remDr$executeScript(
    paste0(
      "document.getElementById('rqstStDt').value = '", yr, "-01-01';",
      "document.getElementById('rqstEndDt').value = '", yr, "-12-31';"
    )
  )

  ## Search button. Not sure why it requires [[2]] and not [[1]]
  remDr$findElements(using = "css selector", ".black")[[2]]$clickElement()
  Sys.sleep(5)

  ## Show more than 10 petitions per page ... never mind
  ## remDr$findElements(using = "css selector", "#listCnt")[[1]]$clickElement()

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
      ## remDr$navigate(paste0(url, pages, p))
      ## petitions <- remDr$findElements(using = "css selector", ".left a")
      remDr$goBack()
      petitions <- remDr$findElements(using = "css selector", ".left a")

      ## Save mid-process (10 petitions at maximum per page)
      if (i == length(petitions)) {
        save(
          pub_petition_content,
          file = here(
            "data", "raw", paste0("pub_petition_content_list_", yr, ".Rda")
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
        "data", "raw", paste0("pub_petition_content_list_", yr, ".Rda")
      )
    )
  }

  cat("Year", yr, "finished.\n")
}

## Make sure to deduplicate, given the speed at which new petitions come up
