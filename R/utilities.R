# Libraries ====================================================================
## tidyverse
library(plyr)
library(tidyverse)
library(lubridate)
library(rvest)
library(here)
library(tidytext)

## others
library(RSelenium)
library(assertthat)
library(xtable)
library(xml2)

# Functions ====================================================================
extract_pt_content <- function(x, date = NULL) {
  ## List of length three that has title, source, and page_meta
  if (length(x) != 3 | !is.list(x)) {
    stop("Input is not the right list.")
  }
  
  out <- x$source %>%
    read_html()

  ## First, need to preserve <br> tags as \n
  xml_find_all(out, ".//br") %>% xml_add_sibling("text", "\n")
  xml_find_all(out, ".//br") %>% xml_remove()

  ## 예시: 청소년증 필수 발급 변경 제안 (doc subfolder)
  ## 처리기관: 여성가족부는 cellBig로 안 잡히고 div로만 (불편!)
  ## 신청일, 통지일: 심사 중도 마찬가지. 즉 우측 cell들만 안 잡힘 (불편!)
  ## 분야와 추진상황은 잡히지만 (좌측 cell) 마찬가지로 table 형태가 아니라 불편

  ## Metadata
  meta <- out %>%
    ## div class cellBig
    html_nodes(xpath = "//*[@class='cellBig']") %>%
    html_text() %>%
    gsub("\n|\t", "", .)

  ## I hate hardcoding, but here goes
  title_text <- meta[[1]] ## 제목
  area <- meta[[2]] ## 분야
  stars <- meta[[3]] ## 평점
  status <- meta[[4]] ## 추진상황

  ## Section titles
  sec_titles <- out %>%
    html_nodes(xpath = "//*[@class='b_conTit']") %>%
    html_text() %>%
    trimws()

  ## Section content
  sec_content <- out %>%
    html_nodes(xpath = "//*[@class='b_conItem']") %>%
    html_nodes("div") %>%
    html_text() %>%
    ## Strip multiple whitespaces into one
    trimws()
  
  ## Missed metadata (this is the bottleneck in speed)
  misc <- out %>%
    html_nodes("div") %>%
    ## This is to avoid grabbing the entire page
    html_text() %>%
    trimws()
  
  ## Keep only nodes with short text of under 200 characters
  misc <- misc[nchar(misc) < 200]
  
  ## Date petitioned
  date_petitioned <- misc[grepl("신청일", misc)]
  ## Find a pattern such as 2024-01-22
  date_petitioned <- str_extract(date_petitioned, "\\d{4}-\\d{2}-\\d{2}")
  ## Unique date
  date_petitioned <- unique(date_petitioned)
  
  if (length(date_petitioned) != 1) {
    stop("Check this particular petition.")
    cat("Title:", title_text, "\n")
    cat("Page", p, ", petition number", i, "\n")
  }
  
  ## Branch of government petitioned to
  ## Messy approach, but find the "area" and take the next string
  branch <- misc[which(area == misc) + 1]

  if (length(branch) != 1) {
    stop("Check this particular petition.")
    cat("Title:", title_text, "\n")
    cat("Page", p, ", petition number", i, "\n")
  }
  
  ## Combine into a tibble
  pub_content_df <- tibble(
    title = title_text,
    date = date_petitioned,
    area = area,
    branch = branch,
    stars = stars,
    status = status,
    sec_titles = sec_titles,
    sec_content = sec_content
  ) %>%
    pivot_wider(
      names_from = sec_titles,
      values_from = sec_content
    ) %>%
    mutate(date_scraped = date)
  
  return(pub_content_df)
}