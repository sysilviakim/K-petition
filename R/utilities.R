# Libraries ====================================================================
## tidyverse
library(plyr)
library(tidyverse)
library(lubridate)
library(rvest)
library(here)
library(tidytext)
library(readxl)
library(writexl)

## others
library(RSelenium)
library(assertthat)
library(xtable)
library(xml2)
library(readxl)
library(netstat)
library(patchwork)

## https://mrchypark.github.io/post/KoNLP-설치-방법/
## devtools::install_github('haven-jeon/KoNLP')
## https://blog.naver.com/song_sec/221800361879
library(KoNLP)

# Functions ====================================================================
extract_pt_content <- function(x, date = NULL) {
  ## List of length three that has title, source, and page_meta
  if (length(x) != 3 | !is.list(x)) {
    stop("Input is not the right list.")
  }
  
  ## Initialize as NULL for those which the script will fail
  sec_content <- sec_titles <- title_text <- area <- attachment <- status <- 
    date_implemented <- date_answered <- date_petitioned <- branch <- NULL
  
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
  if (length(meta) > 0) {
    title_text <- meta[[1]] ## 제목
    area <- meta[[2]] ## 분야
    attachment <- meta[[3]] ## 평점 또는 첨부파일
    status <- meta[[4]] ## 추진상황
  }

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
  
  if (length(sec_content) != length(sec_titles)) {
    sec_content <- out %>%
      html_nodes(xpath = "//*[@class='b_conItem']") %>%
      html_text() %>%
      trimws()
  }
  
  if (length(sec_content) != length(sec_titles)) {
    stop("Section title and content lengths do not match.")
  }
  
  ## Missed metadata
  ## The first approach creates too much of a bottleneck
  ## Matching with page_meta is a better approach, 
  ## and let's do it outside the function ---> which has other problems!
  
  misc <- out %>%
    html_nodes("div") %>%
    ## This is to avoid grabbing the entire page
    html_text() %>%
    trimws()

  ## Keep only nodes with short text of under 200 characters
  misc <- misc[nchar(misc) < 200]

  ## Date petitioned: find a pattern such as 2024-01-22
  date_petitioned <- str_extract(misc, "\\d{4}-\\d{2}-\\d{2}")
  ## Unique date
  date_petitioned <- setdiff(unique(date_petitioned), NA)
  
  ## Extremely dirty loop, but the patterns are all over the place
  if (
    ("검토내용" %in% sec_titles) & 
    !("실시 결과" %in% sec_titles) & 
    length(date_petitioned) > 1
  ) {
    ## If there are multiple dates, likely it is the case that the first one is
    ## the date petitioned, and the second one is the date notified of an answer
    if (length(date_petitioned) == 2) {
      date_answered <- max(date_petitioned)
      date_petitioned <- min(date_petitioned)
    } else {
      ## 3 or more
      ## e.g., 2012, page 60, 결빙이 잦은 곳에는 지역 푯말 밑에 긴급연락망 ...
      ## Date recognized from attachment file name
      date_answered <- max(date_petitioned)
      ## Redo pattern recognition
      date_petitioned <- 
        setdiff(unique(str_extract(misc, "^\\d{4}-\\d{2}-\\d{2}$")), NA)
      if (length(date_petitioned) == 2) {
        date_petitioned <- setdiff(date_petitioned, date_answered)
      }
      ## If the length is still larger, stop the loop
      if (length(date_petitioned) > 1) {
        stop("Date petitioned is still problematic.")
      }
    }
  } else if (
    ## This part needs to be thoroughly checked...
    ("검토내용" %in% sec_titles) & 
    ("실시 결과" %in% sec_titles) & 
    length(date_petitioned) > 1
  ) {
    if (length(date_petitioned) == 3) {
      cat("실시 결과 in", i, "\n")
      date_implemented <- max(date_petitioned)
      date_answered <- max(setdiff(date_petitioned, date_implemented))
      date_petitioned <- min(date_petitioned)
    } else if (length(date_petitioned) > 3) {
      ## 4 or more, again, date recognized from attachment file name
      date_implemented <- max(date_petitioned)
      date_answered <- max(setdiff(date_petitioned, date_implemented))
      ## Redo pattern recognition
      date_petitioned <- 
        setdiff(unique(str_extract(misc, "^\\d{4}-\\d{2}-\\d{2}$")), NA)
      if (length(date_petitioned) == 3) {
        date_petitioned <- 
          setdiff(date_petitioned, c(date_answered, date_implemented))
      }
      ## If the length is still larger, stop the loop
      if (length(date_petitioned) > 1) {
        stop("Date petitioned is still problematic.")
      }
    } else {
      ## This is interesting; length is less than 3
      ## Jan 7, 2015: 행정자치부 국가상징(대통령표장)안내 개선
      date_implemented <- date_answered <- max(date_petitioned)
      date_petitioned <- min(date_petitioned)
    }
  } else if (
    !("검토내용" %in% sec_titles) & 
    !("실시 결과" %in% sec_titles) & 
    length(date_petitioned) > 1
  ) {
    ## Still need the max
    date_petitioned <- max(date_petitioned)
  }

  ## Branch of government petitioned to
  ## Messy approach, but find the "area" and take the next string
  branch <- misc[which(area == misc) + 1]
  
  ## Combine into a tibble
  pub_content_df <- tibble(
    title = title_text,
    area = area,
    attachment = attachment,
    status = status,
    date_petitioned = date_petitioned, 
    date_answered = date_answered,
    date_implemented = date_implemented,
    branch = branch,
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

week_list_fxn <- function(year_from, year_to = NULL) {
  if (is.null(year_to)) {
    year_to <- year_from
  }
  week_list <- seq(year_from, year_to) %>%
    map(
      ~ seq(
        as.Date(paste0(.x, "-01-01")), as.Date(paste0(.x, "-12-31")),
        by = "week"
      )
    ) %>%
    unlist() %>%
    as.Date(., origin = "1970-01-01")
  return(week_list)
}

pub_title_wrangle <- function(x) {
  out <- x %>%
    bind_rows() %>%
    Kmisc::dedup() %>%
    group_by(`번호`, `제목`) %>%
    filter(`조회` == max(`조회`)) %>%
    ungroup() %>%
    rename(
      number = `번호`,
      title = `제목`,
      views = `조회`,
      status2 = `추진상황`,
      branch2 = `처리 기관`,
      date_petitioned2 = `신청일`
    )
  return(out)
}
