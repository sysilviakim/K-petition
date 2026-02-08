# Libraries ====================================================================
library(MCMCpack)

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
library(assertthat)
library(xtable)
library(xml2)
library(patchwork)
library(viridis)
library(stringi)
library(janitor)
library(ggpubr)

## optional: only needed by scraping/NLP scripts
if (requireNamespace("RSelenium", quietly = TRUE)) {
  library(RSelenium)
  library(netstat)
}
if (requireNamespace("KoNLP", quietly = TRUE)) {
  library(KoNLP)
}

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
  if (length(meta) > 1) {
    title_text <- meta[[1]] ## 제목
    area <- meta[[2]] ## 분야
    attachment <- meta[[3]] ## 평점 또는 첨부파일
    status <- meta[[4]] ## 추진상황
  } else if (length(meta) == 1) {
    ## 실시제안
    title_text <- x$title[[1]]
    branch <- meta
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
  if (is.null(branch)) {
    branch <- misc[which(area == misc) + 1]
  }

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

get_mode <- function(x) {
  x %>%
    table() %>%
    which.max() %>%
    names() %>%
    as.numeric()
}

irt_summ <- function(param_name, posterior) {
  draws <- posterior[, grep(param_name, colnames(posterior))]
  list(
    median = apply(draws, 2, median),
    q025 = apply(draws, 2, quantile, prob = 0.025),
    q975 = apply(draws, 2, quantile, prob = 0.975)
  )
}

theta_post_viz <- function(stats_summ,
                           shape = NULL, color = NULL, label = FALSE) {
  if (!is.null(shape) & is.null(color)) {
    p <- ggplot(
      stats_summ$theta,
      aes(x = theta1_median, y = theta2_median, shape = !!sym(shape))
    )
  } else if (is.null(shape) & !is.null(color)) {
    p <- ggplot(
      stats_summ$theta,
      aes(x = theta1_median, y = theta2_median, color = !!sym(color))
    )
  } else if (!is.null(shape) & !is.null(color)) {
    p <- ggplot(
      stats_summ$theta,
      aes(
        x = theta1_median, y = theta2_median,
        shape = !!sym(shape), color = !!sym(color)
      )
    )
  } else {
    p <- ggplot(
      stats_summ$theta,
      aes(x = theta1_median, y = theta2_median)
    )
  }
  
  if (isTRUE(label)) {
    p <- p + 
      geom_text(aes(label = item), hjust = 0, vjust = 0, color = "black")
  }

  temp <- stats_summ$gamma %>%
    ## Keep only rows with maximum median, minimum median, 
    ## and median of median
    filter(
      median == max(median) | median == min(median) | median == median(median)
    )
  
  if (nrow(temp) == 2) {
    ## Generate the median, as the number of rows was probably even
    temp <- temp %>%
      bind_rows(
        tibble(
          respondent = "",
          median = median(stats_summ$gamma$median)
        )
      )
  }
  
  p <- p +
    geom_point() +
    xlim(-3, 3) +
    ylim(-3, 3) +
    xlab("Theta 1D") +
    ylab("Theta 2D") +
    theme_minimal() +
    ## Apply arrows to show item parameters overlayed with respondent vectors
    geom_segment(
      data = temp,
      aes(
        x = 0, y = 0,
        xend = cos(median),
        yend = sin(median)
      ),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.1, "inches")),
      color = "red"
    ) +
    scale_color_viridis_d(end = .85) +
    theme(legend.position = "bottom", legend.box = "vertical")
  return(p)
}

prop <- function(df, vars, digit = 1, sort = NULL, head = NULL, print = TRUE,
                 useNA = "ifany") {
  if (length(vars) > 2) {
    stop("Too many variables.")
  }
  if (length(vars) < 1) {
    stop("Invalid vars argument.")
  }
  if (!(useNA %in% c("no", "ifany", "always"))) {
    stop("Invalid useNA argument.")
  }
  
  if (length(vars) == 1) {
    temp <- prop.table(table(df[[vars]], dnn = vars, useNA = useNA)) * 100
  }
  if (length(vars) == 2) {
    temp <- prop.table(
      table(df[[vars[1]]], df[[vars[2]]], dnn = vars, useNA = useNA)
    ) * 100
  }
  
  if (!is.null(sort)) {
    temp <- sort(temp, decreasing = sort)
  }
  if (!is.null(head)) {
    temp <- head(temp, head)
  }
  
  temp <- formatC(temp, format = "f", digits = digit)
  if (print) {
    print(temp, quote = FALSE)
  } else {
    return(temp)
  }
}

# Global objects ===============================================================
## Population weights (KOSIS)
## https://kosis.kr/visual/populationKorea/
##   PopulationPyramidDetail.do
demo_weight <- tibble(
  gender = rep(c("M", "F"), each = 7),
  age_group = rep(
    c(
      "20-29", "30-39", "40-49",
      "50-59", "60-69", "70-79", "80+"
    ),
    2
  ),
  population = c(
    2131906, 1421062, 1055723,
    698026, 372092, 126042, 21092,
    2123879, 1505391, 1049241,
    776204, 492048, 195551, 38090
  )
) %>%
  ## Collapse 60+ to match survey SQ2_2 coding
  mutate(
    age_group = ifelse(
      age_group %in% c("60-69", "70-79", "80+"),
      "60+",
      age_group
    )
  ) %>%
  group_by(gender, age_group) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  ) %>%
  mutate(weight = population / sum(population)) %>%
  select(gender, age_group, weight)
