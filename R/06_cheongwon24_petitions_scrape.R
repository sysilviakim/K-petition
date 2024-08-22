## 행정안전부 청원24 (공개청원)
## https://cheongwon.go.kr/portal/petition/open/view?pageIndex=200&searchType=1&slideYn=N&searchKeyword=&searchBgnDe=&searchEndDe=&searchPtnTtl=&searchPtnCn=&searchInstNm=&rlsStts=&type=list
source(here::here("R", "utilities.R"))

## 2,519 public petitions in Cheongwon 24 by Aug 21, 2024
rd <- rsDriver(browser = "firefox", chromever = NULL, port = 6555L)
remDr <- rd$client
root_url <- "https://www.cheongwon.go.kr/portal/petition/open/view?"

# First scrape the titles/URLs =================================================
no_pages <- 210
title_list <- vector("list", length = no_pages)
for (i in seq(no_pages)) {
  url <- paste0(
    root_url,
    "pageIndex=", i,
    "&searchType=1&slideYn=N&searchKeyword=&searchBgnDe=&searchEndDe=",
    "&searchPtnTtl=&searchPtnCn=&searchInstNm=&rlsStts=&type=card"
  )

  remDr$navigate(url)
  Sys.sleep(5)
  temp <- remDr$getPageSource()[[1]]

  title_list[[i]] <- tibble(
    category = read_html(temp) %>% 
      html_nodes(".category") %>%
      html_text(),
    subject = read_html(temp) %>%
      html_nodes(".subject") %>% 
      html_text() %>%
      trimws(),
    visible_text = read_html(temp) %>%
      html_nodes(".text") %>%
      html_text() %>%
      trimws(),
    date = read_html(temp) %>%
      html_nodes(".date") %>%
      html_text() %>%
      gsub("의견수렴기간:\n\t\t\t\t\t\t\t\t\t", "", .),
    URL = read_html(temp) %>%
      html_nodes("a") %>% 
      html_attr("onclick") %>%
      {.[grepl("fn", .)]} %>%
      {gsub("fn_detailView\\('|'\\)", "", .)} %>%
      paste0("https://www.cheongwon.go.kr/portal/petition/open/viewdetail/", .)
  )
  
  if (i %% 10 == 0 | i == no_pages) {
    save(
      title_list,
      file = here("data", "raw", "cheongwon24_title_list.Rda")
    )
  }
  
  Sys.sleep(2.5)
  message(paste0("Page ", i, " scraped."))
}

## Check that all is scraped
assert_that(!any(is.null(title_list)))
title_list %>% map_dbl(nrow) %>% {which(. == 0)}

# Next, scrape the contents ====================================================
title_df <- title_list %>%
  bind_rows()
content_list <- vector("list", length = nrow(title_df))
for (i in seq(nrow(title_df))) {
  remDr$navigate(title_df[i, ]$URL)
  Sys.sleep(5)
  temp <- remDr$getPageSource()[[1]]
  
  ## Opinion aggregation
  ## Red indicates it's finished; blue indicates it's ongoing
  red <- read_html(temp) %>% 
    html_nodes(".red") %>%
    html_text()
  if (length(red) > 0) {
    stage <- red[[1]]
  } else {
    stage <- read_html(temp) %>% 
      html_nodes(".blue") %>%
      html_text() %>%
      .[[1]]
  }
  
  response <- read_html(temp) %>%
    html_nodes(".pet-doc__result") %>%
    html_text() %>% 
    trimws() %>%
    gsub("\\s+", " ", .)
  if (length(response) == 0) {
    response <- NA
  }
  
  process <- read_html(temp) %>%
    html_nodes(".process") %>%
    html_nodes("li") %>% 
    html_text() %>%
    {.[which(grepl("진행", .))]} %>%
    gsub("현재 진행중인 단계", "", .)
  
  possible_date <- read_html(temp) %>%
    html_nodes("p") %>%
    html_text() %>%
    trimws()
  response_date <- ifelse(
    process == "종결",
    ifelse(length(possible_date) > 1, possible_date[[2]], possible_date),
    NA
  )
  
  ## Crude, but only way to do it
  main_text <- read_html(temp) %>%
    html_nodes("p") %>%
    html_text() %>%
    trimws() %>%
    .[[1]]
  if (length(possible_date) == 1) {
    main_text <- read_html(temp) %>%
      html_nodes(".pet-doc__cont") %>%
      html_text() %>%
      trimws() %>%
      .[[1]]
  }
  
  content_list[[i]] <- tibble(
    stage = stage,
    process = process,
    views = read_html(temp) %>% 
      html_nodes(".link") %>% 
      html_nodes("div") %>%
      html_text() %>%
      gsub("조회 수|\\s", "", .) %>%
      as.numeric(),
    category = read_html(temp) %>% 
      html_nodes(".category") %>%
      html_text() %>%
      trimws(),
    subject = read_html(temp) %>%
      html_nodes(".subject") %>% 
      html_text() %>%
      trimws(),
    text = main_text,
    attachments = read_html(temp) %>%
      html_nodes(".pet-doc__file") %>%
      html_nodes("a") %>%
      html_text() %>%
      trimws() %>%
      list(),
    ## No date element; later merge from title_df
    response = response,
    response_date = response_date,
    when_opinion = read_html(temp) %>%
      html_nodes("h3") %>% 
      html_text() %>% 
      trimws() %>% 
      gsub("\\s+", " ", .) %>%
      .[[1]],
    how_opinion = read_html(temp) %>%
      html_nodes("h3") %>% 
      html_text() %>% 
      trimws() %>% 
      gsub("\\s+", " ", .) %>%
      .[[2]],
    comments_total = read_html(temp) %>%
      html_nodes(".total") %>%
      html_text() %>% 
      trimws(),
    ## Looks like comments are repeated; to capture all comments, must scrape
    ## further, going through the pages
    comments_list_top10 = read_html(temp) %>%
      html_nodes(".list") %>%
      html_nodes("li") %>%
      html_text() %>%
      trimws() %>%
      gsub("\\s+", " ", .) %>%
      list(),
    scrape_date = Sys.time()
  )
  
  if (i %% 10 == 0 | i == no_pages) {
    save(
      content_list,
      file = here("data", "raw", "cheongwon24_content_list.Rda")
    )
  }
  
  Sys.sleep(2.5)
  message(paste0("Page ", i, " scraped."))
}

## Check that all is scraped
assert_that(!any(is.null(content_list)))
content_list %>% map_dbl(nrow) %>% {which(. == 0)}

content_df <- content_list %>%
  bind_rows()
