## http://webarchives.pa.go.kr/19th/www.president.go.kr/petitions
## Moon presidency petitions (Yoon presidency ones cannot be scraped
## as they are not public)
## Jul 8, 2019 to May 9, 2022
source(here::here("R", "utilities.R"))

# Official archive: setup ======================================================
rd <- rsDriver(browser = "firefox", chromever = NULL, port = 5555L)
remDr <- rd$client
root_url <- "http://webarchives.pa.go.kr/19th/www.president.go.kr/petitions/"

## Categories are numbered such: use seq(35, 51)
## 35: 정치개혁
## 36: 외교/통일/국방
## 37: 일자리
## 38: 미래
## 39: 성장동력
## 40: 농산어촌
## 41: 보건복지
## 42: 육아/교육
## 43: 안전/환경
## 44: 저출산/고령화대책
## 45: 행정
## 46: 반려동물
## 47: 교통/건축/국토
## 48: 경제민주화
## 49: 인권/성평등
## 50: 문화/예술/체육/언론
## 51: 기타

# Loop scrapes: ongoing petitions ==============================================
page_count <- 37 ## total 253 that are unanswered
scrape_unanswered_list <- vector("list", page_count)

for (i in seq(page_count)) {
  ## url <- paste0(root_url, "?c=", 35, "&only=1&page=", 1, "&order=1")
  url <- paste0(root_url, "?c=0&only=1&page=", i, "&order=1")
  remDr$navigate(url)

  ## Takes some time to load
  Sys.sleep(5)
  source_url <- remDr$getPageSource()[[1]]
  temp <- read_html(source_url) %>%
    html_nodes(".petition_list") %>%
    .[[1]] %>%
    html_nodes(".bl_wrap")

  scrape_unanswered_list[[i]] <- tibble(
    category = temp %>%
      html_nodes(".bl_category") %>%
      html_text(),
    URL = temp %>%
      html_nodes(".relpy_w") %>%
      html_attr("href"),
    title = temp %>%
      html_nodes(".relpy_w") %>%
      html_text(),
    date = temp %>%
      html_nodes(".bl_date") %>%
      html_text(),
    participants = temp %>%
      html_nodes(".bl_agree") %>%
      html_text()
  )

  save(
    scrape_unanswered_list,
    file = here("data", "raw", "moon_president_petitions_unanswered.Rda")
  )
  Sys.sleep(2.5)
  message(paste0("Page ", i, " scraped."))
}

## Assert that non of them have zero rows
assert_that(all(sapply(scrape_unanswered_list, nrow) > 0))

# Loop scrapes: answered petitions =============================================
## Not quite DRY, but convenient to run separately
page_count <- 65698 ## Manually found
scrape_answered_list <- vector("list", page_count)

for (i in seq(page_count)) {
  url <- paste0(root_url, "?c=0&only=2&page=", i, "&order=1")
  remDr$navigate(url)

  ## Takes some time to load
  Sys.sleep(5)
  source_url <- remDr$getPageSource()[[1]]
  temp <- read_html(source_url) %>%
    html_nodes(".petition_list") %>%
    .[[1]] %>%
    html_nodes(".bl_wrap")

  scrape_answered_list[[i]] <- tibble(
    category = temp %>%
      html_nodes(".bl_category") %>%
      html_text(),
    URL = temp %>%
      html_nodes(".relpy_w") %>%
      html_attr("href"),
    title = temp %>%
      html_nodes(".relpy_w") %>%
      html_text(),
    date = temp %>%
      html_nodes(".bl_date") %>%
      html_text(),
    participants = temp %>%
      html_nodes(".bl_agree") %>%
      html_text()
  )

  if (i %% 10 == 0 | i == page_count) {
    save(
      scrape_answered_list,
      file = here("data", "raw", "moon_president_petitions_answered.Rda")
    )
  }
  Sys.sleep(2.5)
  message(paste0("Page ", i, " scraped."))
}

## Assert that non of them have zero rows; if not, re-run the loop for those
assert_that(scrape_answered_list %>% map_lgl(~ !is.null(.x)) %>% all())
assert_that(all(sapply(scrape_answered_list, nrow) > 0))

# Now actually go to the URLs and scrape the contents ==========================
load(here("data", "raw", "moon_president_petitions_unanswered.Rda"))
load(here("data", "raw", "moon_president_petitions_answered.Rda"))

## Bind rows
scrape_df_unanswered <- bind_rows(scrape_unanswered_list) %>%
  mutate(answered = FALSE) %>%
  ## From each dataframe, extract ID of the petition
  ## i.e., from "/19th/www.president.go.kr/petitions/605368", extract 605368
  mutate(ID = str_extract(URL, "\\d+$"))
scrape_df_answered <- bind_rows(scrape_answered_list) %>%
  mutate(answered = TRUE) %>%
  mutate(ID = str_extract(URL, "\\d+$"))

## Loop: unanswered petitions --------------------------------------------------
scrape_content_unanswered <- vector("list", nrow(scrape_df_unanswered))
names(scrape_content_unanswered) <- scrape_df_unanswered$ID
for (i in seq(nrow(scrape_df_unanswered))) {
  url <- scrape_df_unanswered$URL[i]
  remDr$navigate(paste0("http://webarchives.pa.go.kr", url))

  Sys.sleep(5)
  source_url <- read_html(remDr$getPageSource()[[1]])

  scrape_content_unanswered[[scrape_df_unanswered$ID[i]]] <- tibble(
    title = source_url %>%
      html_nodes(".petitionsView_title") %>%
      .[[1]] %>%
      html_text() %>%
      trimws(),
    category = source_url %>%
      html_nodes(".petitionsView_info_list") %>%
      .[[1]] %>%
      html_nodes("li") %>%
      ## Extract <li>\n<p>카테고리</p>기타</li> -> "기타"
      html_text() %>%
      .[[1]] %>%
      gsub("카테고리", "", .),
    start_date = source_url %>%
      html_nodes(".petitionsView_info_list") %>%
      .[[1]] %>%
      html_nodes("li") %>%
      html_text() %>%
      .[[2]] %>%
      gsub("청원시작", "", .) %>%
      trimws(),
    end_date = source_url %>%
      html_nodes(".petitionsView_info_list") %>%
      .[[1]] %>%
      html_nodes("li") %>%
      html_text() %>%
      .[[3]] %>%
      gsub("청원마감", "", .) %>%
      trimws(),
    petitioner = source_url %>%
      html_nodes(".petitionsView_info_list") %>%
      .[[1]] %>%
      html_nodes("li") %>%
      html_text() %>%
      .[[4]] %>%
      gsub("청원인", "", .) %>%
      trimws(),
    status = source_url %>%
      html_nodes(".petitions_txt_ing") %>%
      html_text(),
    text = source_url %>%
      html_nodes(".View_write") %>%
      .[[1]] %>%
      html_text() %>%
      trimws(),
    participants = source_url %>%
      html_nodes(".Reply_area_agree") %>%
      html_text() %>%
      gsub(" 명$|^청원동의 ", "", .),
    URL = url
  )
  if (i %% 10 == 0 | i == nrow(scrape_df_unanswered)) {
    save(
      scrape_content_unanswered,
      file = here(
        "data", "raw", "moon_president_petitions_unanswered_content.Rda"
      )
    )
  }
  Sys.sleep(2.5)
  message(
    paste0("Page ", i, " scraped out of ", nrow(scrape_df_unanswered), ".")
  )
}

scrape_content_unanswered %>%
  bind_rows(.id = "ID") %>%
  write_csv(
    here("data", "raw", "moon_petitions_unanswered_content.csv")
  )

## Loop: answered petitions ----------------------------------------------------
## load(here("data", "raw", "moon_president_petitions_answered_content.Rda"))
scrape_content_answered <- vector("list", 459886)
names(scrape_content_answered) <- scrape_df_answered$ID
## scrape_content_answered %>% map_lgl(~ !is.null(.x)) %>% which() %>% max() 
for (i in seq(179981, nrow(scrape_df_answered))) {
  url <- scrape_df_answered$URL[i]
  remDr$navigate(paste0("http://webarchives.pa.go.kr", url))
  
  Sys.sleep(5)
  source_url <- read_html(remDr$getPageSource()[[1]])
  
  scrape_content_answered[[scrape_df_answered$ID[i]]] <- tibble(
    title = source_url %>%
      html_nodes(".petitionsView_title") %>%
      .[[1]] %>%
      html_text() %>%
      trimws(),
    category = source_url %>%
      html_nodes(".petitionsView_info_list") %>%
      .[[1]] %>%
      html_nodes("li") %>%
      ## Extract <li>\n<p>카테고리</p>기타</li> -> "기타"
      html_text() %>%
      .[[1]] %>%
      gsub("카테고리", "", .),
    start_date = source_url %>%
      html_nodes(".petitionsView_info_list") %>%
      .[[1]] %>%
      html_nodes("li") %>%
      html_text() %>%
      .[[2]] %>%
      gsub("청원시작", "", .) %>%
      trimws(),
    end_date = source_url %>%
      html_nodes(".petitionsView_info_list") %>%
      .[[1]] %>%
      html_nodes("li") %>%
      html_text() %>%
      .[[3]] %>%
      gsub("청원마감", "", .) %>%
      trimws(),
    petitioner = source_url %>%
      html_nodes(".petitionsView_info_list") %>%
      .[[1]] %>%
      html_nodes("li") %>%
      html_text() %>%
      .[[4]] %>%
      gsub("청원인", "", .) %>%
      trimws(),
    status = source_url %>%
      html_nodes(".petitions_txt_ing") %>%
      html_text(),
    text = source_url %>%
      html_nodes(".View_write") %>%
      .[[1]] %>%
      html_text() %>%
      trimws(),
    participants = source_url %>%
      html_nodes(".Reply_area_agree") %>%
      html_text() %>%
      gsub(" 명$|^청원동의 ", "", .),
    URL = url
  )
  if (i %% 10 == 0 | i == nrow(scrape_df_answered)) {
    save(
      scrape_content_answered,
      file = here(
        "data", "raw", "moon_president_petitions_answered_content.Rda"
      )
    )
  }
  Sys.sleep(2.5)
  message(
    paste0("Page ", i, " scraped out of ", nrow(scrape_df_answered), ".")
  )
}

scrape_content_answered %>%
  bind_rows(.id = "ID") %>%
  write_csv(
    here("data", "raw", "moon_petitions_answered_content.csv")
  )

