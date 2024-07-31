## http://webarchives.pa.go.kr/19th/www.president.go.kr/petitions
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
  Sys.sleep(5)
  message(paste0("Page ", i, " scraped."))
}

## Assert that non of them have zero rows
assert_that(all(sapply(scrape_unanswered_list, nrow) > 0))

## Bind rows
scrape_df_unanswered <- bind_rows(scrape_unanswered_list) %>%
  mutate(answered = FALSE)

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
  
  if (i %% 10 == 0) {
    save(
      scrape_answered_list,
      file = here("data", "raw", "moon_president_petitions_answered.Rda")
    )
  }
  Sys.sleep(5)
  message(paste0("Page ", i, " scraped."))
}

## Assert that non of them have zero rows
assert_that(all(sapply(scrape_answered_list, nrow) > 0))

## Bind rows
scrape_df_answered <- bind_rows(scrape_answered_list) %>%
  mutate(answered = TRUE)

# Now actually go to the URLs and scrape the contents ==========================
for (i in seq(nrow(scrape_df))) {
  url <- scrape_df$URL[i]
  remDr$navigate(paste0("http://webarchives.pa.go.kr", url))
  source_url <- remDr$getPageSource()[[1]]
  
  Sys.sleep(5)
  ## Not sure why but must run again to get the results
  source_url <- remDr$getPageSource()[[1]]
  temp <- read_html(source_url) %>%
    html_nodes(".petitionsView") %>%
    .[[1]] %>%
    html_nodes(".View_write")
  
  scrape_df$content
}
