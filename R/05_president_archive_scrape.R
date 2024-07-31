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

# Loop and save through scrapes ================================================
## Because these are only valid if they have gathered 200,000 signatures,
## only a few hundred petitions are available for scraping
page_count <- 37
scrape_list <- vector("list", page_count)

for (i in seq(page_count)) {
  ## url <- paste0(root_url, "?c=", 35, "&only=1&page=", 1, "&order=1")
  url <- paste0(root_url, "?only=1&page=", i, "&order=1")
  remDr$navigate(url)
  source_url <- remDr$getPageSource()[[1]]
  temp <- read_html(source_url) %>%
    html_nodes(".petition_list") %>%
    .[[1]] %>%
    html_nodes(".bl_wrap")
  
  scrape_list[[i]] <- tibble(
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
  
  Sys.sleep(5)
  message(paste0("Page ", i, " scraped."))
}

save(scrape_list, file = here("data", "raw", "moon_president_petitions.Rda"))
