source(here::here("R", "utilities.R"))

# Load latest data =============================================================
list_files <- list.files(
  here("data", "raw"),
  pattern = "pub_petition_content_list_.*\\.Rda", full.names = TRUE
)
load(max(list_files))

# HTML node extraction =========================================================
## Nested list
class(pub_petition_content[[6]])
names(pub_petition_content[[6]][[1]])

out <- pub_petition_content[[6]][[1]]$source %>%
  read_html() ## %>%
  ## html_nodes(".b_content")

## First, need to preserve <br> tags as \n
xml_find_all(out, ".//br") %>% xml_add_sibling("text", "\n")
xml_find_all(out, ".//br") %>% xml_remove()

## 청소년증 필수 발급 변경 제안
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

## Combine into a tibble
pub_content_df <- tibble(
  title = title_text,
  area = area,
  stars = stars,
  status = status,
  sec_titles = sec_titles,
  sec_content = sec_content
) %>%
  pivot_wider(
    names_from = sec_titles,
    values_from = sec_content
  )