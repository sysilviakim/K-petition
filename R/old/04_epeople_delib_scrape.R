## Data collection: crawl 국민신문고 public petitions list
source(here::here("R", "utilities.R"))

# Description of data source ===================================================
## No English description
## See https://www.epeople.go.kr/nep/thk/mibxIntr/selectLearn.npaid

## 국민생각함이란?
## 국민권익위원회에서 운영하는 온라인 “정책소통공간”으로,
## 일상에서 마주하는 여러가지 공공의제에 대해 서로 생각을 나누고
## 좋은 생각은 함께 발전시켜 정부정책으로 만들어 갑니다!

## 이렇게 활용해보세요
## 국민
## 쓰레기 분리수거, 정류장 설치 등 일상 속 문제에 대해 함께 대안을 찾고
##   필요한 제도를 제안해요.
## 나와 비슷한 생각에는 공감하고 다른 생각은 인정하며 즐겁게 소통하고 참여해요.
## 혼자 고민하기 어려운 주제는 전문가를 초대해 생각을 키워요.

## 공공기관
## 정책 수립 전 단계에서 국민의견을 수렴하여 국민이 좋아하는,
##   똑똑한 정책을 만들어요.
## 복잡하게 얽혀있는 문제의 실타래를 풀기위해 집단지성을 활용해
##   최적의 해결책을 찾아요.
## 국민들의 관심과 요구를 살펴보고 우수한 정책 아이디어는
##   적극적으로 정책에 반영해요.

## 전문가
## 특정 분야에 전문적 지식과 견문을 갖고 있다면
##   국민생각함 전문가로 활동해주세요.
## * (대상) 교수, 연구원, 전문경력자, 작가, NGO활동가 등
## 기발하고 참신한 아이디어가 정책으로 실현될 수 있도록
##   참가자들의 생각에 날개를 달아주세요.
## 잘못된 정보로 생각이 왜곡되지 않도록 정확한 정보를 알려주고
##   치우침 없는 합의를 이끌어주세요.

## In summary, aiming more for deliberative democracy rather than a one-sided
## request for action from the government

# Setup ========================================================================
## Root URL --------------------------------------------------------------------
url <- "https://www.epeople.go.kr/nep/thk/subj/SubjThinkList.npaid"
pages <- "?pageIndex="

## Total page number -----------------------------------------------------------
max_pages <- read_html(url) %>%
  html_nodes(".page_list") %>%
  html_nodes("a") %>%
  html_text() %>%
  as.numeric() %>%
  max(na.rm = TRUE)

# Web scraping =================================================================
rd <- rsDriver(browser = "firefox", chromever = NULL, port = free_port())
remDr <- rd$client

## Initialize petition content list
## Nested list: page number -> petition number -> content
delib_content <- vector("list", max_pages)

for (p in 1:max_pages) {
  remDr$navigate(paste0(url, pages, p))
  Sys.sleep(5)

  tab <- remDr$findElements(using = "css selector", ".thbox")

  ## Initialize nested list
  delib_content[[p]] <- vector("list", length(tab))

  ## Loop over petitions
  for (i in 1:length(tab)) {
    tab[[i]]$clickElement()
    Sys.sleep(5)

    title <- remDr$findElement(
      using = "css selector", ".details_tit"
    )$getElementText()[[1]]

    ## Get petition content
    delib_content[[p]][[i]] <- list(
      title = title,
      date = ymd(
        remDr$findElement(using = "css selector", "dd")$getElementText()[[1]]
      ),
      source = remDr$getPageSource()[[1]]
    )

    ## Go back to petition list
    remDr$navigate(paste0(url, pages, p))
    Sys.sleep(5)

    ## Click next petition
    tab <- remDr$findElements(using = "css selector", ".thbox")

    ## Save in the middle
    if (i %% 10 == 0 | i == length(tab)) {
      save(
        delib_content,
        file = here(
          "data", "raw",
          paste0(
            "delib_content_list_", format(Sys.Date(), "%Y%m%d"), ".Rda"
          )
        )
      )
    }
  }

  cat("Page", p, "finished.\n")
}
