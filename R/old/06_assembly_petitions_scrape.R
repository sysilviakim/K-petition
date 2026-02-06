## Data collection: crawl 국회청원 petitions list
source(here::here("R", "utilities.R"))

# Setup ========================================================================
base_url <- "https://www.assembly.go.kr/portal/"
save_dir <- here("data", "raw", "assembly")
if (!dir.exists(save_dir)) dir.create(save_dir)

## API https://open.assembly.go.kr/portal/data/service/selectServicePage.do
## but not needed

## Firefox RSelenium
fprof <- makeFirefoxProfile(
  list(
    browser.download.dir = save_dir,
    browser.download.folderList = 2L,
    browser.download.manager.showWhenStarting = FALSE,
    browser.helperApps.neverAsk.openFile = "text/csv",
    browser.helperApps.neverAsk.saveToDisk = "text/csv"
  )
)

rd <- rsDriver(
  browser = "firefox", chromever = NULL, port = free_port(),
  ## Specify the download folder
  extraCapabilities = fprof
)
remDr <- rd$client

# If using Chrome instead of Firefox
# chrome.drivers <- binman::list_versions("chromedriver")
# latest.chrome <- chrome.drivers[[1]][length(chrome.drivers[[1]])]
# rd <- rsDriver(
#   browser = "chrome",
#   chromever = latest.chrome,
#   verbose = FALSE,
#   port = free_port()
# )
# remDr <- rd$client

table_url <-
  paste0(base_url, "cnts/cntsCont/dataA.do?cntsDivCd=PTT&menuNo=600248")

# 청원처리현황 및 계류현황 표 다운로드 =========================================
## Get URLs --------------------------------------------------------------------
remDr$navigate(table_url)
Sys.sleep(5)

## Click #subtitle__btn_srvmApi button
remDr$findElement(using = "css", "#subtitle__btn_srvmApi")$clickElement()
Sys.sleep(5)

page_source <- remDr$getPageSource()[[1]]

table_url_list <- read_html(page_source) %>%
  html_elements(xpath = '//*[@id="footer__srvmApi_layer_api__list"]') %>%
  html_elements(xpath = '//*[@class="board_subject"]') %>%
  html_attr("href") %>%
  setdiff(., NA)

## 계류현황 --------------------------------------------------------------------
## URL click (it seems like it changes every time)
## After downloading, need to immediately rename file with today's date
fname <- file.path(
  save_dir, paste0("청원계류현황_", format(Sys.Date(), "%Y%m%d"), ".xls")
)

if (!file.exists(fname)) {
  remDr$navigate(table_url_list[[1]])
  remDr$findElement(using = "css", "#sheet-excel-button")$clickElement()
  Sys.sleep(10)

  assert_that(file.exists(file.path(save_dir, "청원계류현황.xls")))
  file.rename(file.path(save_dir, "청원계류현황.xls"), fname)
}

## 처리현황 --------------------------------------------------------------------
fname <- file.path(
  save_dir, paste0("청원처리현황_", format(Sys.Date(), "%Y%m%d"), ".xls")
)

if (!file.exists(fname)) {
  remDr$navigate(table_url_list[[2]])
  remDr$findElement(using = "css", "#sheet-excel-button")$clickElement()
  Sys.sleep(10)

  assert_that(file.exists(file.path(save_dir, "청원처리현황.xls")))
  file.rename(file.path(save_dir, "청원처리현황.xls"), fname)
}

# Import table for processed petitions =========================================
petition_list <- list(
  processed = "청원처리",
  pending = "청원계류"
) %>%
  imap(
    ~ list.files(save_dir, pattern = paste0(.x, ".*xls"), full.names = TRUE) %>%
      max() %>%
      read_excel()
  )

# Exploratory analysis =========================================================
i <- 1
url <- petition_list$processed$상세보기URL[i]
html <- read_html(url)

## summary of the petition (probably not written by the petitioner)
summary <- html %>%
  html_element("#summaryContentDiv") %>%
  html_text()
summary

petition.text <- html %>%
  html_element("tbody") %>%
  html_element("a")
petition.text

## <!-- 청원원문 -->
## <a href="javascript:openBillFile('https://likms.assembly.go.kr/filegate/servlet/FileGate','C1AD8828-40DF-150C-274E-D5C85775F1B8','0');"><img src="/bill/images/icon/icon_hwp.png" alt='attfile' /></a><a href="javascript:openBillFile('https://likms.assembly.go.kr/filegate/servlet/FileGate','C1AD8828-40DF-150C-274E-D5C85775F1B8','1');"><img src="/bill/images/icon/icon_pdf.png" alt='attfile' /></a>

## petition text in pdf files
## server
pdf_path <- "https://likms.assembly.go.kr/filegate/servlet/FileGate"
## unique identifier of the file
file.id <- "C1AD8828-40DF-150C-274E-D5C85775F1B8"
## mode (0: hwp, 1: pdf)
mode <- "1"

# Processed petitions: loop ====================================================
meta_list <- vector("list", nrow(petition_list$processed))
for (i in 1:nrow(petition_list$processed)) {
  ## url to ith petition
  url <- petition_list$processed$상세보기URL[i]

  ## Navigate to the webpage containing the hyperlink
  remDr$navigate(url)
  page_source <- remDr$getPageSource()[[1]] %>% read_html()

  ## Create metadata table
  petition_number <- page_source %>%
    html_elements("td") %>%
    html_text() %>%
    .[[1]] %>%
    trimws()

  title <- page_source %>%
    html_elements(".titCont") %>%
    html_text()

  petition_summary <- page_source %>%
    html_elements(".boxType01") %>%
    html_text() %>%
    trimws()

  petition_stage <- page_source %>%
    html_elements(".boxType01") %>%
    html_elements("span") %>%
    html_text() %>%
    paste(collapse = "|")

  petition_tables <- remDr$findElements(using = "class name", "tableCol01")

  ## If length is 5,
  ## -- 청원접수정보
  ## -- 소관위 심사정보
  ## -- 소관위 회의정보
  ## -- 본회의 심의정보
  ## -- 처리통지
  ## But let's process them later
  table_list <- petition_tables %>%
    map(
      ~ .x$getElementAttribute("outerHTML")[[1]] %>%
        read_html() %>%
        html_table() %>%
        .[[1]]
    )

  ## 청원원문: locate the petition document (will not be available from source)
  petition_docs <- petition_tables %>%
    map(~ .x$findChildElements(using = "tag name", "a")) %>%
    keep(~ length(.) > 0)

  length(petition_docs) ## 3
  petition_docs %>% map_dbl(length) ## 2 4 6

  meta_list[[i]] <- list(
    petition_number = petition_number,
    title = title,
    petition_summary = petition_summary,
    petition_stage = petition_stage,
    tables = table_list
  )

  ## If two files, one is an hwp and the other a pdf
  ## But in 소관위 회의정보, there may be a summary

  ## Some exceptions:
  ## e.g., [2100086] 여성가족부 폐지 반대에 관한 청원(김**외 50,000인)
  if (length(petition_docs) > 0) {
    for (j in 1:length(petition_docs)) {
      K <- length(petition_docs[[j]])
      if (length(K) == 0) {
        cat("For", i, "th petition, no petition document found.\n")
      } else {
        for (k in 1:K) {
          petition_docs[[j]][[k]]$clickElement()
          Sys.sleep(5)

          ## Close all other tabs except the one active
          tabs <- remDr$getWindowHandles()
          if (length(tabs) > 1) {
            ## Except for the current one, close all windows
            for (tab in tabs[-1]) {
              remDr$switchToWindow(tab)
              remDr$closeWindow()
              remDr$switchToWindow(remDr$getWindowHandles()[[1]])
            }
          }
          Sys.sleep(5)
          ## No need to rename these
        }
      }
    }
  }

  cat("Petition", i, "finished.\n")
  Sys.sleep(3)

  save(
    meta_list,
    file = file.path(
      here("data", "raw"),
      paste0(
        "processed_petitions_meta_list_", format(Sys.Date(), "%Y%m%d"), ".Rda"
      )
    )
  )
}

# Pending petitions: loop ======================================================
meta_list <- vector("list", nrow(petition_list$pending))
for (i in 1:nrow(petition_list$pending)) {
  ## url to ith petition
  url <- petition_list$pending$상세보기URL[i]

  ## Navigate to the webpage containing the hyperlink
  remDr$navigate(url)
  page_source <- remDr$getPageSource()[[1]] %>% read_html()

  ## Create metadata table
  petition_number <- page_source %>%
    html_elements("td") %>%
    html_text() %>%
    .[[1]] %>%
    trimws()

  title <- page_source %>%
    html_elements(".titCont") %>%
    html_text()

  petition_summary <- page_source %>%
    html_elements(".boxType01") %>%
    html_text() %>%
    trimws()

  petition_stage <- page_source %>%
    html_elements(".boxType01") %>%
    html_elements("span") %>%
    html_text() %>%
    paste(collapse = "|")

  petition_tables <- remDr$findElements(using = "class name", "tableCol01")

  ## If length is 5,
  ## -- 청원접수정보
  ## -- 소관위 심사정보
  ## -- 소관위 회의정보
  ## -- 본회의 심의정보
  ## -- 처리통지
  ## But let's process them later
  table_list <- petition_tables %>%
    map(
      ~ .x$getElementAttribute("outerHTML")[[1]] %>%
        read_html() %>%
        html_table() %>%
        .[[1]]
    )

  ## 청원원문: locate the petition document (will not be available from source)
  petition_docs <- petition_tables %>%
    map(~ .x$findChildElements(using = "tag name", "a")) %>%
    keep(~ length(.) > 0)

  length(petition_docs) ## 3
  petition_docs %>% map_dbl(length) ## 2 4 6

  meta_list[[i]] <- list(
    petition_number = petition_number,
    title = title,
    petition_summary = petition_summary,
    petition_stage = petition_stage,
    tables = table_list
  )

  ## If two files, one is an hwp and the other a pdf
  ## But in 소관위 회의정보, there may be a summary

  ## Some exceptions:
  ## e.g., [2100086] 여성가족부 폐지 반대에 관한 청원(김**외 50,000인)
  if (length(petition_docs) > 0) {
    for (j in 1:length(petition_docs)) {
      K <- length(petition_docs[[j]])
      if (length(K) == 0) {
        cat("For", i, "th petition, no petition document found.\n")
      } else {
        for (k in 1:K) {
          petition_docs[[j]][[k]]$clickElement()
          Sys.sleep(5)

          ## Close all other tabs except the one active
          tabs <- remDr$getWindowHandles()
          if (length(tabs) > 1) {
            ## Except for the current one, close all windows
            for (tab in tabs[-1]) {
              remDr$switchToWindow(tab)
              remDr$closeWindow()
              remDr$switchToWindow(remDr$getWindowHandles()[[1]])
            }
          }
          Sys.sleep(5)
          ## No need to rename these
        }
      }
    }
  }

  cat("Petition", i, "finished.\n")
  Sys.sleep(3)

  save(
    meta_list,
    file = file.path(
      here("data", "raw"),
      paste0(
        "pending_petitions_meta_list_", format(Sys.Date(), "%Y%m%d"), ".Rda"
      )
    )
  )
}

# Close the browser session and clean up files =================================
## Close
remDr$close()

## Stop the Selenium server
rd$server$stop()

## Delete duplicated files in save_dir
## Anything with -[0-9].pdf or -[0-9].hwp
file_list <- list.files(save_dir, full.names = TRUE)
file_list <-
  file_list[grepl("-[0-9]{1,2}.pdf$|-[0-9]{1,2}.hwp$", tolower(file_list))]
for (f in file_list) file.remove(f)

file_list <- list.files(save_dir, full.names = TRUE)
file_list <- file_list[
  grepl("\\([0-9]{1,2}\\).pdf$|\\([0-9]{1,2}\\).hwp$", tolower(file_list))
]
for (f in file_list) file.remove(f)
