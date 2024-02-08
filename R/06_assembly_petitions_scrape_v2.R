## Data collection: crawl 국회청원 petitions list
source(here::here("R", "utilities.R"))

# Setup ========================================================================
base_url <- "https://www.assembly.go.kr/portal/"

## API https://open.assembly.go.kr/portal/data/service/selectServicePage.do
## but not needed

## Firefox RSelenium
fprof <- makeFirefoxProfile(
  list(
    browser.download.dir = here("data", "raw"),
    browser.download.folderList = 2L,
    browser.download.manager.showWhenStarting = FALSE,
    browser.helperApps.neverAsk.openFile = "text/csv",
    browser.helperApps.neverAsk.saveToDisk = "text/csv"
  )
)

rd <- rsDriver(
  browser = "firefox", chromever = NULL, port = 5555L,
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
fname <- here(
  "data", "raw", paste0("청원계류현황_", format(Sys.Date(), "%Y%m%d"), ".xls")
)

if (!file.exists(fname)) {
  remDr$navigate(table_url_list[[1]])
  remDr$findElement(using = "css", "#sheet-excel-button")$clickElement()
  Sys.sleep(10)
  
  assert_that(file.exists(here("data", "raw", "청원계류현황.xls")))
  file.rename(here("data", "raw", "청원계류현황.xls"), fname)
}

## 처리현황 --------------------------------------------------------------------
fname <- here(
  "data", "raw", paste0("청원처리현황_", format(Sys.Date(), "%Y%m%d"), ".xls")
)

if (!file.exists(fname)) {
  remDr$navigate(table_url_list[[2]])
  remDr$findElement(using = "css", "#sheet-excel-button")$clickElement()
  Sys.sleep(10)
  
  assert_that(file.exists(here("data", "raw", "청원처리현황.xls")))
  file.rename(here("data", "raw", "청원처리현황.xls"), fname)
}

# Import table for processed petitions =========================================
petition_list <- list(
  processed = "청원처리",
  pending = "청원계류"
) %>%
  imap(
    ~ list.files(
      here("data", "raw"),
      pattern = paste0(.x, ".*xls"), full.names = TRUE
    ) %>%
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
for (i in 1:nrow(petition_list$processed)) {
  ## url to ith petition
  url <- petition_list$processed$상세보기URL[i]

  ## Navigate to the webpage containing the hyperlink
  remDr$navigate(url)

  ## locate the petition document
  petition.table <-
    remDr$findElement(using = "class name", "tableCol01")
  petition.document <- petition.table$findChildElements(using = "tag name", "a")
  ## petition.link <- petition.document[[2]]$getElementAttribute("href")
  K <- length(petition.document)
  if (length(K) == 0) {
    cat("For", i, "th petition, no petition document found.\n")
  } else {
    petition.document[[K]]$clickElement()
  }

  cat("Petition", i, "finished.\n")
  Sys.sleep(3)
}

# Pending petitions: loop ======================================================


## Close the browser session
remDr$close()

## Stop the Selenium server
rd$server$stop()

