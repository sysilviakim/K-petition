## Data collection: crawl 국회청원  petitions list
source(here::here("R", "utilities.R"))

## api
## manual: https://open.assembly.go.kr/portal/data/service/selectServicePage.do
api.url <- "https://open.assembly.go.kr/portal/openapi/ncryefyuaflxnqbqo"
api.key <- "REDACTED_API_KEY_ROTATED"
## pointless, the website allows to download the entire table. no need to access each petition using api

## load table
petition.processed <- read_excel("data/raw/청원처리현황.xls")
N <- nrow(petition.processed)

## toy
i <- 1
url <- petition.processed$상세보기URL[i]

html <- read_html(url)

## summary of the petition (probably not written by the petitioner)
summary <- html %>%
    html_element("#summaryContentDiv") %>%
    html_text()
summary;

petition.text <- html %>%
    html_element("tbody") %>%
    html_element("a")
petition.text;
##<!-- 청원원문 -->
##<a href="javascript:openBillFile('https://likms.assembly.go.kr/filegate/servlet/FileGate','C1AD8828-40DF-150C-274E-D5C85775F1B8','0');"><img src="/bill/images/icon/icon_hwp.png" alt='attfile' /></a><a href="javascript:openBillFile('https://likms.assembly.go.kr/filegate/servlet/FileGate','C1AD8828-40DF-150C-274E-D5C85775F1B8','1');"><img src="/bill/images/icon/icon_pdf.png" alt='attfile' /></a>

## petition text in pdf files
file.path <- "https://likms.assembly.go.kr/filegate/servlet/FileGate" ## server
## unique identifier of the file
file.id <- "C1AD8828-40DF-150C-274E-D5C85775F1B8"
## mode (0: hwp, 1: pdf)
mode <- "1"


## activate RSelenium
binman::list_versions("chromedriver")
## Use appropriate webdriver for your browser (e.g., 'firefox', 'chrome')
rs_driver <- rsDriver(browser = "chrome",
                      chromever = "114.0.5735.90",
                      verbose = FALSE,
                      port=free_port())
remote_driver <- rs_driver$client

for(i in 1:N){
    url <- petition.processed$상세보기URL[i]
    html <- html <- read_html(url)
    
    ## Navigate to the webpage containing the hyperlink
    remote_driver$navigate(url)

    server <- "'https://likms.assembly.go.kr/filegate/servlet/FileGate'"
    petition.location <- html %>%
        html_element("tbody") %>%
        html_element("a")
    unique.id <- stringr::str_split(as.character(petition.location),pattern="\\,")[[1]][2]
    mode <- "'1'" ## pdf
    
    ## Execute JavaScript to simulate a click event on the hyperlink
    remote_driver$executeScript(
                      paste0(
                          "var link = document.querySelector(\"a[href='javascript:openBillFile(",server,",",
                          unique.id,",",
                          mode,
                          ");']\");link.click();"
                      )
                  )
    
    if(i %% 100 == 0) cat("i = ",i,"\n")
}
## Close the browser session
remote_driver$close()

## Stop the Selenium server
driver$server$stop()









    ## Execute JavaScript to simulate a click event on the hyperlink
    remote_driver$executeScript("
    var link = document.querySelector(\"a[href='javascript:openBillFile('https://likms.assembly.go.kr/filegate/servlet/FileGate','C1AD8828-40DF-150C-274E-D5C85775F1B8','1');']\");
    link.click();
")
