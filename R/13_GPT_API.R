## toy run to test GPT API

source(here::here("R", "utilities.R"))

## toy run for 2010
## get petitions with no spacing
## feed into API and add spacing
library(gptr)

api_key <- read.table("BK_api_key.txt")

year <- 2012

petition <- read.csv(paste0("data/tidy/pub_petition_content_", year, ".csv"))
names(petition)[8] <- c("bodytext")

petition$year <- substr(petition$date_petitioned, 1, 4)
petition$month <- substr(petition$date_petitioned, 6, 7)

## how to extract petitions with spacing problems?
## 1. extract texts with first word containing more than 10 characters
no_space_petition <- petition %>%
    filter(str_detect(word(bodytext, 1), "[[:alnum:]]{10,}")) 

no_space_petition$bodytext[10]
## "우리주변에불우이웃들이학교도못가고있다"

## set encoding to handle Korean language
options(encoding = "UTF-8")
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

prompt <- "한국어 문장의 잘못된 띄어쓰기를 교정하려고 한다. 아래 문단의 잘못된 띄어쓰기를 교정하여 새로 써줘. \n\n
우리주변에불우이웃들이학교도못가고있다 \n\n"
prompt <- enc2utf8(prompt)

kor_response <- get_response(user_input = prompt, model = "gpt-4o-2024-08-06")
kor_response$choices$message$content;

## what if spacing error takes place in the middle of the petition text?
## extract texts with any word longer than 10 characters?
