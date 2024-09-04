## toy run to test GPT API
source(here::here("R", "utilities.R"))

# Toy run for 2010 =============================================================
## get petitions with no spacing
## feed into API and add spacing
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
## options(encoding = "UTF-8")
## Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# Run GPT API ==================================================================
if (Sys.info()[["nodename"]] == "SNUKIM") {
  rgpt_authenticate("SK_api_key.txt")
} else {
  ## api_key <- unlist(read.table("BK_api_key.txt"))
  rgpt_authenticate("BK_api_key.txt")
}

prompt_text <- paste0(
  "한국어 문장의 잘못된 띄어쓰기를 교정하려고 한다. ", 
  "아래 문단의 잘못된 띄어쓰기를 교정하여 새로 써줘. \n\n", 
  "우리주변에불우이웃들이학교도못가고있다 \n\n"
)

kor_response <- rgpt(
  prompt_role_var = "user",
  prompt_content_var = prompt_text,
  param_model = "gpt-4o",
  param_temperature = 0.5, 
  ## between 0 and 2. high temperature introduces more randomness, 
  ## low temperature makes it more deterministic
  param_max_tokens = 100,
  param_n = 1
)

kor_response[[1]]$gpt_content
## "우리 주변에 불우 이웃들이 학교도 못 가고 있다."
## works!
