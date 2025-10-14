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


# Run for all data =============================================================
## set up the task
prompt_text0 <- paste0(
  ""
)
rgpt(
  prompt_role_var = "user",
  prompt_content_var = prompt_text0,
  param_model = "gpt-4o",
  param_temperature = 0.5,
  ## between 0 and 2. high temperature introduces more randomness,
  ## low temperature makes it more deterministic
  param_max_tokens = 100,
  param_n = 1
)

## create df
years <- 2002:2023
file.names <- paste0("data/tidy/pub_petition_content_", years, ".csv")
petition_df <- as_tibble(map_dfr(file.names, read_csv))
nrow(petition_df) ## 208494

colnames(petition_df)[7] <- c("bodytext")

## append id and date variable
petition_df$id <- 1:nrow(petition_df)
petition_df$year <- substr(petition_df$date_petitioned, 1, 4)
petition_df$month <- substr(petition_df$date_petitioned, 6, 7)

no_space_petition <- petition_df %>%
  filter(str_detect(word(bodytext, 1), "[[:alnum:]]{10,}"))
nrow(no_space_petition) ## 2566

no_space_petition$nid <- 1:nrow(no_space_petition)
no_space_petition$corrected_bodytext <- NA
for (i in 1:nrow(no_space_petition)) {
  prompt_text <- paste0(
    "한국어 문장의 잘못된 띄어쓰기를 교정하려고 한다. 다음 문장의 잘못된 띄어쓰기를 교정하고 교정된 문장만 출력해줘. \n\n",
    no_space_petition$bodytext[i]
  )

  no_space_petition$corrected_bodytext[i] <-
    rgpt(
      prompt_role_var = "user",
      prompt_content_var = prompt_text,
      param_model = "gpt-4o",
      param_temperature = 0.5,
      ## between 0 and 2. high temperature introduces more randomness,
      ## low temperature makes it more deterministic
      param_max_tokens = 500,
      param_n = 1
    )[[1]]$gpt_content

  if (i %% 500 == 0) cat("i = ", i, "\n")
}


## GPT-generated eval criteria
## "Wed Mar 26 08:23:38 2025"
## load data
petition_df <- read_csv("data/tidy/pub_petition_corrected.csv")
## original text with space correction

## sample
petition2020 <- petition_df %>% filter(year == 2020)
idx <- sample(1:nrow(petition2020), 100)
sub <- petition2020[idx, ]
sub <- sub %>%
  filter(nchar(corrected_bodytext) > 300)

## run GPT API
mykey <- readLines("BK_api_key.txt")
Sys.setenv(
  OPENAI_API_KEY = mykey
)

response_df <- petition2020 %>%
  select(title, corrected_bodytext) %>%
  mutate(GPTeval = NA)

for (i in 1:nrow(sub)) {
  prompt0 <- paste("대한민국 정부는 국민의 다양한 행정적 불만을 해소하기 위해 국민이 직접 정부와 소통할 수 있도록 국민청원제도를 운영하고 있다. 다음은 2020년에 작성된 국민청원이다. \n", sub$corrected_bodytext[i], "\n")
  prompt1 <- paste(
    prompt0,
    "이 국민청원은 효과적인 국민청원인가? 위의 국민청원이 효과적인 청원인지를 평가하고 그 평가 기준을 항목별로 정리하여 답하라."
  )

  response <- create_chat_completion(
    model = "gpt-4o-mini",
    temperature = 0,
    messages = list(
      list(
        "role" = "user",
        "content" = prompt1
      )
    )
  )

  response_df[i, "GPTeval"] <- response$choices$message.content
}

response_df[10, "corrected_bodytext"]
response_df[10, "GPTeval"]
