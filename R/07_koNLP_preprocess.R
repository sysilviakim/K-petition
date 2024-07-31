## Preprocessing using KoNLP
## "Thu Feb 15 14:25:15 2024"

source(here::here("R", "utilities.R"))

library(KoNLP)

## apply KoNLP POS tagging
years <- 2002:2012
for (year in years) {
  petition <- read.csv(paste0("data/tidy/pub_petition_content_", year, ".csv"))
  names(petition)[7] <- c("bodytext")

  petition$year <- substr(petition$date_petitioned, 1, 4)
  petition$month <- substr(petition$date_petitioned, 6, 7)

  ## clean up petition text (the followings cause error in KoNLP POS tagging)
  ## 1. nospace petition
  petition <- petition %>%
    filter(str_detect(word(bodytext, 1), "[[:alnum:]]{1,14}"))

  ## 2. special characters, numbers, ...
  petition <- petition %>%
    mutate(bodytext = str_replace_all(bodytext, "[^[:alnum:]]", " ")) %>% ## remove special characters
    mutate(bodytext = str_replace_all(bodytext, "[[:punct:]]", " ")) %>% ## remove punctuations
    mutate(bodytext = str_replace_all(bodytext, "[0-9]", " ")) %>% ## remove numbers
    mutate(bodytext = str_replace_all(bodytext, "\\s+", " ")) ## remove the resulting extra white spacing

  ## KoNLP POS tagging
  voca.df <- petition %>%
    unnest_tokens(pos, bodytext, SimplePos09)

  ## extract 용언 (불용어 제거)
  voca.df.n <- voca.df %>%
    filter(str_detect(pos, "/n")) %>%
    mutate(pos_cleaned = str_remove(pos, "/.*$"))

  ## 어미 통일
  voca.df.p <- voca.df %>%
    filter(str_detect(pos, "/p")) %>%
    mutate(pos_cleaned = str_replace_all(pos, "/.*$", "다"))

  ## n + p
  voca.df.out <- bind_rows(voca.df.n, voca.df.p) %>%
    filter(nchar(pos_cleaned) > 1) %>%
    filter(nchar(pos_cleaned) < 10) %>%
    select(title, area, year, month, pos_cleaned)

  saveRDS(file = paste0("data/tidy/petition_konlp", year, ".rds"), voca.df.out)

  cat("years ", year, " complete \n")
}

## 2013 - present
file.list <- list.files("data/raw", pattern = "pub_petition_content_list_week_")

for (file in file.list) {
  file.path <- paste0("data/raw/", file)
  load(file.path) ## sth wrong with crawling? body text shows full html scrips
}
