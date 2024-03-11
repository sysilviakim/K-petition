## Preprocessing using KoNLP
## "Thu Feb 15 14:25:15 2024"

source(here::here("R", "utilities.R"))

## apply KoNLP POS tagging
years <- 2002:2023
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
    ## remove special characters
    mutate(bodytext = str_replace_all(bodytext, "[^[:alnum:]]", " ")) %>%
    ## remove punctuations
    mutate(bodytext = str_replace_all(bodytext, "[[:punct:]]", " ")) %>%
    ## remove numbers
    mutate(bodytext = str_replace_all(bodytext, "[0-9]", " ")) %>%
    ## remove the resulting extra white spacing
    mutate(bodytext = str_replace_all(bodytext, "\\s+", " "))

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

  cat("Year", year, "complete.\n")
}


# Tidytext =====================================================================
## extract nouns to create unique(feature list)
voca <- unlist(sapply(petition$bodytext, extractNoun, USE.NAMES = FALSE))
tb.voca <- sort(table(voca), decreasing = TRUE)
tb.voca[1:10]

## a more precise preprocessing with KoNLP
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

tb.voca <- table(voca.df.out$pos_cleaned)
voca.ko <- names(tb.voca)
tb.voca.sorted <- sort(tb.voca, decreasing = TRUE)
tb.voca.sorted[1:10]


# Tentative rules for further preprocessing ====================================
## 1. special characters (underbar, emoji, semicolon, hyphen, comma, ...) ------
voca.df.out$pos_cleaned <- str_replace_all(
  string = voca.df.out$pos_cleaned, pattern = "[[:punct:]]", replace = ""
)
voca.df.out <- distinct(voca.df.out)
voca.df.out <- voca.df.out %>% filter(pos_cleaned != "")

voca.df.out <- voca.df.out %>%
  filter(!grepl("\\^|<|>|~", pos_cleaned))

## 2. remove words that contain numbers (dates, counts) ------------------------
voca.df.out <- voca.df.out %>%
  filter(!grepl("[0-9]", pos_cleaned))

tb.voca <- table(voca.df.out$pos_cleaned)
voca.ko <- names(tb.voca)
tb.voca.sorted <- sort(tb.voca, decreasing = TRUE)
tb.voca.sorted[1:10]
