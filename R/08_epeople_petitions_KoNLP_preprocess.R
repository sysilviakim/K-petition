## Preprocessing using KoNLP
## "Thu Feb 15 14:25:15 2024"

source(here::here("R", "utilities.R"))

# Load cleaned data and preserve the raw text ==================================
petition_raw <- readRDS(here("data", "tidy", "petition_konlp_all_years.rds"))

## In earlier years, 현황, 개선방안 not parsed well ---> all in body_problem
## but in later years, the other body fields actually contain something

petition_raw$body_problem_raw <- petition_raw$body_problem
petition_raw$body_proposal_raw <- petition_raw$body_proposal
petition_raw$body_expectation_raw <- petition_raw$body_expectation
petition_raw$body_assessment_raw <- petition_raw$body_assessment
petition_raw$body_result_raw <- petition_raw$body_result

# Apply KoNLP POS tagging ======================================================
## clean up petition text (the followings cause error in KoNLP POS tagging)
## 1. nospace petition ---------------------------------------------------------
## SK: ?? I'm not entirely sure what this piece of code is doing
petition <- petition_raw %>%
  filter(str_detect(word(body_problem, 1), "[[:alnum:]]{1,14}"))

deleted <- petition_raw %>%
  filter(!str_detect(word(body_problem, 1), "[[:alnum:]]{1,14}"))

## How many observations were deleted? 4.7% of observations (9,862)
(nrow(petition_raw) - nrow(petition)) / nrow(petition_raw) * 100

## 2. special characters, numbers, ... -----------------------------------------
petition <- petition %>%
  ## remove special characters
  mutate(
    across(contains("body"), ~ str_replace_all(., "[^[:alnum:]]", " "))
  ) %>%
  ## remove punctuations
  mutate(body_problem = str_replace_all(body_problem, "[[:punct:]]", " ")) %>%
  ## remove numbers
  mutate(body_problem = str_replace_all(body_problem, "[0-9]", " ")) %>%
  ## remove the resulting extra white spacing
  mutate(body_problem = str_replace_all(body_problem, "\\s+", " "))

## KoNLP POS tagging -----------------------------------------------------------
voca.df <- petition %>%
  unnest_tokens(pos, body_problem, SimplePos09)

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

# Tidytext =====================================================================
## extract nouns to create unique(feature list)
voca <- unlist(sapply(petition$body_problem, extractNoun, USE.NAMES = FALSE))
tb.voca <- sort(table(voca), decreasing = TRUE)
tb.voca[1:10]

## a more precise preprocessing with KoNLP
voca.df <- petition %>%
  unnest_tokens(pos, body_problem, SimplePos09)

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
