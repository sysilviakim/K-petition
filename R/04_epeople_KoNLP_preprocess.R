## Preprocessing using KoNLP
## "Thu Feb 15 14:25:15 2024"

source(here::here("R", "utilities.R"))

# Load cleaned data and preserve the raw text ==================================
petition_raw <- readRDS(here::here("data", "tidy", "petition_konlp_all_years.rds"))

## In earlier years, 현황, 개선방안 not parsed well ---> all in body_problem
## but in later years, the other body fields actually contain something

petition_raw$body_problem_raw <- petition_raw$body_problem
petition_raw$body_proposal_raw <- petition_raw$body_proposal
petition_raw$body_expectation_raw <- petition_raw$body_expectation
petition_raw$body_assessment_raw <- petition_raw$body_assessment
petition_raw$body_result_raw <- petition_raw$body_result

# Apply KoNLP POS tagging ======================================================
## clean up petition text (the followings cause error in KoNLP POS tagging)
## 1. delete nospace petition --------------------------------------------------
## SK: ?? I'm not entirely sure what this piece of code is doing
petition <- petition_raw %>%
  filter(str_detect(word(body_problem, 1), "[[:alnum:]]{1,14}"))

deleted <- petition_raw %>%
  filter(!str_detect(word(body_problem, 1), "[[:alnum:]]{1,14}"))

## How many observations were deleted? 4.7% of observations (9,862)
(nrow(petition_raw) - nrow(petition)) / nrow(petition_raw) * 100

## 2. special characters, numbers, ... -----------------------------------------
petition <- petition %>%
  mutate(
    across(
      contains("body") & !contains("raw"),
      ~ {
        ## remove special characters
        out <- str_replace_all(., "[^[:alnum:]]", " ")
        ## remove punctuations
        out <- str_replace_all(out, "[[:punct:]]", " ")
        ## remove numbers
        out <- str_replace_all(out, "[0-9]", " ")
        ## remove the resulting extra white spacing
        out <- str_replace_all(out, "\\s+", " ")
        return(out)
      }
    )
  )

## KoNLP POS tagging -----------------------------------------------------------
voca_list <- list(
  problem = petition %>% unnest_tokens(pos, body_problem, SimplePos09),
  proposal = petition %>% unnest_tokens(pos, body_proposal, SimplePos09),
  expectation = petition %>% unnest_tokens(pos, body_expectation, SimplePos09),
  assessment = petition %>% unnest_tokens(pos, body_assessment, SimplePos09),
  result = petition %>% unnest_tokens(pos, body_result, SimplePos09)
)
save(voca_list, file = here::here("data", "tidy", "voca_list.Rda"))

## Some error messages:
## java.lang.ArrayIndexOutOfBoundsException:
## Index 5000 out of bounds for length 5000

## extract 용언(/n) (불용어 제거)
noun_list <- voca_list %>%
  map(
    ~ .x %>%
      filter(str_detect(pos, "/n")) %>%
      mutate(pos_cleaned = str_remove(pos, "/.*$"))
  )

## 어미(/p) 통일
p_list <- voca_list %>%
  map(
    ~ .x %>%
      filter(str_detect(pos, "/p")) %>%
      mutate(pos_cleaned = str_replace_all(pos, "/.*$", "다"))
  )

## n + p
voca_processed_list <- names(voca_list) %>%
  map(
    ~ bind_rows(noun_list[[.x]], p_list[[.x]]) %>%
      filter(nchar(pos_cleaned) > 1) %>%
      filter(nchar(pos_cleaned) < 10) %>%
      select(title, area, year, month, pos_cleaned)
  )

# Tidytext (exploratory, non-essential) ========================================
tryCatch({
  ## extract nouns to create unique(feature list)
  voca <- unlist(
    sapply(petition$body_problem, extractNoun, USE.NAMES = FALSE)
  )
  tb.voca <- sort(table(voca), decreasing = TRUE)

  tb.voca <- table(voca.df.out$pos_cleaned)
  voca.ko <- names(tb.voca)
  tb.voca.sorted <- sort(tb.voca, decreasing = TRUE)

  voca.df.out$pos_cleaned <- str_replace_all(
    string = voca.df.out$pos_cleaned,
    pattern = "[[:punct:]]",
    replace = ""
  )
  voca.df.out <- distinct(voca.df.out)
  voca.df.out <- voca.df.out %>% filter(pos_cleaned != "")
  voca.df.out <- voca.df.out %>%
    filter(!grepl("\\^|<|>|~", pos_cleaned))
  voca.df.out <- voca.df.out %>%
    filter(!grepl("[0-9]", pos_cleaned))

  tb.voca <- table(voca.df.out$pos_cleaned)
  voca.ko <- names(tb.voca)
  tb.voca.sorted <- sort(tb.voca, decreasing = TRUE)
}, error = function(e) {
  message("Tidytext exploratory section skipped: ", e$message)
})
