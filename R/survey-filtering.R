source(here::here("R", "utilities.R"))

# Data filtering for survey ====================================================
df <- seq(2021, 2024) %>%
  set_names(., .) %>%
  map_dfr(
    ~ here("data", "tidy", paste0("pub_petition_content_", .x, ".csv")) %>%
      read_csv() %>%
      mutate(year = .x)
  ) 

df <- df %>%
  ## 40,840 to 3,530 observations
  filter(branch == "보건복지부") %>%
  ## 233 observations
  filter(grepl("출생|출산|육아", title)) %>%
  ## This is conditional on having been answered by Jan 2024 for 2021--2023
  ## and by Nov 2024 for 2024
  filter(status == "답변완료")

## Export
## Preserve Korean UTF-8
df %>%
  readr::write_excel_csv(
    here("data", "tidy", "pub_petition_content_birth_2021-2024.csv")
  )

# Manually evaluated for 2024 ==================================================
evaluated <- bind_rows(
  readxl::read_excel(
    here("data/tidy/pub_petition_content_birth_2021-2024_SK_evaluated.xlsx")
  ) %>%
    mutate(eval = "SK"),
  readxl::read_excel(
    here("data/tidy/pub_petition_content_birth_2021-2024_KY_evaluated.xlsx")
  ) %>%
    mutate(eval = "KY")
) %>%
  filter(year == 2024 | eval == "KY") %>%
  filter(!is.na(`personal?`)) %>%
  select(
    eval, date_petitioned, title,
    `personal?`, `emotional?`, `good writing?`, `feasible?`, `concrete?`,
    `현황 및 문제점`, `개선방안`, `기대효과`
  ) %>%
  group_by(title, `현황 및 문제점`) %>%
  filter(n() > 1) %>%
  arrange(title, eval)

## Delete duplicates
evaluated <- evaluated[!duplicated(evaluated), ]

## Completely agree: 18 pairs
evaluated <- evaluated %>%
  mutate(
    personal_agree = case_when(
      `personal?`[[1]] == `personal?`[[2]] ~ 1,
      TRUE ~ 0
    ),
    emotional_agree = case_when(
      `emotional?`[[1]] == `emotional?`[[2]] ~ 1,
      TRUE ~ 0
    ),
    good_writing_agree = case_when(
      `good writing?`[[1]] == `good writing?`[[2]] ~ 1,
      TRUE ~ 0
    ),
    feasible_agree = case_when(
      `feasible?`[[1]] == `feasible?`[[2]] ~ 1,
      TRUE ~ 0
    ),
    concrete_agree = case_when(
      `concrete?`[[1]] == `concrete?`[[2]] ~ 1,
      TRUE ~ 0
    ),
    agree = 
      personal_agree + emotional_agree + good_writing_agree + 
      feasible_agree + concrete_agree
  ) %>%
  select(-matches("_agree")) %>%
  select(agree, everything())

round(prop.table(table(evaluated$agree)) * 100, digits = 1)

## Distribution of features
round(prop.table(table(evaluated$`personal?`)) * 100, digits = 1)
round(prop.table(table(evaluated$`emotional?`)) * 100, digits = 1)
round(prop.table(table(evaluated$`good writing?`)) * 100, digits = 1)
round(prop.table(table(evaluated$`feasible?`)) * 100, digits = 1)
round(prop.table(table(evaluated$`concrete?`)) * 100, digits = 1)

round(prop.table(table(evaluated$`personal?`, evaluated$eval)) * 100, digits = 1)
round(prop.table(table(evaluated$`emotional?`, evaluated$eval)) * 100, digits = 1)
round(prop.table(table(evaluated$`good writing?`, evaluated$eval)) * 100, digits = 1)
round(prop.table(table(evaluated$`feasible?`, evaluated$eval)) * 100, digits = 1)
round(prop.table(table(evaluated$`concrete?`, evaluated$eval)) * 100, digits = 1)

## Binning choices: emotional X concrete
binned <- evaluated %>%
  mutate(
    bin = case_when(
      `emotional?` == 1 & `concrete?` == 1 ~ "emotional and concrete",
      `emotional?` == 1 & `concrete?` == 0 ~ "emotional and abstract",
      `emotional?` == 0 & `concrete?` == 1 ~ "dry and concrete",
      `emotional?` == 0 & `concrete?` == 0 ~ "dry and abstract"
    )
  ) %>%
  select(bin, agree, everything()) %>%
  filter(agree >= 4)

binned
