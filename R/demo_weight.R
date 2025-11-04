source(here::here("R", "utilities.R"))
library(janitor)

## https://kosis.kr/visual/populationKorea/PopulationPyramidDetail.do
demo_korea <- read.table(
  text = " 	계	0-9세	10-19세	20-29세	30-39세	40-49세	50-59세	60-69세	70-79세	80세~
전체	25,012,374	7,941,345	5,064,682	4,255,785	2,926,453	2,104,964	1,474,230	864,140	321,593	59,182
남자	12,550,691	4,116,987	2,607,761	2,131,906	1,421,062	1,055,723	698,026	372,092	126,042	21,092
여자	12,461,683	3,824,358	2,456,921	2,123,879	1,505,391	1,049,241	776,204	492,048	195,551	38,090",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  fileEncoding = "euc-kr"
) %>%
  rename(gender = X) %>% 
  filter(gender != "전체") %>%
  clean_names() %>%
  select(-gye) %>%
  rename_with(~ gsub("x", "age_", .x)) %>%
  rename_with(~ gsub("se", "", .x)) %>%
  mutate(across(!gender, ~ as.numeric(gsub(",", "", .x)))) %>%
  ## don't count underage
  select(-age_0_9, -age_10_19)

demo_sum <- sum(demo_korea %>% select(-gender))

## Create weights for age groups
demo_weight <- demo_korea %>%
  pivot_longer(
    -gender,
    names_to = "age_group",
    values_to = "population"
  ) %>%
  mutate(weight = population / demo_sum) %>%
  select(gender, age_group, weight) %>%
  mutate(
    age_group = gsub("age_", "", age_group),
    age_group = gsub("_", "-", age_group),
    age_group = gsub("80", "80+", age_group),
    gender = case_when(
      gender == "남자" ~ "M",
      TRUE ~ "F"
    )
  )
