# Setup ========================================================================
source(here::here("R", "utilities.R"))
fname <- here(
  stri_trans_nfc(
    "data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx"
  )
)

## Theta constraints -----------------------------------------------------------
## item 1: 1011. clear, weak logic, well mannered, valid
## item 15: 0000. unclear, illogical, untoned, invalid
## item 49: 0101. unclear, logical, untoned, valid
## 1st dim: clarity and manner
## 2nd dim: logic and validity
theta_constraints <- list(
  item.1 = list(1, 2),
  item.1 = list(2, 2),
  item.15 = list(1, -2),
  item.15 = list(2, -2),
  item.49 = list(1, "-"),
  item.49 = list(2, "+")
)

## 성x연령 균등할당
## 30대 남 123, 40대 여 123, 나머지 전부 122

## Variable groups -------------------------------------------------------------
time_vars <- paste0("q13_q14_time_", 1:8)
petition_vars <- c(
  "diff_quality", "diff_nchar", "diff_nword", "diff_nsent", "diff_avg_sent",
  "diff_clarity", "diff_logic", "diff_manner", "diff_validity", "same_category"
)

## Nested additive blocks (Option A)
## M1: demographics
## M2: + socioeconomic
## M3: + political
## M4: + attitudes / trust
m1_resp <- c(
  "female", "age", "edu", "income", 
  "seoul", "married", "has_young_child", "log_time"
)
m2_resp <- c(m1_resp, "subj_class", "life_sat")
m3_resp <- c(m2_resp, "ideology", "ppp", "voted_yoon", "voted_lee")
m4_resp <- c(m3_resp, "social_trust", "populism_idx", "instit_trust")

# Load data ====================================================================
df_list <- list(
  raw = read_xlsx(
    stri_trans_nfc(fname),
    sheet = "Raw"
  ),
  label = read_xlsx(
    stri_trans_nfc(fname),
    sheet = "Label"
  ),
  open = read_xlsx(
    stri_trans_nfc(fname),
    sheet = "Open"
  ),
  questions = read_xlsx(
    stri_trans_nfc(fname),
    sheet = stri_trans_nfc("변수 가이드")
  ) %>%
    rename(
      varname = stri_trans_nfc("변수명"),
      question = stri_trans_nfc("변수 내용")
    )
)
df <- df_list$raw

# Data wrangling ===============================================================
df <- df %>%
  rename(
    ## SQ2_1, SQ2. 귀하의 연령은 어떻게 되십니까? - 나이
    age = SQ2_1
  ) %>%
  mutate(
    ## SQ1. 귀하의 성별은 무엇입니까? 1 = 남자, 2 = 여자
    female = as.numeric(SQ1 == 2),
    ## SQ2_2, SQ2. 귀하의 연령은 어떻게 되십니까? - 연령대
    age_group = factor(
      SQ2_2, levels = seq(5), 
      labels = c("20-29", "30-39", "40-49", "50-59", "60+")
    ),
    ## SQ3. 귀하의 최종 학력은 어떻게 되십니까?
    edu = factor(
      SQ3, levels = seq(6),
      labels = c(
        "Middle school or below",
        "High school",
        "Some college",
        "College grad",
        "Some postgrad",
        "Postgrad"
      )
    ),
    edu4 = factor(
      case_when(
        SQ3 <= 2 ~ "High school",
        SQ3 == 3 ~ "Some college",
        SQ3 == 4 | SQ3 == 5 ~ "College grad",
        SQ3 == 6 ~ "Postgrad"
      ),
      levels = c("High school", "Some college", "College grad", "Postgrad")
    ),
    income = factor(SQ7),
    ideology = factor(Q2),
    ppp = as.numeric(Q3 == 2),
    populist = as.numeric(Q6_7 >= 4),
    ## V1: demographics
    seoul = as.numeric(SQ4 == 1),
    married = as.numeric(SQ5 %in% c(1, 2)),
    has_young_child = as.numeric(SQ6 == 1),
    ## V2: political engagement
    subj_class = factor(SQ9),
    life_sat = factor(Q1),
    voted_yoon = as.numeric(Q4 == 2),
    voted_lee = as.numeric(Q5 == 1),
    ## V3: trust indices (means of Likert items)
    social_trust = (
      Q6_1 + Q6_2 + Q6_3 + Q6_4
    ) / 4,
    populism_idx = (
      Q6_5 + Q6_6 + Q6_7
    ) / 3,
    instit_trust = (
      Q7_1 + Q7_2 + Q7_3 +
        Q7_4 + Q7_5 + Q7_6 + Q7_7
    ) / 7,
    ## Response time per respondent
    median_time = apply(
      select(., all_of(time_vars)), 1, median
    )
  ) %>%
  select(
    -SQ1, -SQ3
  )

# Survey weights ===============================================================
## Map survey codes to demo_weight groups
## SQ1: 1=M, 2=F
## SQ2_2: 1=20s, 2=30s, 3=40s, 4=50s, 5=60+
df <- df %>%
  mutate(
    wt_gender = ifelse(female == 0, "M", "F"),
    wt_age = age_group
  ) %>%
  left_join(
    demo_weight,
    by = c(
      "wt_gender" = "gender",
      "wt_age" = "age_group"
    )
  ) %>%
  rename(wt = weight)
