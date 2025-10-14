# Setup ========================================================================
source(here::here("R", "utilities.R"))
fname <- here("data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx")

# Load data ====================================================================
df_list <- list(
  raw = read_xlsx(stri_trans_nfc(fname), sheet = "Raw"),
  label = read_xlsx(stri_trans_nfc(fname), sheet = "Label"),
  open = read_xlsx(stri_trans_nfc(fname), sheet = "Open"),
  questions = read_xlsx(stri_trans_nfc(fname), sheet = "변수 가이드") %>%
    rename(varname = `변수명`, question = `변수 내용`)
)
df <- df_list$raw

## 성x연령 균등할당
## 30대 남 123, 40대 여 123, 나머지 전부 122

# Petition post characteristics ================================================
post_types <- stri_trans_nfc("data/공개제안_tidy.xlsx") %>%
  read_xlsx() %>%
  select(id, category, comb_fin, text)

## comb_fin:
## 1. clarity, specificity
## 2. logic, consistency
## 3. tone, manner
## 4. validity, feasibility
post_types <- post_types %>%
  mutate(
    clarity_specificity = substr(comb_fin, 1, 1),
    logic_consistency = substr(comb_fin, 2, 2),
    tone_manner = substr(comb_fin, 3, 3),
    validity_feasibility = substr(comb_fin, 4, 4)
  ) %>%
  rename(item = id) %>%
  mutate(item = as.character(item))

## create df for mcmc
pwc_df <- map_dfr(1:8, function(i) {
  df %>%
    select(
      NO,
      !!sym(paste0("Q13_gCode", i, "_1")),
      !!sym(paste0("Q13_gCode", i, "_2")),
      !!sym(paste0("Q13_", i))
    ) %>%
    rename(
      Item1 = !!sym(paste0("Q13_gCode", i, "_1")),
      Item2 = !!sym(paste0("Q13_gCode", i, "_2")),
      Choice = !!sym(paste0("Q13_", i))
    )
})

## transform df to fit mcmc function
pwc_df <- pwc_df %>%
  rowwise() %>%
  mutate(
    Item1 = paste0("item.", Item1),
    Item2 = paste0("item.", Item2),
    Choice = c(Item1, Item2)[Choice]
  ) %>%
  ungroup()
pwc_df <- as.data.frame(pwc_df)

# Renaming function ============================================================
survey_rename <- function(x, wrangle = TRUE) {
  out <- x %>%
    rename(
      gender = SQ1,
      age = SQ2_1,
      age_range = SQ2_2,
      edu = SQ3,
      residence = SQ4,
      married = SQ5,
      kids = SQ6,
      income = SQ7,
      occupation = SQ8,
      occupation_etc = SQ8_etc,
      livelihood = SQ9,
      life = Q1,
      ideology = Q2,
      party = Q3,
      party_etc = Q3_etc,
      pres22 = Q4,
      pres22_etc = Q4_etc,
      pres25 = Q5,
      pres25_etc = Q5_etc
    )
  
  if (wrangle) {
    out <- out %>%
      mutate(
        ## Option 5: 400만원 이상-500만원 미만
        ## 2025 기준 3인 가족 중위소득 = 500만원
        median_income = case_when(
          income > 5 ~ 1,
          TRUE ~ 0,
        ),
        median_income = factor(
          median_income, levels = c(0, 1),
          labels = c("Below Median", "Above Median")
        )
      )
  }
  
  return(out)
}

# Create survey weight =========================================================

# Attention ====================================================================
pdf("output/main/attention_time.pdf", width = 10, height = 6.5)
par(mfrow = c(2, 4))
hist(df$q13_q14_time_1, breaks = 50, xlab = "Time for Pair Comparison 1 (Seconds)", main = "")
hist(df$q13_q14_time_2, breaks = 50, xlab = "Time for Pair Comparison 2 (Seconds)", main = "")
hist(df$q13_q14_time_3, breaks = 50, xlab = "Time for Pair Comparison 3 (Seconds)", main = "")
hist(df$q13_q14_time_4, breaks = 50, xlab = "Time for Pair Comparison 4 (Seconds)", main = "")
hist(df$q13_q14_time_5, breaks = 50, xlab = "Time for Pair Comparison 5 (Seconds)", main = "")
hist(df$q13_q14_time_6, breaks = 50, xlab = "Time for Pair Comparison 6 (Seconds)", main = "")
hist(df$q13_q14_time_7, breaks = 50, xlab = "Time for Pair Comparison 7 (Seconds)", main = "")
hist(df$q13_q14_time_8, breaks = 50, xlab = "Time for Pair Comparison 8 (Seconds)", main = "")
dev.off()

median(df$q13_q14_time_1) ## 49
median(df$q13_q14_time_2) ## 29
median(df$q13_q14_time_3) ## 26
median(df$q13_q14_time_4) ## 25.5
median(df$q13_q14_time_5) ## 24
median(df$q13_q14_time_6) ## 23
median(df$q13_q14_time_7) ## 22
median(df$q13_q14_time_8) ## 22

# Demographics =================================================================
