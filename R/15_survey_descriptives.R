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

