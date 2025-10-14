# Setup ========================================================================
source(here::here("R", "utilities.R"))
fname <- here("data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx")

# Load data ====================================================================
df <- list(
  raw = read_xlsx(stri_trans_nfc(fname), sheet = "Raw"),
  label = read_xlsx(stri_trans_nfc(fname), sheet = "Label"),
  open = read_xlsx(stri_trans_nfc(fname), sheet = "Open"),
  questions = read_xlsx(stri_trans_nfc(fname), sheet = "변수 가이드") %>%
    rename(varname = `변수명`, question = `변수 내용`)
)
