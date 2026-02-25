## fit sIBP on K-petition survey

## Rscript may launch in the C locale; set UTF-8 so Korean paths resolve.
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(tidytext)
## texteffect loads MASS, which shadows dplyr::select.  Load it only after
## utilities.R has finished (where dplyr::select is still needed).

# Setup ========================================================================
## utilities.R loads MCMCpack → MASS (masks dplyr::select) and plyr (masks
## dplyr::mutate, summarise, group_by, …) before re-attaching dplyr via
## library(tidyverse).  Because library(tidyverse) on an already-loaded
## tidyverse only re-attaches the meta-package—not dplyr itself—the individual
## dplyr verbs remain buried below plyr/MASS on the search path.
## Pinning the affected dplyr functions in .GlobalEnv (position 0, searched
## before any package) ensures the right versions are used inside source().
filter    <- dplyr::filter
select    <- dplyr::select
mutate    <- dplyr::mutate
rename    <- dplyr::rename
summarise <- dplyr::summarise
summarize <- dplyr::summarize
group_by  <- dplyr::group_by
arrange   <- dplyr::arrange
source(here::here("R", "utilities.R"))
## Re-attach dplyr so its verbs win over plyr/MASS for the rest of the script.
library(dplyr)
rm(filter, select, mutate, rename, summarise, summarize, group_by, arrange)
fname <- here(
  stri_trans_nfc(
    "data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx"
  )
)

# Load data ====================================================================
## Korean filenames cause encoding issues in readxl on some systems;
## copy to a temp path with an ASCII name before reading.
file.copy(fname, "/tmp/survey_sibp.xlsx", overwrite = TRUE)
df_list <- list(
  raw = read_xlsx("/tmp/survey_sibp.xlsx", sheet = "Raw"),
  label = read_xlsx("/tmp/survey_sibp.xlsx", sheet = "Label"),
  open = read_xlsx("/tmp/survey_sibp.xlsx", sheet = "Open"),
  questions = read_xlsx(
    "/tmp/survey_sibp.xlsx",
    sheet = stri_trans_nfc("변수 가이드")
  ) |>
    ## dplyr 1.0+ requires any_of() to rename via character values
    rename(any_of(c(
      varname  = stri_trans_nfc("변수명"),
      question = stri_trans_nfc("변수 내용")
    )))
)
df <- df_list$raw

# reformat data ====================================================================
## each respondent was assigned 5 petitions
df_long1 <- df |>
  mutate(res_id = 1:nrow(df)) |>
  select(
    res_id, SQ1, SQ2_1, SQ2_2, SQ3, SQ4, SQ5, SQ6, SQ7, SQ8, SQ9,
    Q1, Q2, Q3, Q4, Q5,
    starts_with("Q6"), starts_with("Q7"), starts_with("Q8"),
    starts_with("Q9"), starts_with("Q10"), starts_with("Q11"),
    starts_with("Q15")
  ) |>
  pivot_longer(
    cols = starts_with("Q15_gCode"),
    names_to = "petition",
    values_to = "petition_id"
  )
df_long2 <- df |>
  select(Q15_1, Q15_2, Q15_3, Q15_4, Q15_5) |>
  pivot_longer(
    cols = starts_with("Q15_"),
    names_to = "thermo_id",
    values_to = "thermo_score"
  )
## bind_cols is safe here because pivot order matches: gCode1↔Q15_1, ...
df_long <- df_long1 |> bind_cols(df_long2)

# Petition texts ===============================================================
## T: petition texts of Q15_gCode1~5
## Korean filename: copy to ASCII temp path
file.copy(
  "data/screenshots/공개제안_tidy_url_appended.xlsx",
  "/tmp/petition_text_sibp.xlsx",
  overwrite = TRUE
)
petition_text <- read_xlsx("/tmp/petition_text_sibp.xlsx")

## preprocess the text field in petition_text ---------------------------------
## Write texts to CSV for Python/kiwipiepy preprocessing
petition_text |>
  select(id, text) |>
  write.csv("/tmp/petition_texts_input.csv", row.names = FALSE)

## Python script: tokenise Korean with kiwipiepy, keep nouns, drop stopwords
py_script <- '
import kiwipiepy, re, pandas as pd

texts = pd.read_csv("/tmp/petition_texts_input.csv")
stopwords = set(
    pd.read_csv("data/kiwipiepy_stopwords.csv")["Stopword"].tolist()
)

k = kiwipiepy.Kiwi()
results = []
for _, row in texts.iterrows():
    text = str(row["text"]) if pd.notna(row["text"]) else ""
    # keep only Korean characters and whitespace
    text = re.sub(r"[^가-힣\\s]", " ", text)
    text = re.sub(r"\\s+", " ", text).strip()
    tokens = k.tokenize(text)
    # NNG = common noun, NNP = proper noun; skip single characters
    nouns = [
        t.form for t in tokens
        if t.tag.startswith("NN")
        and t.form not in stopwords
        and len(t.form) > 1
    ]
    results.append({"id": int(row["id"]), "tokens": " ".join(nouns)})

pd.DataFrame(results).to_csv("/tmp/petition_tokens_output.csv", index=False)
print("kiwipiepy preprocessing done")
'
writeLines(py_script, "/tmp/kiwi_preprocess.py")
## Run in the project root so "data/kiwipiepy_stopwords.csv" resolves correctly
system(
  paste(
    "cd", here(),
    "&& python3 /tmp/kiwi_preprocess.py"
  )
)

## read preprocessed token lists (space-separated nouns per petition)
petition_tokens <- read.csv("/tmp/petition_tokens_output.csv")

# create dfm from the preprocessed text =======================================
## Build a tidy long-form term count table
dfm_tidy <- petition_tokens |>
  mutate(tokens = strsplit(tokens, " ")) |>
  unnest(cols = tokens) |>
  rename(token = tokens) |>
  filter(nchar(token) > 1) |>
  count(id, token) |>
  ## Trim rare terms: keep only tokens present in ≥ 2 of the 65 petitions
  group_by(token) |>
  filter(n_distinct(id) >= 2) |>
  ungroup()

## Sparse matrix: rows = petition id, cols = vocabulary
dfm_sparse <- dfm_tidy |>
  cast_sparse(row = id, column = token, value = n)

## Dense matrix needed by texteffect::sibp
dfm_dense <- as.matrix(dfm_sparse)
cat(
  "DFM (petitions × vocab):",
  nrow(dfm_dense), "×", ncol(dfm_dense), "\n"
)

# merge dfm with df_long using petition_id =====================================
## Each row in df_long is one respondent-petition pair; attach the corresponding
## petition's word counts.  Petition IDs are stored as row names of dfm_dense.
petition_ids_in_dfm <- as.integer(rownames(dfm_dense))
df_long_sub <- df_long |>
  filter(petition_id %in% petition_ids_in_dfm) |>
  ## drop any rows with missing thermo_score
  filter(!is.na(thermo_score))

## x_mat: observations (respondent-petition pairs) × vocabulary
x_mat <- dfm_dense[as.character(df_long_sub$petition_id), , drop = FALSE]
## y_vec: thermometer score (0–100)
y_vec <- df_long_sub$thermo_score

cat("x_mat (obs × vocab):", nrow(x_mat), "×", ncol(x_mat), "\n")
cat("y_vec length:", length(y_vec), "\n")

## Optional group membership matrix g_mat (AMCE can vary by demographics)
## SQ1: gender (1 = male, 2 = female); SQ2_2: age group (1–5)
g_mat <- model.matrix(
  ~ factor(SQ1) + factor(SQ2_2),
  data = df_long_sub
)[, -1, drop = FALSE]
cat("g_mat (obs × groups):", nrow(g_mat), "×", ncol(g_mat), "\n")

# fit sIBP over the dfm columns, with demographic variables as controls ========
library(texteffect)

set.seed(1234)
## 70/30 train-test split; same indices reused for both model variants
train_ind <- sample(seq_len(nrow(x_mat)), size = floor(0.7 * nrow(x_mat)))

## Helper: select the best-ranked fit from a sibp_param_search result
pick_best <- function(search, x) {
  rnk <- sibp_rank_runs(search, x, num.words = 10)
  best <- rnk[1, ]
  alpha_key <- as.character(best$alpha)
  sigma_key <- as.character(best$sigmasq.n)
  list(
    fit      = search[[alpha_key]][[sigma_key]][[best$iter]],
    rank     = rnk,
    best_row = best
  )
}

# Model A: G = NULL — K marginal AMCE estimates ================================
## sibp.fit$L = 1, so sibp_amce returns exactly K = 3 rows
cat("\n--- Model A: no demographic groups (G = NULL) ---\n")
search_a <- sibp_param_search(
  x_mat, y_vec,
  K = 3,
  alphas = c(2, 4),
  sigmasq.ns = c(0.5, 1.0),
  iters = 2,
  train.ind = train_ind
)
res_a <- pick_best(search_a, x_mat)
fit_a <- res_a$fit
cat("\nParameter ranking (Model A):\n")
print(res_a$rank)
cat(
  "\nBest config: alpha =", res_a$best_row$alpha,
  "| sigmasq.n =", res_a$best_row$sigmasq.n,
  "| iter =", res_a$best_row$iter, "\n"
)

cat("\n=== Top words — Model A ===\n")
print(sibp_top_words(fit_a, colnames(x_mat), num.words = 10, verbose = TRUE))
# [1,] "경력"   "평가"   "본래"
# [2,] "단절"   "윤리"   "우리나라"
# [3,] "필요"   "마지막" "내용"
# [4,] "모자"   "성향"   "발전"
# [5,] "모순"   "과정"   "대학"
# [6,] "부담감" "방식"   "비중"
# [7,] "여가"   "이하"   "분야"
# [8,] "사내"   "분야"   "정치"
# [9,] "산후"   "정치"   "수학"
# [10,] "조리"   "요구"   "체제"

cat("\n=== AMCE (test set) — Model A: K = 3 marginal estimates ===\n")
amce_a <- sibp_amce(fit_a, x_mat, y_vec, G = NULL)
print(amce_a)
sibp_amce_plot(
  amce_a,
  xlab = "Latent treatment",
  ylab = "Effect on thermometer score (0–100)"
)

# Model B: G = g_mat — K × ncol(g_mat) group-specific AMCE estimates ==========
## sibp.fit$L = ncol(g_mat), so sibp_amce returns K × ncol(g_mat) rows
## g_mat has ncol = 1 (gender) + 4 (age dummies) = 5, giving 3 × 5 = 15 rows
cat("\n--- Model B: demographic groups (G = g_mat) ---\n")
search_b <- sibp_param_search(
  x_mat, y_vec,
  K = 3,
  alphas = c(2, 4),
  sigmasq.ns = c(0.5, 1.0),
  iters = 2,
  G = g_mat,
  train.ind = train_ind
)
res_b <- pick_best(search_b, x_mat)
fit_b <- res_b$fit
cat("\nParameter ranking (Model B):\n")
print(res_b$rank)
cat(
  "\nBest config: alpha =", res_b$best_row$alpha,
  "| sigmasq.n =", res_b$best_row$sigmasq.n,
  "| iter =", res_b$best_row$iter, "\n"
)

cat("\n=== Top words — Model B ===\n")
print(sibp_top_words(fit_b, colnames(x_mat), num.words = 10, verbose = TRUE))
# [1,] "필요"   "평가"   "본래"
# [2,] "경력"   "윤리"   "우리나라"
# [3,] "단절"   "마지막" "내용"
# [4,] "모자"   "성향"   "발전"
# [5,] "모순"   "방식"   "대학"
# [6,] "부담감" "과정"   "비중"
# [7,] "여가"   "이하"   "분야"
# [8,] "사내"   "분야"   "정치"
# [9,] "산후"   "정치"   "수학"
# [10,] "조리"   "고려"   "체제"

cat("\n=== AMCE (test set) — Model B: K × ncol(g_mat) group estimates ===\n")
amce_b <- sibp_amce(fit_b, x_mat, y_vec, G = g_mat)
print(amce_b)
sibp_amce_plot(
  amce_b,
  xlab = "Latent treatment × demographic group",
  ylab = "Effect on thermometer score (0–100)"
)

# Model C: numeric controls — age (SQ2_1) and income (SQ7) ====================
## SQ2_1: actual age in years (already numeric in the Raw sheet).
## SQ7:   income bracket code 1–11 stored as a numeric in the Raw sheet.
##        Labels are "1) 100만원 미만", "2) 100만원 이상-200만원 미만", etc.
##        str_extract("^\\d+") pulls the leading integer, making the intent
##        explicit even though the Raw sheet already holds pure numbers.
## Both columns are scaled so that sibp treats them on a common unit.
g_mat_c <- scale(cbind(
  age    = as.numeric(df_long_sub$SQ2_1),
  income = as.numeric(
    stringr::str_extract(as.character(df_long_sub$SQ7), "^\\d+")
  )
))

cat("\n--- Model C: numeric controls — age + income ---\n")
search_c <- sibp_param_search(
  x_mat, y_vec,
  K          = 3,
  alphas     = c(2, 4),
  sigmasq.ns = c(0.5, 1.0),
  iters      = 2,
  G          = g_mat_c,
  train.ind  = train_ind
)
res_c <- pick_best(search_c, x_mat)
fit_c <- res_c$fit
cat("\nParameter ranking (Model C):\n")
print(res_c$rank)
cat(
  "\nBest config: alpha =", res_c$best_row$alpha,
  "| sigmasq.n =", res_c$best_row$sigmasq.n,
  "| iter =", res_c$best_row$iter, "\n"
)

cat("\n=== Top words — Model C ===\n")
print(sibp_top_words(fit_c, colnames(x_mat), num.words = 10, verbose = TRUE))

## sibp_amce with G = g_mat_c returns K × ncol(g_mat_c) = 3 × 2 = 6 rows:
## one intercept-of-treatment + one slope per numeric control per treatment
cat("\n=== AMCE (test set) — Model C: K × 2 numeric-control estimates ===\n")
amce_c <- sibp_amce(fit_c, x_mat, y_vec, G = g_mat_c)
print(amce_c)
sibp_amce_plot(
  amce_c,
  xlab = "Latent treatment × numeric control",
  ylab = "Effect on thermometer score (0–100)"
)

# Model D: Model C + political party ID (Q3) ==================================
## Q3 codes: 1 = liberal, 2 = conservative, 3–8 = others (reference)
## Recode into a factor, dummy-encode with "others" as baseline, then
## column-bind with the two scaled numeric controls from Model C.
party <- dplyr::case_when(
  df_long_sub$Q3 == 1 ~ "liberal",
  df_long_sub$Q3 == 2 ~ "conservative",
  TRUE                ~ "others"
)
party <- factor(party, levels = c("others", "liberal", "conservative"))
party_dummies <- model.matrix(~ party)[, -1, drop = FALSE]

## g_mat_d: age (scaled) + income (scaled) + liberal dummy + conservative dummy
g_mat_d <- cbind(g_mat_c, party_dummies)

cat("\n--- Model D: numeric controls + political party ID ---\n")
search_d <- sibp_param_search(
  x_mat, y_vec,
  K          = 3,
  alphas     = c(2, 4),
  sigmasq.ns = c(0.5, 1.0),
  iters      = 2,
  G          = g_mat_d,
  train.ind  = train_ind
)
res_d <- pick_best(search_d, x_mat)
fit_d <- res_d$fit
cat("\nParameter ranking (Model D):\n")
print(res_d$rank)
cat(
  "\nBest config: alpha =", res_d$best_row$alpha,
  "| sigmasq.n =", res_d$best_row$sigmasq.n,
  "| iter =", res_d$best_row$iter, "\n"
)

cat("\n=== Top words — Model D ===\n")
print(sibp_top_words(fit_d, colnames(x_mat), num.words = 10, verbose = TRUE))

## K × ncol(g_mat_d) = 3 × 4 = 12 rows
cat("\n=== AMCE (test set) — Model D: K × 4 estimates ===\n")
amce_d <- sibp_amce(fit_d, x_mat, y_vec, G = g_mat_d)
print(amce_d)
sibp_amce_plot(
  amce_d,
  xlab = "Latent treatment × control",
  ylab = "Effect on thermometer score (0–100)"
)

# Export results ===============================================================
out_dir <- here("output", "sIBP")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## Collect all models into one list for compact iteration
model_exports <- list(
  A = list(
    fit = fit_a, amce = amce_a, rank = res_a$rank,
    label = "model_a_no_controls"
  ),
  B = list(
    fit = fit_b, amce = amce_b, rank = res_b$rank,
    label = "model_b_demo_groups"
  ),
  C = list(
    fit = fit_c, amce = amce_c, rank = res_c$rank,
    label = "model_c_age_income"
  ),
  D = list(
    fit = fit_d, amce = amce_d, rank = res_d$rank,
    label = "model_d_age_income_party"
  )
)

for (nm in names(model_exports)) {
  m   <- model_exports[[nm]]
  pfx <- file.path(out_dir, m$label)

  ## 1. fitted model object (reload with readRDS for downstream use)
  saveRDS(m$fit, paste0(pfx, "_fit.rds"))

  ## 2. AMCE table — row names identify treatment × group combinations
  write.csv(m$amce, paste0(pfx, "_amce.csv"))

  ## 3. top 10 words per latent treatment
  write.csv(
    sibp_top_words(m$fit, colnames(x_mat), num.words = 10),
    paste0(pfx, "_top_words.csv"),
    row.names = FALSE
  )

  ## 4. parameter ranking from sibp_rank_runs
  write.csv(m$rank, paste0(pfx, "_rank.csv"), row.names = FALSE)

  ## 5. AMCE plot as PNG
  ggsave(
    filename = paste0(pfx, "_amce_plot.png"),
    plot     = sibp_amce_plot(m$amce),
    width    = 8,
    height   = 5,
    dpi      = 150
  )

  cat("Exported Model", nm, "→", pfx, "\n")
}
