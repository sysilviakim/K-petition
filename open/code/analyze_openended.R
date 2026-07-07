## Demographic analysis of LLM-coded open-ended responses (Q16).
## Reads output/openended_llm_classified.csv (from llm_classify.py), joins
## respondent demographics from the survey workbook, and reproduces the paper's
## analysis on the LLM labels: frequency table, Monte Carlo chi-square tests,
## and per-demographic heatmaps.  Self-contained: does NOT source utilities.R.
##
## ---- PAPER EXHIBITS produced here (stable tab:/fig: labels in paper/open.tex) ----
##   Table 1  (tab:openended)      distribution of the 14 categories   [Frequency table block]
##   Table 3  (tab:chisq)          chi-square tests of association      [Chi-square block]
##   Figure 1 (fig:demo_age)       category distribution by age         [Heatmaps block]
##   Figures 4-6 (fig:demo_marital, _gender, _null)  other demographics [Heatmaps block]
##   Figure 2 (fig:frame_by_age)   object-of-evaluation frame by age    [FRAME-by-age block]
##   (frame & income nulls here also feed Table 6 / tab:adjudicate and Sec. 5.5)
## Table/Figure NUMBERS are for the current draft; the tab:/fig: labels are the stable refs.

Sys.setlocale("LC_ALL", "en_US.UTF-8")
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(stringi)
  library(scales)
})

fname <- here(stri_trans_nfc(
  "data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx"
))
file.copy(fname, "/tmp/survey_open.xlsx", overwrite = TRUE)
raw <- read_xlsx("/tmp/survey_open.xlsx", sheet = "Raw")

## LLM labels (NO, primary, primary_en, all_labels)
llm <- read_csv(here("output", "openended_llm_classified.csv"),
                show_col_types = FALSE) |>
  dplyr::select(NO, primary, primary_en, all_labels, regex)

## leading-integer extractor (Raw codes are numeric but may carry "N) ..." text)
code <- function(x) as.integer(stringr::str_extract(as.character(x), "^\\s*\\d+"))

df <- raw |>
  mutate(NO = as.integer(NO)) |>
  dplyr::select(NO, SQ1, SQ2_1, SQ3, SQ5, SQ7, SQ8, Q2) |>
  left_join(llm, by = "NO") |>
  mutate(
    gender = factor(code(SQ1), levels = 1:2, labels = c("Male", "Female")),
    age_group = cut(as.integer(SQ2_1), breaks = c(19, 29, 39, 49, 59, Inf),
                    labels = c("20s", "30s", "40s", "50s", "60+"), right = TRUE),
    education = factor(code(SQ3), levels = 1:6,
      labels = c("Middle school or less", "High school", "In college",
                 "College grad", "In grad school", "Grad school")),
    edu3 = fct_collapse(education,
      "HS or less" = c("Middle school or less", "High school"),
      "College"    = c("In college", "College grad"),
      "Grad+"      = c("In grad school", "Grad school")),
    marital = factor(code(SQ5), levels = 1:4,
      labels = c("Married (children)", "Married (no children)", "Single", "Other")),
    income = fct_collapse(factor(code(SQ7)),
      "< 200" = c("1", "2"), "200-400" = c("3", "4"),
      "400-700" = c("5", "6", "7"), "700-1000" = c("8"),
      "1000+" = c("9", "10", "11")),
    occupation = factor(code(SQ8), levels = 1:12,
      labels = c("Office worker", "Homemaker", "Student", "Professional",
                 "Service/Sales", "Agriculture", "Self-employed",
                 "Civil servant/Teacher", "Freelancer", "Technical",
                 "Unemployed/Retired", "Other")),
    ideology = factor(code(Q2), levels = 1:7,
      labels = c("Very progressive", "Progressive", "Lean progressive",
                 "Moderate", "Lean conservative", "Conservative", "Very conservative")),
    ideo3 = fct_collapse(ideology,
      "Progressive" = c("Very progressive", "Progressive", "Lean progressive"),
      "Moderate" = "Moderate",
      "Conservative" = c("Lean conservative", "Conservative", "Very conservative"))
  )

out <- here("output"); dir.create(out, showWarnings = FALSE)
figdir <- here("paper", "fig"); dir.create(figdir, recursive = TRUE, showWarnings = FALSE)

# Frequency table (primary label) ============================================
# -> PAPER Table 1 (tab:openended): distribution of the 14 response categories
freq <- df |>
  count(primary, primary_en, name = "n") |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))
cat("\n========== Primary-label frequency (LLM) ==========\n")
print(as.data.frame(freq), row.names = FALSE)
write_csv(freq, file.path(out, "openended_llm_freq.csv"))

# Multi-label coverage: each category as a binary indicator ===================
# -> feeds the "roughly a third of answers carry 2+ labels" point in Sec. 5.1 (no standalone table)
multi <- df |>
  mutate(lab = strsplit(as.character(all_labels), "\\|")) |>
  unnest(lab) |>
  filter(lab != "", !is.na(lab)) |>
  mutate(lab = as.integer(lab)) |>
  count(lab, name = "n_mentions") |>
  arrange(desc(n_mentions))
cat("\n========== Category mentions (multi-label) ==========\n")
print(as.data.frame(multi), row.names = FALSE)
write_csv(multi, file.path(out, "openended_llm_multilabel.csv"))

# Chi-square tests (Monte Carlo, B=5000), excluding No-opinion(9) & Other(14) =
# -> PAPER Table 3 (tab:chisq): chi-square + Cramer's V, demographics x opinion
df_sub <- df |> filter(!primary %in% c(9L, 14L))
demo_vars <- c("gender", "age_group", "edu3", "marital", "income", "occupation", "ideo3")
demo_labels <- c("Gender", "Age Group", "Education", "Marital Status",
                 "Income", "Occupation", "Political Ideology")

set.seed(1234)
chi_results <- tibble()
for (k in seq_along(demo_vars)) {
  tbl <- table(df_sub[[demo_vars[k]]], df_sub$primary_en)
  tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0]
  chi <- chisq.test(tbl, simulate.p.value = TRUE, B = 5000)
  ## Cramer's V effect size: shows the "null" class/party associations are
  ## weak but not negligible (degree, not kind) -- reported in the paper.
  cv <- sqrt(unname(chi$statistic) / (sum(tbl) * (min(dim(tbl)) - 1)))
  chi_results <- bind_rows(chi_results, tibble(
    variable = demo_labels[k],
    chi_sq = round(unname(chi$statistic), 2),
    cramers_v = round(cv, 3),
    p_value = round(chi$p.value, 4),
    sig = case_when(chi$p.value < .001 ~ "***", chi$p.value < .01 ~ "**",
                    chi$p.value < .05 ~ "*", chi$p.value < .10 ~ ".", TRUE ~ "")))
}
cat("\n========== Chi-square (Monte Carlo B=5000; excl. No-opinion & Other) ==========\n")
print(as.data.frame(chi_results), row.names = FALSE)
write_csv(chi_results, file.path(out, "openended_llm_chisq.csv"))

# Heatmaps per demographic ====================================================
# -> PAPER Figure 1 (fig:demo_age, age) in main text; Figures 4-6 (fig:demo_marital,
#    _gender, _null = education/income/ideology) in Appendix C.  One PDF per variable;
#    open.tex combines education+income+ideology into the single fig:demo_null panel.
make_heatmap <- function(d, dv, dl) {
  pd <- d |> filter(!is.na(.data[[dv]])) |>
    count(.data[[dv]], primary_en) |>
    group_by(.data[[dv]]) |> mutate(prop = n / sum(n)) |> ungroup()
  pval <- chi_results |> filter(variable == dl) |> pull(p_value)
  ggplot(pd, aes(x = primary_en, y = .data[[dv]], fill = prop)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.0f%%", prop * 100)), size = 2.5) +
    scale_fill_gradient(low = "white", high = "#2166AC", labels = percent_format()) +
    labs(title = paste("Response Category by", dl),
         subtitle = paste0("(Excl. No-opinion & Other; chi-sq p = ", pval, ")"),
         x = NULL, y = dl, fill = "Proportion") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7),
          plot.title = element_text(face = "bold", size = 11))
}
fig_names <- c("gender", "age_group", "education", "marital",
               "income", "occupation", "ideology")
walk2(demo_vars, seq_along(demo_vars), function(dv, k) {
  p <- make_heatmap(df_sub, dv, demo_labels[k])
  ggsave(file.path(figdir, paste0("openended_", fig_names[k], ".pdf")),
         p, width = 14, height = 6)
})
cat("\nHeatmaps written to", figdir, "\n")

write_csv(df |> dplyr::select(NO, primary, primary_en, all_labels, regex,
                             gender, age_group, edu3, marital, income,
                             occupation, ideo3),
          file.path(out, "openended_analysis_data.csv"))
cat("Analysis data written to output/openended_analysis_data.csv\n")

# Object-of-evaluation FRAME by age (frame coding from code/llm_frame.py) ======
# -> PAPER Figure 2 (fig:frame_by_age): gov vs citizen-content frame share by age.
#    The gov:citizen logistic, and the income-null checks below, feed Sec. 5.5 and
#    the object-of-evaluation row of Table 6 (tab:adjudicate).
## Tests the monotonic cohort shift: government- vs citizen-content frame.
frame_path <- file.path(out, "openended_frame_classified.csv")
if (file.exists(frame_path)) {
  fr <- read_csv(frame_path, show_col_types = FALSE) |> dplyr::select(NO, frame)
  dfr <- df |> dplyr::select(NO, age_group) |> left_join(fr, by = "NO")

  cat("\n========== FRAME distribution (all responses) ==========\n")
  print(dfr |> count(frame) |> mutate(pct = round(100 * n / sum(n), 1)) |> as.data.frame(),
        row.names = FALSE)

  ## among framed responses: government:citizen ratio by age
  framed <- dfr |> filter(frame %in% c("government", "citizen"))
  cat("\n========== gov:citizen frame ratio by age (framed responses) ==========\n")
  print(framed |> group_by(age_group) |>
          summarise(gov_pct = round(100 * mean(frame == "government")),
                    citizen_pct = round(100 * mean(frame == "citizen")),
                    gov_to_citizen = round(mean(frame == "government") /
                                           mean(frame == "citizen"), 2), n = n()) |>
          as.data.frame(), row.names = FALSE)

  ## figure: stacked share of government vs citizen-content frame by age group
  ## (paper Figure fig:frame_by_age, paper/fig/frame_by_age.pdf)
  fr_plot <- framed |>
    mutate(frame = recode(frame,
                          government = "Government institution",
                          citizen    = "Citizen-content platform")) |>
    count(age_group, frame) |>
    group_by(age_group) |> mutate(prop = n / sum(n)) |> ungroup()
  p_frame <- ggplot(fr_plot, aes(x = age_group, y = prop, fill = frame)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey40") +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)),
              position = position_stack(vjust = 0.5), size = 3.2, color = "white") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("Citizen-content platform" = "#CC6677",
                                 "Government institution" = "#332288")) +
    labs(x = "Age group", y = "Share of framed responses", fill = NULL,
         title = "What citizens take the e-petition system to be, by age",
         subtitle = "Open-ended responses coded by object of evaluation (framed responses only)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top", plot.title = element_text(face = "bold", size = 12))
  ggsave(file.path(figdir, "frame_by_age.pdf"), p_frame, width = 8, height = 5)
  cat("\nFrame-by-age figure written to", file.path(figdir, "frame_by_age.pdf"), "\n")

  ## logistic test of the monotonic gradient: P(government frame) ~ age (years)
  age_yr <- raw |> transmute(NO = as.integer(NO), age = as.numeric(SQ2_1),
                             inc = as.numeric(SQ7))
  glm_df <- framed |> left_join(age_yr, by = "NO") |>
    mutate(gov = as.integer(frame == "government"),
           agez = as.numeric(scale(age)), incz = as.numeric(scale(inc)))
  cat("\nLogistic P(government frame) ~ age:\n")
  print(round(summary(glm(gov ~ age, data = glm_df, family = binomial))$coefficients["age", ], 4))

  ## Is the FRAME a class (redistributive) effect rather than a cohort one?
  ## Tests whether higher-income respondents adopt the government frame more.
  ## Result: null -- income is unrelated to the frame; age remains the driver.
  cat("\n--- Government-frame share by income bracket (framed responses) ---\n")
  inc_tab <- glm_df |>
    mutate(inc_bracket = cut(inc, c(0, 2, 4, 7, 8, Inf),
                             labels = c("<200", "200-400", "400-700", "700-1000", "1000+"))) |>
    filter(!is.na(inc_bracket)) |> group_by(inc_bracket) |>
    summarise(gov_pct = round(100 * mean(gov)), n = n())
  print(as.data.frame(inc_tab), row.names = FALSE)
  cat(sprintf("cor(income, government frame) = %.3f\n",
              cor(glm_df$inc, glm_df$gov, use = "complete.obs")))
  cat("Logistic P(government frame) ~ income (bivariate):\n")
  print(round(summary(glm(gov ~ incz, data = glm_df, family = binomial))$coefficients["incz", ], 4))
  cat("Logistic P(government frame) ~ income + age (income net of age):\n")
  print(round(summary(glm(gov ~ incz + agez, data = glm_df, family = binomial))$coefficients[c("incz", "agez"), ], 4))

  ## category-grouping proxy (robustness): government-output vs citizen-input frame
  gov_cats <- c(1, 3, 4, 13); basket_cats <- c(2, 7, 10, 5, 6, 12)
  prox <- df |> filter(!primary %in% c(9L, 14L)) |>
    group_by(age_group) |>
    summarise(gov = round(100 * mean(primary %in% gov_cats)),
              basket = round(100 * mean(primary %in% basket_cats)),
              gov_to_basket = round(mean(primary %in% gov_cats) /
                                    mean(primary %in% basket_cats), 2))
  cat("\nCategory-grouping proxy (gov-output vs citizen-input share by age):\n")
  print(as.data.frame(prox), row.names = FALSE)
} else {
  cat("\n(frame file not found; run code/llm_frame.py to enable frame-by-age analysis)\n")
}
