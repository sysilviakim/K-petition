## Feeling-thermometer analysis (Q15): how citizens RATE real petitions, and
## whether their ratings cohere with what they SAY in the open-ended Q16.
##
## Three layers:
##   L1  thermo ~ quality dims (tone/clarity/logic/validity) + length  -> does
##       rating reward presentation over substance? (validation vs Paper 1)
##   L2  thermo ~ quality x open-ended opinion type  -> THE BRIDGE: do citizens
##       who voice quality concerns penalize low-quality petitions more?
##   L3  thermo ~ quality x demographics + demographic intercepts -> life-cycle
##
## Unit: respondent-petition pair (~6,110). SEs two-way clustered by respondent
## (NO) and petition (pid). Topic fixed effects (not petition FE: quality
## covariates are petition-level). Self-contained; does not source utilities.R.
##
## ---- PAPER EXHIBITS produced here (stable tab:/fig: labels in paper/open.tex) ----
##   Table 4  (tab:thermo_quality)  thermometer ~ quality dimensions     [LAYER 1]
##   Figure 3 (fig:bridge) + Table 7 (tab:bridge)  quality slope by opinion type  [LAYER 2]
##   Table 5  (tab:thermo_demo)     level + quality x age/gender         [LAYER 3]
##   Table 6  (tab:adjudicate)      verdicts on competing accounts       [ADJUDICATION block]
##   plus the frame slope (3.8 vs 2.1) and the H-free account numbers in Sec. 5.4-5.5 & Appendix A
## Table/Figure NUMBERS are for the current draft; the tab:/fig: labels are the stable refs.
## NOTE: reads the companion continuous-quality CSV by hard-coded path; if absent, the
##       assembled model frame is cached in output/thermo_model_frame.rds.

Sys.setlocale("LC_ALL", "en_US.UTF-8")
suppressPackageStartupMessages({
  library(tidyverse); library(readxl); library(here); library(stringi)
  library(fixest); library(modelsummary); library(scales)
})

out <- here("output"); dir.create(out, showWarnings = FALSE)
figdir <- here("paper", "fig"); dir.create(figdir, recursive = TRUE, showWarnings = FALSE)
tabdir <- here("tab"); dir.create(tabdir, showWarnings = FALSE)

# 1. Thermometer long form (NO, pid, thermo) =================================
fname <- here(stri_trans_nfc("data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx"))
file.copy(fname, "/tmp/survey_thermo.xlsx", overwrite = TRUE)
raw <- read_xlsx("/tmp/survey_thermo.xlsx", sheet = "Raw") |> mutate(NO = as.integer(NO))

long <- raw |>
  dplyr::select(NO, starts_with("Q15_gCode"), Q15_1, Q15_2, Q15_3, Q15_4, Q15_5)
gc <- paste0("Q15_gCode", 1:5); sc <- paste0("Q15_", 1:5)
long <- map_dfr(1:5, ~ tibble(NO = long$NO, pid = long[[gc[.x]]], thermo = long[[sc[.x]]])) |>
  filter(!is.na(pid), !is.na(thermo)) |>
  mutate(pid = as.integer(pid), thermo = as.numeric(thermo))

# 2. Petition covariates =====================================================
pet <- read_xlsx(here(stri_trans_nfc("data/공개제안_tidy_url_appended.xlsx"))) |>
  mutate(bits = sprintf("%04d", as.integer(comb_fin)),
         clarity_b  = as.integer(substr(bits, 1, 1)),  # bit order confirmed:
         logic_b    = as.integer(substr(bits, 2, 2)),  # Clarity, Logic,
         tone_b     = as.integer(substr(bits, 3, 3)),  # Tone, Validity
         validity_b = as.integer(substr(bits, 4, 4)),
         len_chars  = nchar(text),
         topic = category)

## continuous LLM scores (matched by title) from the companion project
ev <- read_csv(here("data", "tidy", "evaluated_data_2024_final.csv"),
               show_col_types = FALSE) |>
  distinct(title, .keep_all = TRUE) |>
  dplyr::select(title, clarity_specificity_score, logic_consistency_score,
                tone_manner_score, validity_feasibility_score)
pet <- pet |> left_join(ev, by = "title")

## petition-level standardization (1 SD = 1-SD petition difference)
z <- function(x) as.numeric(scale(x))
pet <- pet |>
  mutate(clarity  = z(clarity_specificity_score),
         logic    = z(logic_consistency_score),
         tone     = z(tone_manner_score),
         validity = z(validity_feasibility_score),
         length_z = z(len_chars),
         presentation = z((clarity + tone) / 2),     # Paper-1 style axis
         substance    = z((logic + validity) / 2),
         quality_idx  = z((clarity + logic + tone + validity) / 4))

# 3. Respondent opinion type (open-ended primary) + demographics =============
od <- read_csv(here("output", "openended_analysis_data.csv"), show_col_types = FALSE) |>
  dplyr::select(NO, opinion = primary_en, opinion_id = primary,
                gender, age_group, edu3, marital, income, occupation, ideo3)

# 4. Assemble model frame ====================================================
d <- long |>
  left_join(pet |> dplyr::select(pid = id, topic, clarity, logic, tone, validity,
                                 length_z, presentation, substance, quality_idx,
                                 clarity_b, logic_b, tone_b, validity_b), by = "pid") |>
  left_join(od, by = "NO") |>
  filter(!is.na(quality_idx)) |>
  ## drop non-substantive opinion types from bridge models (keep for L1/L3)
  mutate(opinion = factor(opinion))

## demographic control vector X_i, added to every interaction model (bridge, age, frame)
CTRL <- "gender + marital + edu3 + income + ideo3"

cat("Model frame:", nrow(d), "resp-petition pairs |",
    n_distinct(d$NO), "respondents |", n_distinct(d$pid), "petitions\n")

## respondents per opinion type (power check for the bridge)
cat("\n--- Respondents per opinion type (primary label) ---\n")
print(od |> count(opinion, sort = TRUE) |> as.data.frame(), row.names = FALSE)

# ============================================================================
# LAYER 1: does rating reward presentation over substance?
# -> PAPER Table 4 (tab:thermo_quality): cols 1 (LLM dims), 2 (pres/subst), 3 (expert binary)
# ============================================================================
l1_llm <- feols(thermo ~ tone + clarity + logic + validity + length_z | topic,
                data = d, cluster = ~ NO + pid)
l1_axis <- feols(thermo ~ presentation + substance + length_z | topic,
                 data = d, cluster = ~ NO + pid)
l1_bin <- feols(thermo ~ tone_b + clarity_b + logic_b + validity_b | topic,
                data = d, cluster = ~ NO + pid)

cat("\n========== LAYER 1: thermometer ~ petition quality ==========\n")
print(etable(l1_llm, l1_axis, l1_bin,
             headers = c("LLM dims", "Pres/Subst", "Expert binary")))
modelsummary(list("LLM dimensions" = l1_llm, "Presentation/Substance" = l1_axis,
                  "Expert binary" = l1_bin),
             output = file.path(tabdir, "thermo_layer1.tex"),
             stars = TRUE, gof_omit = "AIC|BIC|RMSE|Within|Std")

# ============================================================================
# LAYER 2: THE BRIDGE -- quality slope by open-ended opinion type
# -> PAPER Figure 3 (fig:bridge, the p2 plot) and Table 7 (tab:bridge, the `slopes` object)
# ============================================================================
## interact composite quality index with opinion type; report slope per type.
db <- d |> filter(!opinion_id %in% c(9L, 14L)) |>
  mutate(opinion = fct_drop(opinion))
l2 <- feols(as.formula(paste("thermo ~ quality_idx * opinion +", CTRL, "| topic")),
            data = db, cluster = ~ NO + pid)

## marginal quality slope for each opinion type = base + interaction
co <- coef(l2); vc <- vcov(l2)
base <- "quality_idx"
int_terms <- grep("^quality_idx:opinion", names(co), value = TRUE)
types <- sub("^quality_idx:opinion", "", int_terms)
ref_type <- setdiff(levels(db$opinion), types)[1]
slopes <- tibble(
  opinion = c(ref_type, types),
  slope = c(co[base], co[base] + co[int_terms]),
  ## SE via delta method (var(base) + var(int) + 2cov)
  se = c(sqrt(vc[base, base]),
         sapply(int_terms, function(t)
           sqrt(vc[base, base] + vc[t, t] + 2 * vc[base, t])))) |>
  mutate(lo = slope - 1.96 * se, hi = slope + 1.96 * se) |>
  arrange(slope)

cat("\n========== LAYER 2: quality slope by opinion type ==========\n")
print(as.data.frame(slopes), row.names = FALSE, digits = 3)
write_csv(slopes, file.path(out, "thermo_quality_slope_by_opinion.csv"))

p2 <- ggplot(slopes, aes(x = slope, y = reorder(opinion, slope))) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi)) +
  labs(x = "Thermometer points per 1-SD petition quality",
       y = NULL,
       title = "Do citizens who voice a concern rate by quality?",
       subtitle = "Quality-index slope on thermometer, by open-ended opinion type") +
  theme_minimal(base_size = 11)
ggsave(file.path(figdir, "thermo_bridge_slopes.pdf"), p2, width = 9, height = 5)

## presentation vs substance slopes by opinion type (do readability/quality
## critics weight presentation more?)
l2ps <- feols(as.formula(paste("thermo ~ (presentation + substance) * opinion +", CTRL, "| topic")),
              data = db, cluster = ~ NO + pid)
modelsummary(list("Quality x opinion" = l2, "Pres/Subst x opinion" = l2ps),
             output = file.path(tabdir, "thermo_layer2.tex"),
             stars = TRUE, gof_omit = "AIC|BIC|RMSE|Within|Std")

# ============================================================================
# LAYER 3: demographics -- thermometer level and quality slope by age
# -> PAPER Table 5 (tab:thermo_demo): l3_lvl = col 1 (level), l3_age = col 2 (quality x age,
#    with demographic controls X_i); l3_age_re = within-person robustness cited in Sec. 5.3.2.
#    (Quality x gender was dropped from the paper: atheoretical and statistically null.)
# ============================================================================
l3_lvl <- feols(thermo ~ age_group + gender + marital + edu3 + income + ideo3 | topic,
                data = d, cluster = ~ NO + pid)
l3_age <- feols(as.formula(paste("thermo ~ quality_idx * age_group +", CTRL, "| topic")),
                data = d, cluster = ~ NO + pid)

## within-person robustness: respondent FE absorbs every between-person confound
## (level warmth, scale use, acquiescence); the quality x age interaction is then
## identified purely within respondent. Age main effects drop (collinear w/ NO FE).
l3_age_re <- feols(thermo ~ quality_idx * age_group | NO + topic, data = d, cluster = ~ NO + pid)
cat("\n--- L3 robustness: quality x age, respondent FE (within-person) ---\n")
print(round(coeftable(l3_age_re)[grep("quality_idx:age", rownames(coeftable(l3_age_re))), ], 4))

cat("\n========== LAYER 3: demographics ==========\n")
print(etable(l3_lvl, l3_age, headers = c("Level", "Quality x age")))
modelsummary(list("Thermo level" = l3_lvl, "Quality x age" = l3_age),
             output = file.path(tabdir, "thermo_layer3.tex"),
             stars = TRUE, gof_omit = "AIC|BIC|RMSE|Within|Std")

saveRDS(d, file.path(out, "thermo_model_frame.rds"))

# ============================================================================
# ADJUDICATION OF COMPETING ACCOUNTS + FRAME (cohort-framing analysis)
# Reproduces the strong-inference and frame numbers reported in the paper.
# -> PAPER Table 6 (tab:adjudicate) verdicts; the frame-slope test (mf / mf_re:
#    3.8 vs 2.1, p=.047 / .024) and the eliminated-account numbers (usage, trust,
#    acquiescence, scale-use, individualism) appear in Sec. 5.4-5.5 and Appendix A.
# ============================================================================
num <- function(x) suppressWarnings(as.numeric(x))

## continuous age + selected survey items (respondent level)
aux <- raw |>
  transmute(
    NO,
    age        = num(SQ2_1),
    never_used = !is.na(Q8_7),                       # Q8_7 = "never used any"
    n_used     = (!is.na(Q8_1)) + (!is.na(Q8_2)) + (!is.na(Q8_3)) +
                 (!is.na(Q8_4)) + (!is.na(Q8_5)) + (!is.na(Q8_6)),
    trust      = rowMeans(across(c(Q7_1, Q7_2, Q7_3, Q7_4, Q7_5, Q7_6, Q7_7), num), na.rm = TRUE),
    q11_pos    = rowMeans(across(c(Q11_1, Q11_2, Q11_3), num), na.rm = TRUE),  # positive impressions
    q11_neg    = rowMeans(across(c(Q11_4, Q11_5), num), na.rm = TRUE),          # negative impressions
    self_expr  = !is.na(Q10_5),                       # self-expression motive
    collective = (!is.na(Q10_1)) | (!is.na(Q10_2)) | (!is.na(Q10_3))) |>
  mutate(ever_used = n_used > 0,
         age_group = cut(age, c(19, 29, 39, 49, 59, Inf),
                         labels = c("20s", "30s", "40s", "50s", "60+")))

## object-of-evaluation frame (from code/llm_frame.py)
frame <- read_csv(here("output", "openended_frame_classified.csv"),
                  show_col_types = FALSE) |>
  dplyr::select(NO, frame)

## obs-level frame augmented with continuous age, trust, ever_used, frame
dd <- d |>
  left_join(aux |> dplyr::select(NO, age, trust, ever_used), by = "NO") |>
  left_join(frame, by = "NO") |>
  filter(!is.na(age)) |>
  mutate(agec = age - mean(age), trustz = as.numeric(scale(trust)))

cat("\n========== EFFECT SIZE: standardized warmth gap by age ==========\n")
sd_t <- sd(d$thermo)
g20 <- mean(d$thermo[d$age_group == "20s"]); g60 <- mean(d$thermo[d$age_group == "60+"])
cat(sprintf("thermo SD = %.1f; 20s = %.1f, 60+ = %.1f; gap = %.1f pts (%.2f SD)\n",
            sd_t, g20, g60, g60 - g20, (g60 - g20) / sd_t))

cat("\n========== AGE SHAPE: warmth & quality slope, linear vs quadratic ==========\n")
print(round(coeftable(feols(thermo ~ agec | topic, dd, cluster = ~ NO + pid))["agec", ], 4))
print(round(coeftable(feols(thermo ~ agec + I(agec^2) | topic, dd, cluster = ~ NO + pid))[c("agec", "I(agec^2)"), ], 5))
print(round(coeftable(feols(thermo ~ quality_idx * agec | topic, dd, cluster = ~ NO + pid))["quality_idx:agec", ], 5))

cat("\n========== scale use: quality slope x age on within-person z ==========\n")
dz <- dd |> group_by(NO) |> mutate(psd = sd(thermo), pm = mean(thermo), nn = n()) |>
  ungroup() |> filter(nn >= 3, psd > 0) |> mutate(zthermo = (thermo - pm) / psd)
print(round(coeftable(feols(zthermo ~ quality_idx * agec | topic, dz, cluster = ~ NO + pid))["quality_idx:agec", ], 5))

cat("\n========== Quality slope NOT structured by class/party (joint Wald p) ==========\n")
for (v in c("edu3", "income", "ideo3")) {
  dv <- dd |> filter(!is.na(.data[[v]]))
  m  <- feols(as.formula(paste0("thermo ~ quality_idx*", v, " | topic")), dv, cluster = ~ NO + pid)
  w  <- tryCatch(fixest::wald(m, paste0("quality_idx:", v)), error = function(e) NULL)
  cat(sprintf("  quality_idx x %-6s : joint p = %s\n", v, ifelse(is.null(w), "NA", signif(w$p, 3))))
}

cat("\n========== FRAME: quality slope by object-of-evaluation frame ==========\n")
db_fr <- dd |> filter(frame %in% c("government", "citizen")) |>
  mutate(frame = factor(frame, levels = c("government", "citizen")))
mf <- feols(as.formula(paste("thermo ~ quality_idx * frame +", CTRL, "| topic")),
            db_fr, cluster = ~ NO + pid)
cof <- coef(mf)
cat(sprintf("  government-frame slope = %.2f ; citizen-frame slope = %.2f\n",
            cof["quality_idx"], cof["quality_idx"] + cof["quality_idx:framecitizen"]))
print(round(coeftable(mf)["quality_idx:framecitizen", ], 4))
## within-person robustness: respondent FE -> frame slope gap firms (p~.024)
mf_re <- feols(thermo ~ quality_idx * frame | NO + topic, db_fr, cluster = ~ NO + pid)
cat("  [respondent-FE, within-person] quality x frame interaction:\n")
print(round(coeftable(mf_re)["quality_idx:framecitizen", ], 4))

cat("\n========== ELIMINATED ACCOUNTS (respondent-level summaries) ==========\n")
cat("usage: never-used rate & mean platforms by age\n")
print(aux |> group_by(age_group) |> summarise(never = round(mean(never_used), 2),
        n_used = round(mean(n_used), 2)), n = 20)
cat("usage mediation: age->warmth before/after controlling ever_used\n")
print(round(coeftable(feols(thermo ~ agec | topic, dd, cluster = ~ NO + pid))["agec", ], 4))
print(round(coeftable(feols(thermo ~ agec + ever_used | topic, dd, cluster = ~ NO + pid))[c("agec", "ever_usedTRUE"), ], 4))
cat("trust (Q7) by age + mediation (trustz)\n")
print(aux |> group_by(age_group) |> summarise(trust = round(mean(trust, na.rm = TRUE), 2)), n = 20)
print(round(coeftable(feols(thermo ~ agec + trustz | topic, dd, cluster = ~ NO + pid))[c("agec", "trustz"), ], 4))
cat("acquiescence: Q11 positive/negative by age (flat across age = no yea-saying)\n")
print(aux |> group_by(age_group) |> summarise(q11_pos = round(mean(q11_pos, na.rm = TRUE), 2),
        q11_neg = round(mean(q11_neg, na.rm = TRUE), 2)), n = 20)
cat("individualism: Q10 self-expression vs collective by age (users only)\n")
print(aux |> filter(ever_used) |> group_by(age_group) |>
        summarise(self_expr = round(mean(self_expr), 2),
                  collective = round(mean(collective), 2), n = n()), n = 20)

cat("\nDone. Tables -> tab/thermo_layer{1,2,3}.tex ; figures -> paper/fig/thermo_bridge_slopes.pdf\n")
