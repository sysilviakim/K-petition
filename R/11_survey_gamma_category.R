# Gamma regression and category confounds
# 1. What predicts respondent dimension weighting (gamma)?
# 2. Do petition categories cluster in the theta space?

# Setup ========================================================================
source(here::here("R", "09_survey_descriptives.R"))

## Load full-sample model
post_out <- readRDS(here("output/mcmc_out.rds"))
stats_summ <- stats_summ_create(post_out)

## Load DP model
postDP_out <- readRDS(here("output/mcmcDP_out.rds"))
stats_summ_dp <- stats_summ_create(postDP_out)

# 1. Gamma regression ==========================================================
## gamma = respondent direction parameter (angle in radians)
## Higher gamma -> more weight on theta2 (logic/validity)
## Lower gamma -> more weight on theta1 (clarity/manner)

gamma_df <- stats_summ$gamma

## T-tests ---------------------------------------------------------------------
t_party <- t.test(
  gamma_df$median[gamma_df$pid3 == "PPP"],
  gamma_df$median[gamma_df$pid3 == "Opposition"]
)
t_party

t_income <- t.test(
  gamma_df$median[gamma_df$median_income == "Below Median"],
  gamma_df$median[gamma_df$median_income == "Above Median"]
)
t_income

## Significant
t_populist <- t.test(
  gamma_df$median[gamma_df$populist == TRUE],
  gamma_df$median[gamma_df$populist == FALSE]
)
t_populist

## Export t-test and KS test results
summarise_ttest <- function(tt, label) {
  data.frame(
    Comparison = label,
    t = round(tt$statistic, 3),
    df = round(tt$parameter, 1),
    p = round(tt$p.value, 4),
    Mean_1 = round(tt$estimate[1], 3),
    Mean_2 = round(tt$estimate[2], 3),
    row.names = NULL
  )
}

ttest_table <- bind_rows(
  summarise_ttest(t_party, "PPP vs. Opp."),
  summarise_ttest(t_income, "Below vs. Above"),
  summarise_ttest(t_populist, "Pop. vs. Non-pop.")
)

## KS tests --------------------------------------------------------------------
ks_party <- ks.test(
  gamma_df$median[gamma_df$pid3 == "PPP"],
  gamma_df$median[gamma_df$pid3 == "Opposition"]
)

ks_income <- ks.test(
  gamma_df$median[
    gamma_df$median_income == "Below Median"
  ],
  gamma_df$median[
    gamma_df$median_income == "Above Median"
  ]
)

## Significant
ks_populist <- ks.test(
  gamma_df$median[gamma_df$populist == TRUE],
  gamma_df$median[gamma_df$populist == FALSE]
)

summarise_ks <- function(ks, label) {
  data.frame(
    Comparison = label,
    D = round(ks$statistic, 3),
    p = round(ks$p.value, 4),
    row.names = NULL
  )
}

ks_table <- bind_rows(
  summarise_ks(ks_party, "PPP vs. Opp."),
  summarise_ks(ks_income, "Below vs. Above"),
  summarise_ks(ks_populist, "Pop. vs. Non-pop.")
)

## Combined t-test + KS table
gamma_tests <- ttest_table %>%
  left_join(
    ks_table %>%
      rename(KS_D = D, KS_p = p),
    by = "Comparison"
  )

save_xtable(
  gamma_tests,
  caption = "T-Tests and KS Tests of $\\\\gamma$ by Subgroup",
  label = "tab:gamma_tests",
  file = "gamma_tests.tex",
  digits = 4,
  sanitize = TRUE
)

## Regression ------------------------------------------------------------------
m_gamma <- lm(
  median ~ female + age_group + edu4 + seoul + married +
    has_young_child + median_income + subj_class3 +
    ideo3 + ppp + voted_yoon_2022 + voted_lee_2025 +
    populist + anti_elitist + instit_trust_high,
  data = gamma_df
)
summary(m_gamma)

## Save regression table
## key vars: populist, female
save_xtable(
  summary(m_gamma),
  caption = paste(
    "OLS Regression of Respondent Direction",
    "Parameter ($\\\\gamma$) on Demographics"
  ),
  label = "tab:gamma_regression",
  file = "gamma_regression.tex",
  include_rownames = TRUE,
  sanitize = TRUE
)

# 2. Category confounds ========================================================
## Do petition categories cluster in the theta space?
## If yes, perceived quality may partly reflect topic preference
theta_df <- stats_summ$theta
theta_df$category_en <- category_en[theta_df$category]

p_category <- ggplot(
  theta_df,
  aes(
    x = theta1_median, y = theta2_median,
    color = category_en
  )
) +
  geom_point(size = 3) +
  xlim(-3, 3) +
  ylim(-3, 3) +
  xlab("Theta 1D (Clarity/Manner)") +
  ylab("Theta 2D (Logic/Validity)") +
  scale_color_viridis_d(name = "Topic Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

pdf(here("fig", "theta_by_category.pdf"), width = 5, height = 5)
print(Kmisc::pdf_default(p_category))
dev.off()

## ANOVA: does category predict theta position?
m_cat_t1 <- aov(
  theta1_median ~ category,
  data = theta_df
)
summary(m_cat_t1)

m_cat_t2 <- aov(
  theta2_median ~ category,
  data = theta_df
)
summary(m_cat_t2)

## Export ANOVA results
summarise_aov <- function(m, label) {
  s <- summary(m)[[1]]
  data.frame(
    Outcome = label,
    F = round(s["category", "F value"], 3),
    df1 = s["category", "Df"],
    df2 = s["Residuals", "Df"],
    p = round(s["category", "Pr(>F)"], 4),
    Eta_sq = round(
      s["category", "Sum Sq"] /
        sum(s[, "Sum Sq"]),
      3
    ),
    row.names = NULL
  )
}

anova_table <- bind_rows(
  summarise_aov(m_cat_t1, "Theta 1D"),
  summarise_aov(m_cat_t2, "Theta 2D")
)

save_xtable(
  anova_table,
  caption = "One-Way ANOVA: Petition Category Predicting Theta Position",
  label = "tab:anova_category",
  file = "anova_category.tex",
  digits = 4,
  sanitize = TRUE
)

## Regression: quality codes vs category ---------------------------------------
m_quality_only <- lm(
  theta1_median ~ clarity_specificity +
    logic_consistency + tone_manner +
    validity_feasibility,
  data = theta_df
)
m_quality_cat <- lm(
  theta1_median ~ clarity_specificity +
    logic_consistency + tone_manner +
    validity_feasibility + category,
  data = theta_df
)

## Same for theta2
m_quality_only_t2 <- lm(
  theta2_median ~ clarity_specificity +
    logic_consistency + tone_manner +
    validity_feasibility,
  data = theta_df
)
m_quality_cat_t2 <- lm(
  theta2_median ~ clarity_specificity +
    logic_consistency + tone_manner +
    validity_feasibility + category,
  data = theta_df
)

## Export quality-code regressions
## Compare quality-only vs quality + category
quality_reg <- bind_rows(
  broom::tidy(m_quality_only) %>%
    mutate(
      model = "Quality only",
      outcome = "Theta 1D"
    ),
  broom::tidy(m_quality_cat) %>%
    mutate(
      model = "Quality + Category",
      outcome = "Theta 1D"
    ),
  broom::tidy(m_quality_only_t2) %>%
    mutate(
      model = "Quality only",
      outcome = "Theta 2D"
    ),
  broom::tidy(m_quality_cat_t2) %>%
    mutate(
      model = "Quality + Category",
      outcome = "Theta 2D"
    )
) %>%
  select(
    outcome, model, term,
    estimate, std.error, p.value
  )

save_xtable(
  quality_reg,
  caption = "OLS: Quality Codes vs. Quality Codes + Category Predicting Theta",
  label = "tab:quality_category_reg",
  file = "quality_category_reg.tex",
  digits = 3
)

## Model fit comparison
quality_fit <- data.frame(
  Outcome = c(
    "Theta 1D", "Theta 1D",
    "Theta 2D", "Theta 2D"
  ),
  Model = rep(
    c("Quality only", "Quality + Category"),
    2
  ),
  R2 = c(
    summary(m_quality_only)$r.squared,
    summary(m_quality_cat)$r.squared,
    summary(m_quality_only_t2)$r.squared,
    summary(m_quality_cat_t2)$r.squared
  ),
  Adj_R2 = c(
    summary(m_quality_only)$adj.r.squared,
    summary(m_quality_cat)$adj.r.squared,
    summary(m_quality_only_t2)$adj.r.squared,
    summary(m_quality_cat_t2)$adj.r.squared
  )
)

save_xtable(
  quality_fit,
  caption = "Model Fit: Quality Codes vs. Quality + Category",
  label = "tab:quality_category_fit",
  file = "quality_category_fit.tex",
  digits = 4
)
