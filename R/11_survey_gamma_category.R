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

m_gamma <- lm(
  median ~ female + age_group + edu4 + seoul + married + 
    has_young_child + median_income + subj_class3 + 
    ideo3 + ppp + voted_yoon_2022 + voted_lee_2025 + 
    populist_broad + instit_trust_high,
  data = gamma_df
)

## Save regression table
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

## Visualize gamma distribution by subgroup
p_gamma_party <- ggplot(
  gamma_df %>% filter(pid3 != "Other"),
  aes(x = median, fill = pid3)
) +
  geom_density(alpha = 0.5) +
  xlab(expression(gamma ~ "(Respondent Direction)")) +
  ylab("Density") +
  scale_fill_viridis_d(name = "Party") +
  theme_minimal() +
  theme(legend.position = "bottom")

p_gamma_pop <- ggplot(
  gamma_df,
  aes(
    x = median,
    fill = factor(
      populist,
      labels = c("Non-Populist", "Populist")
    )
  )
) +
  geom_density(alpha = 0.5) +
  xlab(expression(gamma ~ "(Respondent Direction)")) +
  ylab("Density") +
  scale_fill_viridis_d(name = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

p_gamma_income <- ggplot(
  gamma_df,
  aes(x = median, fill = median_income)
) +
  geom_density(alpha = 0.5) +
  xlab(expression(gamma ~ "(Respondent Direction)")) +
  ylab("Density") +
  scale_fill_viridis_d(name = "Income") +
  theme_minimal() +
  theme(legend.position = "bottom")

p_gamma_combined <- ggarrange(
  p_gamma_party, p_gamma_pop, p_gamma_income,
  ncol = 3, nrow = 1
)
pdf(
  here("fig", "gamma_distributions.pdf"),
  width = 14, height = 4.5
)
print(p_gamma_combined)
dev.off()

## KS tests for formal comparison
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
ks_pop <- ks.test(
  gamma_df$median[gamma_df$populist == TRUE],
  gamma_df$median[gamma_df$populist == FALSE]
)

# 2. Category confounds ========================================================
## Do petition categories cluster in the theta space?
## If yes, perceived quality may partly reflect topic preference
theta_df <- stats_summ$theta

p_category <- ggplot(
  theta_df,
  aes(
    x = theta1_median, y = theta2_median,
    color = category
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

pdf(
  here("fig", "theta_by_category.pdf"),
  width = 7, height = 7
)
print(p_category)
dev.off()

## ANOVA: does category predict theta position?
m_cat_t1 <- aov(
  theta1_median ~ category,
  data = theta_df
)
m_cat_t2 <- aov(
  theta2_median ~ category,
  data = theta_df
)

## Regression: quality codes vs category
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
