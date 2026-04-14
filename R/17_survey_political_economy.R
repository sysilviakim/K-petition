# Political economy of petition quality evaluation
# Tests consensus finding against redistribution,
# welfare chauvinism, class politics, and citizen
# competence literatures.
# Sources 09 (no new MCMC runs).

# Setup ========================================================================
source(here::here("R", "09_survey_descriptives.R"))

## Load existing MCMC outputs
post_out <- readRDS(here::here("output/mcmc_out.rds"))
stats_summ <- stats_summ_create(post_out)

# Shared: welfare classification ===============================================
## welfare_categories and nonwelfare_categories
## are defined in utilities.R

classify_welfare <- function(cat) {
  case_when(
    cat %in% welfare_categories ~ "Welfare",
    cat %in% nonwelfare_categories ~ "Non-welfare",
    TRUE ~ "Semi-welfare"
  )
}

# Shared: beneficiary match function ===========================================
beneficiary_match <- function(category,
                              age_group,
                              has_young_child,
                              seoul) {
  case_when(
    category == "\uC800\uCD9C\uC0B0" &
      (has_young_child == 1 | age_group %in% c("20-29", "30-39")) ~ 1,
    category == "\uC5F0\uAE08" &
      age_group %in% c("50-59", "60+") ~ 1,
    category == "\uBD80\uB3D9\uC0B0" &
      (seoul == 1 | age_group %in% c("20-29", "30-39")) ~ 1,
    category == "\uC0AC\uAD50\uC721" &
      (has_young_child == 1 | age_group %in% c("30-39", "40-49")) ~ 1,
    TRUE ~ 0
  )
}

# Beneficiary-Match Choice Accuracy ============================================
## Test: does being a "beneficiary" (stake in the game) change accuracy?

pwc_accuracy <- pwc_df %>%
  filter(!is.na(Choice)) %>%
  mutate(
    id1 = gsub("item.", "", Item1),
    id2 = gsub("item.", "", Item2)
  ) %>%
  left_join(
    post_types %>%
      select(item, comb1 = comb_fin, cat1 = category) %>%
      rename(id1 = item),
    by = "id1"
  ) %>%
  left_join(
    post_types %>%
      select(item, comb2 = comb_fin, cat2 = category) %>%
      rename(id2 = item),
    by = "id2"
  )

pwc_accuracy <- pwc_accuracy %>%
  mutate(
    q1 = count_ones(comb1),
    q2 = count_ones(comb2),
    quality_diff = abs(q1 - q2),
    better_item = case_when(
      q1 > q2 ~ Item1,
      q2 > q1 ~ Item2,
      TRUE ~ NA_character_
    ),
    chose_better = case_when(
      is.na(better_item) ~ NA,
      Choice == better_item ~ TRUE,
      TRUE ~ FALSE
    )
  )

pwc_diff <- pwc_accuracy %>%
  filter(!is.na(chose_better))

## Join respondent demographics
pwc_diff_demo <- pwc_diff %>%
  left_join(
    df %>% select(
      NO, female, age_group, edu4, seoul, married, has_young_child,
      median_income, subj_class3, ideo3, ppp,
      voted_yoon_2022, voted_lee_2025, populist, anti_elitist,
      instit_trust_high, log_time
    ),
    by = "NO"
  )

## Pair category (both items share category)
## Use cat1 as representative
pwc_diff_demo <- pwc_diff_demo %>%
  mutate(
    pair_category = cat1,
    beneficiary = beneficiary_match(pair_category, as.character(age_group),
      has_young_child, seoul)
  )

## Logistic regression with beneficiary
m_beneficiary <- glm(
  chose_better ~ quality_diff + beneficiary + beneficiary:quality_diff +
    female + age_group + edu4 + median_income + subj_class3 + ideo3 + ppp +
    populist + anti_elitist + instit_trust_high + log_time,
  family = binomial,
  data = pwc_diff_demo
)

save_xtable(
  summary(m_beneficiary)$coefficients,
  caption = "Beneficiary-Match and Choice Accuracy: Logistic Regression",
  label = "tab:beneficiary_match_logit",
  file = "beneficiary_match_logit.tex",
  include_rownames = TRUE
)

## Figure: accuracy by category and beneficiary
acc_by_cat <- pwc_diff_demo %>%
  mutate(
    welfare_type = classify_welfare(pair_category),
    beneficiary_lab = ifelse(beneficiary == 1, "Beneficiary", "Non-beneficiary")
  ) %>%
  group_by(welfare_type, beneficiary_lab) %>%
  summarise(
    n = n(),
    accuracy = mean(chose_better),
    se = sqrt(accuracy * (1 - accuracy) / n),
    .groups = "drop"
  )

p_benef <- ggplot(
  acc_by_cat,
  aes(x = welfare_type, y = accuracy, fill = beneficiary_lab)
) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = accuracy - 1.96 * se, ymax = accuracy + 1.96 * se),
    position = position_dodge(0.8),
    width = 0.2
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  xlab("Category Type") +
  ylab("Proportion Choosing Better Petition") +
  ylim(0, 1) +
  scale_fill_viridis_d(name = "", end = 0.7) +
  theme_minimal() +
  theme(legend.position = "bottom")

pdf(
  here::here("fig", "beneficiary_accuracy_by_category.pdf"),
  width = 7, height = 5)
print(p_benef)
dev.off()

# Category-Specific Subgroup Theta Correlations ================================
## Load existing subgroup MCMC outputs
ss_inc0 <- stats_summ_create(
  readRDS(here::here("output/mcmcDP_out_median_income0.rds"))
)
ss_inc1 <- stats_summ_create(
  readRDS(here::here("output/mcmcDP_out_median_income1.rds"))
)
ss_ppp <- stats_summ_create(readRDS(here::here("output/mcmcDP_out_ppp.rds")))
ss_opp <- stats_summ_create(readRDS(here::here("output/mcmcDP_out_opp.rds")))
ss_pop <- stats_summ_create(readRDS(here::here("output/mcmcDP_out_pop.rds")))

## Helper: compute correlations by welfare type
compute_subgroup_cors <- function(ss_a, ss_b, label) {
  merged <- inner_join(
    ss_a$theta %>%
      select(item, category, t1a = theta1_median, t2a = theta2_median),
    ss_b$theta %>%
      select(item, t1b = theta1_median, t2b = theta2_median),
    by = "item"
  ) %>%
    mutate(welfare_type = classify_welfare(category))

  merged %>%
    group_by(welfare_type) %>%
    summarise(
      cor_theta1 = cor(t1a, t1b),
      cor_theta2 = cor(t2a, t2b),
      n_items = n(),
      .groups = "drop"
    ) %>%
    mutate(comparison = label)
}

cors_income <- compute_subgroup_cors(ss_inc0, ss_inc1, "Income")
cors_party <- compute_subgroup_cors(ss_ppp, ss_opp, "Party")

## For populist, compare to full sample
cors_populist <- compute_subgroup_cors(ss_pop, stats_summ, "Populist vs. All")

cors_all <- bind_rows(cors_income, cors_party, cors_populist) %>%
  select(comparison, welfare_type, cor_theta1, cor_theta2, n_items)

save_xtable(
  cors_all,
  caption = "Subgroup Theta Correlations by Welfare Category Type",
  label = "tab:subgroup_category_cors",
  file = "subgroup_category_cors.tex",
  digits = 3
)

## Figure: scatter plots faceted by welfare type
## Income subgroups as main example
scatter_df <- inner_join(
  ss_inc0$theta %>%
    select(item, category, t1_low = theta1_median, t2_low = theta2_median),
  ss_inc1$theta %>%
    select(item, t1_high = theta1_median, t2_high = theta2_median),
  by = "item"
) %>%
  mutate(welfare_type = classify_welfare(category))

p_scatter_t1 <- ggplot(scatter_df, aes(x = t1_low, y = t1_high)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = ACCENT) +
  facet_wrap(~welfare_type) +
  xlab("Theta 1D (Below Median Income)") +
  ylab("Theta 1D (Above Median Income)") +
  theme_minimal()

p_scatter_t2 <- ggplot(scatter_df, aes(x = t2_low, y = t2_high)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = ACCENT) +
  facet_wrap(~welfare_type) +
  xlab("Theta 2D (Below Median Income)") +
  ylab("Theta 2D (Above Median Income)") +
  theme_minimal()

p_scatter_combined <- ggarrange(p_scatter_t1, p_scatter_t2,
  ncol = 1, nrow = 2)

pdf(here::here("fig", "subgroup_theta_by_welfare.pdf"), width = 10, height = 8)
print(p_scatter_combined)
dev.off()

# Category-Stratified Feature Importance =======================================
## Build prediction dataset (same as script 16)
pred_df <- pwc_df %>%
  filter(!is.na(Choice)) %>%
  mutate(
    id1 = gsub("item.", "", Item1),
    id2 = gsub("item.", "", Item2),
    chose_item1 = as.numeric(Choice == Item1)
  ) %>%
  left_join(
    text_features %>%
      rename_with(~ paste0("i1_", .), -item) %>%
      rename(id1 = item),
    by = "id1"
  ) %>%
  left_join(
    text_features %>%
      rename_with(~ paste0("i2_", .), -item) %>%
      rename(id2 = item),
    by = "id2"
  ) %>%
  left_join(
    df %>%
      select(NO, all_of(m4_resp), median_time),
    by = "NO"
  ) %>%
  mutate(
    diff_quality = i1_quality_sum - i2_quality_sum,
    diff_nchar = i1_nchar - i2_nchar,
    diff_nword = i1_nword - i2_nword,
    diff_nsent = i1_nsent - i2_nsent,
    diff_avg_sent = i1_avg_sent_len - i2_avg_sent_len,
    diff_clarity = as.numeric(i1_clarity_specificity) -
      as.numeric(i2_clarity_specificity),
    diff_logic = as.numeric(i1_logic_consistency) -
      as.numeric(i2_logic_consistency),
    diff_manner = as.numeric(i1_tone_manner) - as.numeric(i2_tone_manner),
    diff_validity = as.numeric(i1_validity_feasibility) -
      as.numeric(i2_validity_feasibility),
    same_category = as.numeric(i1_category == i2_category),
    welfare_type = classify_welfare(i1_category)
  )

## Split by welfare type and run RF
pred_w <- pred_df %>% filter(welfare_type == "Welfare")
pred_nw <- pred_df %>% filter(welfare_type == "Non-welfare")
pred_sw <- pred_df %>% filter(welfare_type == "Semi-welfare")

res_w_pet <- run_rf_cv(pred_w, "chose_item1", petition_vars)
res_w_full <- run_rf_cv(pred_w, "chose_item1", c(petition_vars, m4_resp))

res_nw_pet <- run_rf_cv(pred_nw, "chose_item1", petition_vars)
res_nw_full <- run_rf_cv(pred_nw, "chose_item1", c(petition_vars, m4_resp))

res_sw_pet <- run_rf_cv(pred_sw, "chose_item1", petition_vars)
res_sw_full <- run_rf_cv(pred_sw, "chose_item1", c(petition_vars, m4_resp))

pred_by_welfare <- data.frame(
  Category = rep(c("Welfare", "Non-welfare", "Semi-welfare"), each = 2),
  Model = rep(c("Petition only", "Full (M4)"), 3),
  AUC = c(res_w_pet["AUC"], res_w_full["AUC"], res_nw_pet["AUC"],
    res_nw_full["AUC"], res_sw_pet["AUC"], res_sw_full["AUC"]),
  Accuracy = c(res_w_pet["Accuracy"], res_w_full["Accuracy"],
    res_nw_pet["Accuracy"], res_nw_full["Accuracy"],
    res_sw_pet["Accuracy"], res_sw_full["Accuracy"])
)

save_xtable(
  pred_by_welfare,
  caption = "Random Forest Prediction by Welfare Category Type",
  label = "tab:prediction_by_welfare",
  file = "prediction_by_welfare.tex",
  digits = 3
)

## Permutation importance by welfare type
imp_welfare <- fit_and_rank(pred_w, "chose_item1", c(petition_vars, m4_resp),
  ref_vars = petition_vars) %>%
  mutate(subset = "Welfare")

imp_nonwelfare <- fit_and_rank(pred_nw, "chose_item1",
  c(petition_vars, m4_resp), ref_vars = petition_vars) %>%
  mutate(subset = "Non-welfare")

imp_combined <- bind_rows(imp_welfare, imp_nonwelfare)

importance_theme <- theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 12)
  )

p_imp_welfare <- ggplot(
  imp_combined %>%
    group_by(subset) %>%
    slice_max(importance, n = 15) %>%
    ungroup(),
  aes(x = reorder(variable, importance), y = importance, fill = type)
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~subset, scales = "free_y") +
  xlab("") +
  ylab("Permutation Importance") +
  scale_x_discrete(labels = label_table_text) +
  scale_fill_viridis_d(name = "Feature Type") +
  importance_theme

pdf(here::here("fig", "importance_by_welfare.pdf"), width = 12, height = 6)
print(p_imp_welfare)
dev.off()

# Class Gradient in Dimension Sensitivity ======================================
## Decompose quality difference into presentation vs substance
pwc_decomposed <- pwc_diff_demo %>%
  mutate(
    diff_pres = abs(as.numeric(substr(comb1, 1, 1)) -
      as.numeric(substr(comb2, 1, 1))) +
      abs(as.numeric(substr(comb1, 3, 3)) - as.numeric(substr(comb2, 3, 3))),
    diff_sub = abs(as.numeric(substr(comb1, 2, 2)) -
      as.numeric(substr(comb2, 2, 2))) +
      abs(as.numeric(substr(comb1, 4, 4)) - as.numeric(substr(comb2, 4, 4))),
    pair_type = case_when(
      diff_pres > diff_sub ~ "Presentation",
      diff_sub > diff_pres ~ "Substance",
      TRUE ~ "Mixed"
    ),
    pair_type = factor(pair_type,
      levels = c("Presentation", "Mixed", "Substance"))
  )

m_dimension_class <- glm(
  chose_better ~ pair_type * median_income + pair_type * subj_class3 +
    log_time,
  family = binomial,
  data = pwc_decomposed
)

save_xtable(
  summary(m_dimension_class)$coefficients,
  caption = "Accuracy by Quality Dimension and Economic Position",
  label = "tab:accuracy_dimension_class",
  file = "accuracy_dimension_class.tex",
  include_rownames = TRUE
)

## Figure: grouped bar chart
acc_dim_class <- pwc_decomposed %>%
  group_by(pair_type, median_income) %>%
  summarise(
    n = n(),
    accuracy = mean(chose_better),
    se = sqrt(accuracy * (1 - accuracy) / n),
    .groups = "drop"
  )

p_dim_class <- ggplot(
  acc_dim_class,
  aes(x = pair_type, y = accuracy, fill = median_income)
) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = accuracy - 1.96 * se, ymax = accuracy + 1.96 * se),
    position = position_dodge(0.8),
    width = 0.2
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  xlab("Pair Type (Dominant Dimension)") +
  ylab("Proportion Choosing Better") +
  ylim(0, 1) +
  scale_fill_viridis_d(name = "Income", end = 0.7) +
  theme_minimal() +
  theme(legend.position = "bottom")

pdf(here::here("fig", "accuracy_by_dimension_class.pdf"), width = 7, height = 5)
print(p_dim_class)
dev.off()

# Gamma Interaction with Economic Position =====================================
gamma_df <- stats_summ$gamma

m_gamma_interact <- lm(
  median ~ female + age_group + edu4 + seoul + married + has_young_child +
    median_income * subj_class3 + ideo3 + ppp + voted_yoon_2022 +
    voted_lee_2025 + median_income * populist + median_income * anti_elitist +
    instit_trust_high,
  data = gamma_df
)

save_xtable(
  summary(m_gamma_interact)$coefficients,
  caption = "Gamma Regression with Economic Position Interactions",
  label = "tab:gamma_interactions",
  file = "gamma_interactions.tex",
  include_rownames = TRUE
)

## Marginal effect plots
## Compute predicted gamma by income x class
newdata_class <- expand.grid(
  female = 0,
  age_group = factor("40-49", levels = levels(gamma_df$age_group)),
  edu4 = factor("College grad", levels = levels(gamma_df$edu4)),
  seoul = 0,
  married = 1,
  has_young_child = 0,
  median_income = levels(gamma_df$median_income),
  subj_class3 = levels(gamma_df$subj_class3),
  ideo3 = factor("moderate", levels = levels(gamma_df$ideo3)),
  ppp = 0,
  voted_yoon_2022 = 0,
  voted_lee_2025 = 0,
  populist = 0,
  anti_elitist = 0,
  instit_trust_high = 0
)

preds_class <- predict(m_gamma_interact, newdata_class, se.fit = TRUE)
newdata_class$fit <- preds_class$fit
newdata_class$se <- preds_class$se.fit

p_interact_class <- ggplot(
  newdata_class,
  aes(x = subj_class3, y = fit, color = median_income, group = median_income)
) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(
    aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
    position = position_dodge(0.3),
    width = 0.2
  ) +
  xlab("Subjective Class") +
  ylab(expression("Predicted" ~ gamma)) +
  scale_color_viridis_d(name = "Income", end = 0.7) +
  theme_minimal() +
  theme(legend.position = "bottom")

## Income x populist
newdata_pop <- expand.grid(
  female = 0,
  age_group = factor("40-49", levels = levels(gamma_df$age_group)),
  edu4 = factor("College grad", levels = levels(gamma_df$edu4)),
  seoul = 0,
  married = 1,
  has_young_child = 0,
  median_income = levels(gamma_df$median_income),
  subj_class3 = factor("Middle", levels = levels(gamma_df$subj_class3)),
  ideo3 = factor("moderate", levels = levels(gamma_df$ideo3)),
  ppp = 0,
  voted_yoon_2022 = 0,
  voted_lee_2025 = 0,
  populist = c(0, 1),
  anti_elitist = 0,
  instit_trust_high = 0
)

preds_pop <- predict(m_gamma_interact, newdata_pop, se.fit = TRUE)
newdata_pop$fit <- preds_pop$fit
newdata_pop$se <- preds_pop$se.fit
newdata_pop$pop_lab <- ifelse(newdata_pop$populist == 1,
  "Populist", "Non-populist")

p_interact_pop <- ggplot(
  newdata_pop,
  aes(x = pop_lab, y = fit, color = median_income, group = median_income)
) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(
    aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
    position = position_dodge(0.3),
    width = 0.2
  ) +
  xlab("Populist Attitudes") +
  ylab(expression("Predicted" ~ gamma)) +
  scale_color_viridis_d(name = "Income", end = 0.7) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_gamma_interact <- ggarrange(p_interact_class, p_interact_pop,
  ncol = 2, common.legend = TRUE, legend = "bottom")

pdf(here::here("fig", "gamma_interaction_effects.pdf"), width = 10, height = 5)
print(p_gamma_interact)
dev.off()
