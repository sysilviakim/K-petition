# Supervised learning: predict pairwise petition choices
# Can observable features predict which petition a respondent chooses?

# Setup ========================================================================
source(here::here("R", "09_survey_descriptives.R"))

## Load full-sample IRT estimates for comparison
post_out <- readRDS(here::here("output/mcmc_out.rds"))
stats_summ <- stats_summ_create(post_out)

# Feature engineering ==========================================================
## text_features loaded from 09_survey_descriptives.R

## Respondent features (from 08_survey_wrangling.R)
resp_features <- df %>%
  select(
    NO, female, age_group, edu4, seoul,
    married, has_young_child, median_income, subj_class3,
    ideo3, ppp, voted_yoon_2022, voted_lee_2025,
    populist, anti_elitist, instit_trust_high
  )

## Build prediction dataset
pred_df <- pwc_df %>%
  filter(!is.na(Choice)) %>%
  mutate(
    id1 = gsub("item.", "", Item1),
    id2 = gsub("item.", "", Item2),
    chose_item1 = as.numeric(Choice == Item1)
  )

## Join petition features for both items
pred_df <- pred_df %>%
  left_join(
    text_features %>%
      rename_with(
        ~ paste0("i1_", .),
        -item
      ) %>%
      rename(id1 = item),
    by = "id1"
  ) %>%
  left_join(
    text_features %>%
      rename_with(
        ~ paste0("i2_", .),
        -item
      ) %>%
      rename(id2 = item),
    by = "id2"
  )

## Join respondent features
pred_df <- pred_df %>%
  left_join(resp_features, by = "NO") %>%
  left_join(
    df %>% select(NO, median_time),
    by = "NO"
  )

## Create difference features (item1 - item2)
pred_df <- pred_df %>%
  mutate(
    diff_quality = i1_quality_sum - i2_quality_sum,
    diff_nchar = i1_nchar - i2_nchar,
    diff_nword = i1_nword - i2_nword,
    diff_nsent = i1_nsent - i2_nsent,
    diff_avg_sent = i1_avg_sent_len -
      i2_avg_sent_len,
    diff_clarity = as.numeric(
      i1_clarity_specificity
    ) - as.numeric(i2_clarity_specificity),
    diff_logic = as.numeric(
      i1_logic_consistency
    ) - as.numeric(i2_logic_consistency),
    diff_manner = as.numeric(
      i1_tone_manner
    ) - as.numeric(i2_tone_manner),
    diff_validity = as.numeric(
      i1_validity_feasibility
    ) - as.numeric(i2_validity_feasibility),
    same_category = as.numeric(
      i1_category == i2_category
    ),
    log_time = log(median_time + 1)
  )

# Model matrix =================================================================
## petition_vars defined in 09_survey_descriptives.R

## Respondent features
resp_vars <- c(
  "female", "age_group", "edu4", "seoul", "married", "has_young_child",
  "median_income", "subj_class3", "ideo3", "ppp",
  "voted_yoon_2022", "voted_lee_2025",
  "populist", "anti_elitist", "instit_trust_high", "log_time"
)

## Full feature set
all_vars <- c(petition_vars, resp_vars)

## Remove rows with NAs
model_df <- pred_df %>%
  select(chose_item1, all_of(all_vars)) %>%
  na.omit()

## One-hot encode for LASSO (factors -> dummies)
model_oh <- cbind(
  model_df["chose_item1"],
  onehot(model_df, all_vars)
)
oh_vars <- setdiff(names(model_oh), "chose_item1")

# Cross-validation =============================================================
set.seed(1234)
n <- nrow(model_df)
k <- 5
folds <- sample(rep(1:k, length.out = n))

## Storage for predictions
cv_preds <- data.frame(
  fold = folds,
  y = model_df$chose_item1,
  logit_pet = NA_real_,
  logit_full = NA_real_,
  lasso_pet = NA_real_,
  lasso_full = NA_real_,
  rf_pet = NA_real_,
  rf_full = NA_real_
)

for (f in 1:k) {
  train_idx <- folds != f
  test_idx <- folds == f

  train <- model_df[train_idx, ]
  test <- model_df[test_idx, ]

  y_train <- train$chose_item1
  y_test <- test$chose_item1

  ## Logistic: petition-only ---------------------------------------------------
  m1 <- glm(
    chose_item1 ~ .,
    data = train[, c("chose_item1", petition_vars)],
    family = binomial
  )
  cv_preds$logit_pet[test_idx] <- predict(
    m1, test,
    type = "response"
  )

  ## Logistic: full ------------------------------------------------------------
  m2 <- glm(
    chose_item1 ~ .,
    data = train[, c("chose_item1", all_vars)],
    family = binomial
  )
  cv_preds$logit_full[test_idx] <- predict(
    m2, test,
    type = "response"
  )

  ## LASSO: petition-only ------------------------------------------------------
  X_train_pet <- as.matrix(
    train[, petition_vars]
  )
  X_test_pet <- as.matrix(
    test[, petition_vars]
  )
  cv_lasso1 <- cv.glmnet(
    X_train_pet, y_train,
    family = "binomial", alpha = 1
  )
  cv_preds$lasso_pet[test_idx] <- as.numeric(
    predict(
      cv_lasso1, X_test_pet,
      s = "lambda.min", type = "response"
    )
  )

  ## LASSO: full (one-hot encoded) ---------------------------------------------
  train_oh <- model_oh[train_idx, ]
  test_oh <- model_oh[test_idx, ]
  X_train_full <- as.matrix(
    train_oh[, oh_vars]
  )
  X_test_full <- as.matrix(
    test_oh[, oh_vars]
  )
  cv_lasso2 <- cv.glmnet(
    X_train_full, y_train,
    family = "binomial", alpha = 1
  )
  cv_preds$lasso_full[test_idx] <- as.numeric(
    predict(
      cv_lasso2, X_test_full,
      s = "lambda.min", type = "response"
    )
  )

  ## Random forest: petition-only ----------------------------------------------
  rf1 <- ranger(
    chose_item1 ~ .,
    data = train[, c("chose_item1", petition_vars)],
    probability = TRUE,
    num.trees = 500,
    min.node.size = 20
  )
  cv_preds$rf_pet[test_idx] <- predict(
    rf1, test
  )$predictions[, 2]

  ## Random forest: full -------------------------------------------------------
  rf2 <- ranger(
    chose_item1 ~ .,
    data = train[, c("chose_item1", all_vars)],
    probability = TRUE,
    num.trees = 500,
    min.node.size = 20
  )
  cv_preds$rf_full[test_idx] <- predict(
    rf2, test
  )$predictions[, 2]
}

# Evaluate =====================================================================
models <-
  c("logit_pet", "logit_full", "lasso_pet", "lasso_full", "rf_pet", "rf_full")
model_labels <- c(
  "Logistic (petition)", "Logistic (full)", "LASSO (petition)", "LASSO (full)",
  "Random Forest (petition)", "Random Forest (full)"
)

auc_results <- data.frame(
  Model = model_labels,
  AUC = sapply(models, function(m) {
    as.numeric(roc(cv_preds$y, cv_preds[[m]], quiet = TRUE)$auc)
  }),
  Accuracy = sapply(models, function(m) {
    pred_class <- as.numeric(cv_preds[[m]] > 0.5)
    mean(pred_class == cv_preds$y)
  }),
  stringsAsFactors = FALSE
)
rownames(auc_results) <- NULL

save_xtable(
  auc_results,
  caption = "Cross-Validated Prediction of Pairwise Petition Choices",
  label = "tab:prediction_cv",
  file = "prediction_cv.tex",
  digits = 3
)

# Nested RF: petition-only + M1-M4 =============================================
## m1_resp-m4_resp defined in 09_survey_descriptives.R
set.seed(1234)
res_pet <- run_rf_cv(
  pred_df, "chose_item1", petition_vars
)
res_m1 <- run_rf_cv(
  pred_df, "chose_item1",
  c(petition_vars, m1_resp)
)
res_m2 <- run_rf_cv(
  pred_df, "chose_item1",
  c(petition_vars, m2_resp)
)
res_m3 <- run_rf_cv(
  pred_df, "chose_item1",
  c(petition_vars, m3_resp)
)
res_m4 <- run_rf_cv(
  pred_df, "chose_item1",
  c(petition_vars, m4_resp)
)

pred_comparison <- data.frame(
  Model = c(
    "Petition only",
    "M1 (demographics)",
    "M2 (+ socioeconomic)",
    "M3 (+ political)",
    "M4 (+ attitudes)"
  ),
  AUC = c(
    res_pet["AUC"], res_m1["AUC"],
    res_m2["AUC"], res_m3["AUC"],
    res_m4["AUC"]
  ),
  Accuracy = c(
    res_pet["Accuracy"], res_m1["Accuracy"],
    res_m2["Accuracy"], res_m3["Accuracy"],
    res_m4["Accuracy"]
  )
)

save_xtable(
  pred_comparison,
  caption = paste(
    "Random Forest Prediction:",
    "Nested Model Comparison"
  ),
  label = "tab:prediction_extended",
  file = "prediction_extended.tex",
  digits = 3
)

# Feature importance ===========================================================
## Fit full random forest on all data for importance
rf_full_all <- ranger(
  chose_item1 ~ .,
  data = model_df[, c("chose_item1", all_vars)],
  importance = "impurity",
  probability = TRUE,
  num.trees = 1000,
  min.node.size = 20
)

imp_df <- data.frame(
  variable = names(rf_full_all$variable.importance),
  importance = rf_full_all$variable.importance
) %>%
  arrange(desc(importance)) %>%
  mutate(
    type = ifelse(
      variable %in% petition_vars,
      "Petition", "Respondent"
    )
  )

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

p_importance <- ggplot(
  imp_df,
  aes(
    x = reorder(variable, importance),
    y = importance, fill = type
  )
) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("Variable Importance (Gini)") +
  scale_x_discrete(labels = label_table_text) +
  scale_fill_viridis_d(name = "Feature Type") +
  importance_theme

pdf(here::here("fig", "rf_variable_importance.pdf"), width = 7, height = 5)
print(p_importance)
dev.off()

# LASSO coefficients ===========================================================
## Fit full LASSO for coefficient interpretation
X_all <- as.matrix(model_oh[, oh_vars])
y_all <- model_df$chose_item1

cv_lasso_all <- cv.glmnet(
  X_all, y_all,
  family = "binomial", alpha = 1
)

lasso_coef <- coef(cv_lasso_all, s = "lambda.min")
lasso_df <- data.frame(
  variable = rownames(lasso_coef),
  coefficient = as.numeric(lasso_coef)
) %>%
  filter(
    variable != "(Intercept)",
    coefficient != 0
  ) %>%
  arrange(desc(abs(coefficient))) %>%
  mutate(
    type = ifelse(
      variable %in% petition_vars,
      "Petition", "Respondent"
    )
  )

p_lasso <- ggplot(
  lasso_df,
  aes(
    x = reorder(variable, abs(coefficient)),
    y = coefficient, fill = type
  )
) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("LASSO Coefficient") +
  scale_x_discrete(labels = label_table_text) +
  scale_fill_viridis_d(name = "Feature Type") +
  importance_theme

pdf(here::here("fig", "lasso_coefficients.pdf"), width = 7, height = 5)
print(p_lasso)
dev.off()

# Prediction vs IRT comparison =================================================
## Can the IRT theta difference predict choices?
## Compare ML prediction to IRT-based prediction
theta_lookup <- stats_summ$theta %>%
  select(item, theta1_median, theta2_median)

pred_irt <- pred_df %>%
  left_join(
    theta_lookup %>%
      rename(
        id1 = item,
        t1_1 = theta1_median,
        t2_1 = theta2_median
      ),
    by = "id1"
  ) %>%
  left_join(
    theta_lookup %>%
      rename(
        id2 = item,
        t1_2 = theta1_median,
        t2_2 = theta2_median
      ),
    by = "id2"
  ) %>%
  mutate(
    ## Simple IRT predictor: Euclidean distance
    ## advantage of item1 over item2
    irt_diff = sqrt(t1_1^2 + t2_1^2) -
      sqrt(t1_2^2 + t2_2^2)
  ) %>%
  filter(!is.na(irt_diff))

roc_irt <- roc(
  pred_irt$chose_item1,
  pred_irt$irt_diff,
  quiet = TRUE
)

## Combined comparison
comparison <- rbind(
  auc_results,
  data.frame(
    Model = "IRT theta distance",
    AUC = as.numeric(roc_irt$auc),
    Accuracy = mean(
      as.numeric(pred_irt$irt_diff > 0) ==
        pred_irt$chose_item1
    )
  )
)

save_xtable(
  comparison,
  caption = "Comparison of Predictive Models for Pairwise Petition Choices",
  label = "tab:model_comparison",
  file = "model_comparison.tex",
  digits = 3
)

# Permutation importance =======================================================
## Gini importance is biased toward continuous variables with many
## possible split points. Permutation importance measures the actual
## drop in prediction accuracy when a feature is shuffled.
rf_perm <- ranger(
  chose_item1 ~ .,
  data = model_df[, c("chose_item1", all_vars)],
  importance = "permutation",
  probability = TRUE,
  num.trees = 1000,
  min.node.size = 20
)

perm_df <- data.frame(
  variable = names(rf_perm$variable.importance),
  importance = rf_perm$variable.importance
) %>%
  arrange(desc(importance)) %>%
  mutate(
    type = ifelse(
      variable %in% petition_vars,
      "Petition", "Respondent"
    )
  )

p_perm <- ggplot(
  perm_df,
  aes(
    x = reorder(variable, importance),
    y = importance, fill = type
  )
) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("Permutation Importance") +
  scale_x_discrete(labels = label_table_text) +
  scale_fill_viridis_d(name = "Feature Type") +
  importance_theme

pdf(here::here("fig", "rf_permutation_importance.pdf"), width = 7, height = 5)
print(p_perm)
dev.off()

## Gini vs Permutation comparison
gini_df <- data.frame(
  variable = names(rf_full_all$variable.importance),
  gini = rf_full_all$variable.importance
)

long_imp <- bind_rows(
  perm_df %>%
    mutate(
      method = "Permutation",
      scaled = importance / max(importance)
    ) %>%
    select(variable, type, method, scaled),
  gini_df %>%
    mutate(
      method = "Gini",
      scaled = gini / max(gini),
      type = ifelse(
        variable %in% petition_vars,
        "Petition", "Respondent"
      )
    ) %>%
    select(variable, type, method, scaled)
)

var_order <- perm_df %>%
  arrange(importance) %>%
  pull(variable)

long_imp$variable <- factor(
  long_imp$variable,
  levels = var_order
)

p_compare <- ggplot(
  long_imp,
  aes(x = variable, y = scaled, fill = type)
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~method) +
  xlab("") +
  ylab("Normalized Importance") +
  scale_x_discrete(labels = label_table_text) +
  scale_fill_viridis_d(name = "Feature Type") +
  importance_theme

pdf(here::here("fig", "importance_comparison.pdf"), width = 12, height = 6)
print(p_compare)
dev.off()
