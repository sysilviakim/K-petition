source(here::here("R", "09_survey_descriptives.R"))

# Setup and wrangling ==========================================================
## Load full-sample model
post_out <- readRDS(here("output/mcmc_out.rds"))
stats_summ <- stats_summ_create(post_out)

## Perceived effectiveness (Q15, 0-100 slider) ---------------------------------
## Each respondent rated 5 petitions individually
eff_long <- map_dfr(1:5, function(i) {
  df %>%
    select(
      NO,
      item = !!sym(paste0("Q15_gCode", i)),
      effectiveness = !!sym(paste0("Q15_", i))
    )
}) %>%
  mutate(item = as.character(item))

## Petition-level totals + overall mean ----------------------------------------
mean_eff <- eff_long %>%
  group_by(item) %>%
  summarise(
    sum_eff = sum(effectiveness),
    n_rated = n(),
    .groups = "drop"
  ) %>%
  mutate(mean_eff = sum_eff / n_rated)

## Pairwise accuracy: quality diff + LOO eff -----------------------------------
## For petition j and respondent i:
##   if i rated j: (sum - rating_ij) / (n - 1)
##   else: sum / n  (overall mean)
pwc_accuracy <- pwc_df %>%
  filter(!is.na(Choice)) %>%
  mutate(
    id1 = gsub("item.", "", Item1),
    id2 = gsub("item.", "", Item2)
  ) %>%
  left_join(
    post_types %>%
      select(item, comb1 = comb_fin) %>%
      rename(id1 = item),
    by = "id1"
  ) %>%
  left_join(
    post_types %>%
      select(item, comb2 = comb_fin) %>%
      rename(id2 = item),
    by = "id2"
  ) %>%
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
  ) %>%
  left_join(
    eff_long %>%
      rename(id1 = item, own_eff1 = effectiveness),
    by = c("NO", "id1")
  ) %>%
  left_join(
    mean_eff %>%
      select(
        id1 = item,
        sum1 = sum_eff, n1 = n_rated
      ),
    by = "id1"
  ) %>%
  mutate(
    eff1 = if_else(
      !is.na(own_eff1),
      (sum1 - own_eff1) / (n1 - 1),
      sum1 / n1
    )
  ) %>%
  left_join(
    eff_long %>%
      rename(id2 = item, own_eff2 = effectiveness),
    by = c("NO", "id2")
  ) %>%
  left_join(
    mean_eff %>%
      select(
        id2 = item,
        sum2 = sum_eff, n2 = n_rated
      ),
    by = "id2"
  ) %>%
  mutate(
    eff2 = if_else(
      !is.na(own_eff2),
      (sum2 - own_eff2) / (n2 - 1),
      sum2 / n2
    )
  ) %>%
  select(
    -own_eff1, -sum1, -n1,
    -own_eff2, -sum2, -n2
  ) %>%
  mutate(
    eff_diff = abs(eff1 - eff2),
    eff_better_item = case_when(
      eff1 > eff2 ~ Item1,
      eff2 > eff1 ~ Item2,
      TRUE ~ NA_character_
    ),
    eff_chose_better = case_when(
      is.na(eff_better_item) ~ NA,
      Choice == eff_better_item ~ TRUE,
      TRUE ~ FALSE
    )
  )

## Only pairs with a clear quality difference ----------------------------------
pwc_diff <- pwc_accuracy %>%
  filter(!is.na(chose_better))

## Pairs with effectiveness-based better item ----------------------------------
pwc_eff_diff <- pwc_accuracy %>%
  filter(!is.na(eff_chose_better)) %>%
  mutate(
    eff_gap_bin = cut(
      eff_diff,
      breaks = quantile(
        eff_diff,
        probs = c(0, 0.25, 0.5, 0.75, 1)
      ),
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4")
    )
  )

# Choice accuracy model ========================================================

## Accuracy by quality gap by experts ------------------------------------------
acc_expert <- pwc_diff %>%
  group_by(quality_diff) %>%
  summarise(
    n = n(),
    accuracy = mean(chose_better),
    se = sqrt(accuracy * (1 - accuracy) / n),
    .groups = "drop"
  )

p_acc_expert <- ggplot(
  acc_expert,
  aes(x = factor(quality_diff), y = accuracy)
) +
  geom_col(fill = ACCENT) +
  geom_errorbar(
    aes(
      ymin = accuracy - 1.96 * se,
      ymax = accuracy + 1.96 * se
    ),
    width = 0.2
  ) +
  geom_text(
    aes(label = sprintf("%.2f", accuracy)),
    vjust = -1.5, size = 3
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed", color = ACCENT
  ) +
  xlab("Expert Score Difference (0–4)") +
  ylab("Proportion Choosing Better Petition") +
  ylim(0, 1) +
  theme_minimal()

pdf(here("fig", "choice_acc_expert.pdf"), width = 4, height = 3.5)
print(p_acc_expert)
dev.off()

## Accuracy by perceived effectiveness gap
acc_eff <- pwc_eff_diff %>%
  group_by(eff_gap_bin) %>%
  summarise(
    n = n(),
    accuracy = mean(eff_chose_better),
    se = sqrt(accuracy * (1 - accuracy) / n),
    .groups = "drop"
  )

p_acc_eff <- ggplot(
  acc_eff,
  aes(x = eff_gap_bin, y = accuracy)
) +
  geom_col(fill = ACCENT) +
  geom_errorbar(
    aes(
      ymin = accuracy - 1.96 * se,
      ymax = accuracy + 1.96 * se
    ),
    width = 0.2
  ) +
  geom_text(
    aes(label = sprintf("%.2f", accuracy)),
    vjust = -1.5, size = 3
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed", color = ACCENT
  ) +
  xlab("Mean Effectiveness Gap (Quartile)") +
  ylab("Proportion Choosing Higher-Rated Petition") +
  ylim(0, 1) +
  theme_minimal()

pdf(here("fig", "choice_acc_eff.pdf"), width = 6, height = 5)
print(p_acc_eff)
dev.off()

## LPM: what predicts choosing better? -----------------------------------------
## Common RHS formula
acc_rhs <- paste(
  "female + age_group + edu4 + seoul",
  "+ married + has_young_child + median_income + subj_class3",
  "+ ideo3 + ppp + voted_yoon_2022 + voted_lee_2025",
  "+ populist + anti_elitist + instit_trust_high + log_time"
)

## (a) Expert coding (count of 1s) ---------------------------------------------
pwc_expert_demo <- pwc_diff %>%
  left_join(df, by = "NO")

lm_acc_expert <- lm(
  as.formula(paste("chose_better ~ quality_diff +", acc_rhs)),
  data = pwc_expert_demo
)
summary(lm_acc_expert)

ct_expert <- coeftest(
  lm_acc_expert,
  vcov = vcovHC(lm_acc_expert, type = "HC1")
)

save_xtable(
  unclass(ct_expert),
  caption = "LPM: Choosing the Higher-Quality Petition (Expert Coding)",
  label = "tab:choice_accuracy_expert",
  file = "choice_accuracy_expert.tex",
  include_rownames = TRUE
)

## (b) Mean perceived effectiveness --------------------------------------------
pwc_eff_demo <- pwc_eff_diff %>%
  left_join(df, by = "NO")

lm_acc_eff <- lm(
  as.formula(paste("eff_chose_better ~ eff_diff +", acc_rhs)),
  data = pwc_eff_demo
)

ct_eff <- coeftest(
  lm_acc_eff,
  vcov = vcovHC(lm_acc_eff, type = "HC1")
)

save_xtable(
  unclass(ct_eff),
  caption = 
    "LPM: Choosing the Higher-Effectiveness Petition (Respondent Rating)",
  label = "tab:choice_accuracy_eff",
  file = "choice_accuracy_eff.tex",
  include_rownames = TRUE
)
