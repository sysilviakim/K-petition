# Response time robustness check
# Exclude inattentive respondents and re-run IRT

# Setup ========================================================================
source(here::here("R", "09_survey_descriptives.R"))

## Load full-sample model
post_out <- readRDS(here::here("output/mcmc_out.rds"))
stats_summ <- stats_summ_create(post_out)

# Flag fast respondents (likely inattentive) ===================================
time_wide <- df %>%
  select(NO, all_of(time_vars))

time_wide <- time_wide %>%
  mutate(
    median_time = apply(
      select(., all_of(time_vars)), 1, median
    ),
    min_time = apply(
      select(., all_of(time_vars)), 1, min
    )
  )

## Thresholds: median response time < 3 seconds
## or any single pair < 1 second
time_wide <- time_wide %>%
  mutate(
    inattentive = median_time < 3 | min_time < 1
  )


# Re-run IRT excluding inattentive respondents =================================
attentive_ids <- time_wide %>%
  filter(!inattentive) %>%
  pull(NO)

pwc_attentive <- pwc_df %>%
  filter(NO %in% attentive_ids)

fname_att <- here::here("output/mcmc_out_attentive.rds")
set.seed(1234)
if (!file.exists(fname_att)) {
  post_attentive <- MCMCpaircompare2d(
    pwc.data = as.data.frame(pwc_attentive),
    theta.constraints = theta_constraints,
    burnin = MCMC_BURNIN,
    mcmc = MCMC_ITER,
    thin = MCMC_THIN,
    verbose = 3000,
    store.theta = TRUE,
    store.gamma = TRUE,
    tune = MCMC_TUNE
  )
  saveRDS(post_attentive, fname_att)
} else {
  post_attentive <- readRDS(fname_att)
}

ss_attentive <- stats_summ_create(post_attentive)

## Correlation between full and attentive theta estimates
theta_compare <- inner_join(
  stats_summ$theta %>%
    select(
      item,
      theta1_full = theta1_median,
      theta2_full = theta2_median
    ),
  ss_attentive$theta %>%
    select(
      item,
      theta1_att = theta1_median,
      theta2_att = theta2_median
    ),
  by = "item"
)

cor_t1 <- cor(theta_compare$theta1_full, theta_compare$theta1_att)
cor_t2 <- cor(theta_compare$theta2_full, theta_compare$theta2_att)
p_robust <- ggplot(
  theta_compare,
  aes(x = theta1_full, y = theta1_att)
) +
  geom_point() +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = ACCENT
  ) +
  xlab("Theta 1D (Full Sample)") +
  ylab("Theta 1D (Attentive Only)") +
  annotate(
    "text",
    x = -2, y = 2,
    label = paste("r =", round(cor_t1, 3)),
    hjust = 0, size = 5
  ) +
  theme_minimal()

p_robust2 <- ggplot(
  theta_compare,
  aes(x = theta2_full, y = theta2_att)
) +
  geom_point() +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = ACCENT
  ) +
  xlab("Theta 2D (Full Sample)") +
  ylab("Theta 2D (Attentive Only)") +
  annotate(
    "text",
    x = -2, y = 2,
    label = paste("r =", round(cor_t2, 3)),
    hjust = 0, size = 5
  ) +
  theme_minimal()

p_robust_combined <- ggarrange(p_robust, p_robust2, ncol = 2)
pdf(here::here("fig", "robustness_attentive.pdf"), width = 10, height = 5)
print(p_robust_combined)
dev.off()
