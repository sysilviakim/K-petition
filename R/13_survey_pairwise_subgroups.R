# Subgroup pairwise IRT analysis

# Setup ========================================================================
source(here::here("R", "09_survey_descriptives.R"))

## Helper: run DP model for a subgroup
run_subgroup_dp <- function(sub_pwc,
                            fname,
                            constraints) {
  if (!file.exists(fname)) {
    out <- MCMCpaircompare2dDP(
      pwc.data = as.data.frame(sub_pwc),
      theta.constraints = constraints,
      burnin = MCMC_BURNIN,
      mcmc = MCMC_ITER,
      thin = MCMC_THIN,
      verbose = 5000,
      store.theta = TRUE,
      store.gamma = TRUE,
      tune = MCMC_TUNE
    )
    saveRDS(out, fname)
  } else {
    out <- readRDS(fname)
  }
  out
}

## Helper: make 2x2 grid of theta plots
make_subgroup_grid <- function(ss, title) {
  p1 <- theta_post_viz(ss, label = TRUE) +
    ggtitle(title)
  p2 <- theta_post_viz(
    ss,
    color = "clarity_specificity"
  ) +
    guides(
      color = guide_legend(
        title = "Clarity"
      )
    )
  p3 <- theta_post_viz(
    ss,
    color = "logic_consistency"
  ) +
    guides(
      color = guide_legend(
        title = "Logic"
      )
    )
  p4 <- theta_post_viz(
    ss,
    color = "tone_manner"
  ) +
    guides(
      color = guide_legend(
        title = "Manner"
      )
    )
  ggarrange(
    p1, p2, p3, p4,
    ncol = 2, nrow = 2
  )
}

# By economic status ===========================================================
pwc_with_demo <- pwc_df %>%
  left_join(
    df %>% select(NO, median_income),
    by = "NO"
  )

post_income0 <- run_subgroup_dp(
  pwc_with_demo %>%
    filter(median_income == "Below Median") %>%
    select(-median_income),
  here::here("output/mcmcDP_out_median_income0.rds"),
  theta_constraints
)

post_income1 <- run_subgroup_dp(
  pwc_with_demo %>%
    filter(median_income == "Above Median") %>%
    select(-median_income),
  here::here("output/mcmcDP_out_median_income1.rds"),
  theta_constraints
)

ss_income0 <- stats_summ_create(post_income0)
ss_income1 <- stats_summ_create(post_income1)

g_income0 <- make_subgroup_grid(
  ss_income0, "Below Median Income"
)
pdf(here::here("fig", "theta_post_dp_income0.pdf"), width = 10, height = 10)
print(g_income0)
dev.off()

g_income1 <- make_subgroup_grid(
  ss_income1, "Above Median Income"
)
pdf(here::here("fig", "theta_post_dp_income1.pdf"), width = 10, height = 10)
print(g_income1)
dev.off()

# By party =====================================================================
pwc_with_party <- pwc_df %>%
  left_join(
    df %>% select(NO, pid3),
    by = "NO"
  )

post_ppp <- run_subgroup_dp(
  pwc_with_party %>%
    filter(pid3 == "PPP") %>%
    select(-pid3),
  here::here("output/mcmcDP_out_ppp.rds"),
  theta_constraints
)

post_opp <- run_subgroup_dp(
  pwc_with_party %>%
    filter(pid3 == "Opposition") %>%
    select(-pid3),
  here::here("output/mcmcDP_out_opp.rds"),
  theta_constraints
)

ss_ppp <- stats_summ_create(post_ppp)
ss_opp <- stats_summ_create(post_opp)

g_ppp <- make_subgroup_grid(
  ss_ppp, "PPP Supporters"
)
pdf(here::here("fig", "theta_post_median_combined_ppp.pdf"), width = 10, height = 10)
print(g_ppp)
dev.off()

g_opp <- make_subgroup_grid(
  ss_opp, "Opposition Supporters"
)
pdf(here::here("fig", "theta_post_median_combined_opp.pdf"), width = 10, height = 10)
print(g_opp)
dev.off()

# By populist attitudes ========================================================
pwc_with_pop <- pwc_df %>%
  left_join(
    df %>% select(NO, populist),
    by = "NO"
  )

post_pop <- run_subgroup_dp(
  pwc_with_pop %>%
    filter(populist == TRUE) %>%
    select(-populist),
  here::here("output/mcmcDP_out_pop.rds"),
  theta_constraints
)

ss_pop <- stats_summ_create(post_pop)

g_pop <- make_subgroup_grid(
  ss_pop, "Populist Attitudes"
)
pdf(here::here("fig", "theta_post_median_combined_pop.pdf"), width = 10, height = 10)
print(g_pop)
dev.off()

# Combined subgroup comparison =================================================
## Load full-sample model from script 16
fname_full <- here::here("output/mcmc_out.rds")
if (!file.exists(fname_full)) {
  stop(
    "Run 10_survey_pairwise_irt.R first ",
    "to generate mcmc_out.rds"
  )
}
ss_all <- stats_summ_create(
  readRDS(fname_full)
)

p_all <- theta_post_viz(
  ss_all,
  label = TRUE
) + ggtitle("All Respondents")
p_ppp_c <- theta_post_viz(
  ss_ppp,
  label = TRUE
) + ggtitle("PPP Supporters")
p_opp_c <- theta_post_viz(
  ss_opp,
  label = TRUE
) + ggtitle("Opposition")
p_pop_c <- theta_post_viz(
  ss_pop,
  label = TRUE
) + ggtitle("Populist Attitudes")

g_subgroups <- ggarrange(
  p_all, p_ppp_c, p_opp_c, p_pop_c,
  ncol = 2, nrow = 2
)
pdf(here::here("fig", "theta_post_median_subgroups.pdf"), width = 12, height = 12)
print(g_subgroups)
dev.off()
