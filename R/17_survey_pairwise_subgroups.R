# Setup ========================================================================
source(here::here("R", "15_survey_descriptives.R"))
theta_constraints <- list(
  item.1 = list(1, 2),
  item.1 = list(2, 2),
  item.15 = list(1, -2),
  item.15 = list(2, -2),
  item.49 = list(1, "-"),
  item.49 = list(2, "+")
)

# By economic status ===========================================================
temp <- pwc_df %>%
  left_join(survey_rename(df) %>% select(NO, median_income))

fname <- here("output/mcmcDP_out_median_income.rds")
if (!file.exists(fname)) {
  postDP_median0_out <- MCMCpaircompare2dDP(
    pwc.data = temp %>%
      filter(median_income == "Below Median") %>%
      select(-median_income),
    theta.constraints = theta_constraints,
    burnin = 500, mcmc = 10000, thin = 5, verbose = 1000,
    store.theta = TRUE, store.gamma = TRUE, tune = 0.5
  )

  postDP_median1_out <- MCMCpaircompare2dDP(
    pwc.data = temp %>%
      filter(median_income == "Above Median") %>%
      select(-median_income),
    theta.constraints = theta_constraints,
    burnin = 500, mcmc = 10000, thin = 5, verbose = 1000,
    store.theta = TRUE, store.gamma = TRUE, tune = 0.5
  )
  
  saveRDS(
    list(
      median0 = postDP_median0_out,
      median1 = postDP_median1_out
    ),
    fname
  )
} else {
  postDP_out <- readRDS(fname)
}
