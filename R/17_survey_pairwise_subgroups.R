# Setup ========================================================================
source(here::here("R", "15_survey_descriptives.R"))

# By economic status ===========================================================
temp <- pwc_df %>%
  left_join(survey_rename(df) %>% select(NO, median_income))

fname <- here("output/mcmcDP_out_median_income0.rds")
if (!file.exists(fname)) {
  postDP_median_income0_out <- MCMCpaircompare2dDP(
    pwc.data = temp %>%
      filter(median_income == "Below Median") %>%
      select(-median_income),
    theta.constraints = theta_constraints,
    burnin = 500, mcmc = 10000, thin = 5, verbose = 1000,
    store.theta = TRUE, store.gamma = TRUE, tune = 0.5
  )

  postDP_median_income1_out <- MCMCpaircompare2dDP(
    pwc.data = temp %>%
      filter(median_income == "Above Median") %>%
      select(-median_income),
    theta.constraints = theta_constraints,
    burnin = 500, mcmc = 10000, thin = 5, verbose = 1000,
    store.theta = TRUE, store.gamma = TRUE, tune = 0.5
  )
  
  saveRDS(
    postDP_median_income0_out,
    here("output/mcmcDP_out_median_income0.rds")
  )
  saveRDS(
    postDP_median_income1_out,
    here("output/mcmcDP_out_median_income1.rds")
  )
} else {
  postDP_median_income0_out <-
    readRDS(here("output/mcmcDP_out_median_income0.rds"))
  postDP_median_income1_out <-
    readRDS(here("output/mcmcDP_out_median_income1.rds"))
}

stats_summ_median_income0 <- stats_summ_create(postDP_median_income0_out)
stats_summ_median_income1 <- stats_summ_create(postDP_median_income1_out)

## Visualize
p0_list <- stats_summ_median_income0 %>%
  list(
    p1 = theta_post_viz(.x, label = TRUE),
    p2 = theta_post_viz(.x, color = "clarity_specificity") +
      guides(color = guide_legend(title = "Clarity and Specificity")),
    p3 = theta_post_viz(.x, color = "logic_consistency") +
      guides(color = guide_legend(title = "Logic and Consistency")),
    p4 = theta_post_viz(.x, color = "tone_manner") +
      guides(color = guide_legend(title = "Tone and Manner"))
  )

p1_list <- stats_summ_median_income1 %>%
  list(
    p1 = theta_post_viz(.x, label = TRUE),
    p2 = theta_post_viz(.x, color = "clarity_specificity") +
      guides(color = guide_legend(title = "Clarity and Specificity")),
    p3 = theta_post_viz(.x, color = "logic_consistency") +
      guides(color = guide_legend(title = "Logic and Consistency")),
    p4 = theta_post_viz(.x, color = "tone_manner") +
      guides(color = guide_legend(title = "Tone and Manner"))
  )

ggpubr::ggarrange(
  plotlist = p0_list[c("p1", "p2", "p3", "p4")],
  ncol = 2, nrow = 2
)
ggsave("fig/theta_post_DP_income_median0.pdf", width = 6, height = 6)

ggpubr::ggarrange(
  plotlist = p1_list[c("p1", "p2", "p3", "p4")],
  ncol = 2, nrow = 2
)
ggsave("fig/theta_post_DP_income_median1.pdf", width = 6, height = 6)

