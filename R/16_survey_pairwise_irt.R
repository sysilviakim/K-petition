# Fits the Bayesian pairwise IRT for the survey data

# Setup ========================================================================
source(here::here("R", "15_survey_descriptives.R"))

# MCMC: 2D pairwise IRT =======================================================
fname <- here("output/mcmc_out.rds")
set.seed(1234)
if (!file.exists(fname)) {
  post_out <- MCMCpaircompare2d(
    pwc.data = pwc_df,
    theta.constraints = theta_constraints,
    burnin = 5000,
    mcmc = 100000,
    thin = 5,
    verbose = 3000,
    store.theta = TRUE,
    store.gamma = TRUE,
    tune = 0.5
  )
  saveRDS(post_out, fname)
} else {
  post_out <- readRDS(fname)
}

stats_summ <- stats_summ_create(post_out)

## Convergence diagnostics ---------------------------------------------------
theta_cols <- grep(
  "^theta", colnames(post_out), value = TRUE
)
pdf(
  here("fig", "mcmc_diagnostics.pdf"),
  width = 10, height = 8
)
par(mfrow = c(2, 2))
for (col in theta_cols[1:4]) {
  traceplot(post_out[, col], main = col)
}
dev.off()

## Visualize -----------------------------------------------------------------
pdf(
  here("fig", "theta_post_median.pdf"),
  width = 6, height = 6
)
theta_post_viz(stats_summ, label = TRUE)
dev.off()

pdf(
  here("fig", "theta_post_median_clarity.pdf"),
  width = 6, height = 6
)
theta_post_viz(
  stats_summ, color = "clarity_specificity"
) +
  guides(
    color = guide_legend(
      title = "Clarity and Specificity"
    )
  )
dev.off()

pdf(
  here("fig", "theta_post_median_logic.pdf"),
  width = 6, height = 6
)
theta_post_viz(
  stats_summ, color = "logic_consistency"
) +
  guides(
    color = guide_legend(
      title = "Logic and Consistency"
    )
  )
dev.off()

pdf(
  here("fig", "theta_post_median_manner.pdf"),
  width = 6, height = 6
)
theta_post_viz(
  stats_summ, color = "tone_manner"
) +
  guides(
    color = guide_legend(
      title = "Tone and Manner"
    )
  )
dev.off()

pdf(
  here("fig", "theta_post_median_validity.pdf"),
  width = 6, height = 6
)
theta_post_viz(
  stats_summ, color = "validity_feasibility"
) +
  guides(
    color = guide_legend(
      title = "Validity and Feasibility"
    )
  )
dev.off()

# MCMC: 2D pairwise IRT with DP ===============================================
fname <- here("output/mcmcDP_out.rds")
if (!file.exists(fname)) {
  postDP_out <- MCMCpaircompare2dDP(
    pwc.data = pwc_df,
    theta.constraints = theta_constraints,
    burnin = 5000,
    mcmc = 100000,
    thin = 5,
    verbose = 10000,
    store.theta = TRUE,
    store.gamma = TRUE,
    tune = 0.5
  )
  saveRDS(postDP_out, fname)
} else {
  postDP_out <- readRDS(fname)
}

stats_summ_dp <- stats_summ_create(postDP_out)

## Visualize -----------------------------------------------------------------
pdf(
  here("fig", "theta_post_median_dp.pdf"),
  width = 6, height = 6
)
theta_post_viz(stats_summ_dp, label = TRUE)
dev.off()

pdf(
  here(
    "fig",
    "theta_post_median_dp_clarity.pdf"
  ),
  width = 6, height = 6
)
theta_post_viz(
  stats_summ_dp,
  color = "clarity_specificity"
) +
  guides(
    color = guide_legend(
      title = "Clarity and Specificity"
    )
  )
dev.off()

pdf(
  here(
    "fig", "theta_post_median_dp_logic.pdf"
  ),
  width = 6, height = 6
)
theta_post_viz(
  stats_summ_dp, color = "logic_consistency"
) +
  guides(
    color = guide_legend(
      title = "Logic and Consistency"
    )
  )
dev.off()

pdf(
  here(
    "fig", "theta_post_median_dp_manner.pdf"
  ),
  width = 6, height = 6
)
theta_post_viz(
  stats_summ_dp, color = "tone_manner"
) +
  guides(
    color = guide_legend(
      title = "Tone and Manner"
    )
  )
dev.off()

pdf(
  here(
    "fig",
    "theta_post_median_dp_validity.pdf"
  ),
  width = 6, height = 6
)
theta_post_viz(
  stats_summ_dp,
  color = "validity_feasibility"
) +
  guides(
    color = guide_legend(
      title = "Validity and Feasibility"
    )
  )
dev.off()

# DP cluster analysis ==========================================================
n_clust <- table(postDP_out[, "n.clusters"])

dp_cluster <- stats_summ_dp$gamma %>%
  mutate(cluster = (median < 1)) %>%
  group_split(cluster) %>%
  set_names(c("1", "2")) %>%
  map(
    ~ .x$respondent %>%
      str_extract("\\d+") %>%
      as.numeric()
  ) %>%
  map(~ df %>% filter(NO %in% .x)) %>%
  bind_rows(.id = "cluster") %>%
  group_by(cluster) %>%
  summarise(
    sex = mean(SQ1),
    age = mean(SQ2_1),
    edu = mean(SQ3),
    married = mean(SQ5),
    kid = mean(SQ6),
    income = mean(SQ7),
    life = mean(Q1),
    ideal = mean(Q2),
    party = get_mode(Q3)
  )

dp_xtable <- xtable(
  dp_cluster,
  caption = paste(
    "DP Cluster Demographics Summary"
  ),
  label = "tab:dp_cluster"
)
print(
  dp_xtable,
  include.rownames = FALSE,
  booktabs = TRUE,
  file = here("tab", "dp_cluster.tex")
)

## Combined figure for paper -------------------------------------------------
p_combined <- theta_post_viz(
  stats_summ, label = TRUE
)
pdf(
  here("fig", "theta_post_median_combined.pdf"),
  width = 7, height = 7
)
p_combined
dev.off()
