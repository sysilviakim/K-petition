# Fits the Bayesian pairwise IRT for the survey data

# Setup ========================================================================
source(here::here("R", "15_survey_descriptives.R"))

## pet_list <- read_xlsx("data/screenshots/공개제안_pairs.xlsx")
## pair question id: Q13_1 ~ Q13_8
## petition id for choice: Q13_1 -> (Q13_gCode1_1, Q13_gCode1_2)

## Experimenting
## Researcher's labels don't match the respondents, maybe?
sort(table(pwc_df$Choice), decreasing = TRUE)

## set 1st dimension to be about clarity and manner
## set 2nd dimension to be about logic and validity
# theta_constraints <- list(
#   item.57 = list(1, 2),
#   item.57 = list(2, 2),
#   item.15 = list(1, -2),
#   item.15 = list(2, -2),
#   item.54 = list(1, "+"),
#   item.54 = list(2, "-")
# )

# MCMC run  ====================================================================
## 2 dim pairwise IRT ----------------------------------------------------------
fname <- here("output/mcmc_out.rds")
set.seed(1234)
if (!file.exists(fname)) {
  post_out <- MCMCpaircompare2d(
    pwc.data = pwc_df,
    theta.constraints = theta_constraints,
    burnin = 5000, mcmc = 100000, thin = 5, verbose = 3000,
    store.theta = TRUE, store.gamma = TRUE, tune = 0.5
  )
  saveRDS(post_out, fname)
} else {
  post_out <- readRDS(fname)
}

stats_summ <- stats_summ_create(post_out)

## Visualize -------------------------------------------------------------------
pdf("output/main/theta_post_median.pdf", width = 6, height = 6)
theta_post_viz(stats_summ, label = TRUE)
dev.off()

pdf("output/main/theta_post_median_clarity.pdf", width = 6, height = 6)
theta_post_viz(stats_summ, color = "clarity_specificity") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Clarity and Specificity"))
dev.off()

pdf("output/main/theta_post_median_logic.pdf", width = 6, height = 6)
theta_post_viz(stats_summ, color = "logic_consistency") +
  guides(color = guide_legend(title = "Logic and Consistency"))
dev.off()

pdf("output/main/theta_post_median_manner.pdf", width = 6, height = 6)
theta_post_viz(stats_summ, color = "tone_manner") +
  guides(color = guide_legend(title = "Tone and Manner"))
dev.off()

pdf("output/main/theta_post_median_validity.pdf", width = 6, height = 6)
theta_post_viz(stats_summ, color = "validity_feasibility") +
  guides(color = guide_legend(title = "Validity and Feasibility"))
dev.off()

## 2 dim pairwise IRT DP -------------------------------------------------------
fname <- here("output/mcmcDP_out.rds")
if (!file.exists(fname)) {
  postDP_out <- MCMCpaircompare2dDP(
    pwc.data = pwc_df,
    theta.constraints = theta_constraints,
    burnin = 5000, mcmc = 100000, thin = 5, verbose = 10000,
    store.theta = TRUE, store.gamma = TRUE, tune = 0.5
  )
  saveRDS(postDP_out, fname)
} else {
  postDP_out <- readRDS(fname)
}

stats_summ_dp <- stats_summ_create(postDP_out)

## Visualize -------------------------------------------------------------------
pdf("output/main/theta_post_DP_median.pdf", width = 6, height = 6)
theta_post_viz(stats_summ_dp, label = TRUE)
dev.off()

pdf("output/main/theta_post_DP_median_clarity_DP.pdf", width = 6, height = 6)
theta_post_viz(stats_summ_dp, color = "clarity_specificity") +
  guides(color = guide_legend(title = "Clarity and Specificity"))
dev.off()

pdf("output/main/theta_post_DP_median_logic_DP.pdf", width = 6, height = 6)
theta_post_viz(stats_summ_dp, color = "logic_consistency") +
  guides(color = guide_legend(title = "Logic and Consistency"))
dev.off()

pdf("output/main/theta_post_DP_median_manner_DP.pdf", width = 6, height = 6)
theta_post_viz(stats_summ_dp, color = "tone_manner") +
  guides(color = guide_legend(title = "Tone and Manner"))
dev.off()

pdf("output/main/theta_post_DP_median_validity_DP.pdf", width = 6, height = 6)
theta_post_viz(stats_summ_dp, color = "validity_feasibility") +
  guides(color = guide_legend(title = "Validity and Feasibility"))
dev.off()

## Check clusters --------------------------------------------------------------
table(postDP_out[, "n.clusters"]) 
## how many distinct respondent parameter values?
##   2    3    4    5    6    7    8    9   10   11   12   13   14
## 962 3070 4663 4408 3350 1955  973  394  153   46   18    7    1
## respondents mostly fall under 4 to 5 distinct clusters

## two clusters in pilot study data
## summarize demographics of clusters
dp_cluster <- stats_summ_dp$gamma %>%
  mutate(cluster = (median < 1)) %>%
  group_split(cluster) %>%
  set_names(c("1", "2")) %>%
  map(~ .x$respondent %>%
    str_extract(., "\\d+") %>%
    as.numeric()) %>%
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
dp_cluster

#  cluster   sex   age   edu married   kid income  life ideal party
#  <chr>   <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
# 1 1        1.49  44.2  3.94    2.05  1.90   5.45  5.76  3.92     1
# 2 2        1.53  45.7  3.85    2.06  1.90   5.53  5.82  4.11     1
