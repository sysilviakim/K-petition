# Fits the Bayesian pairwise IRT for the survey data

# Setup ========================================================================
source(here::here("R", "utilities.R"))
fname <- "data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx"
df <- readxl::read_xlsx(stringi::stri_trans_nfc(fname), sheet = "Raw")
## pet_list <- readxl::read_xlsx("data/screenshots/공개제안_pairs.xlsx")

## pair question id: Q13_1 ~ Q13_8
## petition id for choice: Q13_1 -> (Q13_gCode1_1, Q13_gCode1_2)

# Petition post characteristics ================================================
post_types <-
  stringi::stri_trans_nfc("data/screenshots/main/공개제안_tidy.xlsx") %>%
  readxl::read_xlsx() %>%
  dplyr::select(id, category, comb_fin, text)

## comb_fin:
## 1. clarity, specificity
## 2. logic, consistency
## 3. tone, manner
## 4. validity, feasibility
post_types <- post_types %>%
  mutate(
    clarity_specificity = substr(comb_fin, 1, 1),
    logic_consistency = substr(comb_fin, 2, 2),
    tone_manner = substr(comb_fin, 3, 3),
    validity_feasibility = substr(comb_fin, 4, 4)
  ) %>%
  rename(item = id) %>%
  mutate(item = as.character(item))

## check attention
pdf("output/main/attention_time.pdf", width = 10, height = 6.5)
par(mfrow = c(2, 4))
hist(df$q13_q14_time_1, breaks = 50, xlab = "Time for Pair Comparison 1 (Seconds)", main = "")
hist(df$q13_q14_time_2, breaks = 50, xlab = "Time for Pair Comparison 2 (Seconds)", main = "")
hist(df$q13_q14_time_3, breaks = 50, xlab = "Time for Pair Comparison 3 (Seconds)", main = "")
hist(df$q13_q14_time_4, breaks = 50, xlab = "Time for Pair Comparison 4 (Seconds)", main = "")
hist(df$q13_q14_time_5, breaks = 50, xlab = "Time for Pair Comparison 5 (Seconds)", main = "")
hist(df$q13_q14_time_6, breaks = 50, xlab = "Time for Pair Comparison 6 (Seconds)", main = "")
hist(df$q13_q14_time_7, breaks = 50, xlab = "Time for Pair Comparison 7 (Seconds)", main = "")
hist(df$q13_q14_time_8, breaks = 50, xlab = "Time for Pair Comparison 8 (Seconds)", main = "")
dev.off()

median(df$q13_q14_time_1) ## 49
median(df$q13_q14_time_2) ## 29
median(df$q13_q14_time_3) ## 26
median(df$q13_q14_time_4) ## 25.5
median(df$q13_q14_time_5) ## 24
median(df$q13_q14_time_6) ## 23
median(df$q13_q14_time_7) ## 22
median(df$q13_q14_time_8) ## 22

## create df for mcmc
pwc_df <- map_dfr(1:8, function(i) {
  df %>%
    dplyr::select(
      NO,
      !!sym(paste0("Q13_gCode", i, "_1")),
      !!sym(paste0("Q13_gCode", i, "_2")),
      !!sym(paste0("Q13_", i))
    ) %>%
    dplyr::rename(
      Item1 = !!sym(paste0("Q13_gCode", i, "_1")),
      Item2 = !!sym(paste0("Q13_gCode", i, "_2")),
      Choice = !!sym(paste0("Q13_", i))
    )
})

## transform df to fit mcmc function
pwc_df <- pwc_df %>%
  rowwise() %>%
  dplyr::mutate(
    Item1 = paste0("item.", Item1),
    Item2 = paste0("item.", Item2),
    Choice = c(Item1, Item2)[Choice]
  ) %>%
  ungroup()
pwc_df <- as.data.frame(pwc_df)

# Theta constraints  ===========================================================
## item 1: 1011. clear, somewhat weak logic, well mannered, valid(?)
## item 15: 0000. poor manner, no logic, low validity, not very clear
## item 49: 0101. poor manner, but clear and valid, but sounds personal

## set 1st dimension to be about clarity and manner
## set 2nd dimension to be about logic and validity
theta_constraints <- list(
  item.1 = list(1, 2),
  item.1 = list(2, 2),
  item.15 = list(1, -2),
  item.15 = list(2, -2),
  item.49 = list(1, "-"),
  item.49 = list(2, "+")
)

# MCMC run  ====================================================================
## 2 dim pairwise IRT
set.seed(1234)
post_out <- MCMCpaircompare2d(
  pwc.data = pwc_df,
  theta.constraints = theta_constraints,
  burnin = 5000, mcmc = 100000, thin = 5, verbose = 3000,
  store.theta = TRUE, store.gamma = TRUE, tune = 0.5
)

saveRDS(post_out, "data/mcmc_out.rds")

theta_summ <- left_join(
  irt_summ("theta1", post_out) %>%
    as.data.frame() %>%
    rownames_to_column(var = "item") %>%
    rename_with(~ paste0("theta1_", .), -item) %>%
    mutate(item = gsub("theta1.", "", item)),
  irt_summ("theta2", post_out) %>%
    as.data.frame() %>%
    rownames_to_column(var = "item") %>%
    rename_with(~ paste0("theta2_", .), -item) %>%
    mutate(item = gsub("theta2.", "", item))
) %>%
  mutate(item = gsub("item.", "", item)) %>%
  left_join(post_types %>% select(-text), by = "item")

gamma_summ <- irt_summ("gamma", post_out) %>%
  as.data.frame() %>%
  rownames_to_column(var = "respondent") %>%
  mutate(respondent = gsub("gamma.", "", respondent)) %>%
  ## merge with demographic questions in the survey data
  left_join(
    df %>%
      select(
        NO, SQ1, SQ2_1, SQ2_2, SQ3, SQ4, SQ5, SQ6, SQ7, SQ8, SQ8_etc,
        SQ9, Q1, Q2, Q3, Q3_etc, Q4, Q4_etc, Q5, Q5_etc
      ) %>%
      mutate(respondent = as.character(NO)),
    by = "respondent"
  )
gamma_summ <- gamma_summ %>%
  rename(
    gender = SQ1,
    age = SQ2_1,
    age_range = SQ2_2,
    edu = SQ3,
    residence = SQ4,
    married = SQ5,
    kids = SQ6,
    income = SQ7,
    occupation = SQ8,
    occupation_etc = SQ8_etc,
    livelihood = SQ9,
    life = Q1,
    ideology = Q2,
    party = Q3,
    party_etc = Q3_etc,
    pres22 = Q4,
    pres22_etc = Q4_etc,
    pres25 = Q5,
    pres25_etc = Q5_etc
  )

stats_summ <- list(theta = theta_summ, gamma = gamma_summ)

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
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Logic and Consistency"))
dev.off()

pdf("output/main/theta_post_median_manner.pdf", width = 6, height = 6)
theta_post_viz(stats_summ, color = "tone_manner") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Tone and Manner"))
dev.off()

pdf("output/main/theta_post_median_validity.pdf", width = 6, height = 6)
theta_post_viz(stats_summ, color = "validity_feasibility") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Validity and Feasibility"))
dev.off()


## 2 dim pairwise IRT DP
postDP_out <- MCMCpaircompare2dDP(
  pwc.data = pwc_df,
  theta.constraints = theta_constraints,
  burnin = 5000, mcmc = 100000, thin = 5, verbose = 10000,
  store.theta = TRUE, store.gamma = TRUE, tune = 0.5
)

saveRDS(postDP_out, "data/mcmcDP_out.rds")

theta_summ <- left_join(
  irt_summ("theta1", postDP_out) %>%
    as.data.frame() %>%
    rownames_to_column(var = "item") %>%
    rename_with(~ paste0("theta1_", .), -item) %>%
    mutate(item = gsub("theta1.", "", item)),
  irt_summ("theta2", postDP_out) %>%
    as.data.frame() %>%
    rownames_to_column(var = "item") %>%
    rename_with(~ paste0("theta2_", .), -item) %>%
    mutate(item = gsub("theta2.", "", item))
) %>%
  mutate(item = gsub("item.", "", item)) %>%
  left_join(post_types %>% select(-text), by = "item")

gamma_summ <- irt_summ("gamma", postDP_out) %>%
  as.data.frame() %>%
  rownames_to_column(var = "respondent") %>%
  mutate(respondent = gsub("gamma.", "", respondent)) %>%
  ## merge with demographic questions in the survey data
  left_join(df %>% select(NO, SQ1, SQ2_1, SQ2_2, SQ3, SQ4, SQ5, SQ6, SQ7, SQ8, SQ8_etc, SQ9, Q1, Q2, Q3, Q3_etc, Q4, Q4_etc, Q5, Q5_etc) %>% mutate(respondent = as.character(NO)), by = "respondent")
gamma_summ <- gamma_summ %>%
  rename(
    gender = SQ1,
    age = SQ2_1,
    age_range = SQ2_2,
    edu = SQ3,
    residence = SQ4,
    married = SQ5,
    kids = SQ6,
    income = SQ7,
    occupation = SQ8,
    occupation_etc = SQ8_etc,
    livelihood = SQ9,
    life = Q1,
    ideology = Q2,
    party = Q3,
    party_etc = Q3_etc,
    pres22 = Q4,
    pres22_etc = Q4_etc,
    pres25 = Q5,
    pres25_etc = Q5_etc
  )

stats_summ_dp <- list(theta = theta_summ, gamma = gamma_summ)

pdf("output/main/theta_post_DP_median.pdf", width = 6, height = 6)
theta_post_viz(stats_summ_dp, label = TRUE)
dev.off()

pdf("output/main/theta_post_DP_median_clarity_DP.pdf", width = 6, height = 6)
theta_post_viz(stats_summ_dp, color = "clarity_specificity") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Clarity and Specificity"))
dev.off()

pdf("output/main/theta_post_DP_median_logic_DP.pdf", width = 6, height = 6)
theta_post_viz(stats_summ_dp, color = "logic_consistency") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Logic and Consistency"))
dev.off()

pdf("output/main/theta_post_DP_median_manner_DP.pdf", width = 6, height = 6)
theta_post_viz(stats_summ_dp, color = "tone_manner") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Tone and Manner"))
dev.off()

pdf("output/main/theta_post_DP_median_validity_DP.pdf", width = 6, height = 6)
theta_post_viz(stats_summ_dp, color = "validity_feasibility") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Validity and Feasibility"))
dev.off()



## Check clusters --------------------------------------------------------------
table(postDP_out[, "n.clusters"]) ## how many distinct respondent parameter values?
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
