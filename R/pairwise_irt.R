# Fits the Bayesian pairwise IRT for the survey data
# Setup ========================================================================
source(here::here("R", "utilities.R"))
df <- read_csv("data/pilot/raw_data.csv")

# Wrangle ======================================================================
pwc_df <- seq(10) %>%
  ## Wide to long data
  map_dfr(
    ~ df %>%
      select(
        No,
        !!sym(paste0("Info", .x, "_A")),
        !!sym(paste0("Info", .x, "_B")),
        !!sym(paste0("Q11_", .x))
      ) %>%
      rename(
        Item1 = !!sym(paste0("Info", .x, "_A")),
        Item2 = !!sym(paste0("Info", .x, "_B")),
        Choice = !!sym(paste0("Q11_", .x))
      )
  ) %>%
  ## Paste text string "item" to and make the Choice column reflect the item
  mutate(
    across(Item1:Item2, ~ paste0("item.", .)),
    ## Choice chooses either Item1 or Item2 according to whether value is 1 or 2
    Choice_no = Choice,
    Choice = case_when(
      Choice == 1 ~ Item1,
      Choice == 2 ~ Item2
    )
  ) %>%
  ## Paste the rest of the data
  left_join(
    df %>%
      select(-matches("Info[0-9]_A|Info[0-9]_B|Q11_[0-9]")),
    by = "No"
  )

# Attention/sanity check =======================================================
## Respondent 41 chose 1 for all 10 pairs
## No respondents chose 2 for all pairs
inattention <- pwc_df %>%
  group_by(No) %>%
  summarise(k = mean(Choice_no)) %>%
  filter(k == 1 | k == 2)

## Mark and create version where we remove inattentive respondents
pwc_df <- pwc_df %>%
  mutate(
    inattention = case_when(
      No %in% inattention$No ~ 1,
      TRUE ~ 0
    )
  )

pwc_list <- list(
  all = pwc_df,
  attentive = pwc_df %>%
    filter(inattention == 0)
) %>%
  map(
    ~ .x %>%
      select(No, Item1, Item2, Choice) %>%
      as.data.frame()
  )

## main data
pwc_main <- pwc_list$attentive
rm(pwc_df)

# IRT ==========================================================================
## item 1: emotional, concrete, good grammar
## item 2: dry, abstract, bad grammar
## item 8: dry, concrete, good grammar

## theta constraints
## set 1st dimension to be about concrete + grammar
## set 2nd dimension to be about emotions vs dry
set.seed(1234)
post_out <- MCMCpaircompare2d(
  pwc.data = pwc_main,
  theta.constraints = list(
    item.1 = list(1, 2),
    item.1 = list(2, 2),
    item.2 = list(1, -2),
    item.2 = list(2, -2),
    item.8 = list(1, "+"),
    item.8 = list(2, "-")
  ),
  burnin = 500, mcmc = 20000, thin = 5, verbose = 1000,
  store.theta = TRUE, store.gamma = TRUE, tune = 0.5
)

stats_summ <- list(
  theta = left_join(
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
  ),
  gamma = irt_summ("gamma", post_out) %>%
    as.data.frame() %>%
    rename_all(~ paste0("gamma_", .))
)

## visualize theta posteriors (item parameters)
labs <- gsub("theta1.", "", names(theta1_stats$median))
pdf("output/theta_post_median.pdf", width = 10, height = 10)
# plot(
#   theta1_stats$median, theta2_stats$median,
#   type = "n",
#   xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5),
#   xlab = "Theta 1D", ylab = "Theta 2D"
# )
# text(x = theta1.post.med, y = theta2.post.med, label = labs)
## ggplot version
ggplot(stats_summ$theta, aes(x = theta1_median, y = theta2_median)) +
  geom_point() +
  geom_text(aes(label = item), hjust = 0, vjust = 0) +
  xlim(-2.5, 2.5) +
  ylim(-2.5, 2.5) +
  xlab("Theta 1D") +
  ylab("Theta 2D")
dev.off()

## visualize theta with gamma (item parameters overlayed with respondent vectors)
plot(theta1.post.med, theta2.post.med,
  type = "n",
  xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5),
  xlab = "Theta 1D", ylab = "Theta 2D"
)
text(x = theta1.post.med, y = theta2.post.med, label = labs)

for (i in 1:length(gamma.post.med)) {
  arrows(
    x0 = 0, y0 = 0,
    x1 = cos(gamma.post.med[i]),
    y1 = sin(gamma.post.med[i]),
    col = rgb(1, 0, 0, 0.2), len = 0.05, lwd = 0.5
  )
}

## visualize with gamma posteriors (individual parameters)




## Dirichlet process
postDP_out <- MCMCpaircompare2dDP(
  pwc.data = pwc_main,
  theta.constraints = list(
    item.1 = list(1, 2),
    item.1 = list(2, 2),
    item.2 = list(1, -2),
    item.2 = list(2, -2),
    item.8 = list(1, "+"),
    item.8 = list(2, "-")
  ),
  burnin = 500, mcmc = 20000, thin = 5, verbose = 1000,
  store.theta = TRUE, store.gamma = TRUE, tune = 0.5
)

theta1.draws <- postDP_out[, grep("theta1", colnames(postDP_out))]
theta2.draws <- postDP_out[, grep("theta2", colnames(postDP_out))]
gamma.draws <- postDP_out[, grep("gamma", colnames(postDP_out))]
theta1.postDP.med <- apply(theta1.draws, 2, median)
theta2.postDP.med <- apply(theta2.draws, 2, median)
gamma.postDP.med <- apply(gamma.draws, 2, median)
theta1.postDP.025 <- apply(theta1.draws, 2, quantile, prob = 0.025)
theta1.postDP.975 <- apply(theta1.draws, 2, quantile, prob = 0.975)
theta2.postDP.025 <- apply(theta2.draws, 2, quantile, prob = 0.025)
theta2.postDP.975 <- apply(theta2.draws, 2, quantile, prob = 0.975)
gamma.postDP.025 <- apply(gamma.draws, 2, quantile, prob = 0.025)
gamma.postDP.975 <- apply(gamma.draws, 2, quantile, prob = 0.975)

pdf("output/theta_gamma_postDP_median.pdf", width = 10, height = 10)
plot(theta1.postDP.med, theta2.postDP.med,
  type = "n",
  xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5),
  xlab = "Theta 1D", ylab = "Theta 2D"
)
text(x = theta1.postDP.med, y = theta2.postDP.med, label = labs)

for (i in 1:length(gamma.postDP.med)) {
  arrows(
    x0 = 0, y0 = 0,
    x1 = cos(gamma.postDP.med[i]),
    y1 = sin(gamma.postDP.med[i]),
    col = rgb(1, 0, 0, 0.2), len = 0.05, lwd = 0.5
  )
}
dev.off()

## check clusters
table(postDP_out[, 131]) ## how many distinct respondent parameter values?
##    2    3    4    5    6    7    8    9
## 1165 1507  892  323   82   24    6    1
## respondents mostly fall under 3 distinct clusters

## two clusters in pilot study data
## summarize demographics of clusters
c1 <- str_extract(names(gamma.postDP.med[gamma.postDP.med < 1]), "\\d+")
c1 <- as.numeric(c1)
c2 <- str_extract(names(gamma.postDP.med[gamma.postDP.med > 1]), "\\d+")
c2 <- as.numeric(c2)

df <- df %>%
  mutate(cluster = 1)
df$cluster[df$No %in% c2] <- 2

df %>%
  group_by(cluster) %>%
  summarise(
    sex = mean(SQ1),
    age = mean(SQ2),
    edu = mean(SQ3),
    married = mean(SQ5),
    kid = mean(SQ6),
    income = mean(SQ7),
    life = mean(Q1),
    ideal = mean(Q2),
    party = get_mode(Q3)
  )

##  cluster   sex   age   edu married   kid income  life ideal party
##    <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
## 1       1  1.47  43.8   3.8    1.97  1.97   5.17  5.13   4.2     1
## 2       2  1.55  44.2   4.1    1.8   1.85   6.7   6.15   3.8     1
## clusters are very similar in demographics
## both clusters identify with liberal party
## cluster 2 has higher income level, life satisfaction

## cluster 1 more towards 2nd dimension of theta
## cluster 2 more towards 1st dimension of theta
