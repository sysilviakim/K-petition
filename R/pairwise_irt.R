# Fits the Bayesian pairwise IRT for the survey data
# Setup ========================================================================
source(here::here("R", "utilities.R"))
df <- read_csv("data/pilot/raw_data.csv")

## Petition post characteristics -----------------------------------------------
post_types <- tibble(
  item = c(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
    13, 14, 15, 16
  ),
  type = c(
    "emotional-concrete-goodgrammar",
    "dry-abstract-badgrammar", "dry-abstract-badgrammar", 
    "dry-abstract-goodgrammar",
    "dry-abstract-goodgrammar", "dry-concrete-badgrammar",
    "dry-concrete-badgrammar", "dry-concrete-goodgrammar",
    "dry-concrete-goodgrammar", "emotional-abstract-badgrammar",
    "emotional-abstract-badgrammar", "emotional-abstract-goodgrammar",
    "emotional-abstract-goodgrammar", "emotional-concrete-badgrammar",
    "emotional-concrete-badgrammar", "emotional-concrete-goodgrammar"
  )
) %>%
  separate(type, into = c("emotion", "concrete", "grammar"), sep = "-") %>%
  mutate(
    grammar = gsub("grammar", "", grammar),
    item = paste0("item.", item),
    concrete_grammar = paste(concrete, grammar, sep = "-")
  )

## Theta constraints -----------------------------------------------------------
## item 1: emotional, concrete, good grammar
## item 2: dry, abstract, bad grammar
## item 8: dry, concrete, good grammar

## set 1st dimension to be about concrete + grammar
## set 2nd dimension to be about emotions vs dry
theta_constraints <- list(
  item.1 = list(1, 2),
  item.1 = list(2, 2),
  item.2 = list(1, -2),
  item.2 = list(2, -2),
  item.8 = list(1, "+"),
  item.8 = list(2, "-")
)

# Wrangle data =================================================================
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

## Attention/sanity check ------------------------------------------------------
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

## Indicators for subsets ------------------------------------------------------
subset_list <- list(
  ppp = df %>%
    filter(Q3 == 1) %>%
    .$No,
  opp = df %>%
    filter(Q3 %in% c(2, 3, 5)) %>%
    .$No,
  pop = df %>%
    mutate(
      score = 0,
      score = score + ifelse(Q5_3 >= 4, 1, 0),
      score = score + ifelse(Q5_4 >= 4, 1, 0),
      score = score + ifelse(Q5_5 >= 4, 1, 0)
    ) %>%
    filter(score >= 2 & Q5_3 >= 3 & Q5_4 >= 3 & Q5_5 >= 3) %>%
    .$No
) %>%
  map(~ paste0("gamma.", .x))

# IRT ==========================================================================
## MCMC ------------------------------------------------------------------------
set.seed(1234)
post_out <- MCMCpaircompare2d(
  pwc.data = pwc_main,
  theta.constraints = theta_constraints,
  burnin = 500, mcmc = 20000, thin = 5, verbose = 1000,
  store.theta = TRUE, store.gamma = TRUE, tune = 0.5
)

## Dirichlet process
set.seed(1234)
postDP_out <- MCMCpaircompare2dDP(
  pwc.data = pwc_main,
  theta.constraints = theta_constraints,
  burnin = 500, mcmc = 20000, thin = 5, verbose = 1000,
  store.theta = TRUE, store.gamma = TRUE, tune = 0.5
)

## Summary statistics ----------------------------------------------------------
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
  ) %>%
    left_join(., post_types),
  gamma = irt_summ("gamma", post_out) %>%
    as.data.frame() %>%
    rownames_to_column(var = "respondent") %>%
    mutate(emotion = "", concrete = "", grammar = "", concrete_grammar = "")
)

stats_summ_dp <- list(
  theta = left_join(
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
    left_join(., post_types),
  gamma = irt_summ("gamma", postDP_out) %>%
    as.data.frame() %>%
    rownames_to_column(var = "respondent") %>%
    mutate(emotion = "", concrete = "", grammar = "", concrete_grammar = "")
)

## Visualization ---------------------------------------------------------------
## visualize theta posteriors (item parameters)
pdf("output/theta_post_median.pdf", width = 6, height = 6)
theta_post_viz(stats_summ, label = TRUE)
dev.off()

## DP
pdf("output/theta_post_median_dp.pdf", width = 6, height = 6)
## Why so different?
theta_post_viz(stats_summ_dp, label = TRUE)
dev.off()

## Yu and Quinn 2021 Fig 3 apply
pdf("output/theta_post_median_emotion.pdf", width = 4, height = 4)
theta_post_viz(stats_summ, color = "emotion") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Emotion"))
dev.off()

pdf("output/theta_post_median_concrete.pdf", width = 4, height = 4)
theta_post_viz(stats_summ, color = "concrete") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Concreteness"))
dev.off()

pdf("output/theta_post_median_grammar.pdf", width = 4, height = 4)
theta_post_viz(stats_summ, color = "grammar") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Grammar"))
dev.off()

pdf("output/theta_post_median_dp_emotion.pdf", width = 4, height = 4)
theta_post_viz(stats_summ_dp, color = "emotion") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Emotion"))
dev.off()

pdf("output/theta_post_median_dp_concrete.pdf", width = 4, height = 4)
theta_post_viz(stats_summ_dp, color = "concrete") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Concreteness"))
dev.off()

pdf("output/theta_post_median_dp_grammar.pdf", width = 4, height = 4)
theta_post_viz(stats_summ_dp, color = "grammar") +
  ## combine guide for color and shape
  guides(color = guide_legend(title = "Grammar"))
dev.off()

## Combined viz
pdf("output/theta_post_median_combined.pdf", width = 4, height = 4.5)
p1 <-
  theta_post_viz(stats_summ, color = "concrete_grammar", shape = "emotion") +
  guides(
    color = guide_legend(title = "Concreteness/\nGrammar", byrow = T, nrow = 2),
    shape = guide_legend(title = "Emotion")
  )
p1
dev.off()

pdf("output/theta_post_median_dp_combined.pdf", width = 4, height = 4.5)
theta_post_viz(stats_summ_dp, color = "concrete_grammar", shape = "emotion") +
  guides(
    color = guide_legend(title = "Concreteness/\nGrammar", byrow = T, nrow = 2),
    shape = guide_legend(title = "Emotion")
  )
dev.off()

## Check clusters --------------------------------------------------------------
table(postDP_out[, 131]) ## how many distinct respondent parameter values?
##    2    3    4    5    6    7    8    9
## 1165 1507  892  323   82   24    6    1
## respondents mostly fall under 3 distinct clusters

## two clusters in pilot study data
## summarize demographics of clusters
dp_cluster <- stats_summ_dp$gamma %>%
  mutate(cluster = (median < 1)) %>%
  group_split(cluster) %>%
  set_names(c("1", "2")) %>%
  map(~ .x$respondent %>% str_extract(., "\\d+") %>% as.numeric()) %>%
  map(~ df %>% filter(No %in% .x)) %>%
  bind_rows(.id = "cluster") %>%
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
dp_cluster

#   cluster   sex   age   edu married   kid income  life ideal party
#   <chr>   <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
# 1 1        1.46  42.9  3.79    2.04  1.96   5.11  5.11  4.21     1
# 2 2        1.57  45.0  4.10    1.76  1.86   6.67  6     3.86     1

## cluster 1 more towards 2nd dimension of theta
## cluster 2 more towards 1st dimension of theta

## Subgroups -------------------------------------------------------------------
pdf("output/theta_post_median_combined_ppp.pdf", width = 4, height = 4.5)
p2 <- theta_post_viz(
  gamma_filter(stats_summ, "ppp"),
  color = "concrete_grammar", shape = "emotion"
) +
  guides(
    color = guide_legend(title = "Concreteness/\nGrammar", byrow = T, nrow = 2),
    shape = guide_legend(title = "Emotion")
  ) +
  xlim(-3, 3) +
  ylim(-3, 3)
p2
dev.off()

pdf("output/theta_post_median_combined_opp.pdf", width = 4, height = 4.5)
p3 <- theta_post_viz(
  gamma_filter(stats_summ, "opp"),
  color = "concrete_grammar", shape = "emotion"
) +
  guides(
    color = guide_legend(title = "Concreteness/\nGrammar", byrow = T, nrow = 2),
    shape = guide_legend(title = "Emotion")
  )
p3
dev.off()

pdf("output/theta_post_median_combined_pop.pdf", width = 4, height = 4.5)
p4 <- theta_post_viz(
  gamma_filter(stats_summ, "pop"),
  color = "concrete_grammar", shape = "emotion"
) +
  guides(
    color = guide_legend(title = "Concreteness/\nGrammar", byrow = T, nrow = 2),
    shape = guide_legend(title = "Emotion")
  )
p4
dev.off()
