# Re-estimates the pairwise IRT under alternative third anchors, for the PSJ
# R&R re-analysis with the complete three-author codes (see
# R/22_v2_expert_codes.R).
#
# The submitted model fixes petition 1 at (2, 2) and petition 15 at (-2, -2),
# and restricts petition 49, then coded 0101 (presentation-weak,
# substance-strong), to the upper-left quadrant. Under the new codes petition
# 49 is 1101, so it no longer fits the rule that selected it. Petition 59 is
# the only petition coded 0101 under the new codes.
#
# Anchor sets (petitions 1 and 15 are fixed in all of them):
#   a49   petition 49 in the upper left   submitted model; stored fits reused
#   a59   petition 59 in the upper left   same rule applied to the new codes
#   a31   petition 31 in the upper left   sensitivity: unanimous 0100
#   m10   petition 10 in the lower right  sensitivity: mirror constraint on a
#                                         unanimous 1010 petition
#   none  no third anchor                 sensitivity: reflection left free
#
# Models: for a49 and a59, the full-sample model, the Dirichlet Process
# variant, the attentive-only subsample (script 15), and the five respondent
# subgroups (plain model, as in R/21_subgroup_reruns.R). For the sensitivity
# sets, the full-sample model only, stored without the respondent parameters.
#
# The attentive-only fit is skipped when script 15's filter excludes nobody,
# which is the case in these data (the shortest response time is 9 seconds,
# against thresholds of 3 and 1): that fit would duplicate the full-sample one.
#
# Input:   survey workbook (via R/08_survey_wrangling.R)
#          data/mcmc_out.rds, data/mcmcDP_out.rds, output/subgroup_rerun/*.rds
#          (submitted fits, reused as anchor set a49)
# Output:  output/v2/fits/<set>/*.rds        posterior draws
#          output/v2/fits/<set>/theta_medians.csv
#          output/v2/fits/<set>/subgroup_thetas.csv
#          output/v2/irt_theta_medians.csv   all sets and models, long
#          output/v2/irt_anchor_agreement.csv
#
# MCMCpack draws from its own seeded generator, so every fit is reproducible:
# re-running the submitted specification returns data/mcmc_out.rds exactly.
# A full-sample fit takes about 1.5 minutes; fits run in parallel.
#
# Run from the project root under a UTF-8 locale:
#   LC_ALL=en_US.UTF-8 Rscript --vanilla R/23_v2_irt_fits.R
# Set V2_DP_SUBGROUPS=1 to also fit the Dirichlet Process subgroup
# models that R/13_survey_pairwise_subgroups.R expects.

# Setup ========================================================================
suppressPackageStartupMessages({
  source(here::here("R", "utilities.R"))
  source(here::here("R", "08_survey_wrangling.R"))
  library(parallel)
})

fits_dir <- here::here("output", "v2", "fits")
n_cores <- max(1, min(10, detectCores() - 2))
run_dp_subgroups <- Sys.getenv("V2_DP_SUBGROUPS", "0") == "1"

## Pairwise comparison data, as in 09_survey_descriptives.R --------------------
pwc_df <- map_dfr(1:8, function(i) {
  df %>%
    select(
      NO,
      Item1 = !!sym(paste0("Q13_gCode", i, "_1")),
      Item2 = !!sym(paste0("Q13_gCode", i, "_2")),
      Choice = !!sym(paste0("Q13_", i))
    )
}) %>%
  rowwise() %>%
  mutate(
    Item1 = paste0("item.", Item1),
    Item2 = paste0("item.", Item2),
    Choice = c(Item1, Item2)[Choice]
  ) %>%
  ungroup() %>%
  as.data.frame()

## Respondent subsets ----------------------------------------------------------
## attentive: as in 15_survey_robustness.R
time_mat <- as.matrix(df[, time_vars])
attentive_ids <- df$NO[
  !(apply(time_mat, 1, median) < 3 | apply(time_mat, 1, min) < 1)
]
attentive_is_full <- length(attentive_ids) == nrow(df)
cat("Attentive-only subsample:", length(attentive_ids), "of", nrow(df),
    "respondents.\n")
## subgroups: as in 13_survey_pairwise_subgroups.R / 21_subgroup_reruns.R
subgroup_ids <- list(
  ppp = df$NO[df$pid3 == "PPP"],
  opp = df$NO[df$pid3 == "Opposition"],
  pop = df$NO[df$populist == 1],
  inc_below = df$NO[df$median_income == "Below Median"],
  inc_above = df$NO[df$median_income == "Above Median"]
)
## file stems that script 13 uses for its Dirichlet Process subgroup fits
dp_subgroup_stem <- c(
  ppp = "mcmcDP_out_ppp", opp = "mcmcDP_out_opp", pop = "mcmcDP_out_pop",
  inc_below = "mcmcDP_out_median_income0",
  inc_above = "mcmcDP_out_median_income1"
)

## Anchor sets -----------------------------------------------------------------
fixed <- list(
  item.1 = list(1, 2), item.1 = list(2, 2),
  item.15 = list(1, -2), item.15 = list(2, -2)
)
quadrant <- function(item, s1, s2) {
  setNames(list(list(1, s1), list(2, s2)), rep(paste0("item.", item), 2))
}
anchor_sets <- list(
  a49 = c(fixed, quadrant(49, "-", "+")),
  a59 = c(fixed, quadrant(59, "-", "+")),
  a31 = c(fixed, quadrant(31, "-", "+")),
  m10 = c(fixed, quadrant(10, "+", "-")),
  none = fixed
)
main_sets <- c("a49", "a59")

# Fit specifications ===========================================================
## Submitted fits are reused for a49 rather than re-estimated.
submitted <- c(
  full = here::here("data", "mcmc_out.rds"),
  dp = here::here("data", "mcmcDP_out.rds"),
  setNames(
    here::here(
      "output", "subgroup_rerun",
      paste0("mcmc_", names(subgroup_ids), ".rds")
    ),
    paste0("sub_", names(subgroup_ids))
  )
)
fit_path <- function(set, model) {
  if (set == "a49" && model %in% names(submitted)) {
    return(unname(submitted[model]))
  }
  stem <- switch(model,
    full = "mcmc_out",
    dp = "mcmcDP_out",
    attentive = "mcmc_out_attentive",
    if (grepl("^dpsub_", model)) {
      dp_subgroup_stem[sub("^dpsub_", "", model)]
    } else {
      sub("^sub_", "mcmc_", model)
    }
  )
  file.path(fits_dir, set, paste0(stem, ".rds"))
}

models_main <- c(
  "full", "dp", if (!attentive_is_full) "attentive",
  paste0("sub_", names(subgroup_ids))
)
if (run_dp_subgroups) {
  models_main <- c(models_main, paste0("dpsub_", names(subgroup_ids)))
}
specs <- rbind(
  expand.grid(
    set = main_sets, model = models_main, stringsAsFactors = FALSE
  ),
  data.frame(set = setdiff(names(anchor_sets), main_sets), model = "full")
)
specs$path <- mapply(fit_path, specs$set, specs$model)

run_fit <- function(set, model, path) {
  ids <- if (model %in% c("full", "dp")) {
    df$NO
  } else if (model == "attentive") {
    attentive_ids
  } else {
    subgroup_ids[[sub("^(dp)?sub_", "", model)]]
  }
  is_dp <- model == "dp" || grepl("^dpsub_", model)
  fitter <- if (is_dp) MCMCpaircompare2dDP else MCMCpaircompare2d
  set.seed(1234)
  out <- fitter(
    pwc.data = pwc_df[pwc_df$NO %in% ids, ],
    theta.constraints = anchor_sets[[set]],
    burnin = MCMC_BURNIN,
    mcmc = MCMC_ITER,
    thin = MCMC_THIN,
    verbose = 0,
    store.theta = TRUE,
    ## script 21 drops gamma for the plain subgroup fits; kept consistent.
    ## Sensitivity sets need petition positions only.
    store.gamma = !grepl("^sub_", model) && set %in% main_sets,
    tune = MCMC_TUNE
  )
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(out, path)
  path
}

todo <- specs[!file.exists(specs$path), ]
cat(nrow(specs), "fits specified;", nrow(todo), "to estimate on",
    n_cores, "cores.\n")
if (nrow(todo) > 0) {
  t0 <- Sys.time()
  res <- mclapply(
    seq_len(nrow(todo)),
    function(i) {
      tryCatch(
        run_fit(todo$set[i], todo$model[i], todo$path[i]),
        error = function(e) paste("ERROR:", conditionMessage(e))
      )
    },
    mc.cores = n_cores, mc.preschedule = FALSE
  )
  failed <- grepl("^ERROR", unlist(res)) | !file.exists(todo$path)
  cat("Estimated in", round(as.numeric(difftime(Sys.time(), t0,
      units = "mins")), 1), "minutes.\n")
  if (any(failed)) {
    print(cbind(todo[failed, c("set", "model")], msg = unlist(res)[failed]))
    stop("Some fits failed.")
  }
}

# Summaries ====================================================================
theta_medians <- function(m) {
  tc <- grep("^theta[12]\\.item", colnames(m), value = TRUE)
  med <- apply(m[, tc, drop = FALSE], 2, median)
  parts <- do.call(rbind, strsplit(names(med), ".", fixed = TRUE))
  w <- reshape(
    data.frame(
      item = as.integer(parts[, 3]), dim = parts[, 1],
      median = as.numeric(med)
    ),
    idvar = "item", timevar = "dim", direction = "wide"
  )
  names(w) <- c("item", "theta1", "theta2")
  w[order(w$item), ]
}

summ <- lapply(seq_len(nrow(specs)), function(i) {
  m <- as.matrix(readRDS(specs$path[i]))
  th <- theta_medians(m)
  gc <- grep("^gamma", colnames(m), value = TRUE)
  g <- if (length(gc) > 0) apply(m[, gc, drop = FALSE], 2, median) else NA
  list(
    theta = cbind(set = specs$set[i], model = specs$model[i], th),
    gamma = data.frame(
      set = specs$set[i], model = specs$model[i],
      gamma_min = min(g), gamma_median = median(g), gamma_max = max(g),
      n_respondents = length(gc)
    )
  )
})
theta_long <- do.call(rbind, lapply(summ, `[[`, "theta"))
gamma_summ <- do.call(rbind, lapply(summ, `[[`, "gamma"))
write.csv(
  theta_long, here::here("output", "v2", "irt_theta_medians.csv"),
  row.names = FALSE
)

## Agreement of every fit with the submitted full-sample positions -------------
## "direct" compares like dimensions; "crossed" compares theta1 with theta2.
## A fit whose crossed agreement exceeds its direct agreement has its two
## dimensions exchanged relative to the submitted model.
ref <- theta_long[theta_long$set == "a49" & theta_long$model == "full", ]
sp <- function(x, y) cor(x, y, method = "spearman")
agree <- do.call(rbind, lapply(
  split(theta_long, paste(theta_long$set, theta_long$model)),
  function(d) {
    d <- d[match(ref$item, d$item), ]
    third <- c(a49 = 49, a59 = 59, a31 = 31, m10 = 10, none = NA)[d$set[1]]
    data.frame(
      set = d$set[1], model = d$model[1],
      rho_theta1 = sp(ref$theta1, d$theta1),
      rho_theta2 = sp(ref$theta2, d$theta2),
      rho_crossed_12 = sp(ref$theta1, d$theta2),
      rho_crossed_21 = sp(ref$theta2, d$theta1),
      exchanged = sp(ref$theta1, d$theta2) + sp(ref$theta2, d$theta1) >
        sp(ref$theta1, d$theta1) + sp(ref$theta2, d$theta2),
      third_anchor = third,
      third_theta1 = if (is.na(third)) NA else d$theta1[d$item == third],
      third_theta2 = if (is.na(third)) NA else d$theta2[d$item == third],
      p49_theta1 = d$theta1[d$item == 49], p49_theta2 = d$theta2[d$item == 49],
      p59_theta1 = d$theta1[d$item == 59], p59_theta2 = d$theta2[d$item == 59]
    )
  }
))
agree <- merge(agree, gamma_summ, by = c("set", "model"))
agree <- agree[order(match(agree$set, names(anchor_sets)), agree$model), ]
write.csv(
  agree, here::here("output", "v2", "irt_anchor_agreement.csv"),
  row.names = FALSE
)

## Per-set files that the downstream scripts read ------------------------------
for (s in main_sets) {
  dir.create(file.path(fits_dir, s), showWarnings = FALSE, recursive = TRUE)
  full <- theta_long[theta_long$set == s & theta_long$model == "full", ]
  write.csv(
    full[, c("item", "theta1", "theta2")],
    file.path(fits_dir, s, "theta_medians.csv"), row.names = FALSE
  )
  ## subgroup positions in the layout of output/subgroup_rerun/
  ## subgroup_thetas.csv, relabeled where a subgroup fit converged to the
  ## dimension-exchanged mode relative to the same set's full-sample fit
  sub <- do.call(rbind, lapply(names(subgroup_ids), function(nm) {
    th <- theta_long[
      theta_long$set == s & theta_long$model == paste0("sub_", nm),
      c("item", "theta1", "theta2")
    ]
    th <- th[match(full$item, th$item), ]
    swapped <- sp(full$theta1, th$theta2) + sp(full$theta2, th$theta1) >
      sp(full$theta1, th$theta1) + sp(full$theta2, th$theta2)
    data.frame(
      item = th$item,
      t1 = if (swapped) th$theta2 else th$theta1,
      t2 = if (swapped) th$theta1 else th$theta2,
      subgroup = nm, relabeled = swapped
    )
  }))
  write.csv(
    sub, file.path(fits_dir, s, "subgroup_thetas.csv"), row.names = FALSE
  )
}

cat("\nAgreement with the submitted full-sample positions (Spearman):\n")
print(
  format(
    agree[, c(
      "set", "model", "rho_theta1", "rho_theta2", "rho_crossed_12",
      "rho_crossed_21", "exchanged", "third_theta1", "third_theta2"
    )],
    digits = 2
  ),
  row.names = FALSE
)
