# Re-runs the survey analysis pipeline under the v2 petition codes and the
# re-anchored IRT (part 1), then compares the results with the submitted ones
# (part 2), for the PSJ R&R. Requires R/22_v2_expert_codes.R and
# R/23_v2_irt_fits.R to have been run.
#
# Scenarios:
#   S0_submitted   submitted codes, submitted IRT (anchors 1, 15, 49)
#                  -- a replication baseline on this machine
#   S1_v2          v2 codes, submitted IRT
#                  -- isolates the effect of the codes
#   S2_v2_a59      v2 codes, IRT re-estimated with petition 59 as the
#                  third anchor
#                  -- codes and anchor both updated; the results in v3 of
#                     the manuscript
#
# Orientation. The model is unchanged when its two dimensions are exchanged
# (theta1 <-> theta2, gamma -> pi/2 - gamma), and the third anchor decides
# which of the two equivalent labelings a fit returns. Each re-estimated fit
# is compared with the submitted fit of the same model (positions in
# output/v2/irt_theta_medians.csv) and, where its dimensions come out
# exchanged relative to that fit, relabeled before the scripts read it. All
# scenarios therefore report results in the orientation of the submitted
# files, which the scripts assume (script 20, for one, relabels the
# Dirichlet Process fit unconditionally). The relabeling is exact; the stored
# draws in output/v2/fits/ are left as estimated.
#
# The original scripts are run UNMODIFIED. For each scenario a sandbox project
# root is assembled in a temporary directory, holding copies of the scripts,
# the stimuli file with that scenario's codes, and links to that scenario's
# posterior draws under the file names the scripts expect. Because the
# scripts resolve every path through here::here() or relative to the working
# directory, they read the scenario's inputs and write their tables and
# figures inside the sandbox; nothing in tab/ or fig/ is overwritten.
#
# Scripts run per scenario: 10, 11, 12, 14, 15, 16, 17 (which source 08 and 09),
# then 18 (65-petition administrative outcomes) and 20 (revised figures).
# Script 13 and the subgroup-correlation block of 17 need Dirichlet Process
# subgroup fits; they run only if those fits exist (V2_DP_SUBGROUPS=1
# when running script 23).
#
# Two departures from the originals, both forced:
#   - R/11: Kmisc::pdf_default() is dropped (package not installed here; it
#     only restyles one figure).
#   - R/18: the corpus columns still use the misaligned LLM scores, so only
#     the 65-petition models are kept and tab/admin_outcomes.tex is not
#     collected.
#
# Output of part 1, per scenario, in output/v2/<scenario>/:
#   tab/       LaTeX tables, same file names as tab/
#   fig/       figures, same file names as fig/
#   fig_rev/   revised-manuscript figures (script 20)
#   objects/   <script>.rds, the small result objects left in the workspace
#   logs/      console output of every script
#
# Part 2 reads those outputs, together with
#          data/tidy/petition65_codes_v2.csv
#          output/v2/intercoder_reliability.csv
#          output/v2/irt_anchor_agreement.csv, irt_theta_medians.csv
# and writes
#          output/v2/comparison.csv      one row per quantity
#          output/v2/comparison.md       the same, as a readable report
#          output/v2/replication_check.csv
#                                        S0 tables against tab/*.tex
#
# Run from the project root under a UTF-8 locale:
#   LC_ALL=en_US.UTF-8 Rscript --vanilla R/24_v2_rerun_and_compare.R
# V2_SCENARIOS=S2_v2_a59 restricts part 1 to named scenarios
# (comma-separated); part 2 compares whichever scenarios have output.

library(parallel)

root <- normalizePath(".")
stopifnot(file.exists(file.path(root, "R", "utilities.R")))
out_root <- file.path(root, "output", "v2")
fits_dir <- file.path(out_root, "fits")
stim_name <- stringi::stri_trans_nfc("공개제안_tidy_url_appended.xlsx")
stim_v2_name <- stringi::stri_trans_nfc("공개제안_tidy_url_appended_v2.xlsx")

scenarios <- list(
  S0_submitted = list(codes = "old", anchors = "a49"),
  S1_v2 = list(codes = "new", anchors = "a49"),
  S2_v2_a59 = list(codes = "new", anchors = "a59")
)
keep <- Sys.getenv("V2_SCENARIOS", "")
if (nzchar(keep)) scenarios <- scenarios[strsplit(keep, ",")[[1]]]

## Posterior draws for an anchor set, under the names the scripts expect -------
fit_files <- function(set) {
  own <- file.path(fits_dir, set)
  full <- if (set == "a49") file.path(root, "data") else own
  ## script 23 fits no attentive-only model when the attentiveness filter
  ## excludes nobody; script 15 then reads the full-sample fit
  att <- file.path(own, "mcmc_out_attentive.rds")
  if (!file.exists(att)) att <- file.path(full, "mcmc_out.rds")
  f <- c(
    "output/mcmc_out_attentive.rds" = att,
    "output/theta_medians.csv" = file.path(own, "theta_medians.csv"),
    "output/subgroup_rerun/subgroup_thetas.csv" =
      file.path(own, "subgroup_thetas.csv"),
    "output/mcmc_out.rds" = file.path(full, "mcmc_out.rds"),
    "output/mcmcDP_out.rds" = file.path(full, "mcmcDP_out.rds"),
    "data/mcmc_out.rds" = file.path(full, "mcmc_out.rds"),
    "data/mcmcDP_out.rds" = file.path(full, "mcmcDP_out.rds")
  )
  dpsub <- paste0(
    "mcmcDP_out_",
    c("ppp", "opp", "pop", "median_income0", "median_income1"), ".rds"
  )
  if (all(file.exists(file.path(own, dpsub)))) {
    f <- c(f, setNames(file.path(own, dpsub), file.path("output", dpsub)))
  }
  f
}

## Is a fit exchanged relative to the submitted fit of the same model?
theta_long <- read.csv(file.path(out_root, "irt_theta_medians.csv"))
needs_exchange <- function(set, model) {
  if (set == "a49") return(FALSE)
  ref <- theta_long[theta_long$set == "a49" & theta_long$model == model, ]
  x <- theta_long[theta_long$set == set & theta_long$model == model, ]
  x <- x[match(ref$item, x$item), ]
  sp <- function(a, b) cor(a, b, method = "spearman")
  sp(ref$theta1, x$theta2) + sp(ref$theta2, x$theta1) >
    sp(ref$theta1, x$theta1) + sp(ref$theta2, x$theta2)
}
## the model behind each input file; position tables follow the full fit
model_of <- function(path) {
  stem <- sub("\\.rds$", "", basename(path))
  dpsub <- c(
    mcmcDP_out_ppp = "dpsub_ppp", mcmcDP_out_opp = "dpsub_opp",
    mcmcDP_out_pop = "dpsub_pop",
    mcmcDP_out_median_income0 = "dpsub_inc_below",
    mcmcDP_out_median_income1 = "dpsub_inc_above"
  )
  if (stem == "mcmcDP_out") "dp" else if (stem %in% names(dpsub)) {
    unname(dpsub[stem])
  } else {
    "full"
  }
}
## Exchange the two dimensions of a fit or of a table of positions
exchange_file <- function(from, to) {
  if (grepl("\\.rds$", from)) {
    m <- readRDS(from)
    ## swap the draws, not the names: some scripts take the dimensions by
    ## column position
    cn <- colnames(m)
    i1 <- grep("^theta1\\.", cn)
    i2 <- match(sub("^theta1", "theta2", cn[i1]), cn)
    stopifnot(!anyNA(i2))
    d1 <- m[, i1]
    m[, i1] <- m[, i2]
    m[, i2] <- d1
    g <- grepl("^gamma", cn)
    m[, g] <- pi / 2 - m[, g]
    saveRDS(m, to, compress = FALSE)
  } else {
    d <- read.csv(from)
    a <- intersect(c("theta1", "t1"), names(d))
    b <- intersect(c("theta2", "t2"), names(d))
    names(d)[match(c(a, b), names(d))] <- c(b, a)
    d <- d[, c(setdiff(names(d), c(a, b))[1], a, b,
               setdiff(names(d), c(a, b))[-1])]
    write.csv(d, to, row.names = FALSE)
  }
}

## Small result objects left in the workspace by a script ----------------------
capture_code <- '
capture_one <- function(x) {
  if (inherits(x, "coxph")) {
    list(coef = summary(x)$coefficients, n = x$n, nevent = x$nevent)
  } else if (inherits(x, c("lm", "glm"))) {
    s <- summary(x)
    list(coef = s$coefficients, r2 = s$r.squared, adj_r2 = s$adj.r.squared,
         n = stats::nobs(x))
  } else if (inherits(x, "aov")) {
    summary(x)[[1]]
  } else if (inherits(x, "coeftest")) {
    unclass(x)
  } else if (inherits(x, "htest")) {
    unclass(x[c("statistic", "parameter", "p.value", "estimate")])
  } else if (is.data.frame(x)) {
    if (object.size(x) < 2e6) as.data.frame(x) else NULL
  } else if (is.list(x) && length(x) > 0 && length(x) < 50 &&
             all(vapply(x, inherits, TRUE, c("lm", "glm", "coxph")))) {
    lapply(x, capture_one)
  } else if (is.atomic(x) && length(x) <= 500) {
    x
  } else {
    NULL
  }
}
capture_all <- function(file) {
  nms <- setdiff(ls(globalenv()), c("capture_one", "capture_all"))
  out <- lapply(setNames(nms, nms), function(nm) {
    tryCatch(capture_one(get(nm, globalenv())), error = function(e) NULL)
  })
  saveRDS(out[!vapply(out, is.null, TRUE)], file)
}
'

run_scenario <- function(name) {
  sc <- scenarios[[name]]
  sb <- file.path(tempdir(), paste0("v2_", name))
  unlink(sb, recursive = TRUE)
  for (d in c("R", "data/screenshots", "data/tidy", "output/subgroup_rerun",
              "fig", "tab", "fig_rev", "objects", "logs")) {
    dir.create(file.path(sb, d), recursive = TRUE)
  }
  file.create(file.path(sb, ".here"))

  ## scripts
  scripts <- list.files(
    file.path(root, "R"),
    pattern = "^(utilities|08_|09_|1[0-7]_|18_survey|20_).*\\.(R|py)$"
  )
  file.copy(file.path(root, "R", scripts), file.path(sb, "R"))
  s11 <- file.path(sb, "R", "11_survey_gamma_category.R")
  writeLines(
    gsub("Kmisc::pdf_default(p_category)", "p_category",
         readLines(s11), fixed = TRUE),
    s11
  )

  ## inputs
  file.symlink(file.path(root, "data", "main"), file.path(sb, "data", "main"))
  file.copy(
    file.path(
      root, "data", "screenshots",
      if (sc$codes == "new") stim_v2_name else stim_name
    ),
    file.path(sb, "data", "screenshots", stim_name)
  )
  file.symlink(
    file.path(root, "data", "tidy", "evaluated_data_2024_final.csv"),
    file.path(sb, "data", "tidy", "evaluated_data_2024_final.csv")
  )
  ff <- fit_files(sc$anchors)
  if (!all(file.exists(ff))) {
    stop("Missing fits for ", name, ": run R/23_v2_irt_fits.R first.\n",
         paste(ff[!file.exists(ff)], collapse = "\n"))
  }
  ## one relabeled copy per distinct exchanged input, linked under each
  ## name the scripts expect
  uniq <- unique(ff)
  swap <- vapply(
    uniq, function(f) needs_exchange(sc$anchors, model_of(f)), TRUE
  )
  exchanged <- basename(uniq[swap])
  if (any(swap)) {
    tmp <- file.path(sb, "relabeled", basename(uniq[swap]))
    dir.create(dirname(tmp[1]), recursive = TRUE)
    for (i in seq_along(tmp)) exchange_file(uniq[swap][i], tmp[i])
    uniq_new <- uniq
    uniq_new[swap] <- tmp
    ff <- setNames(uniq_new[match(ff, uniq)], names(ff))
  }
  file.symlink(ff, file.path(sb, names(ff)))
  has_dpsub <- "output/mcmcDP_out_ppp.rds" %in% names(ff)

  ## without Dirichlet Process subgroup fits, drop the block of 17 that
  ## reads them (it uses no random numbers, so later results are unaffected)
  if (!has_dpsub) {
    s17 <- file.path(sb, "R", "17_survey_political_economy.R")
    x <- readLines(s17)
    a <- grep("^# Category-Specific Subgroup Theta Correlations", x)
    b <- grep("^# Category-Stratified Feature Importance", x)
    stopifnot(length(a) == 1, length(b) == 1, a < b)
    writeLines(x[-(a:(b - 1))], s17)
  }

  writeLines(capture_code, file.path(sb, "capture.R"))
  rscript <- file.path(R.home("bin"), "Rscript")
  run <- function(tag, expr, env = character()) {
    log <- file.path(sb, "logs", paste0(tag, ".log"))
    code <- system2(
      rscript, c("--vanilla", "-e", shQuote(expr)),
      stdout = log, stderr = log,
      env = c("LC_ALL=en_US.UTF-8", "LANG=en_US.UTF-8", env)
    )
    code
  }
  src <- function(script) {
    tag <- sub("\\.R$", "", script)
    run(tag, sprintf(
      paste0(
        "setwd('%s'); source('capture.R'); ",
        "source(file.path('R', '%s'), encoding = 'UTF-8'); ",
        "capture_all(file.path('objects', '%s.rds'))"
      ),
      sb, script, tag
    ))
  }

  status <- c()
  survey <- c(
    "10_survey_pairwise_irt.R", "11_survey_gamma_category.R",
    "12_survey_accuracy_validation.R", "14_survey_prediction_pairwise.R",
    "15_survey_robustness.R", "16_survey_freeform.R",
    "17_survey_political_economy.R"
  )
  if (has_dpsub) survey <- c(survey, "13_survey_pairwise_subgroups.R")
  for (s in survey) status[s] <- src(s)

  ## administrative outcomes, 65 petitions only
  status["18_build.py"] <- system2(
    "sh",
    c("-c", shQuote(paste(
      "cd", shQuote(sb),
      "&& LC_ALL=en_US.UTF-8 python3 R/18_survey_admin_outcomes_build.py"
    ))),
    stdout = file.path(sb, "logs", "18_build.log"),
    stderr = file.path(sb, "logs", "18_build.log")
  )
  status["18_survey_admin_outcomes.R"] <- run(
    "18_survey_admin_outcomes",
    sprintf(
      paste0(
        "setwd('%s'); source('capture.R'); ",
        "source('R/18_survey_admin_outcomes.R'); rm(mc, mods, dc); ",
        "capture_all('objects/18_survey_admin_outcomes.rds')"
      ),
      sb
    )
  )
  unlink(file.path(sb, "tab", "admin_outcomes.tex"))

  ## revised-manuscript figures
  status["20_revise_theta_figures.R"] <- run(
    "20_revise_theta_figures",
    sprintf("setwd('%s'); source('R/20_revise_theta_figures.R')", sb),
    env = "PAPER_FIG=fig_rev"
  )

  ## collect
  dest <- file.path(out_root, name)
  unlink(dest, recursive = TRUE)
  dir.create(dest, recursive = TRUE)
  for (d in c("tab", "fig", "fig_rev", "objects", "logs")) {
    file.copy(file.path(sb, d), dest, recursive = TRUE)
  }
  file.copy(
    file.path(sb, "data", "tidy", "admin_outcomes_65.csv"),
    file.path(dest, "admin_outcomes_65.csv")
  )
  writeLines(
    c(
      paste("scenario:", name),
      paste("codes:", sc$codes),
      paste("anchor set:", sc$anchors),
      paste(
        "inputs relabeled to the submitted orientation:",
        if (length(exchanged)) paste(exchanged, collapse = ", ") else "none"
      ),
      paste("Dirichlet Process subgroup fits:", has_dpsub),
      paste("run:", format(Sys.time())),
      "", "exit status by script (0 = ok):",
      paste(format(names(status), width = 36), status)
    ),
    file.path(dest, "README.txt")
  )
  unlink(sb, recursive = TRUE)
  status
}

res <- mclapply(
  names(scenarios), function(nm) {
    tryCatch(run_scenario(nm), error = function(e) conditionMessage(e))
  },
  mc.cores = length(scenarios), mc.preschedule = FALSE
)
names(res) <- names(scenarios)
print(res)
if (any(vapply(res, function(x) is.character(x) || any(x != 0), TRUE))) {
  stop("Some scripts failed; see output/v2/<scenario>/logs/.")
}


# Part 2: compare the scenarios ================================================

out_root <- file.path("output", "v2")
scen <- c("S0_submitted", "S1_v2", "S2_v2_a59")
scen <- scen[dir.exists(file.path(out_root, scen))]

# Load =========================================================================
obj <- lapply(setNames(scen, scen), function(s) {
  f <- list.files(file.path(out_root, s, "objects"), full.names = TRUE)
  o <- lapply(f, readRDS)
  names(o) <- sub("^([0-9]+)_.*", "s\\1", basename(f))
  o
})
codes <- read.csv(file.path("data", "tidy", "petition65_codes_v2.csv"),
                  colClasses = c(SK = "character", BK = "character",
                                 KY = "character", comb_old = "character",
                                 comb_new = "character"))
codes_of <- function(s) {
  if (s == "S0_submitted") codes$comb_old else codes$comb_new
}

# Row builder ==================================================================
rows <- list()
add <- function(section, quantity, f, digits = 3) {
  vals <- vapply(scen, function(s) {
    v <- tryCatch(f(obj[[s]], s), error = function(e) NA)
    if (length(v) != 1) v <- NA
    if (is.numeric(v)) formatC(v, format = "f", digits = digits) else
      as.character(v)
  }, "")
  rows[[length(rows) + 1]] <<- data.frame(
    section = section, quantity = quantity, t(vals),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
star <- function(p) {
  if (is.na(p)) "" else if (p < .001) "***" else if (p < .01) "**" else
    if (p < .05) "*" else if (p < .1) "+" else ""
}
## "estimate (se) stars" from a coefficient matrix
est <- function(m, term, digits = 2) {
  if (!term %in% rownames(m)) return(NA_character_)
  p <- m[term, ncol(m)]
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)%s  p=%.3f"),
    m[term, 1], m[term, if ("se(coef)" %in% colnames(m)) "se(coef)" else 2],
    star(p), p
  )
}
est_df <- function(d, outcome, term, digits = 2) {
  r <- d[d$outcome == outcome & d$term == term, ]
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)%s  p=%.3f"),
    r$estimate, r$std.error, star(r$p.value), r$p.value
  )
}
dims <- c(
  clarity_specificity1 = "clarity", logic_consistency1 = "logic",
  tone_manner1 = "tone", validity_feasibility1 = "validity"
)

# 1. Codes =====================================================================
sec <- "1. Expert codes"
add(sec, "Petitions whose code differs from submitted", function(o, s) {
  sum(codes_of(s) != codes$comb_old)
}, 0)
for (d in 1:4) {
  local({
    d <- d
    add(sec, paste("Share coded 1:", c("clarity", "logic", "tone",
                                        "validity")[d]), function(o, s) {
      mean(substr(codes_of(s), d, d) == "1")
    }, 2)
  })
}
add(sec, "Presentation-strong, substance-weak (1010)", function(o, s) {
  sum(codes_of(s) == "1010")
}, 0)
add(sec, "Substance-strong, presentation-weak (0101)", function(o, s) {
  sum(codes_of(s) == "0101")
}, 0)

# 2. Choice accuracy (script 12) ===============================================
sec <- "2. Choice accuracy against the expert index"
for (g in 1:4) {
  local({
    g <- g
    add(sec, paste0("Accuracy, expert gap = ", g, " (n pairs)"),
        function(o, s) {
          a <- o$s12$acc_expert
          a <- a[a$quality_diff == g, ]
          sprintf("%.1f%% (%d)", 100 * a$accuracy, a$n)
        })
  })
}
add(sec, "Accuracy, all pairs with a gap", function(o, s) {
  a <- o$s12$acc_expert
  sprintf("%.1f%% (%d)", 100 * sum(a$accuracy * a$n) / sum(a$n), sum(a$n))
})
add(sec, "LPM slope of accuracy on gap (HC1)", function(o, s) {
  est(o$s12$ct_expert, "quality_diff", 3)
})
add(sec, "Spearman rho, expert index vs mean effectiveness", function(o, s) {
  h <- o$s12$cor_expert_eff
  sprintf("%.3f  p=%.4f", h$estimate, h$p.value)
})

# 3. Table 2 ===================================================================
sec <- "3. Table 2: mean effectiveness on expert codes (HC1)"
for (tm in names(dims)) {
  local({
    tm <- tm
    add(sec, dims[tm], function(o, s) est(o$s12$ct_eff_dim, tm))
  })
}
add(sec, "R2", function(o, s) o$s12$lm_eff_expert$r2)

# 4. Table 3 ===================================================================
for (k in 1:2) {
  local({
    k <- k
    sec <- sprintf("4. Table 3: theta%d on expert codes", k)
    oc <- sprintf("Theta %dD", k)
    for (tm in names(dims)) {
      local({
        tm <- tm
        add(sec, dims[tm], function(o, s) est_df(o$s12$dim_table, oc, tm))
      })
    }
    add(sec, "Adjusted R2", function(o, s) {
      o$s12[[sprintf("lm_dim_theta%d", k)]]$adj_r2
    })
  })
}
sec <- "4. Table 3 (cont.): theta on mean effectiveness"
add(sec, "theta1 slope on mean effectiveness", function(o, s) {
  est_df(o$s12$eff_table, "Theta 1D", "mean_eff", 3)
})
add(sec, "theta2 slope on mean effectiveness", function(o, s) {
  est_df(o$s12$eff_table, "Theta 2D", "mean_eff", 3)
})

# 5. Topic and gamma (script 11) ===============================================
sec <- "5. Topic structure and respondent weights"
add(sec, "ANOVA topic -> theta1", function(o, s) {
  a <- o$s11$anova_table
  sprintf("F=%.2f  p=%.3f", a$F[1], a$p[1])
})
add(sec, "ANOVA topic -> theta2", function(o, s) {
  a <- o$s11$anova_table
  sprintf("F=%.2f  p=%.3f", a$F[2], a$p[2])
})
add(sec, "theta1 ~ codes + topic: adjusted R2", function(o, s) {
  q <- o$s11$quality_fit
  q$Adj_R2[q$Outcome == "Theta 1D" & q$Model == "Quality + Category"]
})
add(sec, "theta2 ~ codes + topic: adjusted R2", function(o, s) {
  q <- o$s11$quality_fit
  q$Adj_R2[q$Outcome == "Theta 2D" & q$Model == "Quality + Category"]
})
add(sec, "gamma regression: adjusted R2", function(o, s) o$s11$m_gamma$adj_r2)
add(sec, "gamma regression: female", function(o, s) {
  est(o$s11$m_gamma$coef, "female", 3)
})
add(sec, "gamma regression: populist", function(o, s) {
  est(o$s11$m_gamma$coef, "populist", 3)
})
add(sec, "gamma regression: PPP", function(o, s) {
  est(o$s11$m_gamma$coef, "ppp", 3)
})
add(sec, "gamma, PPP vs opposition (t-test p)", function(o, s) {
  o$s11$gamma_tests$p[1]
})
add(sec, "gamma, populist vs not (t-test p)", function(o, s) {
  o$s11$gamma_tests$p[3]
})

# 6. Prediction (script 14) ====================================================
sec <- "6. Predicting pairwise choices"
for (m in c(
  "Logistic (petition)", "Logistic (full)", "LASSO (petition)",
  "LASSO (full)", "Random Forest (petition)", "Random Forest (full)",
  "IRT theta distance"
)) {
  local({
    m <- m
    add(sec, paste(m, "AUC / accuracy"), function(o, s) {
      r <- o$s14$comparison
      r <- r[r$Model == m, ]
      sprintf("%.3f / %.3f", r$AUC, r$Accuracy)
    })
  })
}
for (v in c("diff_manner", "diff_validity", "diff_clarity", "diff_logic",
            "diff_quality", "log_time")) {
  local({
    v <- v
    add(sec, paste("LASSO coefficient:", v), function(o, s) {
      l <- o$s14$lasso_df
      if (v %in% l$variable) l$coefficient[l$variable == v] else 0
    })
  })
}
add(sec, "LASSO: largest three |coefficients|", function(o, s) {
  l <- o$s14$lasso_df
  paste(head(l$variable[order(-abs(l$coefficient))], 3), collapse = ", ")
})
add(sec, "RF permutation importance: top five", function(o, s) {
  paste(head(o$s14$perm_df$variable, 5), collapse = ", ")
})
add(sec, "RF permutation importance: rank of ppp", function(o, s) {
  p <- o$s14$perm_df
  sprintf("%d of %d", which(p$variable == "ppp"), nrow(p))
})

# 7. Robustness of positions ===================================================
sec <- "7. Robustness of estimated positions"
add(sec, "Attentive-only vs full sample, r on theta1", function(o, s) {
  o$s15$cor_t1
})
add(sec, "Attentive-only vs full sample, r on theta2", function(o, s) {
  o$s15$cor_t2
})

## Agreement of the Dirichlet Process and subgroup fits with the same
## scenario's full-sample fit (Figures 8 and 9 of the revised manuscript),
## after relabeling any fit that converged to the dimension-exchanged mode
theta_long <- read.csv(file.path(out_root, "irt_theta_medians.csv"))
set_of <- function(s) if (s == "S2_v2_a59") "a59" else "a49"
rho_with_full <- function(model) {
  function(o, s) {
    d <- theta_long[theta_long$set == set_of(s), ]
    f <- d[d$model == "full", ]
    x <- d[d$model == model, ]
    x <- x[match(f$item, x$item), ]
    sp <- function(a, b) cor(a, b, method = "spearman")
    direct <- c(sp(f$theta1, x$theta1), sp(f$theta2, x$theta2))
    crossed <- c(sp(f$theta1, x$theta2), sp(f$theta2, x$theta1))
    r <- if (sum(crossed) > sum(direct)) crossed else direct
    ## report in the submitted orientation (tone dimension first)
    if (set_of(s) == "a59") r <- rev(r)
    sprintf("%.2f / %.2f", r[1], r[2])
  }
}
add(sec, "Dirichlet Process vs main model, rho (theta1 / theta2)",
    rho_with_full("dp"))
for (g in c(ppp = "PPP supporters", opp = "Opposition supporters",
            pop = "Populist attitudes", inc_below = "Below-median income",
            inc_above = "Above-median income")) {
  local({
    g <- g
    nm <- names(which(c(ppp = "PPP supporters", opp = "Opposition supporters",
                        pop = "Populist attitudes",
                        inc_below = "Below-median income",
                        inc_above = "Above-median income") == g))
    add(sec, paste0("Subgroup vs full sample, rho: ", g),
        rho_with_full(paste0("sub_", nm)))
  })
}
add(sec, "DP model: median respondent weight (radians)", function(o, s) {
  a <- read.csv(file.path(out_root, "irt_anchor_agreement.csv"))
  a$gamma_median[a$set == set_of(s) & a$model == "dp"]
}, 2)

# 8. SI analyses (script 17) ===================================================
sec <- "8. SI: accuracy by pair type and beneficiary status"
for (pt in c("Presentation", "Mixed", "Substance")) {
  local({
    pt <- pt
    add(sec, paste("Accuracy,", pt, "pairs (below / above median income)"),
        function(o, s) {
          a <- o$s17$acc_dim_class
          a <- a[a$pair_type == pt, ]
          sprintf(
            "%.1f%% / %.1f%%  (n=%d)",
            100 * a$accuracy[a$median_income == "Below Median"],
            100 * a$accuracy[a$median_income == "Above Median"], sum(a$n)
          )
        })
  })
}
add(sec, "Beneficiary logit: quality gap", function(o, s) {
  est(o$s17$m_beneficiary$coef, "quality_diff", 3)
})
add(sec, "Beneficiary logit: beneficiary", function(o, s) {
  est(o$s17$m_beneficiary$coef, "beneficiary", 3)
})

# 9. Administrative outcomes, 65 petitions (script 18) =========================
sec <- "9. Administrative outcomes, 65 petitions (Cox log-hazard)"
cox <- function(model, term) {
  function(o, s) est(o$s18$m65[[model]]$coef, term, 3)
}
add(sec, "theta1 (col. 1)", cox("thetas", "z1"))
add(sec, "theta2 (col. 1)", cox("thetas", "z2"))
add(sec, "theta1, topic strata (col. 2)", cox("thetas_strata", "z1"))
add(sec, "theta2, topic strata (col. 2)", cox("thetas_strata", "z2"))
add(sec, "coded presentation (col. 3)", cox("expert", "presentation"))
add(sec, "coded substance (col. 3)", cox("expert", "substance"))
add(sec, "coded presentation, strata (col. 4)",
    cox("expert_strata", "presentation"))
add(sec, "coded substance, strata (col. 4)", cox("expert_strata", "substance"))
add(sec, "four dimensions: tone", cox("four_dims", "manner"))
add(sec, "theta1, log length added", cox("thetas_len", "z1"))

comparison <- do.call(rbind, rows)
write.csv(comparison, file.path(out_root, "comparison.csv"), row.names = FALSE)

# Replication check ============================================================
## S0 tables against the tables in tab/. Row and column labels were restyled
## after the stored tables were generated and one table lost its intercept
## rows, so the check is on the numbers: every decimal number in the S0
## table, in order, against those in the stored table, allowing for the
## two tables' different printed precision.
decimals <- function(f) {
  x <- readLines(f, warn = FALSE)
  x <- x[!grepl("^%", x)]
  as.numeric(unlist(regmatches(x, gregexpr("-?[0-9]+\\.[0-9]+", x))))
}
is_subsequence <- function(a, b) {
  j <- 1
  for (v in a) {
    k <- match(v, b[j:length(b)])
    if (j > length(b) || is.na(k)) return(FALSE)
    j <- j + k
  }
  TRUE
}
rep_check <- NULL
if ("S0_submitted" %in% scen) {
  s0 <- list.files(file.path(out_root, "S0_submitted", "tab"), pattern = "tex$")
  rep_check <- do.call(rbind, lapply(s0, function(f) {
    ref <- file.path("tab", f)
    if (!file.exists(ref)) {
      return(data.frame(table = f, status = "no stored copy in tab/"))
    }
    a <- decimals(file.path(out_root, "S0_submitted", "tab", f))
    b <- decimals(ref)
    gap <- if (length(a) == length(b)) max(abs(a - b)) else NA
    data.frame(
      table = f,
      status = if (!is.na(gap) && gap < 0.0051) {
        "numbers identical"
      } else if (is_subsequence(sprintf("%.2f", a), sprintf("%.2f", b))) {
        "numbers identical (stored table has extra rows)"
      } else if (!is.na(gap)) {
        sprintf("largest difference %.3f", gap)
      } else {
        "different layout; not comparable"
      }
    )
  }))
  write.csv(rep_check, file.path(out_root, "replication_check.csv"),
            row.names = FALSE)
}

# Markdown report ==============================================================
md_table <- function(d) {
  d[] <- lapply(d, function(x) gsub("|", "\\|", as.character(x), fixed = TRUE))
  c(
    paste("|", paste(names(d), collapse = " | "), "|"),
    paste("|", paste(rep("---", ncol(d)), collapse = " | "), "|"),
    apply(d, 1, function(r) paste("|", paste(r, collapse = " | "), "|"))
  )
}
rel <- read.csv(file.path(out_root, "intercoder_reliability.csv"))
rel[-1] <- lapply(rel[-1], function(x) formatC(x, format = "f", digits = 2))
agree <- read.csv(file.path(out_root, "irt_anchor_agreement.csv"))
num <- vapply(agree, is.numeric, TRUE)
agree[num] <- lapply(agree[num], function(x) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = 2))
})

md <- c(
  "# New petition codes and re-anchored IRT: comparison with submitted results",
  "",
  paste("Generated", format(Sys.time(), "%Y-%m-%d %H:%M"),
        "by R/24_v2_rerun_and_compare.R."),
  "",
  "- **S0_submitted**: submitted codes, submitted IRT (anchors 1, 15, 49).",
  "- **S1_v2**: new three-author codes, submitted IRT.",
  paste("- **S2_v2_a59**: new codes, IRT re-estimated with petition 59",
        "as the third anchor."),
  "",
  "Entries are estimate (standard error), with + p<.1, * p<.05, ** p<.01,",
  "*** p<.001.",
  "",
  "## Petitions whose code changed",
  "",
  md_table(codes[codes$changed,
                 c("id", "SK", "BK", "KY", "comb_old", "comb_new")]),
  "",
  "## Inter-coder reliability (three authors, 65 petitions)",
  "",
  md_table(rel),
  "",
  "## IRT positions under alternative anchors",
  "",
  "Spearman correlation of each fit's petition positions with the submitted",
  "full-sample positions. `exchanged` is TRUE when the fit's two dimensions",
  "match the submitted ones better crossed than direct.",
  "",
  md_table(agree[, c(
    "set", "model", "rho_theta1", "rho_theta2", "rho_crossed_12",
    "rho_crossed_21", "exchanged", "third_anchor", "third_theta1",
    "third_theta2", "gamma_median"
  )]),
  ""
)
for (sname in unique(comparison$section)) {
  md <- c(
    md, paste("##", sname), "",
    md_table(comparison[comparison$section == sname, -1]), ""
  )
}
if (!is.null(rep_check)) {
  md <- c(
    md, "## Replication check: S0 tables against tab/", "",
    "Random-forest and cross-validation tables depend on the R and package",
    "versions, so they are compared across scenarios run on one machine.",
    "", md_table(rep_check), ""
  )
}
writeLines(md, file.path(out_root, "comparison.md"))
cat("wrote", file.path(out_root, "comparison.md"), "\n")
