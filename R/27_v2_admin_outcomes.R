# Administrative-outcomes analysis (PSJ R&R, comments R1.2 / R1.6).
# Supersedes R/18_survey_admin_outcomes.R and
# R/18b_admin_outcomes_response_tables.R, whose corpus columns used LLM
# scores attached to the wrong petitions.
#
# One outcome: the hazard of receiving a government answer (Cox proportional
# hazards; proposals not yet answered at data collection are right-censored).
# Escalation beyond an answer is not used, because the platform's status
# updates for it reflect an administrative issue rather than the timing of
# decisions.
#
#   1. Validation: claude-fable-5-1's 1-5 scores against the three-author
#      codes on the 65 survey petitions.
#   2. R1.6, citizens' signals: the 65 survey petitions, answer hazard on the
#      estimated positions (theta1, theta2; IRT anchored on petition 59, in
#      the submitted orientation, as produced by R/24_v2_rerun_and_compare.R),
#      stratified by topic.
#   3. R1.2, the four dimensions: the 65 survey petitions on the authors'
#      four 0/1 codes, stratified by topic, and the 2024 corpus (the year from
#      which the survey petitions were drawn) on claude-fable-5-1's four
#      scores, standardized on the corpus, with log length, filing date, and
#      policy-area strata.
#
# Input:   data/tidy/admin_outcomes_corpus2024_rescored.csv
#          data/tidy/llm_scores_65.csv
#          output/v2/S2_v2_a59/admin_outcomes_65.csv
# Output:  output/v2/admin/llm_validation.csv
#          output/v2/admin/admin_models.csv          every coefficient, long
#          output/v2/admin/tab/admin_outcomes_v3.tex  the reported table
#
# Base R + survival. Run from the project root:
#   Rscript --vanilla R/27_v2_admin_outcomes.R

library(survival)

out_dir <- file.path("output", "v2", "admin")
dir.create(file.path(out_dir, "tab"), showWarnings = FALSE, recursive = TRUE)

scorer <- "fable"
dims <- c("clarity", "logic", "manner", "validity")

# 1. Validation against the author codes =======================================
v65 <- read.csv(file.path("data", "tidy", "llm_scores_65.csv"))

cohen_kappa <- function(a, b) {
  po <- mean(a == b)
  pe <- mean(a) * mean(b) + (1 - mean(a)) * (1 - mean(b))
  if (pe == 1) NA else (po - pe) / (1 - pe)
}
## probability that a petition coded 1 outscores a petition coded 0
auc <- function(score, code) {
  r <- rank(score)
  n1 <- sum(code == 1)
  n0 <- sum(code == 0)
  (sum(r[code == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

validation <- do.call(rbind, lapply(dims, function(d) {
  sc <- v65[[paste0(scorer, "_", d)]]
  au <- v65[[paste0("author_", d)]]
  ok <- !is.na(sc)
  data.frame(
    dimension = d, n = sum(ok),
    mean_score = mean(sc[ok]),
    share_4plus = mean(sc[ok] >= 4),
    auc = auc(sc[ok], au[ok]),
    spearman = cor(sc[ok], au[ok], method = "spearman"),
    kappa_at_4 = cohen_kappa(as.integer(sc[ok] >= 4), au[ok])
  )
}))
write.csv(validation, file.path(out_dir, "llm_validation.csv"),
          row.names = FALSE)
cat("== claude-fable-5-1 scores against author codes, 65 petitions ==\n")
print(format(validation, digits = 2), row.names = FALSE)

# 2-3. The 65 survey petitions =================================================
d65 <- read.csv(
  file.path("output", "v2", "S2_v2_a59", "admin_outcomes_65.csv")
)
d65$z1 <- scale(d65$theta1)[, 1]
d65$z2 <- scale(d65$theta2)[, 1]

m65 <- list(
  thetas = coxph(
    Surv(time, answered) ~ z1 + z2 + strata(category), data = d65
  ),
  codes = coxph(
    Surv(time, answered) ~ clarity + logic + manner + validity +
      strata(category),
    data = d65
  )
)

# 3. The 2024 corpus ===========================================================
dc <- read.csv(
  file.path("data", "tidy", "admin_outcomes_corpus2024_rescored.csv")
)
dc$fdate <- as.numeric(as.Date(dc$date_petitioned))
for (k in dims) dc[[k]] <- dc[[paste0(scorer, "_", k)]]
dc <- dc[complete.cases(dc[, dims]), ]
for (k in dims) dc[[k]] <- scale(dc[[k]])[, 1]
cat("\n== 2024 corpus, scored by claude-fable-5-1 ==\nn =", nrow(dc),
    " answered =", sum(dc$answered), "\n")

mcorp <- list(
  dims = coxph(
    Surv(time, answered) ~ clarity + logic + manner + validity +
      log(nchar) + fdate + strata(area),
    data = dc
  )
)

## Every coefficient, long -----------------------------------------------------
tidy_model <- function(m, sample, model) {
  co <- summary(m)$coefficients
  data.frame(
    sample = sample, model = model, term = rownames(co),
    estimate = co[, 1], se = co[, 3], p = co[, 5],
    n = m$n, events = m$nevent, row.names = NULL
  )
}
admin_models <- rbind(
  do.call(rbind, lapply(names(m65), function(nm) {
    tidy_model(m65[[nm]], "65 petitions", nm)
  })),
  do.call(rbind, lapply(names(mcorp), function(nm) {
    tidy_model(mcorp[[nm]], "2024 corpus", nm)
  }))
)
write.csv(admin_models, file.path(out_dir, "admin_models.csv"),
          row.names = FALSE)
cat("\n== Answer hazard (Cox) ==\n")
print(format(admin_models, digits = 2), row.names = FALSE)

# Table ========================================================================
cell <- function(m, term) {
  co <- summary(m)$coefficients
  if (!term %in% rownames(co)) return("")
  p <- co[term, 5]
  stars <- if (p < .001) "$^{***}$" else if (p < .01) "$^{**}$" else
    if (p < .05) "$^{*}$" else if (p < .1) "$^{\\dagger}$" else ""
  sprintf("%.3f%s (%.3f)", co[term, 1], stars, co[term, 3])
}
mods <- c(m65, mcorp)
terms <- list(
  c("Perceived presentation ($\\theta_1$, std.)", "z1"),
  c("Perceived substance ($\\theta_2$, std.)", "z2"),
  c("Clarity/specificity", "clarity"),
  c("Logic/consistency", "logic"),
  c("Tone/manner", "manner"),
  c("Validity/feasibility", "validity"),
  c("Log petition length", "log(nchar)")
)
rows <- vapply(terms, function(tt) {
  paste0("  ", tt[1], " & ",
         paste(vapply(mods, cell, "", term = tt[2]), collapse = " & "),
         " \\\\")
}, "")
n_fmt <- function(n) formatC(as.integer(n), format = "d", big.mark = "{,}")
writeLines(
  c(
    "% generated by R/27_v2_admin_outcomes.R; corpus scores: claude-fable-5-1",
    "\\begin{tabular}{lccc}",
    "  \\toprule",
    paste0("  & \\multicolumn{2}{c}{Survey petitions}",
           " & 2024 corpus \\\\"),
    "  \\cmidrule(lr){2-3} \\cmidrule(lr){4-4}",
    "  & (1) & (2) & (3) \\\\",
    paste0("  Quality measure & Estimated positions",
           " & Expert codes (0/1) & LLM scores (std.) \\\\"),
    "  \\midrule",
    rows,
    "  \\midrule",
    "  Filing date & & & \\checkmark \\\\",
    "  Topic strata & \\checkmark & \\checkmark & \\\\",
    "  Policy-area strata & & & \\checkmark \\\\",
    paste0("  Proposals & ",
           paste(n_fmt(vapply(mods, function(m) m$n, 1)), collapse = " & "),
           " \\\\"),
    paste0("  Answered (events) & ",
           paste(n_fmt(vapply(mods, function(m) m$nevent, 1)),
                 collapse = " & "), " \\\\"),
    "  \\bottomrule",
    "\\end{tabular}"
  ),
  file.path(out_dir, "tab", "admin_outcomes_v3.tex")
)
cat("\nwrote", file.path(out_dir, "tab", "admin_outcomes_v3.tex"), "\n")
