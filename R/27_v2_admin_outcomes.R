# Administrative-outcomes analysis on the rescored 2024 corpus (PSJ R&R,
# comments R1.2 / R1.6). Supersedes R/18_survey_admin_outcomes.R and
# R/18b_admin_outcomes_response_tables.R, whose corpus columns used LLM
# scores attached to the wrong petitions.
#
# Three scorers are run side by side (see R/26_v2_corpus_build.py):
# the realigned gpt-4o-mini scores, gpt-6-astra, and claude-fable-5-1.
#
#   1. Validation: each scorer's 1-5 scores against the three-author codes on
#      the 65 survey petitions.
#   2. The 65 survey petitions: answer hazard on the estimated positions and
#      on the author codes (new codes; IRT anchored on petition 59, in the
#      submitted orientation), as produced by R/24_v2_rerun_and_compare.R.
#   3. The corpus: answer hazard and escalation beyond an answer on each
#      scorer's presentation (clarity + tone) and substance (logic +
#      validity) scores; then the same LLM-score models on the six-topic
#      subset of the corpus and on the 65 survey petitions.
#   4. Each scorer as a second benchmark: estimated positions on its scores.
#
# Input:   data/tidy/admin_outcomes_corpus2024_rescored.csv
#          data/tidy/llm_scores_65.csv
#          output/v2/S2_v2_a59/admin_outcomes_65.csv
# Output:  output/v2/admin/llm_validation.csv
#          output/v2/admin/admin_models.csv      every coefficient, long
#          output/v2/admin/subset_models.csv     LLM-score models on the
#              full corpus, the six-topic subset, and the 65 petitions
#          output/v2/admin/theta_on_llm.csv
#          output/v2/admin/tab/admin_outcomes_<scorer>.tex    (answer hazard)
#          output/v2/admin/tab/admin_escalation_<scorer>.tex  (escalation)
#          output/v2/admin/tab/admin_outcomes_response_A.tex
#          output/v2/admin/tab/admin_outcomes_response_B_<scorer>.tex
#
# Base R + survival. Run from the project root:
#   Rscript --vanilla R/27_v2_admin_outcomes.R

library(survival)

out_dir <- file.path("output", "v2", "admin")
dir.create(file.path(out_dir, "tab"), showWarnings = FALSE, recursive = TRUE)

scorers <- c(
  gpt4omini_realigned = "gpt-4o-mini (realigned)",
  astra = "gpt-6-astra",
  fable = "claude-fable-5-1"
)
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

validation <- do.call(rbind, lapply(names(scorers), function(s) {
  do.call(rbind, lapply(dims, function(d) {
    sc <- v65[[paste0(s, "_", d)]]
    au <- v65[[paste0("author_", d)]]
    ok <- !is.na(sc)
    data.frame(
      scorer = s, dimension = d, n = sum(ok),
      mean_score = mean(sc[ok]),
      share_4plus = mean(sc[ok] >= 4),
      auc = auc(sc[ok], au[ok]),
      spearman = cor(sc[ok], au[ok], method = "spearman"),
      kappa_at_4 = cohen_kappa(as.integer(sc[ok] >= 4), au[ok])
    )
  }))
}))
write.csv(validation, file.path(out_dir, "llm_validation.csv"),
          row.names = FALSE)
cat("== LLM scores against author codes, 65 petitions ==\n")
print(format(validation, digits = 2), row.names = FALSE)

# 2. The 65 survey petitions ===================================================
d65 <- read.csv(
  file.path("output", "v2", "S2_v2_a59", "admin_outcomes_65.csv")
)
d65$z1 <- scale(d65$theta1)[, 1]
d65$z2 <- scale(d65$theta2)[, 1]
d65$fdate <- as.numeric(as.Date(d65$date_petitioned))

m65 <- list(
  thetas = coxph(Surv(time, answered) ~ z1 + z2, data = d65),
  thetas_strata = coxph(
    Surv(time, answered) ~ z1 + z2 + strata(category), data = d65
  ),
  thetas_fdate = coxph(Surv(time, answered) ~ z1 + z2 + fdate, data = d65),
  thetas_len = coxph(Surv(time, answered) ~ z1 + z2 + log(nchar), data = d65),
  expert = coxph(Surv(time, answered) ~ presentation + substance, data = d65),
  expert_strata = coxph(
    Surv(time, answered) ~ presentation + substance + strata(category),
    data = d65
  ),
  four_dims = coxph(
    Surv(time, answered) ~ clarity + logic + manner + validity, data = d65
  ),
  no_anchors = coxph(
    Surv(time, answered) ~ z1 + z2, data = subset(d65, !item %in% c(1, 15))
  ),
  logit_answered = glm(answered ~ z1 + z2, family = binomial, data = d65),
  ols_logdays = lm(
    log(days_to_answer) ~ z1 + z2, data = subset(d65, answered == 1)
  )
)

# 3. The corpus ================================================================
dc <- read.csv(file.path("data", "tidy", "admin_outcomes_corpus2024_rescored.csv"))
dc$fdate <- as.numeric(as.Date(dc$date_petitioned))
dc$esc <- as.integer(
  dc$status %in% c("심사완료", "제안실현", "제안실시 (완전실시)",
                   "제안실시 (일부실시)")
)
cat("\n== 2024 corpus ==\nn =", nrow(dc), " answered =", sum(dc$answered),
    " escalated =", sum(dc$esc), "\n")

corpus_models <- function(s) {
  d <- dc
  for (k in dims) d[[k]] <- d[[paste0(s, "_", k)]]
  d <- d[complete.cases(d[, dims]), ]
  d$pres <- scale(d$clarity + d$manner)[, 1]
  d$subs <- scale(d$logic + d$validity)[, 1]
  for (k in dims) d[[paste0("z_", k)]] <- scale(d[[k]])[, 1]
  ans <- subset(d, answered == 1)
  list(
    cox = coxph(Surv(time, answered) ~ pres + subs, data = d),
    cox_full = coxph(
      Surv(time, answered) ~ pres + subs + log(nchar) + fdate + strata(area),
      data = d
    ),
    cox_dims = coxph(
      Surv(time, answered) ~ z_clarity + z_logic + z_manner + z_validity +
        log(nchar) + fdate + strata(area),
      data = d
    ),
    esc = glm(esc ~ pres + subs, family = binomial, data = ans),
    esc_full = glm(
      esc ~ pres + subs + log(nchar) + factor(area),
      family = binomial, data = ans
    ),
    esc_dims = glm(
      esc ~ z_clarity + z_logic + z_manner + z_validity + log(nchar) +
        factor(area),
      family = binomial, data = ans
    )
  )
}
mc <- lapply(setNames(names(scorers), names(scorers)), corpus_models)

## Every coefficient, long -----------------------------------------------------
tidy_model <- function(m, sample, scorer, model) {
  co <- summary(m)$coefficients
  co <- co[!grepl("factor\\(area\\)|Intercept", rownames(co)), , drop = FALSE]
  data.frame(
    sample = sample, scorer = scorer, model = model, term = rownames(co),
    estimate = co[, 1],
    se = co[, if (inherits(m, "coxph")) "se(coef)" else 2],
    p = co[, ncol(co)],
    n = if (inherits(m, "coxph")) m$n else nobs(m),
    events = if (inherits(m, "coxph")) m$nevent else NA,
    row.names = NULL
  )
}
all_models <- rbind(
  do.call(rbind, lapply(names(m65), function(nm) {
    tidy_model(m65[[nm]], "65 petitions", "", nm)
  })),
  do.call(rbind, lapply(names(mc), function(s) {
    do.call(rbind, lapply(names(mc[[s]]), function(nm) {
      tidy_model(mc[[s]][[nm]], "corpus", s, nm)
    }))
  }))
)
write.csv(all_models, file.path(out_dir, "admin_models.csv"), row.names = FALSE)

cat("\n== Corpus: presentation and substance, by scorer ==\n")
show <- all_models[
  all_models$sample == "corpus" & all_models$term %in% c("pres", "subs") &
    all_models$model %in% c("cox", "cox_full", "esc", "esc_full"),
  c("scorer", "model", "term", "estimate", "se", "p")
]
print(format(show, digits = 2), row.names = FALSE)

# 3b. Like-for-like comparison: LLM scores on a common footing ================
## The 65-petition models above use the estimated positions and the author
## codes; the corpus models use LLM scores. To compare the samples on one
## measure, the same LLM-score models are fit on (i) the 65 survey petitions
## and (ii) the corpus proposals on the six survey topics. Scores are
## standardized with the full-corpus mean and standard deviation throughout,
## so coefficients are on one scale across samples.
topics <- grep("^topic_", names(dc), value = TRUE)
dc$on_topic <- rowSums(dc[, topics]) > 0
cat("\n== Proposals on a survey topic:", sum(dc$on_topic), "; answered:",
    sum(dc$answered[dc$on_topic]), "; escalated:", sum(dc$esc[dc$on_topic]),
    "==\n")

like_fits <- list()
subset_models <- do.call(rbind, lapply(names(scorers), function(s) {
  d <- dc
  for (k in dims) d[[k]] <- d[[paste0(s, "_", k)]]
  d <- d[complete.cases(d[, dims]), ]
  mu <- c(mean(d$clarity + d$manner), mean(d$logic + d$validity))
  sdv <- c(sd(d$clarity + d$manner), sd(d$logic + d$validity))
  d$pres <- (d$clarity + d$manner - mu[1]) / sdv[1]
  d$subs <- (d$logic + d$validity - mu[2]) / sdv[2]
  tp <- d[d$on_topic, ]
  s65 <- merge(d65, v65[, c("item", paste0(s, "_", dims))], by = "item")
  s65$pres <- (s65[[paste0(s, "_clarity")]] + s65[[paste0(s, "_manner")]] -
    mu[1]) / sdv[1]
  s65$subs <- (s65[[paste0(s, "_logic")]] + s65[[paste0(s, "_validity")]] -
    mu[2]) / sdv[2]
  topic_rhs <- paste(topics, collapse = " + ")
  fits <- list(
    "full corpus|cox" = coxph(Surv(time, answered) ~ pres + subs, data = d),
    "full corpus|cox_controls" = coxph(
      Surv(time, answered) ~ pres + subs + log(nchar) + fdate, data = d
    ),
    "full corpus|esc" = glm(
      esc ~ pres + subs, family = binomial, data = subset(d, answered == 1)
    ),
    "topic subset|cox" = coxph(Surv(time, answered) ~ pres + subs, data = tp),
    "topic subset|cox_controls" = coxph(
      Surv(time, answered) ~ pres + subs + log(nchar) + fdate, data = tp
    ),
    "topic subset|cox_topics" = coxph(
      as.formula(paste(
        "Surv(time, answered) ~ pres + subs + log(nchar) + fdate +", topic_rhs
      )),
      data = tp
    ),
    "topic subset|esc" = glm(
      esc ~ pres + subs, family = binomial, data = subset(tp, answered == 1)
    ),
    "topic subset|esc_controls" = glm(
      esc ~ pres + subs + log(nchar), family = binomial,
      data = subset(tp, answered == 1)
    ),
    "65 petitions|logit_answered" = glm(
      answered ~ pres + subs, family = binomial, data = s65
    ),
    "65 petitions|cox" = coxph(Surv(time, answered) ~ pres + subs, data = s65),
    "65 petitions|cox_controls" = coxph(
      Surv(time, answered) ~ pres + subs + log(nchar) + fdate, data = s65
    ),
    "65 petitions|cox_topics" = coxph(
      Surv(time, answered) ~ pres + subs + strata(category), data = s65
    )
  )
  like_fits[[s]] <<- fits
  do.call(rbind, lapply(names(fits), function(nm) {
    key <- strsplit(nm, "|", fixed = TRUE)[[1]]
    out <- tidy_model(fits[[nm]], key[1], s, key[2])
    out[out$term %in% c("pres", "subs", "log(nchar)"), ]
  }))
}))
write.csv(subset_models, file.path(out_dir, "subset_models.csv"),
          row.names = FALSE)
cat("\n== LLM presentation and substance, by sample ==\n")
print(
  format(
    subset_models[subset_models$term != "log(nchar)",
                  c("scorer", "sample", "model", "term", "estimate", "se",
                    "p", "n", "events")],
    digits = 2
  ),
  row.names = FALSE
)

# 4. Each scorer as a second benchmark for the estimated positions =============
theta_on_llm <- do.call(rbind, lapply(names(scorers), function(s) {
  d <- merge(d65[, c("item", "theta1", "theta2")], v65, by = "item")
  for (k in dims) d[[k]] <- scale(d[[paste0(s, "_", k)]])[, 1]
  do.call(rbind, lapply(c("theta1", "theta2"), function(y) {
    m <- lm(reformulate(dims, y), data = d)
    co <- summary(m)$coefficients[-1, , drop = FALSE]
    data.frame(
      scorer = s, outcome = y, term = rownames(co), estimate = co[, 1],
      se = co[, 2], p = co[, 4], adj_r2 = summary(m)$adj.r.squared,
      row.names = NULL
    )
  }))
}))
write.csv(theta_on_llm, file.path(out_dir, "theta_on_llm.csv"),
          row.names = FALSE)

# Tables =======================================================================
cell <- function(m, term) {
  co <- summary(m)$coefficients
  if (!term %in% rownames(co)) return("")
  b <- co[term, 1]
  se <- co[term, if (inherits(m, "coxph")) 3 else 2]
  p <- co[term, ncol(co)]
  stars <- if (p < .01) "$^{**}$" else if (p < .05) "$^{*}$" else
    if (p < .1) "$^{\\dagger}$" else ""
  sprintf("%.3f%s (%.3f)", b, stars, se)
}
tab_rows <- function(mods, terms) {
  vapply(terms, function(tt) {
    paste0("  ", tt[1], " & ",
           paste(vapply(mods, cell, "", term = tt[2]), collapse = " & "),
           " \\\\")
  }, "")
}
nobs_of <- function(m) {
  if (inherits(m, "coxph")) as.character(m$n) else as.character(nobs(m))
}
events_of <- function(m) {
  if (inherits(m, "coxph")) as.character(summary(m)$nevent) else ""
}
n_fmt <- function(n) formatC(as.integer(n), format = "d", big.mark = "{,}")

## Manuscript table, one per scorer --------------------------------------------
## Panel A: hazard of a government answer (Cox). Panel B: escalation beyond an
## answer among answered proposals (logit). LLM scores are standardized on the
## full corpus in every column.
for (s in names(scorers)) {
  lf <- like_fits[[s]]
  modsA <- list(
    m65$thetas, m65$thetas_strata, lf[["65 petitions|cox"]],
    lf[["topic subset|cox_controls"]], lf[["full corpus|cox"]],
    mc[[s]]$cox_full
  )
  termsA <- list(
    c("Perceived presentation ($\\theta_1$, std.)", "z1"),
    c("Perceived substance ($\\theta_2$, std.)", "z2"),
    c("LLM presentation (clarity $+$ tone, std.)", "pres"),
    c("LLM substance (logic $+$ validity, std.)", "subs"),
    c("Log petition length", "log(nchar)")
  )
  writeLines(
    c(
      paste0("% generated by R/27_v2_admin_outcomes.R; LLM scores: ",
             scorers[s]),
      "\\begin{tabular}{lcccccc}",
      "  \\toprule",
      paste0("  & \\multicolumn{3}{c}{Survey petitions}",
             " & Six-topic subset & \\multicolumn{2}{c}{2024 corpus} \\\\"),
      "  \\cmidrule(lr){2-4} \\cmidrule(lr){5-5} \\cmidrule(lr){6-7}",
      "  & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
      "  \\midrule",
      tab_rows(modsA, termsA),
      "  \\midrule",
      "  Filing date & & & & \\checkmark & & \\checkmark \\\\",
      paste0("  Topic or policy-area strata & & \\checkmark & & & &",
             " \\checkmark \\\\"),
      paste0("  Proposals & ",
             paste(n_fmt(vapply(modsA, function(m) m$n, 1)), collapse = " & "),
             " \\\\"),
      paste0("  Answered (events) & ",
             paste(n_fmt(vapply(modsA, function(m) m$nevent, 1)),
                   collapse = " & "), " \\\\"),
      "  \\bottomrule",
      "\\end{tabular}"
    ),
    file.path(out_dir, "tab", paste0("admin_outcomes_", s, ".tex"))
  )
  modsB <- list(
    lf[["topic subset|esc"]], lf[["topic subset|esc_controls"]],
    mc[[s]]$esc, mc[[s]]$esc_full
  )
  writeLines(
    c(
      paste0("% generated by R/27_v2_admin_outcomes.R; LLM scores: ",
             scorers[s]),
      "\\begin{tabular}{lcccc}",
      "  \\toprule",
      paste0("  & \\multicolumn{2}{c}{Six-topic subset}",
             " & \\multicolumn{2}{c}{2024 corpus} \\\\"),
      "  \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
      "  & (7) & (8) & (9) & (10) \\\\",
      "  \\midrule",
      tab_rows(modsB, termsA[3:5]),
      "  \\midrule",
      "  Policy-area fixed effects & & & & \\checkmark \\\\",
      paste0("  Answered proposals & ",
             paste(n_fmt(vapply(modsB, function(m) nobs(m), 1)),
                   collapse = " & "), " \\\\"),
      paste0("  Escalated & ",
             paste(n_fmt(vapply(modsB, function(m) sum(m$y), 1)),
                   collapse = " & "), " \\\\"),
      "  \\bottomrule",
      "\\end{tabular}"
    ),
    file.path(out_dir, "tab", paste0("admin_escalation_", s, ".tex"))
  )
}

## Response-letter table A: 65 petitions, robustness (scorer-independent) ------
modsA <- list(
  m65$thetas, m65$thetas_strata, m65$thetas_fdate, m65$thetas_len,
  m65$no_anchors, m65$expert, m65$four_dims
)
termsA <- list(
  c("Perceived presentation ($\\theta_1$, std.)", "z1"),
  c("Perceived substance ($\\theta_2$, std.)", "z2"),
  c("Filing date (days)", "fdate"),
  c("Log petition length", "log(nchar)"),
  c("Coded presentation (clarity $+$ tone, 0--2)", "presentation"),
  c("Coded substance (logic $+$ validity, 0--2)", "substance"),
  c("Clarity/specificity (0/1)", "clarity"),
  c("Logic/consistency (0/1)", "logic"),
  c("Tone/manner (0/1)", "manner"),
  c("Validity/feasibility (0/1)", "validity")
)
writeLines(
  c(
    "% generated by R/27_v2_admin_outcomes.R",
    "\\begin{tabular}{lccccccc}",
    "  \\toprule",
    "  & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\\\",
    "  & Baseline & Topic strata & Filing date & Log length",
    "  & Anchors dropped & Author codes & Four codes \\\\",
    "  \\midrule",
    tab_rows(modsA, termsA),
    "  \\midrule",
    "  Topic strata & & \\checkmark & & & & & \\\\",
    paste0("  Petitions & ",
           paste(vapply(modsA, nobs_of, ""), collapse = " & "), " \\\\"),
    paste0("  Answered (events) & ",
           paste(vapply(modsA, events_of, ""), collapse = " & "), " \\\\"),
    "  \\bottomrule",
    "\\end{tabular}"
  ),
  file.path(out_dir, "tab", "admin_outcomes_response_A.tex")
)

## Response-letter table B: alternative outcomes, one per scorer ---------------
for (s in names(scorers)) {
  modsB <- list(m65$logit_answered, m65$ols_logdays, mc[[s]]$esc,
                mc[[s]]$esc_full)
  termsB <- list(
    c("Perceived presentation ($\\theta_1$, std.)", "z1"),
    c("Perceived substance ($\\theta_2$, std.)", "z2"),
    c("LLM presentation (std.)", "pres"),
    c("LLM substance (std.)", "subs"),
    c("Log petition length", "log(nchar)")
  )
  writeLines(
    c(
      paste0("% generated by R/27_v2_admin_outcomes.R; corpus scores: ",
             scorers[s]),
      "\\begin{tabular}{lcccc}",
      "  \\toprule",
      "  & \\multicolumn{2}{c}{Survey petitions ($n=65$)}",
      "  & \\multicolumn{2}{c}{2024 corpus, answered petitions} \\\\",
      "  \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
      "  & (1) & (2) & (3) & (4) \\\\",
      "  Outcome & Answered (logit) & Log days to answer (OLS)",
      "  & Escalated (logit) & Escalated (logit) \\\\",
      "  \\midrule",
      tab_rows(modsB, termsB),
      "  \\midrule",
      "  Area fixed effects & & & & \\checkmark \\\\",
      paste0("  Observations & ",
             paste(vapply(modsB, nobs_of, ""), collapse = " & "), " \\\\"),
      "  \\bottomrule",
      "\\end{tabular}"
    ),
    file.path(out_dir, "tab", paste0("admin_outcomes_response_B_", s, ".tex"))
  )
}

cat("\n== Estimated positions on standardized LLM scores, 65 petitions ==\n")
print(format(theta_on_llm, digits = 2), row.names = FALSE)
cat("\nwrote", out_dir, "\n")
