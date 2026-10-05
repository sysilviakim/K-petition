# Administrative outcomes analysis for the PSJ R&R (reviewers R1.6 / R4.4).
#
# Question: do the quality signals recovered from citizen co-assessment (and
# the expert/LLM quality codes) predict observable administrative outcomes --
# speed of government answer and status escalation beyond an answer?
#
# Two levels:
#   (a) the 65 survey stimulus petitions, using the pairwise-IRT posterior
#       median positions (theta1 = presentation, theta2 = substance) and the
#       author-coded binary dimensions;
#   (b) the full evaluated 2024 corpus (n = 6,447), using LLM-scored 1-5
#       dimension scores.
#
# Time-to-answer is modeled with Cox proportional hazards; petitions not yet
# answered at scrape date are right-censored at (date_scraped -
# date_petitioned). Status escalation (심사완료 / 제안실현 / 제안실시) is
# modeled with logistic regression among answered petitions.
#
# NOTE: intentionally self-contained (base R + survival only) so it runs
# outside the renv environment. Run R/18_survey_admin_outcomes_build.py first
# to generate the input CSVs; theta_medians.csv is created below from
# data/mcmc_out.rds if missing.

library(survival)

# ---- posterior median thetas (once) ------------------------------------------
theta_file <- "output/theta_medians.csv"
if (!file.exists(theta_file)) {
  post <- readRDS("data/mcmc_out.rds")
  m <- as.matrix(post)
  tc <- grep("^theta[12]\\.item", colnames(m), value = TRUE)
  med <- apply(m[, tc], 2, median)
  parts <- do.call(rbind, strsplit(names(med), ".", fixed = TRUE))
  long <- data.frame(
    item = as.integer(parts[, 3]),
    dim = parts[, 1],
    median = as.numeric(med)
  )
  wide <- reshape(long, idvar = "item", timevar = "dim", direction = "wide")
  names(wide) <- c("item", "theta1", "theta2")
  write.csv(wide[order(wide$item), ], theta_file, row.names = FALSE)
}

# ---- (a) 65 stimulus petitions -----------------------------------------------
d65 <- read.csv("data/tidy/admin_outcomes_65.csv")
d65$z1 <- scale(d65$theta1)[, 1]
d65$z2 <- scale(d65$theta2)[, 1]
d65$fdate <- as.numeric(as.Date(d65$date_petitioned))

cat("== 65 stimulus petitions ==\n")
print(table(d65$status))

m65 <- list(
  thetas = coxph(Surv(time, answered) ~ z1 + z2, data = d65),
  thetas_strata = coxph(
    Surv(time, answered) ~ z1 + z2 + strata(category),
    data = d65
  ),
  thetas_fdate = coxph(
    Surv(time, answered) ~ z1 + z2 + fdate,
    data = d65
  ),
  thetas_len = coxph(
    Surv(time, answered) ~ z1 + z2 + log(nchar),
    data = d65
  ),
  expert = coxph(
    Surv(time, answered) ~ presentation + substance,
    data = d65
  ),
  expert_strata = coxph(
    Surv(time, answered) ~ presentation + substance + strata(category),
    data = d65
  ),
  four_dims = coxph(
    Surv(time, answered) ~ clarity + logic + manner + validity,
    data = d65
  ),
  no_anchors = coxph(
    Surv(time, answered) ~ z1 + z2,
    data = subset(d65, !item %in% c(1, 15))
  ),
  logit_answered = glm(
    answered ~ z1 + z2,
    family = binomial, data = d65
  ),
  ols_logdays = lm(
    log(days_to_answer) ~ z1 + z2,
    data = subset(d65, answered == 1)
  )
)
for (nm in names(m65)) {
  cat("\n----", nm, "----\n")
  print(round(summary(m65[[nm]])$coefficients, 4))
}

# ---- (b) full 2024 corpus ----------------------------------------------------
dc <- read.csv("data/tidy/admin_outcomes_corpus2024.csv")
dc$pres <- scale(dc$clarity + dc$manner)[, 1]
dc$subs <- scale(dc$logic + dc$validity)[, 1]
dc$fdate <- as.numeric(as.Date(dc$date_petitioned))
dc$esc <- as.integer(
  dc$status %in% c("심사완료", "제안실현", "제안실시 (완전실시)",
                   "제안실시 (일부실시)")
)

cat("\n== 2024 corpus ==\n")
cat("n =", nrow(dc), " answered =", sum(dc$answered),
    " escalated =", sum(dc$esc), "\n")

mc <- list(
  cox = coxph(Surv(time, answered) ~ pres + subs, data = dc),
  cox_full = coxph(
    Surv(time, answered) ~ pres + subs + log(nchar) + fdate + strata(area),
    data = dc
  ),
  esc = glm(
    esc ~ pres + subs,
    family = binomial, data = subset(dc, answered == 1)
  ),
  esc_full = glm(
    esc ~ pres + subs + log(nchar) + factor(area),
    family = binomial, data = subset(dc, answered == 1)
  )
)
for (nm in names(mc)) {
  cat("\n----", nm, "----\n")
  co <- round(summary(mc[[nm]])$coefficients, 4)
  print(co[!grepl("factor", rownames(co)), ])
}

# ---- paper table (tab/admin_outcomes.tex) ------------------------------------
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
mods <- list(
  m65$thetas, m65$thetas_strata, m65$expert, m65$expert_strata,
  mc$cox, mc$cox_full
)
terms <- list(
  c("Perceived presentation ($\\theta_1$, std.)", "z1"),
  c("Perceived substance ($\\theta_2$, std.)", "z2"),
  c("Coded presentation (clarity $+$ tone)", "presentation"),
  c("Coded substance (logic $+$ validity)", "substance"),
  c("LLM presentation (std.)", "pres"),
  c("LLM substance (std.)", "subs"),
  c("Log petition length", "log(nchar)")
)
lines <- c(
  "% generated by R/18_survey_admin_outcomes.R",
  "\\begin{tabular}{lcccccc}",
  "  \\toprule",
  paste0(" & \\multicolumn{4}{c}{Survey petitions ($n=65$)}",
         " & \\multicolumn{2}{c}{2024 corpus ($n=6{,}447$)} \\\\"),
  "  \\cmidrule(lr){2-5} \\cmidrule(lr){6-7}",
  "  & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "  \\midrule"
)
for (tt in terms) {
  row <- paste(vapply(mods, cell, "", term = tt[2]), collapse = " & ")
  lines <- c(lines, paste0("  ", tt[1], " & ", row, " \\\\"))
}
ev <- vapply(mods, function(m) as.character(summary(m)$nevent), "")
lines <- c(
  lines,
  "  \\midrule",
  paste0("  Topic/area strata & & \\checkmark & & \\checkmark & &",
         " \\checkmark \\\\"),
  paste0("  Answered (events) & ", paste(ev, collapse = " & "), " \\\\"),
  "  \\bottomrule",
  "\\end{tabular}"
)
writeLines(lines, "tab/admin_outcomes.tex")
cat("\nwrote tab/admin_outcomes.tex\n")
