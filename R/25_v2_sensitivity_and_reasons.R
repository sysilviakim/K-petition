# Part 1: sensitivity of the pairwise IRT to the composition of the petition
# sample (PSJ R&R, comments R1.3 / R3.1 / R4.3). No new data: every fit uses the
# existing pairwise choices, restricted to a subset of the 65 petitions.
#
# The reviewers' concern is that screening out very short or incoherent
# submissions removed the bottom of the quality distribution and may have
# shaped the estimated dimensions. The excluded submissions cannot be added
# back, since no respondent saw them. The cutoff can be moved the other way:
# the weakest petitions that were retained are removed, and the model is
# re-estimated. Stability under further trimming indicates that the results
# do not hinge on where the screening line fell.
#
#   1. Trim the bottom: petitions coded 0 on all four dimensions; the
#      shortest fifth; the fifth rated least effective by respondents.
#   2. Trim the top: petitions coded 1 on all four dimensions.
#   3. Leave one topic out (six fits).
#   4. Without the model: how often respondents choose the better-coded
#      petition, by how weak or how short the weaker petition of the pair is.
#
# The three anchor petitions (1, 15, 59) are kept in every fit. Each fit is
# compared with the full-sample fit on the retained petitions, in the
# submitted orientation (first dimension = the one that tracks tone).
#
# Input:   survey workbook (via R/08_survey_wrangling.R)
#          data/tidy/petition65_codes_v2.csv
#          data/screenshots/공개제안_tidy_url_appended_v2.xlsx  (petition text)
#          output/v2/fits/a59/theta_medians.csv
# Output:  output/v2/sensitivity/sensitivity_fits.csv
#          output/v2/sensitivity/sensitivity_positions.csv
#          output/v2/sensitivity/accuracy_by_weaker_petition.csv
#          output/v2/sensitivity/win_rates.csv
#          output/v2/sensitivity/tab/sensitivity_fits.tex
#          output/v2/sensitivity/tab/sensitivity_accuracy.tex
#
# Part 2 tabulates respondents' stated reasons for their pairwise choices
# (survey item Q14), for a short section of the Supplementary Information.
# After each pairwise choice, respondents selected one reason for it from a
# fixed list whose first four options correspond to the four quality
# dimensions. For each reason, the table reports the share of choices and
# the average advantage of the chosen petition over the other petition on
# each expert code (chosen minus not chosen, on the 0/1 code).
# Output:  output/v2/stated_reasons.csv
#          output/v2/tab/stated_reasons.tex
#
# Run from the project root under a UTF-8 locale (about three minutes):
#   LC_ALL=en_US.UTF-8 Rscript --vanilla R/25_v2_sensitivity_and_reasons.R

# Setup ========================================================================
suppressPackageStartupMessages({
  source(here::here("R", "utilities.R"))
  source(here::here("R", "08_survey_wrangling.R"))
  library(parallel)
})
out_dir <- here::here("output", "v2", "sensitivity")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
anchors <- c(1, 15, 59)
theta_constraints <- list(
  item.1 = list(1, 2), item.1 = list(2, 2),
  item.15 = list(1, -2), item.15 = list(2, -2),
  item.59 = list(1, "-"), item.59 = list(2, "+")
)

## Pairwise comparisons, as in 09_survey_descriptives.R ------------------------
pwc <- map_dfr(1:8, function(i) {
  df %>%
    select(
      NO,
      id1 = !!sym(paste0("Q13_gCode", i, "_1")),
      id2 = !!sym(paste0("Q13_gCode", i, "_2")),
      pick = !!sym(paste0("Q13_", i))
    )
}) %>%
  mutate(
    Item1 = paste0("item.", id1),
    Item2 = paste0("item.", id2),
    Choice = ifelse(pick == 1, Item1, Item2)
  ) %>%
  as.data.frame()

## Petition attributes ---------------------------------------------------------
codes <- read.csv(
  here::here("data", "tidy", "petition65_codes_v2.csv"),
  colClasses = c(comb_new = "character")
)
stim <- read_xlsx(here::here(
  stri_trans_nfc("data/screenshots/공개제안_tidy_url_appended_v2.xlsx")
))
eff <- map_dfr(1:5, function(i) {
  df %>%
    select(
      item = !!sym(paste0("Q15_gCode", i)),
      effectiveness = !!sym(paste0("Q15_", i))
    )
}) %>%
  group_by(item) %>%
  summarise(mean_eff = mean(effectiveness), .groups = "drop")
pet <- codes %>%
  transmute(
    item = id, category, comb = comb_new,
    clarity = clarity_new, logic = logic_new, manner = manner_new,
    validity = validity_new,
    quality_sum = clarity + logic + manner + validity
  ) %>%
  left_join(
    tibble(item = as.integer(stim$id), nchar = nchar(stim$text)),
    by = "item"
  ) %>%
  left_join(eff, by = "item")
stopifnot(nrow(pet) == 65, !anyNA(pet))

# Subsets ======================================================================
fifth <- 13 # one fifth of 65
droppable <- pet %>% filter(!item %in% anchors)
drops <- c(
  list(
    "Bottom: coded 0 on all four" =
      droppable$item[droppable$comb == "0000"],
    "Bottom: shortest fifth" =
      droppable$item[order(droppable$nchar)][1:fifth],
    "Bottom: lowest fifth by rated effectiveness" =
      droppable$item[order(droppable$mean_eff)][1:fifth],
    "Top: coded 1 on all four" =
      droppable$item[droppable$comb == "1111"]
  ),
  setNames(
    lapply(unique(pet$category), function(k) {
      droppable$item[droppable$category == k]
    }),
    paste("Topic left out:", category_en[unique(pet$category)])
  )
)

# Fits =========================================================================
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
  names(w) <- c("item", "raw1", "raw2")
  w[order(w$item), ]
}
fit_subset <- function(drop) {
  keep <- !(pwc$id1 %in% drop | pwc$id2 %in% drop)
  set.seed(1234)
  out <- MCMCpaircompare2d(
    pwc.data = pwc[keep, c("NO", "Item1", "Item2", "Choice")],
    theta.constraints = theta_constraints,
    burnin = MCMC_BURNIN, mcmc = MCMC_ITER, thin = MCMC_THIN,
    verbose = 0, store.theta = TRUE, store.gamma = FALSE, tune = MCMC_TUNE
  )
  list(theta = theta_medians(as.matrix(out)), n_pairs = sum(keep))
}
fits <- mclapply(
  drops, fit_subset,
  mc.cores = max(1, min(length(drops), detectCores() - 2)),
  mc.preschedule = FALSE
)
stopifnot(all(vapply(fits, is.list, TRUE)))

## Full-sample reference, in the submitted orientation -------------------------
## (the anchor-59 fit as estimated has its two dimensions exchanged)
full <- read.csv(here::here(
  "output", "v2", "fits", "a59", "theta_medians.csv"
))
ref <- data.frame(item = full$item, t1 = full$theta2, t2 = full$theta1)
sp <- function(a, b) cor(a, b, method = "spearman")

code_reg <- function(d, y) {
  co <- summary(lm(reformulate(c("clarity", "logic", "manner", "validity"), y),
                   data = d))$coefficients[-1, , drop = FALSE]
  co
}
describe <- function(th, label, dropped, n_pairs) {
  d <- merge(th, pet, by = "item")
  r1 <- code_reg(d, "t1")
  r2 <- code_reg(d, "t2")
  cmp <- merge(th, ref, by = "item", suffixes = c("", "_full"))
  cmp <- cmp[!cmp$item %in% anchors, ]
  data.frame(
    subset = label,
    petitions_dropped = length(dropped),
    petitions_kept = nrow(th),
    comparisons_kept = n_pairs,
    rho_theta1 = sp(cmp$t1, cmp$t1_full),
    rho_theta2 = sp(cmp$t2, cmp$t2_full),
    tone_on_theta1 = r1["manner", 1],
    tone_se = r1["manner", 2],
    tone_p = r1["manner", 4],
    other_codes_on_theta1_min_p = min(r1[rownames(r1) != "manner", 4]),
    any_code_on_theta2_min_p = min(r2[, 4]),
    theta2_min_p_code = rownames(r2)[which.min(r2[, 4])]
  )
}

positions <- list()
rows <- list(describe(ref, "Full sample (65 petitions)", integer(0),
                      nrow(pwc)))
for (nm in names(fits)) {
  th <- fits[[nm]]$theta
  cmp <- merge(th, ref, by = "item")
  direct <- sp(cmp$raw1, cmp$t1) + sp(cmp$raw2, cmp$t2)
  crossed <- sp(cmp$raw1, cmp$t2) + sp(cmp$raw2, cmp$t1)
  th <- if (crossed > direct) {
    data.frame(item = th$item, t1 = th$raw2, t2 = th$raw1)
  } else {
    data.frame(item = th$item, t1 = th$raw1, t2 = th$raw2)
  }
  positions[[nm]] <- cbind(subset = nm, th)
  rows[[nm]] <- describe(th, nm, drops[[nm]], fits[[nm]]$n_pairs)
}
sens <- do.call(rbind, rows)
write.csv(sens, file.path(out_dir, "sensitivity_fits.csv"), row.names = FALSE)
write.csv(do.call(rbind, positions),
          file.path(out_dir, "sensitivity_positions.csv"), row.names = FALSE)

cat("== Re-estimated fits against the full-sample fit ==\n")
print(format(sens, digits = 2), row.names = FALSE)

# Without the model ============================================================
## Does the task get easier as the weaker petition of a pair gets weaker?
pw <- pwc %>%
  left_join(pet %>% select(id1 = item, q1 = quality_sum, n1 = nchar),
            by = "id1") %>%
  left_join(pet %>% select(id2 = item, q2 = quality_sum, n2 = nchar),
            by = "id2") %>%
  mutate(
    gap = abs(q1 - q2),
    weaker_q = pmin(q1, q2),
    chose_better = ifelse(q1 > q2, pick == 1, pick == 2),
    shorter_n = pmin(n1, n2),
    chose_longer = ifelse(n1 > n2, pick == 1, pick == 2)
  )
acc <- pw %>%
  filter(gap > 0) %>%
  mutate(weaker = ifelse(weaker_q == 0, "coded 0 on all four",
                         "coded 1 on at least one")) %>%
  group_by(gap, weaker) %>%
  summarise(
    n = n(), accuracy = mean(chose_better),
    se = sqrt(accuracy * (1 - accuracy) / n), .groups = "drop"
  )
write.csv(acc, file.path(out_dir, "accuracy_by_weaker_petition.csv"),
          row.names = FALSE)
cat("\n== Share choosing the better-coded petition, by expert gap and by",
    "whether the weaker petition is coded 0 on all four ==\n")
print(format(as.data.frame(acc), digits = 3), row.names = FALSE)

## Gap held fixed: logit of choosing the better petition on the weaker
## petition's quality sum and length, with gap fixed effects
m_acc <- glm(
  chose_better ~ factor(gap) + weaker_q + log(shorter_n),
  family = binomial, data = filter(pw, gap > 0)
)
cat("\n== Logit: choosing the better-coded petition ==\n")
print(round(summary(m_acc)$coefficients, 3))

## How often each kind of petition is chosen at all
long <- bind_rows(
  pw %>% transmute(item = id1, chosen = pick == 1),
  pw %>% transmute(item = id2, chosen = pick == 2)
) %>%
  left_join(pet, by = "item")
short_cut <- sort(pet$nchar)[fifth]
win <- bind_rows(
  long %>% group_by(group = paste("Expert index =", quality_sum)) %>%
    summarise(petitions = n_distinct(item), shown = n(),
              win_rate = mean(chosen), .groups = "drop"),
  long %>% group_by(group = ifelse(nchar <= short_cut, "Shortest fifth",
                                   "All other lengths")) %>%
    summarise(petitions = n_distinct(item), shown = n(),
              win_rate = mean(chosen), .groups = "drop")
)
write.csv(win, file.path(out_dir, "win_rates.csv"), row.names = FALSE)
cat("\n== How often petitions are chosen ==\n")
print(format(as.data.frame(win), digits = 3), row.names = FALSE)
cat("\nShortest-fifth cutoff:", short_cut, "characters; shortest petition:",
    min(pet$nchar), "\n")

# Tables =======================================================================
dir.create(file.path(out_dir, "tab"), showWarnings = FALSE)
p_fmt <- function(p) if (p < .001) "$<$.001" else sub("^0", "", sprintf("%.3f", p))
label_of <- function(x) {
  x <- sub("^(Bottom|Top|Topic left out): ", "", x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}
fit_lines <- vapply(seq_len(nrow(sens)), function(i) {
  r <- sens[i, ]
  full <- i == 1
  paste0(
    "  \\quad ", label_of(r$subset), " & ",
    r$petitions_kept, " & ", formatC(r$comparisons_kept, big.mark = "{,}",
                                     format = "d"), " & ",
    if (full) "--- & --- & " else sprintf("%.2f & %.2f & ", r$rho_theta1,
                                          r$rho_theta2),
    sprintf("%.2f (%.2f) & %s & %.2f & %.2f", r$tone_on_theta1, r$tone_se,
            p_fmt(r$tone_p), r$other_codes_on_theta1_min_p,
            r$any_code_on_theta2_min_p),
    " \\\\"
  )
}, "")
group_at <- function(prefix) which(grepl(prefix, sens$subset))[1]
writeLines(
  c(
    "% generated by R/25_v2_sensitivity_and_reasons.R",
    "\\begin{tabular}{lrrrrrrrr}",
    "  \\toprule",
    paste0("  & & & \\multicolumn{2}{c}{$\\rho$ with full fit}",
           " & \\multicolumn{2}{c}{Tone on $\\theta_1$}",
           " & \\multicolumn{2}{c}{Smallest $p$, other codes} \\\\"),
    "  \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}",
    paste0("  Petitions removed & Kept & Comparisons & $\\theta_1$ &",
           " $\\theta_2$ & Coef. (SE) & $p$ & on $\\theta_1$ &",
           " on $\\theta_2$ \\\\"),
    "  \\midrule",
    sub("\\quad Full sample (65 petitions)", "None (full sample)",
        fit_lines[1], fixed = TRUE),
    "  \\addlinespace",
    "  \\multicolumn{9}{l}{\\emph{Weakest petitions}} \\\\",
    fit_lines[grepl("^Bottom", sens$subset)],
    "  \\addlinespace",
    "  \\multicolumn{9}{l}{\\emph{Strongest petitions}} \\\\",
    fit_lines[grepl("^Top:", sens$subset)],
    "  \\addlinespace",
    "  \\multicolumn{9}{l}{\\emph{One topic left out}} \\\\",
    fit_lines[grepl("^Topic", sens$subset)],
    "  \\bottomrule",
    "\\end{tabular}"
  ),
  file.path(out_dir, "tab", "sensitivity_fits.tex")
)

acc_w <- reshape(
  as.data.frame(acc)[, c("gap", "weaker", "n", "accuracy")],
  idvar = "gap", timevar = "weaker", direction = "wide"
)
names(acc_w) <- c("gap", "n0", "a0", "n1", "a1")
cell <- function(a, n) {
  ifelse(is.na(a), "---",
         sprintf("%.1f\\%% (%s)", 100 * a,
                 formatC(n, big.mark = "{,}", format = "d")))
}
writeLines(
  c(
    "% generated by R/25_v2_sensitivity_and_reasons.R",
    "\\begin{tabular}{lcc}",
    "  \\toprule",
    "  & \\multicolumn{2}{c}{Weaker petition of the pair} \\\\",
    "  \\cmidrule(lr){2-3}",
    "  Expert gap & Coded 0 on all four & Coded 1 on at least one \\\\",
    "  \\midrule",
    sprintf("  %d & %s & %s \\\\", acc_w$gap, cell(acc_w$a0, acc_w$n0),
            cell(acc_w$a1, acc_w$n1)),
    "  \\bottomrule",
    "\\end{tabular}"
  ),
  file.path(out_dir, "tab", "sensitivity_accuracy.tex")
)


# Part 2: stated reasons =======================================================

codes <- read.csv(
  here::here("data", "tidy", "petition65_codes_v2.csv"),
  colClasses = c(comb_new = "character")
)
cm <- do.call(rbind, lapply(strsplit(codes$comb_new, ""), as.integer))
rownames(cm) <- codes$id
colnames(cm) <- c("clarity", "logic", "tone", "validity")

choices <- do.call(rbind, lapply(1:8, function(i) {
  a <- df[[paste0("Q13_gCode", i, "_1")]]
  b <- df[[paste0("Q13_gCode", i, "_2")]]
  pick <- df[[paste0("Q13_", i)]]
  chosen <- ifelse(pick == 1, a, b)
  other <- ifelse(pick == 1, b, a)
  data.frame(
    task = i,
    reason = df[[paste0("Q14_", i)]],
    cm[as.character(chosen), ] - cm[as.character(other), ]
  )
}))
stopifnot(!anyNA(choices$reason))

reason_labels <- c(
  "The claim is clear and specific",
  "It is logical and consistent",
  "Its expression and attitude are appropriate",
  "It is valid and highly feasible",
  "Other"
)
by_reason <- do.call(rbind, lapply(1:5, function(r) {
  d <- choices[choices$reason == r, ]
  data.frame(
    reason = reason_labels[r], n = nrow(d), share = nrow(d) / nrow(choices),
    t(colMeans(d[, colnames(cm)]))
  )
}))
all_row <- data.frame(
  reason = "All choices", n = nrow(choices), share = 1,
  t(colMeans(choices[, colnames(cm)]))
)
out <- rbind(by_reason, all_row)
out$share_first_task <- c(
  vapply(1:5, function(r) mean(choices$reason[choices$task == 1] == r), 1), NA
)
write.csv(out, here::here("output", "v2", "stated_reasons.csv"),
          row.names = FALSE)
print(format(out, digits = 2), row.names = FALSE)

row_tex <- function(r) {
  sprintf(
    "  %s & %s & %.0f\\%% & %.2f & %.2f & %.2f & %.2f \\\\",
    r$reason, formatC(r$n, big.mark = "{,}", format = "d"), 100 * r$share,
    r$clarity, r$logic, r$tone, r$validity
  )
}
dir.create(here::here("output", "v2", "tab"), showWarnings = FALSE)
writeLines(
  c(
    "% generated by R/25_v2_sensitivity_and_reasons.R",
    "\\begin{tabular}{lrrcccc}",
    "  \\toprule",
    paste0("  & & & \\multicolumn{4}{c}{Chosen petition's advantage on the",
           " expert code} \\\\"),
    "  \\cmidrule(lr){4-7}",
    paste0("  Stated reason for the choice & Choices & Share & Clarity &",
           " Logic & Tone & Validity \\\\"),
    "  \\midrule",
    vapply(1:5, function(i) row_tex(out[i, ]), ""),
    "  \\midrule",
    row_tex(out[6, ]),
    "  \\bottomrule",
    "\\end{tabular}"
  ),
  here::here("output", "v2", "tab", "stated_reasons.tex")
)
