# Builds the petition quality codes from the complete three-author coding
# (K-petition_new_scoring.xlsx), for the PSJ R&R re-analysis.
#
# The codes used in the submitted paper (comb_fin) came from a whole-profile
# majority vote among four voters (the LLM profile, SK, BK, KY) on incomplete
# author coding. Here every petition has three author codes, and the final
# code is the per-dimension majority of the three authors. No LLM input.
#
# Input:
#   K-petition_new_scoring.xlsx          id, category, title, SK, BK, KY
#     (default location: the paper folder; override with NEW_SCORING_XLSX)
#   data/screenshots/공개제안_tidy_url_appended.xlsx   65 stimuli, old comb_fin
# Output:
#   data/tidy/petition65_codes_v2.csv     per-coder codes, old and new code
#   data/screenshots/공개제안_tidy_url_appended_v2.xlsx
#                                         stimuli file with comb_fin replaced
#   output/v2/intercoder_reliability.csv   Cohen / Fleiss kappa
#   output/v2/tab/intercoder_reliability.tex
#   output/v2/anchor_candidates.csv        petitions eligible as the
#                                                third IRT anchor
#
# Self-contained (readxl, writexl, stringi). Run from the project root under
# a UTF-8 locale:  LC_ALL=en_US.UTF-8 Rscript --vanilla R/22_v2_expert_codes.R

library(readxl)
library(writexl)
library(stringi)

# Setup ========================================================================
new_xlsx <- Sys.getenv(
  "NEW_SCORING_XLSX",
  unset = path.expand(
    "~/Dropbox/KDIS/Research/K-petition/data/K-petition_new_scoring.xlsx"
  )
)
stim_xlsx <- stri_trans_nfc(
  "data/screenshots/공개제안_tidy_url_appended.xlsx"
)
stim_v2_xlsx <- stri_trans_nfc(
  "data/screenshots/공개제안_tidy_url_appended_v2.xlsx"
)
dir.create("output/v2", showWarnings = FALSE, recursive = TRUE)

dims <- c("clarity", "logic", "manner", "validity")
coders <- c("SK", "BK", "KY")

# Load and check ===============================================================
new <- as.data.frame(read_xlsx(new_xlsx, col_types = "text"))
new <- new[!is.na(new$id), ]
new$id <- as.integer(as.numeric(new$id))
stim <- as.data.frame(read_xlsx(stim_xlsx))
stim$id <- as.integer(stim$id)

stopifnot(
  nrow(new) == 65, nrow(stim) == 65,
  identical(sort(new$id), sort(stim$id)),
  !anyNA(new[, coders])
)
new <- new[match(stim$id, new$id), ]
for (cc in coders) {
  new[[cc]] <- formatC(as.integer(new[[cc]]), width = 4, flag = "0")
  stopifnot(all(grepl("^[01]{4}$", new[[cc]])))
}
stopifnot(
  all(stri_trans_nfc(trimws(new$title)) == stri_trans_nfc(trimws(stim$title))),
  all(stri_trans_nfc(new$category) == stri_trans_nfc(stim$category))
)

# Per-dimension majority =======================================================
## votes[petition, dimension, coder]
votes <- array(
  NA_integer_,
  dim = c(65, 4, 3),
  dimnames = list(NULL, dims, coders)
)
for (cc in coders) {
  for (d in 1:4) votes[, d, cc] <- as.integer(substr(new[[cc]], d, d))
}
n_yes <- apply(votes, c(1, 2), sum)
maj <- (n_yes >= 2) * 1L
unanimous <- n_yes %in% c(0, 3)
dim(unanimous) <- dim(n_yes)

codes <- data.frame(
  id = stim$id,
  category = stim$category,
  title = stim$title,
  SK = new$SK, BK = new$BK, KY = new$KY,
  comb_old = formatC(as.integer(stim$comb_fin), width = 4, flag = "0"),
  comb_new = apply(maj, 1, paste0, collapse = ""),
  stringsAsFactors = FALSE
)
for (d in 1:4) codes[[paste0(dims[d], "_new")]] <- maj[, d]
for (d in 1:4) codes[[paste0(dims[d], "_nyes")]] <- n_yes[, d]
codes$all_unanimous <- apply(unanimous, 1, all)
codes$changed <- codes$comb_old != codes$comb_new

write.csv(
  codes, "data/tidy/petition65_codes_v2.csv",
  row.names = FALSE, fileEncoding = "UTF-8"
)

stim_v2 <- stim
stim_v2$comb_fin <- codes$comb_new
write_xlsx(stim_v2, stim_v2_xlsx)

cat("Codes changed for", sum(codes$changed), "of 65 petitions:\n")
print(codes[codes$changed, c("id", "SK", "BK", "KY", "comb_old", "comb_new")],
      row.names = FALSE)

# Inter-coder reliability ======================================================
cohen_kappa <- function(a, b) {
  po <- mean(a == b)
  pe <- mean(a) * mean(b) + (1 - mean(a)) * (1 - mean(b))
  (po - pe) / (1 - pe)
}
## Fleiss' kappa for m raters and a binary code; n_pos = raters coding 1
fleiss_kappa <- function(n_pos, m = 3) {
  p_item <- (n_pos * (n_pos - 1) + (m - n_pos) * (m - n_pos - 1)) /
    (m * (m - 1))
  p1 <- sum(n_pos) / (length(n_pos) * m)
  pe <- p1^2 + (1 - p1)^2
  (mean(p_item) - pe) / (1 - pe)
}

rel <- do.call(rbind, lapply(1:4, function(d) {
  v <- votes[, d, ]
  data.frame(
    dimension = dims[d],
    share_coded_1 = mean(maj[, d]),
    kappa_SK_BK = cohen_kappa(v[, "SK"], v[, "BK"]),
    kappa_SK_KY = cohen_kappa(v[, "SK"], v[, "KY"]),
    kappa_BK_KY = cohen_kappa(v[, "BK"], v[, "KY"]),
    fleiss_kappa = fleiss_kappa(n_yes[, d]),
    share_unanimous = mean(unanimous[, d])
  )
}))
rel$mean_pairwise_kappa <- rowMeans(
  rel[, c("kappa_SK_BK", "kappa_SK_KY", "kappa_BK_KY")]
)
## all four dimensions pooled (260 petition-dimension codes)
pool <- function(cc) as.vector(votes[, , cc])
rel <- rbind(
  rel,
  data.frame(
    dimension = "pooled (65 x 4)",
    share_coded_1 = mean(maj),
    kappa_SK_BK = cohen_kappa(pool("SK"), pool("BK")),
    kappa_SK_KY = cohen_kappa(pool("SK"), pool("KY")),
    kappa_BK_KY = cohen_kappa(pool("BK"), pool("KY")),
    fleiss_kappa = fleiss_kappa(as.vector(n_yes)),
    share_unanimous = mean(unanimous),
    mean_pairwise_kappa = NA
  )
)
rel$mean_pairwise_kappa[5] <- mean(unlist(rel[5, 3:5]))
write.csv(rel, "output/v2/intercoder_reliability.csv", row.names = FALSE)

dim_labels <- c(
  clarity = "Clarity/specificity", logic = "Logic/consistency",
  manner = "Tone/manner", validity = "Validity/feasibility",
  "pooled (65 x 4)" = "All four dimensions pooled"
)
dir.create("output/v2/tab", showWarnings = FALSE)
writeLines(
  c(
    "% generated by R/22_v2_expert_codes.R",
    "\\begin{tabular}{lcccccc}",
    "  \\toprule",
    paste0("  & & \\multicolumn{3}{c}{Cohen's $\\kappa$, coder pairs}",
           " & & \\\\"),
    "  \\cmidrule(lr){3-5}",
    paste0("  Dimension & Coded 1 & A--B & A--C & B--C & Fleiss's $\\kappa$",
           " & Unanimous \\\\"),
    "  \\midrule",
    sprintf(
      "  %s & %.0f\\%% & %.2f & %.2f & %.2f & %.2f & %.0f\\%% \\\\",
      dim_labels[rel$dimension], 100 * rel$share_coded_1, rel$kappa_SK_BK,
      rel$kappa_SK_KY, rel$kappa_BK_KY, rel$fleiss_kappa,
      100 * rel$share_unanimous
    )[c(1:4)],
    "  \\midrule",
    sprintf(
      "  %s & %.0f\\%% & %.2f & %.2f & %.2f & %.2f & %.0f\\%% \\\\",
      dim_labels[rel$dimension], 100 * rel$share_coded_1, rel$kappa_SK_BK,
      rel$kappa_SK_KY, rel$kappa_BK_KY, rel$fleiss_kappa,
      100 * rel$share_unanimous
    )[5],
    "  \\bottomrule",
    "\\end{tabular}"
  ),
  "output/v2/tab/intercoder_reliability.tex"
)

cat("\nInter-coder reliability (three authors, n = 65):\n")
print(format(rel, digits = 2), row.names = FALSE)

# Anchor candidates ============================================================
## The submitted model fixes petitions 1 and 15 at (2, 2) and (-2, -2) and
## restricts a third, coded 0101 (presentation-weak, substance-strong), to the
## upper-left quadrant. The rule is kept; the petitions it selects are listed.
anchors <- codes[
  codes$id %in% c(1, 15, 49) | codes$comb_new == "0101",
  c("id", "category", "SK", "BK", "KY", "comb_old", "comb_new")
]
anchors$role <- ifelse(
  anchors$id == 1, "fixed (2, 2)",
  ifelse(
    anchors$id == 15, "fixed (-2, -2)",
    ifelse(
      anchors$comb_new == "0101",
      "eligible third anchor (coded 0101)",
      "third anchor in submitted model; no longer 0101"
    )
  )
)
write.csv(anchors, "output/v2/anchor_candidates.csv", row.names = FALSE)

cat("\nAnchors under the new codes:\n")
print(anchors[, c("id", "SK", "BK", "KY", "comb_old", "comb_new", "role")],
      row.names = FALSE)
