# ==============================================================================
# Two figures revised after the R&R response letter was drafted (Reviewer 2,
# Comment R2.15), replacing the combined versions from script 20:
#
#   theta_dp_agreement_rev.pdf          Figure 8 (fig:theta_post_dp)
#                                       panel (b) of theta_dp_combined_rev.pdf
#                                       on its own: DP vs main-model positions
#   theta_subgroup_agreement_wide_rev.pdf
#                                       Figure 10 (fig:theta_subgroups)
#                                       the 5x2 subgroup grid of
#                                       theta_subgroup_agreement_rev.pdf in
#                                       landscape (dimensions in rows, subgroups
#                                       in columns), with plain-language
#                                       dimension labels and a reading line
#
# Inputs: the v2 fit with petition 59 as the third anchor (R/23_v2_irt_fits.R),
# which is the fit behind the revised manuscript:
#   output/v2/fits/a59/mcmc_out.rds         full-sample 2D pairwise IRT
#   output/v2/fits/a59/mcmcDP_out.rds       Dirichlet Process variant
#   output/v2/fits/a59/subgroup_thetas.csv  per-subgroup re-estimated positions
#
# Orientation: the a59 fits converged to the dimension-exchanged mode relative
# to the submitted manuscript (tone dimension second). run_v2.R relabels
# mcmc_out.rds and subgroup_thetas.csv before script 20 reads them, and script
# 20 relabels the DP fit itself. This script applies the same relabeling, so
# it runs directly from the project root without the run_v2.R sandbox:
#   PAPER_FIG=/path/to/paper/fig_v2 Rscript R/28_v2_revised_figures.R
# Without PAPER_FIG set, figures are written to ./fig_out/.
#
# Check: the printed rank correlations should be 0.93 / 0.95 (Figure 8) and
# 0.79 0.92 0.86 0.97 0.91 on theta1, 0.71 0.83 0.70 0.94 0.82 on theta2
# (Figure 10), as reported in the manuscript.
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

PAPER_FIG <- Sys.getenv("PAPER_FIG", unset = "fig_out")
dir.create(PAPER_FIG, showWarnings = FALSE)
FITS <- "output/v2/fits/a59"
ANCHORS <- c(1, 15) # excluded from the correlations, as in script 20

# ---- helpers (as in script 20) ----------------------------------------------
theta_medians <- function(m) {
  tc <- grep("^theta[12]\\.item", colnames(m), value = TRUE)
  med <- apply(m[, tc], 2, median)
  parts <- do.call(rbind, strsplit(names(med), ".", fixed = TRUE))
  out <- data.frame(
    item = as.integer(parts[, 3]), dim = parts[, 1], median = as.numeric(med)
  )
  w <- reshape(out, idvar = "item", timevar = "dim", direction = "wide")
  names(w) <- c("item", "theta1_median", "theta2_median")
  w[order(w$item), ]
}
exchange <- function(th) {
  data.frame(
    item = th$item,
    theta1_median = th$theta2_median, theta2_median = th$theta1_median
  )
}
save_fig <- function(p, fname, width, height) {
  pdf(file.path(PAPER_FIG, fname), width = width, height = height)
  print(p)
  dev.off()
  cat("wrote", fname, "\n")
}

# ---- inputs, relabeled to the manuscript's orientation ----------------------
m <- as.matrix(readRDS(file.path(FITS, "mcmc_out.rds")))
th <- exchange(theta_medians(m))
rm(m)
mdp <- as.matrix(readRDS(file.path(FITS, "mcmcDP_out.rds")))
thdp <- exchange(theta_medians(mdp))
rm(mdp)
invisible(gc())
stopifnot(nrow(th) == 65, nrow(thdp) == 65)

# ---- Figure 8: DP vs main-model positions (former panel b) ------------------
ag <- th |>
  left_join(thdp, by = "item", suffix = c("_main", "_dp")) |>
  pivot_longer(-item, names_to = c("dim", "junk", "model"), names_sep = "_",
               values_to = "pos") |>
  select(-junk) |>
  pivot_wider(names_from = model, values_from = pos) |>
  mutate(dim = factor(dim, levels = c("theta1", "theta2"),
                      labels = c("theta[1]", "theta[2]")))
cors_dp <- ag |>
  group_by(dim) |>
  summarise(
    rho = cor(main[!item %in% ANCHORS], dp[!item %in% ANCHORS],
              method = "spearman"),
    .groups = "drop"
  ) |>
  mutate(lab = sprintf("rho == %.2f", rho))
cat("Figure 8, rank correlations:\n")
print(as.data.frame(cors_dp[, c("dim", "rho")]))

p_dp <- ggplot(ag, aes(main, dp)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey55") +
  geom_point(size = 1.5, alpha = 0.75) +
  geom_text(data = cors_dp, aes(x = -2.2, y = 2.4, label = lab),
            parse = TRUE, hjust = 0, size = 3.8) +
  facet_wrap(~dim, labeller = label_parsed) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3)) +
  labs(x = "Estimated position, main model",
       y = "Estimated position, DP model") +
  theme_minimal() +
  theme(panel.spacing = unit(1.2, "lines"))
save_fig(p_dp, "theta_dp_agreement_rev.pdf", 7.5, 3.9)

# ---- Figure 10: subgroup vs full-sample positions, landscape ----------------
sub <- read.csv(file.path(FITS, "subgroup_thetas.csv")) |>
  rename(t1 = t2, t2 = t1) # same relabeling as the full-sample fit
ns <- c(ppp = 256, opp = 681, pop = 264, inc_below = 642, inc_above = 580)
sub_labels <- c(
  ppp = "PPP supporters", opp = "Opposition supporters",
  pop = "Populist attitudes", inc_below = "Below-median income",
  inc_above = "Above-median income"
)
groups <- names(sub_labels)
glab_levels <- sprintf("%s\n(n = %d)", sub_labels[groups], ns[groups])
dim_labels <- c("Dimension~1~(presentation)", "Dimension~2")

dsub <- sub |>
  left_join(th |> select(item, t1 = theta1_median, t2 = theta2_median),
            by = "item", suffix = c("_sub", "_main")) |>
  pivot_longer(c(t1_sub, t2_sub, t1_main, t2_main),
               names_to = c("dim", "src"), names_sep = "_",
               values_to = "pos") |>
  pivot_wider(names_from = src, values_from = pos) |>
  mutate(
    dim = factor(dim, levels = c("t1", "t2"), labels = dim_labels),
    glab = factor(sprintf("%s\n(n = %d)", sub_labels[subgroup], ns[subgroup]),
                  levels = glab_levels)
  )
cors_sub <- dsub |>
  group_by(glab, dim) |>
  summarise(
    rho = cor(main[!item %in% ANCHORS], sub[!item %in% ANCHORS],
              method = "spearman"),
    .groups = "drop"
  ) |>
  mutate(lab = sprintf("rho == %.2f", rho))
cat("Figure 10, rank correlations:\n")
print(as.data.frame(cors_sub[, c("glab", "dim", "rho")]))

p_sub <- ggplot(dsub, aes(main, sub)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey55") +
  geom_point(size = 1.3, alpha = 0.7) +
  geom_text(data = cors_sub, aes(x = -2.7, y = 2.5, label = lab),
            parse = TRUE, hjust = 0, size = 3.2) +
  facet_grid(dim ~ glab, labeller = labeller(dim = label_parsed)) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3)) +
  scale_x_continuous(breaks = c(-2, 0, 2)) +
  scale_y_continuous(breaks = c(-2, 0, 2)) +
  labs(
    x = "Position estimated from the full sample",
    y = "Position estimated from\nthe subgroup's choices only",
    caption = paste(
      "Each dot is one petition.",
      "Dots on the dashed line mean the subgroup places the petition",
      "where the full sample does."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    panel.spacing = unit(0.8, "lines"),
    strip.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0, color = "grey30")
  )
save_fig(p_sub, "theta_subgroup_agreement_wide_rev.pdf", 11, 5.4)
