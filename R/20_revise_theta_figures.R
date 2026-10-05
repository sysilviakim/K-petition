# ==============================================================================
# Generates ALL latent-position (theta) figures used in the revised manuscript
# (paper/quality_revised.tex), and only those. One output file per figure:
#
#   theta_post_median_rev.pdf          Figure 5 (fig:theta_post_median)
#                                      main latent map, numbered points
#   theta_post_median_clarity_rev.pdf  Figure 6, panel (a) (fig:theta_quality)
#   theta_post_median_manner_rev.pdf   Figure 6, panel (b)
#   theta_post_median_logic_rev.pdf    Figure 6, panel (c)
#   theta_post_median_validity_rev.pdf Figure 6, panel (d)
#   theta_topic_combined_rev.pdf       Figure 7 (fig:theta_category)
#                                      topic map (a) + by-topic strips (b)
#   theta_dp_combined_rev.pdf          Figure 8 (fig:theta_post_dp)
#                                      DP map (a) + agreement scatter (b)
#   theta_subgroup_agreement_rev.pdf   Figure 9 (fig:theta_subgroups)
#                                      5x2 subgroup-vs-full-sample grid
#
# Inputs and how to recreate them:
#   data/mcmc_out.rds                  2D pairwise IRT, full sample
#                                      (R/10_survey_pairwise_irt.R)
#   data/mcmcDP_out.rds                Dirichlet Process variant, full sample
#                                      (R/10_survey_pairwise_irt.R)
#   data/tidy/admin_outcomes_65.csv    per-petition expert codes and topics
#                                      (R/18_survey_admin_outcomes_build.py)
#   output/subgroup_rerun/subgroup_thetas.csv
#                                      per-subgroup re-estimated positions
#                                      (R/21_subgroup_reruns.R; ~5 min)
#
# Notes:
#   - Style follows theta_post_viz() in R/utilities.R: posterior-median
#     positions, axes fixed at [-3, 3], red arrows for the respondents with
#     the minimum, median, and maximum posterior-median gamma, viridis colors.
#   - The DP fit converged to the dimension-exchanged (label-switched) mode;
#     this script relabels it, mirroring the footnote in the manuscript. Two
#     subgroup fits were likewise relabeled inside R/21_subgroup_reruns.R.
#   - Self-contained on purpose: base R + ggplot2/dplyr/tidyr/ggpubr from the
#     system library, so it runs while the project renv library is broken.
#
# Run from the analysis-project root:
#   PAPER_FIG=/path/to/paper/fig Rscript R/20_regen_theta_figures.R
# Without PAPER_FIG set, figures are written to ./fig_out/ for copying.
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

PAPER_FIG <- Sys.getenv("PAPER_FIG", unset = "fig_out")
dir.create(PAPER_FIG, showWarnings = FALSE)

# ---- shared helpers ----------------------------------------------------------

# Posterior median position of each petition on the two dimensions.
theta_medians <- function(m) {
  tc <- grep("^theta[12]\\.item", colnames(m), value = TRUE)
  med <- apply(m[, tc], 2, median)
  parts <- do.call(rbind, strsplit(names(med), ".", fixed = TRUE))
  out <- data.frame(
    item = as.integer(parts[, 3]),
    dim = parts[, 1],
    median = as.numeric(med)
  )
  w <- reshape(out, idvar = "item", timevar = "dim", direction = "wide")
  names(w) <- c("item", "theta1_median", "theta2_median")
  w[order(w$item), ]
}

# The three red arrows: direction vectors (cos g, sin g) for the respondents
# with the minimum, median, and maximum posterior-median gamma.
gamma_arrows <- function(m) {
  gc <- grep("^gamma", colnames(m), value = TRUE)
  med <- apply(m[, gc], 2, median)
  data.frame(median = c(min(med), median(med), max(med)))
}

# One theta scatter in the style of theta_post_viz().
base_plot <- function(th, ar, label = FALSE, color = NULL,
                      xlab = expression(theta[1]), ylab = expression(theta[2])) {
  if (is.null(color)) {
    p <- ggplot(th, aes(x = theta1_median, y = theta2_median))
  } else {
    p <- ggplot(
      th,
      aes(x = theta1_median, y = theta2_median, color = !!sym(color))
    )
  }
  if (isTRUE(label)) {
    p <- p +
      geom_text(aes(label = item), hjust = 0, vjust = 0, color = "black")
  }
  p +
    geom_point() +
    coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3)) +
    xlab(xlab) +
    ylab(ylab) +
    theme_minimal() +
    geom_segment(
      data = ar,
      aes(x = 0, y = 0, xend = cos(median), yend = sin(median)),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.1, "inches")),
      color = "red"
    ) +
    scale_color_viridis_d(end = .85) +
    theme(legend.position = "bottom", legend.box = "vertical")
}

save_fig <- function(p, fname, width = 7, height = 7) {
  pdf(file.path(PAPER_FIG, fname), width = width, height = height)
  print(p)
  dev.off()
  cat("wrote", fname, "\n")
}

# ---- per-petition attributes (expert codes, topic in English) ----------------
kor <- c("배달", "부동산", "사교육",
         "연금", "저출산", "킥보드")
eng <- c("Delivery", "Real estate", "Private education",
         "Pension", "Low birth rate", "E-scooter")
attrs <- read.csv("data/tidy/admin_outcomes_65.csv") |>
  mutate(
    category = factor(eng[match(category, kor)]),
    across(c(clarity, logic, manner, validity), factor)
  ) |>
  select(item, category, clarity, logic, manner, validity)

# ---- full-sample model: load once, used by Figures below ---------------------
m <- as.matrix(readRDS("data/mcmc_out.rds"))
th <- theta_medians(m) |> left_join(attrs, by = "item")
ar <- gamma_arrows(m)
stopifnot(nrow(th) == 65)

# ---- Figure "fig:theta_post_median": main latent map -------------------------
save_fig(base_plot(th, ar, label = TRUE), "theta_post_median_rev.pdf")

# ---- Figure "fig:theta_quality" panels (a)-(d): map colored by expert code ---
dim_labels <- c(
  clarity = "Clarity and Specificity",
  logic = "Logic and Consistency",
  manner = "Tone and Manner",
  validity = "Validity and Feasibility"
)
for (d in names(dim_labels)) {
  p <- base_plot(th, ar, color = d) +
    guides(color = guide_legend(title = dim_labels[[d]]))
  save_fig(p, paste0("theta_post_median_", d, "_rev.pdf"), 5, 5)
}

# ---- Figure "fig:theta_category": map + by-topic strips (panels a/b) --------
p_cat <- base_plot(
  th, ar,
  color = "category",
  xlab = expression(theta[1]),
  ylab = expression(theta[2])
) +
  guides(color = guide_legend(title = "Topic Category")) +
  theme(legend.position = "right")

long <- th |>
  pivot_longer(c(theta1_median, theta2_median), names_to = "dim",
               values_to = "pos") |>
  mutate(dim = factor(
    dim, levels = c("theta1_median", "theta2_median"),
    labels = c("theta[1]", "theta[2]")
  ))
topic_order <- th |>
  group_by(category) |>
  summarise(mu = mean(theta2_median)) |>
  arrange(mu) |>
  pull(category)
long$cat_color <- long$category
long$category <- factor(long$category, levels = topic_order)
means <- long |>
  group_by(dim, category) |>
  summarise(mu = mean(pos), .groups = "drop")
# One-way ANOVA of position on topic, per dimension (as in script 11).
f_label <- function(pos) {
  a <- summary(aov(pos ~ th$category))[[1]]
  sprintf(
    "list(F(%d, %d) == %.2f, p == %s)", a$Df[1], a$Df[2], a$`F value`[1],
    sub("^0", "", sprintf("%.3f", a$`Pr(>F)`[1]))
  )
}
ftests <- data.frame(
  dim = factor(levels(long$dim), levels = levels(long$dim)),
  lab = c(f_label(th$theta1_median), f_label(th$theta2_median))
)
set.seed(1)
p_strips <- ggplot(long, aes(x = pos, y = category, color = cat_color)) +
  geom_jitter(height = 0.12, size = 1.5, alpha = 0.75) +
  geom_point(data = means, aes(x = mu, y = category),
             shape = 18, size = 3.2, color = "red") +
  geom_text(data = ftests, aes(x = 3.1, y = Inf, label = lab),
            parse = TRUE, hjust = 1, vjust = 1.6, size = 3.1,
            inherit.aes = FALSE) +
  facet_wrap(~dim, labeller = label_parsed) +
  scale_color_viridis_d(end = .85, guide = "none") +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.3))) +
  labs(x = "Estimated position", y = NULL) +
  theme_minimal() +
  theme(panel.spacing = unit(1.2, "lines"))
fig_topic <- ggarrange(p_cat, p_strips, ncol = 1, heights = c(1.5, 1),
                       labels = c("(a)", "(b)"),
                       font.label = list(size = 12, face = "plain"))
pdf(file.path(PAPER_FIG, "theta_topic_combined_rev.pdf"),
    width = 7.5, height = 8.5)
print(fig_topic)
dev.off()
cat("wrote theta_topic_combined_rev.pdf\n")

# ---- Figure "fig:theta_post_dp": DP map + agreement scatter (panels a/b) -----
mdp <- as.matrix(readRDS("data/mcmcDP_out.rds"))
thdp_raw <- theta_medians(mdp)
# The DP fit converged to the dimension-exchanged mode (the corner anchors
# cannot rule it out); relabel, verified against the full-sample fit.
thdp <- data.frame(item = thdp_raw$item,
                   theta1_median = thdp_raw$theta2_median,
                   theta2_median = thdp_raw$theta1_median)
gdp <- grep("^gamma", colnames(mdp), value = TRUE)
gmed <- apply(mdp[, gdp], 2, median)
ardp <- data.frame(median = pi / 2 - c(min(gmed), median(gmed), max(gmed)))
p_dp_map <- ggplot(thdp, aes(theta1_median, theta2_median)) +
  geom_text(aes(label = item), hjust = 0, vjust = 0, color = "black",
            size = 3.2) +
  geom_point(size = 1.2) +
  geom_segment(
    data = ardp,
    aes(x = 0, y = 0, xend = cos(median), yend = sin(median)),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.1, "inches")), color = "red"
  ) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3)) +
  xlab(expression(theta[1])) +
  ylab(expression(theta[2])) +
  theme_minimal()
ag <- th |>
  select(item, theta1_median, theta2_median) |>
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
    rho = cor(main[!item %in% c(1, 15)], dp[!item %in% c(1, 15)],
              method = "spearman"),
    .groups = "drop"
  ) |>
  mutate(lab = sprintf("rho == %.2f", rho))
p_dp_ag <- ggplot(ag, aes(main, dp)) +
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
fig_dp <- ggarrange(p_dp_map, p_dp_ag, ncol = 1, heights = c(1.5, 1),
                    labels = c("(a)", "(b)"),
                    font.label = list(size = 12, face = "plain"))
pdf(file.path(PAPER_FIG, "theta_dp_combined_rev.pdf"),
    width = 7.5, height = 9)
print(fig_dp)
dev.off()
cat("wrote theta_dp_combined_rev.pdf\n")

# ---- Figure "fig:theta_subgroups": subgroup vs full-sample grid (5x2) --------
sub <- read.csv("output/subgroup_rerun/subgroup_thetas.csv")
ns <- c(ppp = 256, opp = 681, pop = 264, inc_below = 642, inc_above = 580)
sub_labels <- c(
  ppp = "PPP supporters", opp = "Opposition supporters",
  pop = "Populist attitudes", inc_below = "Below-median income",
  inc_above = "Above-median income"
)
groups <- c("ppp", "opp", "pop", "inc_below", "inc_above")
main_th <- th |> select(item, t1 = theta1_median, t2 = theta2_median)
dsub <- sub |>
  left_join(main_th, by = "item", suffix = c("_sub", "_main")) |>
  pivot_longer(c(t1_sub, t2_sub, t1_main, t2_main),
               names_to = c("dim", "src"), names_sep = "_",
               values_to = "pos") |>
  pivot_wider(names_from = src, values_from = pos) |>
  mutate(
    dim = factor(dim, levels = c("t1", "t2"),
                 labels = c("theta[1]", "theta[2]")),
    glab = factor(
      sprintf("%s~(n == %d)", gsub(" ", "~", sub_labels[subgroup]),
              ns[subgroup]),
      levels = sprintf("%s~(n == %d)", gsub(" ", "~", sub_labels[groups]),
                       ns[groups])
    )
  )
cors_sub <- dsub |>
  group_by(glab, dim) |>
  summarise(
    rho = cor(main[!item %in% c(1, 15)], sub[!item %in% c(1, 15)],
              method = "spearman"),
    .groups = "drop"
  ) |>
  mutate(lab = sprintf("rho == %.2f", rho))
p_sub <- ggplot(dsub, aes(main, sub)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey55") +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_text(data = cors_sub, aes(x = -2.4, y = 2.1, label = lab),
            parse = TRUE, hjust = 0, size = 3.3) +
  facet_grid(glab ~ dim, labeller = label_parsed) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3)) +
  labs(x = "Estimated position, full sample",
       y = "Estimated position, subgroup") +
  theme_minimal() +
  theme(panel.spacing = unit(0.9, "lines"))
pdf(file.path(PAPER_FIG, "theta_subgroup_agreement_rev.pdf"),
    width = 6.2, height = 13)
print(p_sub)
dev.off()
cat("wrote theta_subgroup_agreement_rev.pdf\n")
