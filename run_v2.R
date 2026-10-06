# PSJ R&R re-analysis (v2): the complete three-author petition codes, the
# re-anchored IRT, and the analyses added in response to the reviewers.
# Produces every result reported in v3 of the manuscript, the SI, and the
# response letter. Run from the project root under a UTF-8 locale:
#   LC_ALL=en_US.UTF-8 Rscript --vanilla run_v2.R
# Fits already stored in output/v2/fits/ are reused. From scratch, the
# Dirichlet Process fits in step 23 take most of an hour; everything else
# takes a few minutes. Overview of the results: output/v2/comparison.md
Sys.setenv(V2_DP_SUBGROUPS = "1")  # also fit the DP subgroup models (SI)
steps <- c(
  "22_v2_expert_codes.R",            # codes from K-petition_new_scoring.xlsx,
                                     # inter-coder reliability
  "23_v2_irt_fits.R",                # IRT under alternative third anchors
  "24_v2_rerun_and_compare.R",       # scripts 10-20 under each scenario, and
                                     # comparison with the submitted results
  "25_v2_sensitivity_and_reasons.R", # sample-composition sensitivity, stated
                                     # reasons (Q14)
  "26_v2_corpus_build.py",           # 2024 corpus with LLM scores, short tail
                                     # of the frame
  "27_v2_admin_outcomes.R"           # administrative outcomes
)
for (s in steps) {
  cat(sprintf("\n[START] %s\n", s))
  status <- if (grepl("\\.py$", s)) {
    system2("python3", file.path("R", s))
  } else {
    system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", file.path("R", s)))
  }
  if (status != 0) stop(s, " failed")
  cat(sprintf("[DONE ] %s\n", s))
}
