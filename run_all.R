run_script <- function(script_name) {
  cat(sprintf("\n[START] %s\n", script_name))
  source(here::here("R", script_name))
  cat(sprintf("[DONE ] %s\n", script_name))
}

# run_script("01_epeople_petitions_public_scrape.R")
# run_script("02_epeople_parse_html.R")
run_script("03_epeople_wrangle_eda.R")
run_script("04_epeople_KoNLP_preprocess.R")
run_script("05_epeople_word_frequency.R")
run_script("06_epeople_predict_delay.R")
tryCatch(
  run_script("07_epeople_topic_model.R"),
  error = function(e) cat("[SKIP ] 07_epeople_topic_model.R:", e$message, "\n")
)
run_script("08_survey_wrangling.R")
run_script("09_survey_descriptives.R")
run_script("10_survey_pairwise_irt.R")
run_script("11_survey_gamma_category.R")
run_script("12_survey_accuracy_validation.R")
run_script("13_survey_pairwise_subgroups.R")
run_script("14_survey_prediction_pairwise.R")
run_script("15_survey_robustness.R")
run_script("16_survey_freeform.R")
run_script("17_survey_political_economy.R")
