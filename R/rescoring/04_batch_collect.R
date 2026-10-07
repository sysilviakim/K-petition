source(here::here("R", "rescoring", "rescoring_utils.R"))

# Wait for the batches of a run to end and save the provider results as returned
#   Rscript R/rescoring/04_batch_collect.R full_astra_low
#   Rscript R/rescoring/04_batch_collect.R full_fable_low
# - OpenAI: raw/batch_output_<batch id>.jsonl (and raw/batch_errors_<batch id>.jsonl)
# - Anthropic: raw/<pid>.json, the message object of each succeeded request
# Requests that errored or whose text does not parse into four scores are written
# to failed_pids.json, to be re-sent with 03_batch_submit.R <run> --resend.

args <- commandArgs(trailingOnly = TRUE)
run_name <- args[1]
run <- runs[[run_name]]
stopifnot(!is.null(run))
run_path <- file.path(runs_dir, run_name)
raw_path <- file.path(run_path, "raw")
dir.create(raw_path, showWarnings = FALSE)
poll_seconds <- 300

batches <- read_batches(run_name)
failed <- integer()

for (i in seq_along(batches)) {
  if (isTRUE(batches[[i]]$collected)) next
  id <- batches[[i]]$id

  # OpenAI =====================================================================
  if (run$provider == "openai") {
    repeat {
      b <- GET(paste0("https://api.openai.com/v1/batches/", id), api_headers("openai")) %>%
        api_content()
      message(id, " ", b$status, " completed ", b$request_counts$completed,
              " of ", b$request_counts$total)
      if (b$status %in% c("completed", "failed", "expired", "cancelled")) break
      Sys.sleep(poll_seconds)
    }
    download <- function(file_id, name) {
      GET(paste0("https://api.openai.com/v1/files/", file_id, "/content"),
          api_headers("openai"), write_disk(file.path(raw_path, name), overwrite = TRUE))
    }
    if (!is.null(b$error_file_id)) {
      download(b$error_file_id, paste0("batch_errors_", id, ".jsonl"))
      failed <- c(failed, read_lines(file.path(raw_path, paste0("batch_errors_", id, ".jsonl"))) %>%
        map_chr(~ fromJSON(.x)$custom_id) %>%
        pid_of())
    }
    if (!is.null(b$output_file_id)) {
      download(b$output_file_id, paste0("batch_output_", id, ".jsonl"))
      for (line in read_lines(file.path(raw_path, paste0("batch_output_", id, ".jsonl")))) {
        obj <- fromJSON(line, simplifyVector = FALSE)
        ok <- identical(obj$response$status_code, 200L) &&
          !is.na(parse_scores(openai_text(obj$response$body))[[1]])
        if (!ok) failed <- c(failed, pid_of(obj$custom_id))
      }
    }
  }

  # Anthropic ==================================================================
  if (run$provider == "anthropic") {
    repeat {
      b <- GET(paste0("https://api.anthropic.com/v1/messages/batches/", id),
               api_headers("anthropic")) %>%
        api_content()
      message(id, " ", b$processing_status, " succeeded ", b$request_counts$succeeded,
              " errored ", b$request_counts$errored)
      if (b$processing_status == "ended") break
      Sys.sleep(poll_seconds)
    }
    results <- GET(b$results_url, api_headers("anthropic")) %>%
      content(as = "text", encoding = "UTF-8") %>%
      read_lines()
    for (line in results) {
      obj <- fromJSON(line, simplifyVector = FALSE)
      pid <- pid_of(obj$custom_id)
      if (obj$result$type == "succeeded") {
        msg <- obj$result$message
        write_json(msg, file.path(raw_path, paste0(pid, ".json")),
                   auto_unbox = TRUE, null = "null", pretty = TRUE)
        if (is.na(parse_scores(anthropic_text(msg))[[1]])) failed <- c(failed, pid)
      } else {
        failed <- c(failed, pid)
      }
    }
  }

  batches[[i]]$collected <- TRUE
}

write_batches(run_name, batches)
write_json(sort(unique(failed)), file.path(run_path, "failed_pids.json"))
message(length(unique(failed)), " requests to re-send")
