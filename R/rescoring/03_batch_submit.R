source(here::here("R", "rescoring", "rescoring_utils.R"))

# Submit the request files from 02_batch_requests.R as provider batch jobs
#   Rscript R/rescoring/03_batch_submit.R full_astra_low
#   Rscript R/rescoring/03_batch_submit.R full_fable_low
# Add --resend to submit only the requests listed in failed_pids.json by
# 04_batch_collect.R (errored or unparsable results). The batch ids are appended
# to data/llm/runs/<run>/batches.json.

args <- commandArgs(trailingOnly = TRUE)
run_name <- args[1]
resend <- "--resend" %in% args
run <- runs[[run_name]]
stopifnot(!is.null(run))
run_path <- file.path(runs_dir, run_name)

resend_ids <- if (resend) {
  paste0("pid-", unlist(read_json(file.path(run_path, "failed_pids.json"))))
} else {
  NULL
}

# OpenAI: upload the JSONL, then create a batch on /v1/responses ===============
if (run$provider == "openai") {
  jsonl <- file.path(run_path, "batch_input.jsonl")
  if (resend) {
    lines <- read_lines(jsonl)
    keep_line <- map_chr(lines, ~ fromJSON(.x)$custom_id) %in% resend_ids
    jsonl <- file.path(run_path, "batch_input_resend.jsonl")
    write_lines(lines[keep_line], jsonl)
  }
  n <- length(read_lines(jsonl))

  file_id <- POST(
    "https://api.openai.com/v1/files", api_headers("openai"),
    body = list(purpose = "batch", file = upload_file(jsonl)),
    encode = "multipart"
  ) %>%
    api_content() %>%
    pluck("id")

  batch <- POST(
    "https://api.openai.com/v1/batches", api_headers("openai"),
    body = list(
      input_file_id = file_id, endpoint = "/v1/responses",
      completion_window = "24h", metadata = list(run = run_name)
    ),
    encode = "json"
  ) %>%
    api_content()
  new_batch <- list(id = batch$id, input_file_id = file_id, n = n)
}

# Anthropic: one Message Batches request with every params object ==============
if (run$provider == "anthropic") {
  requests_file <- file.path(run_path, "batch_requests.json")
  if (resend) {
    requests <- read_json(requests_file) %>%
      keep(~ .x$custom_id %in% resend_ids)
    payload <- to_json(list(requests = requests))
    n <- length(requests)
  } else {
    payload <- paste0('{"requests":', read_file(requests_file), "}")
    n <- length(read_json(requests_file))
  }

  batch <- POST(
    "https://api.anthropic.com/v1/messages/batches", api_headers("anthropic"),
    content_type_json(),
    body = payload, encode = "raw"
  ) %>%
    api_content()
  new_batch <- list(id = batch$id, n = n)
}

new_batch$created_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
new_batch$collected <- FALSE
write_batches(run_name, c(read_batches(run_name), list(new_batch)))
message("submitted ", new_batch$id, " (", new_batch$n, " requests)")
