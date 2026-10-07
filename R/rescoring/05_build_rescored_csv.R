source(here::here("R", "rescoring", "rescoring_utils.R"))

# Scored file: the frame, the 2025 gpt-4o-mini scores (joined by pid), and each
# run's four scores and reasons. Results are read in batch order, so a re-sent
# request replaces the earlier result for the same pid. Requests that end in a
# refusal or never return four valid scores are left missing.

realigned <- read_csv(realigned_csv, col_types = cols(.default = col_character())) %>%
  mutate(pid = as.integer(pid))

## OpenAI: one row per pid from the batch output JSONL files -------------------
read_openai_run <- function(run_name) {
  ids <- map_chr(read_batches(run_name), "id")
  files <- file.path(runs_dir, run_name, "raw", paste0("batch_output_", ids, ".jsonl"))
  map_dfr(files[file.exists(files)], function(f) {
    lines <- read_lines(f)
    lines <- lines[str_detect(lines, '"custom_id": ?"pid-[0-9]+"')]
    map_dfr(lines, function(line) {
      obj <- fromJSON(line, simplifyVector = FALSE)
      body <- obj$response$body
      ok <- identical(obj$response$status_code, 200L)
      as_tibble(c(
        list(pid = pid_of(obj$custom_id), model_returned = if (ok) body$model else NA_character_),
        parse_scores(if (ok) openai_text(body) else NULL)
      ))
    })
  }) %>%
    slice_tail(n = 1, by = pid)
}

## Anthropic: one message object per pid in raw/<pid>.json ---------------------
read_anthropic_run <- function(run_name, pids) {
  map_dfr(pids, function(p) {
    f <- file.path(runs_dir, run_name, "raw", paste0(p, ".json"))
    if (!file.exists(f)) {
      return(tibble(pid = p))
    }
    msg <- read_json(f)
    as_tibble(c(
      list(pid = p, model_returned = msg$model),
      parse_scores(anthropic_text(msg))
    ))
  })
}

# Assemble =====================================================================
rescored <- realigned %>%
  select(-review_content, -implementation_result) %>%
  rename_with(
    ~ paste0("gpt4omini_realigned_", .x),
    c(ends_with("_score"), ends_with("_reason"))
  )

for (run_name in names(runs)) {
  run <- runs[[run_name]]
  scores <- if (run$provider == "openai") {
    read_openai_run(run_name)
  } else {
    read_anthropic_run(run_name, rescored$pid)
  }
  scores <- scores %>%
    select(
      pid, as.vector(rbind(paste0(dims, "_score"), paste0(dims, "_reason"))),
      model_returned
    ) %>%
    rename_with(~ paste0(run$prefix, "_", .x), -pid)
  rescored <- left_join(rescored, scores, by = "pid")
}

stopifnot(nrow(rescored) == 6726, !anyDuplicated(rescored$pid))
write_excel_csv(rescored, rescored_csv, na = "")
