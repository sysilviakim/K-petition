source(here::here("R", "rescoring", "rescoring_utils.R"))

# Batch request files, one request per proposal (custom_id = "pid-<pid>")
# - OpenAI: batch input JSONL for the Responses API (system message as instructions)
# - Anthropic: requests array for the Message Batches API
# Both carry the same prompt, system message, and JSON schema, and no sampling
# parameters. The original batches also held 200 repeated requests for a
# test-retest check. Those repeats do not enter the scored file and are left out here.

prompt <- load_prompt()

frame <- read_csv(frame_csv, col_types = cols(.default = col_character())) %>%
  mutate(
    pid = as.integer(pid),
    body = build_body(current_issues, improvement_plan, expected_effect)
  )
stopifnot(all(as.character(sha1(frame$body)) == frame$body_sha1))

for (run_name in names(runs)) {
  run <- runs[[run_name]]
  dir.create(file.path(runs_dir, run_name, "raw"), recursive = TRUE, showWarnings = FALSE)
  custom_id <- paste0("pid-", frame$pid)

  if (run$provider == "openai") {
    lines <- map2_chr(custom_id, frame$body, function(id, body) {
      to_json(list(
        custom_id = id, method = "POST", url = "/v1/responses",
        body = openai_body(run, prompt, body)
      ))
    })
    write_lines(lines, file.path(runs_dir, run_name, "batch_input.jsonl"))
  } else {
    requests <- map2(custom_id, frame$body, function(id, body) {
      list(custom_id = id, params = anthropic_params(run, prompt, body))
    })
    write_lines(
      to_json(requests), file.path(runs_dir, run_name, "batch_requests.json")
    )
  }
}
