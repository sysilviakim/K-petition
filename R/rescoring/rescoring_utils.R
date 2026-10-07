# Shared settings and helpers for the LLM re-scoring of the 2024 public-proposal corpus
# Run order: 01_build_frame.R -> 02_batch_requests.R -> 03_batch_submit.R ->
#            04_batch_collect.R -> 05_build_rescored_csv.R
# 03 and 04 call the provider batch APIs and need OPENAI_API_KEY / ANTHROPIC_API_KEY in the
# environment (e.g., in .Renviron). Every other step runs offline.
# The provider responses (data/llm/runs/<run>/raw) are not included in this
# repository, so 04 and 05 record how the scored file was collected and assembled.

library(tidyverse)
library(jsonlite)
library(httr)
library(openssl)

# Paths ========================================================================
prompt_dir <- here::here("R", "rescoring", "prompt")
source_csv <- here::here("data", "tidy", "pub_petition_content_2024_encoding.csv")
gpt4omini_dir <- here::here("data", "llm", "gpt4omini_2025") # eval_<pid>.json
runs_dir <- here::here("data", "llm", "runs")
frame_csv <- here::here("data", "tidy", "frame_2024_6726.csv")
realigned_csv <- here::here(
  "data", "tidy", "evaluated_data_2024_final_realigned_from_json.csv"
)
rescored_csv <- here::here("data", "tidy", "evaluated_data_2024_rescored.csv")

# Runs =========================================================================
# Two full runs of the same prompt. Sampling parameters (temperature, top_p) are
# never passed, so each model runs at its provider default.
runs <- list(
  full_astra_low = list(
    provider = "openai", model = "gpt-6-astra", effort = "low",
    max_tokens = 4000, prefix = "astra"
  ),
  full_fable_low = list(
    provider = "anthropic", model = "claude-fable-5-1", effort = "low",
    max_tokens = 16000, prefix = "fable"
  )
)

dims <- c(
  "clarity_specificity", "logic_consistency", "tone_manner",
  "validity_feasibility"
)
placeholder <- "{{PETITION_BODY}}"

# Prompt =======================================================================
## Read the three prompt files and check them against SHA256SUMS.txt ----------
load_prompt <- function() {
  sums <- read_table(
    file.path(prompt_dir, "SHA256SUMS.txt"),
    col_names = c("sha256", "file"), col_types = "cc"
  )
  read_checked <- function(name) {
    path <- file.path(prompt_dir, name)
    h <- as.character(sha256(file(path)))
    stopifnot(h == sums$sha256[sums$file == name])
    read_file(path)
  }
  out <- list(
    prompt = read_checked("prompt_4dim_exact_ko.txt"),
    system = read_checked("system_message_exact_ko.txt")
  )
  stopifnot(str_detect(out$prompt, fixed(placeholder)))
  out
}

## Petition body: the combined_text used in the 2025 gpt-4o-mini scoring -------
build_body <- function(current_issues, improvement_plan, expected_effect) {
  paste(
    "[현황 및 문제점]", current_issues,
    "\n\n[개선방안]", improvement_plan,
    "\n\n[기대효과]", expected_effect,
    sep = "\n\n"
  )
}

user_prompt <- function(prompt, body) {
  str_replace(prompt$prompt, fixed(placeholder), body)
}

## JSON schema enforced on both providers -------------------------------------
score_schema <- list(
  type = "object",
  properties = set_names(
    map(dims, ~ list(
      type = "object",
      properties = list(
        score = list(type = "integer", enum = 1:5),
        reason = list(type = "string")
      ),
      required = c("score", "reason"),
      additionalProperties = FALSE
    )),
    dims
  ),
  required = dims,
  additionalProperties = FALSE
)

# Request bodies ===============================================================
## OpenAI Responses API (one line of the batch input JSONL) --------------------
openai_body <- function(run, prompt, body) {
  list(
    model = run$model,
    instructions = prompt$system,
    input = user_prompt(prompt, body),
    max_output_tokens = run$max_tokens,
    store = FALSE,
    text = list(format = list(
      type = "json_schema", name = "petition_scores", strict = TRUE,
      schema = score_schema
    )),
    reasoning = list(effort = run$effort)
  )
}

## Anthropic Messages API (params of one Message Batches request) --------------
anthropic_params <- function(run, prompt, body) {
  list(
    model = run$model,
    max_tokens = run$max_tokens,
    system = prompt$system,
    messages = list(list(role = "user", content = user_prompt(prompt, body))),
    output_config = list(
      format = list(type = "json_schema", schema = score_schema),
      effort = run$effort
    )
  )
}

to_json <- function(x) {
  toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
}

# API helpers ==================================================================
api_key <- function(provider) {
  var <- c(openai = "OPENAI_API_KEY", anthropic = "ANTHROPIC_API_KEY")[[provider]]
  key <- Sys.getenv(var)
  if (key == "") stop(var, " is not set")
  key
}

api_headers <- function(provider) {
  if (provider == "openai") {
    add_headers(Authorization = paste("Bearer", api_key("openai")))
  } else {
    add_headers(
      `x-api-key` = api_key("anthropic"), `anthropic-version` = "2023-06-01"
    )
  }
}

api_content <- function(resp) {
  stop_for_status(resp)
  fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

read_batches <- function(run_name) {
  path <- file.path(runs_dir, run_name, "batches.json")
  if (file.exists(path)) read_json(path) else list()
}

write_batches <- function(run_name, batches) {
  write_json(
    batches, file.path(runs_dir, run_name, "batches.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
}

# Parsing model output =========================================================
## Scores and reasons from the JSON text the model returned --------------------
parse_scores <- function(text) {
  empty <- set_names(
    c(rep(list(NA_integer_), 4), rep(list(NA_character_), 4)),
    c(paste0(dims, "_score"), paste0(dims, "_reason"))
  )
  if (is.null(text) || is.na(text) || text == "") {
    return(empty)
  }
  obj <- tryCatch(fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj)) {
    inner <- str_extract(text, "(?s)\\{.*\\}")
    obj <- tryCatch(fromJSON(inner, simplifyVector = FALSE), error = function(e) NULL)
  }
  ok <- !is.null(obj) && all(map_lgl(dims, function(d) {
    s <- suppressWarnings(as.integer(obj[[d]]$score))
    length(s) == 1 && !is.na(s) && s >= 1 && s <= 5 && !is.null(obj[[d]]$reason)
  }))
  if (!ok) {
    return(empty)
  }
  c(
    set_names(map(dims, ~ as.integer(obj[[.x]]$score)), paste0(dims, "_score")),
    set_names(map(dims, ~ as.character(obj[[.x]]$reason)), paste0(dims, "_reason"))
  )
}

## Text of an OpenAI Responses API body (NULL if the model refused) ------------
openai_text <- function(body) {
  parts <- body$output %>%
    keep(~ identical(.x$type, "message")) %>%
    map(~ .x$content) %>%
    list_flatten()
  if (any(map_lgl(parts, ~ identical(.x$type, "refusal")))) {
    return(NULL)
  }
  parts %>%
    keep(~ identical(.x$type, "output_text")) %>%
    map_chr(~ .x$text) %>%
    paste(collapse = "")
}

## Text of an Anthropic message (NULL if the model refused) --------------------
anthropic_text <- function(message) {
  if (identical(message$stop_reason, "refusal")) {
    return(NULL)
  }
  message$content %>%
    keep(~ identical(.x$type, "text")) %>%
    map_chr(~ .x$text) %>%
    paste(collapse = "")
}

pid_of <- function(custom_id) as.integer(str_remove(custom_id, "^pid-"))
