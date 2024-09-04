source(here::here("R", "utilities.R"))

# GPT API Setup ================================================================
if (Sys.info()[["nodename"]] == "SNUKIM") {
  rgpt_authenticate("SK_api_key.txt")
} else {
  ## api_key <- unlist(read.table("BK_api_key.txt"))
  rgpt_authenticate("BK_api_key.txt")
}

prompt_template <- paste0(
  "The following Korean text needs corrections in terms of word spacing. ", 
  "Please correct spacing issues or very obvious typos, if any. ", 
  ## "Delete line breaks if they are not at the end of the sentence. ", 
  "Leave any other text unchanged. Carefully think about each edit. \n\n"
)

# Load Cheongwon 24 data =======================================================
load(
  max(
    list.files(
      here("data", "raw"),
      pattern = "cheongwon_content_",
      full.names = TRUE
    )
  )
)

# Correct Spacing with GPT API =================================================
gpt_response <- vector("list", length = nrow(content_df))
for (i in seq(nrow(content_df))) {
  prompt_text <- paste0(
    prompt_template,
    ## So that we may be able to parse the subject from the text
    "\n\n||  ", content_df$subject[[i]], "  ||\n\n",
    content_df$text[[i]]
  )
  
  gpt_response[[i]] <- kor_response <- rgpt(
    prompt_role_var = "user",
    param_seed = 123,
    prompt_content_var = prompt_text,
    ## default model
    ## param_model = "gpt-4o",
    ## between 0 and 2. high temperature introduces more randomness, 
    ## low temperature makes it more deterministic
    param_temperature = 0.5
  )
  
  content_df$text_corrected[[i]] <- kor_response[[1]]$gpt_content
  Sys.sleep(5)
  
  if (i %% 10 | i == nrow(content_df)) {
    save(
      gpt_response,
      file = here("output", "cheongwon_24_gpt.Rda")
    )
  }
}

