source(here::here("R", "utilities.R"))

# Load latest data =============================================================
list_files <- list.files(
  here("data", "raw"),
  pattern = "pub_petition_content_list_.*\\.Rda", full.names = TRUE
)
chosen_file <- max(list_files)
load(chosen_file)
date_scraped <- str_extract(chosen_file, "\\d{8}")

# HTML node extraction =========================================================
## Nested list
temp <- vector("list", length(pub_petition_content))
for (i in seq(length(pub_petition_content))) {
  temp[[i]] <- pub_petition_content[[i]] %>%
    map_dfr(~ extract_pt_content(.x, date = date_scraped))
}

## Deduplicate
pub_df <- temp %>%
  bind_rows() %>%
  Kmisc::dedup()
