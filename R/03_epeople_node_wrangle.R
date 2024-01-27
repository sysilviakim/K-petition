## Wrangling collected raw petition HTMLs
## This script assumes that certain Rda files have been generated
source(here::here("R", "utilities.R"))

# Loop over years ==============================================================
for (yr in seq(2002, 2012)) {
  ## Load latest data ==========================================================
  fname <- list.files(
    here("data", "raw"),
    pattern = paste0("pub_petition_content_list_", yr, ".Rda"),
    full.names = TRUE
  )

  load(fname)
  date_scraped <- format(as.Date(file.info(fname)[["mtime"]]), "%Y%m%d")

  ## HTML node extraction ======================================================
  ## Nested list
  temp_title <- temp <- vector("list", length(pub_petition_content))
  for (i in seq(length(pub_petition_content))) {
    temp[[i]] <- pub_petition_content[[i]] %>%
      map_dfr(~ extract_pt_content(.x, date = date_scraped))
    temp_title[[i]] <- pub_petition_content[[i]] %>%
      imap_dfr(~ .x$page_meta[.y, ])
    cat("Iteration", i, "finished.\n")
  }

  ## Deduplicate
  pub_df <- temp %>% bind_rows() %>% Kmisc::dedup()
  title_df <- pub_title_wrangle(temp_title)

  ## Some pages have not scraped properly and I'm not entirely sure why
  nrow(pub_df)
  nrow(title_df)

  ## Append title metadata by title... but many to many relationship
  ## because some petitioners have petitioned the same content to multiple
  ## branches of the government, which counts as separate posts
  ## It might really be better to deal with it within extract_pt_content
  ## left_join(pub_df, title_df)

  ## Missing info from pub_df that's only in title_df: views, numbers
  ## But numbers, because they don't provide permanent URLs, are not very
  ## meaningful...

  ## Save to CSV ===============================================================
  write_csv(
    pub_df, here("data", "tidy", paste0("pub_petition_content_", yr, ".csv"))
  )

  cat("Year", yr, "finished.\n")
}

# Loop over weeks ==============================================================
week_list <- week_list_fxn(2013, 2023)

for (wk in week_list) {
  wk <- as.Date(wk, origin = "1970-01-01")
  cat("Week", format(wk, "%Y%m%d"), "started.\n")
  
  ## Load latest data ==========================================================
  fname <- list.files(
    here("data", "raw"),
    pattern = paste0("pub_petition.*", format(wk, "%Y%m%d"), ".Rda"),
    full.names = TRUE
  )
  load(fname)
  date_scraped <- format(as.Date(file.info(fname)[["mtime"]]), "%Y%m%d")

  ## HTML node extraction ======================================================
  temp_title <- temp <- vector("list", length(pub_petition_content))
  for (i in seq(length(pub_petition_content))) {
    temp[[i]] <- pub_petition_content[[i]] %>%
      map_dfr(~ extract_pt_content(.x, date = date_scraped))
    temp_title[[i]] <- pub_petition_content[[i]] %>%
      imap_dfr(~ .x$page_meta[.y, ])
    cat("Iteration", i, "finished.\n")
  }

  ## Deduplicate
  pub_df <- temp %>% bind_rows() %>% Kmisc::dedup()
  title_df <- pub_title_wrangle(temp_title)
  
  ## Some pages have not scraped properly and I'm not entirely sure why
  nrow(pub_df)
  nrow(title_df)
  
  ## Save to CSV ===============================================================
  write_csv(
    pub_df,
    here(
      "data", "tidy", 
      paste0("pub_petition_content_", format(wk, "%Y%m%d"), ".csv")
    )
  )
  
  cat("Week", format(wk, "%Y%m%d"), "finished.\n")
}
