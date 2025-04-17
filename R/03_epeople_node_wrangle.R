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
  pub_df <- temp %>%
    bind_rows() %>%
    Kmisc::dedup()
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
week_list <- week_list_fxn(2013, 2024)

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
  pub_df <- temp %>%
    bind_rows() %>%
    Kmisc::dedup()
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

# Are there missing weeks? =====================================================
week_list %>%
  set_names(., .) %>%
  imap(
    ~ here(
      "data", "tidy",
      paste0("pub_petition_content_", format(.x, "%Y%m%d"), ".csv")
    )
  ) %>%
  map_lgl(~ !file.exists(.x)) %>%
  which() %>%
  names()

# Does the total in annual tidy data match the total in the raw data? ==========
## First, for weeklies, create a full CSV --------------------------------------
load(here("data", "raw", "pub_petition_num_total.Rda"))
for (yr in seq(2013, 2024)) {
  pub_df <- week_list_fxn(yr, yr) %>%
    map_dfr(
      function(x) {
        here(
          "data", "tidy",
          paste0("pub_petition_content_", format(x, "%Y%m%d"), ".csv")
        ) %>%
          ## Read CSV but suppress warnings
          {suppressMessages(read_csv(.))}
      }
    )
  if (pub_total[[paste0("year", yr)]] != nrow(pub_df)) {
    cat(
      "For year", yr, "the weekly total is", nrow(pub_df), 
      "but the raw data says", pub_total[[paste0("year", yr)]], "\n"
    )
    # For year 2013 the weekly total is 33524 but the raw data says 33085                                                  
    # For year 2014 the weekly total is 32908 but the raw data says 32508                                                  
    # For year 2015 the weekly total is 21998 but the raw data says 21806                                                  
    # For year 2016 the weekly total is 15691 but the raw data says 16012                                                  
    # For year 2017 the weekly total is 22530 but the raw data says 25703                                                  
    # For year 2018 the weekly total is 11937 but the raw data says 25420                                                  
    # For year 2019 the weekly total is 15561 but the raw data says 17479                                                  
    # For year 2020 the weekly total is 11997 but the raw data says 12085                                                  
    # For year 2021 the weekly total is  9758 but the raw data says  9714                                                    
    # For year 2022 the weekly total is  8712 but the raw data says  8675                                                    
    # For year 2023 the weekly total is 10131 but the raw data says  9993
  } else {
    cat("For year", yr, "number of rows match the total.\n")
  }

  write_csv(
    pub_df,
    here("data", "tidy", paste0("pub_petition_content_", yr, ".csv"))
  )
}

# Realized petitions ===========================================================
for (yr in seq(2013, 2024)) {
  ## Load latest data ==========================================================
  fname <- list.files(
    here("data", "raw"),
    pattern = paste0("rel_petition_content_list_", yr, ".Rda"),
    full.names = TRUE
  )
  
  load(fname)
  date_scraped <- format(as.Date(file.info(fname)[["mtime"]]), "%Y%m%d")
  
  ## HTML node extraction ======================================================
  ## Nested list
  temp_title <- temp <- vector("list", length(rel_petition_content))
  for (i in seq(length(rel_petition_content))) {
    temp[[i]] <- rel_petition_content[[i]] %>%
      map_dfr(~ extract_pt_content(.x, date = date_scraped))
    temp_title[[i]] <- rel_petition_content[[i]] %>%
      imap_dfr(~ .x$page_meta[.y, ])
    cat("Iteration", i, "finished.\n")
  }
  
  ## Deduplicate
  rel_df <- temp %>%
    bind_rows() %>%
    Kmisc::dedup()
  title_df <- pub_title_wrangle(temp_title)
  
  ## Some pages have not scraped properly and I'm not entirely sure why
  nrow(rel_df)
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
