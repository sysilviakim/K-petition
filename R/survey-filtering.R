source(here::here("R", "utilities.R"))

# Data filtering for survey ====================================================
df <- seq(2021, 2024) %>%
  set_names(., .) %>%
  map_dfr(
    ~ here("data", "tidy", paste0("pub_petition_content_", .x, ".csv")) %>%
      read_csv() %>%
      mutate(year = .x)
  ) 

df <- df %>%
  ## 40,840 to 3,530 observations
  filter(branch == "보건복지부") %>%
  ## 233 observations
  filter(grepl("출생|출산|육아", title)) %>%
  ## This is conditional on having been answered by Jan 2024 for 2021--2023
  ## and by Nov 2024 for 2024
  filter(status == "답변완료")

## Export
df %>%
  write_csv(here("data", "tidy", "pub_petition_content_birth_2021-2024.csv"))
