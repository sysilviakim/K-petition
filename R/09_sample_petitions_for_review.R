## Sample 50 petitions for test review
## "Thu Feb 29 19:58:07 2024"

source(here::here("R", "utilities.R"))

# Create sample to be reviewed =================================================
fname <- here("data/sample/sample_petitions_2002_to_2023.rds")
if (!file.exists(fname)) {
  years <- 2002:2023
  file.names <- paste0("data/tidy/pub_petition_content_", years, ".csv")
  all.petition.df <- as_tibble(map_dfr(file.names, read.csv))
  N <- nrow(all.petition.df) ## 13747
  
  set.seed(4321)
  idx <- sample(1:N, 50)
  sample.petitions <- all.petition.df[idx, ]
  
  saveRDS(file = fname, sample.petitions)
}

# Create a scoreboard ==========================================================
## get rid of variables that can bias the review score
sample.petitions <- readRDS(fname)
scoreboard <- sample.petitions %>%
  select(
    -c(
      현황.및.문제점, 개선방안, 기대효과, 검토내용,
      attachment, date_scraped, date_answered, date_implemented,
      실시.결과, status
    )
  )

## Previously, 실시가능성, 효율성, 적용범위, 계속성, 창의성
scoreboard$`문제 구체성` <- NA
scoreboard$`문제 타당성` <- NA
scoreboard$`제안 구체성` <- NA
scoreboard$`제안 현실성` <- NA
scoreboard$`매너` <- NA
scoreboard$총점 <- NA

## Check system info on BK's computer
if (sessionInfo()$running == "macOS Sonoma 14.2.1") {
  fname <- here("data/sample/score13-23-SK.xlsx")
} else {
  fname <- here("data/sample/score13-23-BK.xlsx")
}

write_xlsx(scoreboard, fname)
## Open file externally in Excel after writing
shell.exec(fname)

# Evaluate each petition =======================================================
## Externally open scoreboard.xlsx and fill in the review scores
## 1-10 natural numbers scale
## auto page turner for petition review
for (i in seq(nrow(scoreboard))) {
  ## wipe screen
  cat("\014")

  cat("i=", i, "\n")
  cat("Title:", sample.petitions$title[i], "\n\n") ## title
  cat("Area:", sample.petitions$area[i], "\n\n") ## area
  cat("Date:", sample.petitions$date_petitioned[i], "\n\n") ## date
  cat(sample.petitions$현황.및.문제점[i], "\n\n") ## body text1
  cat(sample.petitions$개선방안[i], "\n\n") ## body text2
  cat(sample.petitions$기대효과[i], "\n\n") ## body text3

  cat("---------------------------------\n\n\n")
  flush.console()

  time.required <- max(
    nchar(sample.petitions$현황.및.문제점[i]),
    700
  )

  start <- Sys.time()
  while ((as.numeric(Sys.time()) - as.numeric(start)) < time.required / 15) {}
  
  ## wipe screen
  cat("\014")
}
