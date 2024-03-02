## Sample 50 petitions for test review
## "Thu Feb 29 19:58:07 2024"

source(here::here("R", "utilities.R"))

fname <- here("data/sample/sample_petitions.rds")
if (!file.exists(fname)) {
  years <- 2002:2012
  file.names <- paste0("data/tidy/pub_petition_content_", years, ".csv")
  all.petition.df <- as_tibble(map_dfr(file.names, read.csv))
  N <- nrow(all.petition.df) ## 13747
  
  set.seed(4321)
  idx <- sample(1:N, 50)
  sample.petitions <- all.petition.df[idx, ]
  
  saveRDS(file = fname, sample.petitions)
}

## create scoreboard
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
scoreboard$실시가능성 <- NA
scoreboard$효율성 <- NA
scoreboard$적용범위 <- NA
scoreboard$계속성 <- NA
scoreboard$창의성 <- NA
scoreboard$총점 <- NA

write_xlsx(scoreboard, here("data/sample/scoreboard_BK.xlsx"))

## auto page turner for petition review
for (i in 1:50) {
  cat("i=", i, "\n")
  cat("Title:", sample.petitions$title[i], "\n\n") ## title

  cat("Area:", sample.petitions$area[i], "\n\n") ## area

  cat("Date:", sample.petitions$date_petitioned[i], "\n\n") ## date

  cat(sample.petitions$현황.및.문제점[i], "\n\n") ## body text1
  cat(sample.petitions$개선방안[i], "\n\n") ## body text2
  cat(sample.petitions$기대효과[i], "\n\n") ## body text3

  cat("---------------------------------\n\n\n")
  flush.console()

  time.required <- nchar(sample.petitions$현황.및.문제점[i])

  start <- Sys.time()
  while ((as.numeric(Sys.time()) - as.numeric(start)) < time.required / 15) {}
}
