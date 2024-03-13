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

# Load data for comparing researcher assessment ================================
df_list <- list(
  SK_01 = read_excel(here("data/sample/score02-12-SK.xlsx")),
  SK_02 = read_excel(here("data/sample/score13-23-SK.xlsx")),
  BK_01 = read_excel(here("data/sample/score02-12-BK.xlsx")),
  BK_02 = read_excel(here("data/sample/score13-23-BK.xlsx"))
)

## Combine and prune
df <- bind_rows(df_list, .id = "source") %>%
  select(-`1-dim score`) %>%
  filter(!is.na(`총점`)) %>%
  filter(`총점` != 0) %>%
  rowwise() %>%
  mutate(researcher = str_sub(source, 1, 2)) %>%
  ungroup()

# Checking assessment consistency ==============================================
## Inconsistencies within assessor (possible)
df %>%
  filter(title == "국민신문고 정책제안 처리결과 공개") %>%
  select(-area, -branch)

## Delete multiple assessments of the same petition within researcher
df <- df %>%
  distinct(researcher, title, .keep_all = TRUE)

## Inconsistencies between assessors
df_wide <- df %>%
  select(title, researcher, `총점`) %>%
  pivot_wider(names_from = researcher, values_from = `총점`)

## Correlation coefficient: 0.7
cor(df_wide$SK, df_wide$BK, use = "pairwise.complete.obs")
ggplot(df_wide, aes(x = SK, y = BK)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "SK", y = "BK")

## In terms of linear regression, slope coef. is 1.10, intercept -2.23
lm(SK ~ BK, data = df_wide) %>% summary()

## SK gives harsher assessment to low-scoring petitions (indistinguishable)
## but otherwise the two researchers' assessments are highly correlated

## How about within each element of the rubric?
df_wide <- df %>%
  select(title, researcher, `문제 구체성`, `문제 타당성`, `제안 구체성`, `제안 현실성`, `매너`) %>%
  pivot_longer(
    cols = `문제 구체성`:`매너`,
    names_to = "element",
    values_to = "score"
  ) %>%
  unite("elem_researcher", element, researcher, sep = "_") %>%
  pivot_wider(names_from = elem_researcher, values_from = score)

cor(df_wide$`문제 구체성_SK`, df_wide$`문제 구체성_BK`) ## 0.58
cor(df_wide$`문제 타당성_SK`, df_wide$`문제 타당성_BK`) ## 0.62
cor(df_wide$`제안 구체성_SK`, df_wide$`제안 구체성_BK`) ## 0.64
cor(df_wide$`제안 현실성_SK`, df_wide$`제안 현실성_BK`) ## 0.58
cor(df_wide$`매너_SK`, df_wide$`매너_BK`) ## 0.47 (low)

## So perhaps the components cancel out