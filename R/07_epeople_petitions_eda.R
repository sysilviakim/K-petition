## Studying KoNLP with petitions data

source(here::here("R", "utilities.R"))

# Load all data, wrangle, bind, and save =======================================
petition_list <- seq(2002, 2023) %>%
  set_names(., .) %>%
  map(
    ~ {
      temp <- read.csv(here(paste0("data/tidy/pub_petition_content_", .x, ".csv"))) %>%
        rename(
          body_problem = `현황.및.문제점`,
          body_proposal = `개선방안`,
          body_expectation = `기대효과`,
          body_assessment = `검토내용`,
        )

      if ("실시.결과" %in% colnames(temp)) {
        temp <- temp %>%
          rename(body_result = `실시.결과`)
      }

      if (!("date_answered" %in% colnames(temp))) {
        temp$date_answered <- NA
      }
      if (!("date_implemented" %in% colnames(temp))) {
        temp$date_implemented <- NA
      }

      out <- temp %>%
        mutate(
          date_petitioned = as.Date(date_petitioned),
          date_answered = as.Date(date_answered),
          date_implemented = as.Date(date_implemented),
          year_petitioned = year(date_petitioned),
          year_answered = year(date_answered),
          year_implemented = year(date_implemented),
          month_petitioned = month(date_petitioned, label = TRUE),
          month_answered = month(date_answered, label = TRUE),
          month_implemented = month(date_implemented, label = TRUE),
          day_petitioned = day(date_petitioned),
          day_answered = day(date_answered),
          day_implemented = day(date_implemented),
          wday_petitioned = wday(date_petitioned, label = TRUE),
          wday_answered = wday(date_answered, label = TRUE),
          wday_implemented = wday(date_implemented, label = TRUE)
        )
      return(out)
    }
  )

petition <- bind_rows(petition_list, .id = "year")

## Area renaming ---------------------------------------------------------------
## To render correctly in plots, needs to be renamed to English
## or use something like theme_set(theme_grey(base_family = 'NanumGothic'))
petition <- petition %>%
  mutate(
    area = case_when(
      area == "교육/문화/체육/관광" ~ "education/culture/sports/tourism",
      area == "국방/보훈/외교/통일" ~
        "defense/veteran affairs/foreign affairs/unification",
      area == "국토/교통/농림/해양" ~ "land/transport/agriculture/ocean",
      area == "기타" ~ "others",
      area == "노동/환경" ~ "labor/environment",
      area == "법제/사법" ~ "law/judiciary",
      area == "산업/방송/통신/과학" ~
        "industry/broadcast/communication/science",
      area == "식품/보건/복지/가족" ~ "food/health/welfare/family",
      area == "식품의약품" ~ "food/pharmaceuticals",
      area == "재정/금융/소비자" ~ "finance/consumer",
      area == "행정/자치/안전" ~ "administration/autonomy/safety"
    ),
    ## looks like there was a category that should be merged
    ## 식품의약품 + 식품/보건/복지/가족
    area = case_when(
      area == "food/pharmaceuticals" ~ "food/health/welfare/family",
      TRUE ~ area
    ),
    area = factor(
      area,
      levels = c(
        "administration/autonomy/safety",
        "food/health/welfare/family",
        "education/culture/sports/tourism",
        "land/transport/agriculture/ocean",
        "labor/environment",
        "industry/broadcast/communication/science",
        "defense/veteran affairs/foreign affairs/unification",
        "finance/consumer",
        "law/judiciary",
        "others"
      )
    )
  )

## Save the data ---------------------------------------------------------------
saveRDS(petition, here("data", "tidy", "petition_konlp_all_years.rds"))

# Exploratory analysis: load data ==============================================
petition <- readRDS(here("data", "tidy", "petition_konlp_all_years.rds"))

# Which areas were the petitions concentrated on? ==============================
## First, check for missing values ---> none!
any(is.na(petition$area))
any(petition$area == "")

## Total and annual frequencies ------------------------------------------------
sort(table(petition$area), decreasing = TRUE)
## If in Korean
# 1 행정/자치/안전        83222
# 2 식품/보건/복지/가족   25015
# 3 교육/문화/체육/관광   23089
# 4 국토/교통/농림/해양   22646
# 5 노동/환경             16926
# 6 산업/방송/통신/과학   11446
# 7 국방/보훈/외교/통일    8921
# 8 재정/금융/소비자       8556
# 9 법제/사법              3822
# 10 기타                  2550
# 11 식품의약품            2301

## Did these change over time? -------------------------------------------------
p <- ggplot(data = petition, aes(x = year, fill = area)) +
  geom_bar(position = "fill") +
  ylab("") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent)

p + theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 3))
## pdf_default(p) + theme(legend.position = "bottom") +
##   guides(fill = guide_legend(nrow = 3))
ggsave(here("fig", "petition_area_over_time.pdf"), width = 10, height = 5.5)

## Yes, they have! Early 2002--2004 focused on labor
## 2005--2007 on education/culture/sports/tourism
## the current structure/distribution is stable from 2012 onwards
## so I'm guessing this is a data generating process problem
## i.e., which departments are responsible for the petitions

# Is there a seasonality in filing petitions? ==================================
## Month of petition -----------------------------------------------------------
## Just frequency plot without any fills
## Slight increases in Jan, Mar, and Dec
p <- ggplot(data = petition, aes(x = month_petitioned)) +
  geom_bar(colour = "#C6DBEF", fill = "#C6DBEF") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = month.abb) +
  xlab("Month Petitioned") +
  ylab("Count")
p
## pdf_default(p)
ggsave(here("fig", "petition_all_monthly.pdf"), width = 8, height = 5)

## How about if split between years? 2014--2023
## Significant fluctuation of monthly patterns over years
## Likely some salient issue involved
p <- petition %>%
  filter(year_petitioned > 2013 & year_petitioned < 2024) %>%
  ggplot(aes(x = month_petitioned)) +
  geom_bar(colour = "#C6DBEF", fill = "#C6DBEF") +
  facet_wrap(~year_petitioned, ncol = 5) +
  theme_bw() +
  ## x-axis label is month.abb but only every three months
  scale_x_discrete(breaks = c("Mar", "Jun", "Sep", "Dec")) +
  xlab("Month Petitioned") +
  ylab("Count")
p
## pdf_default(p)
ggsave(here("fig", "petition_monthly_by_year.pdf"), width = 8, height = 5)

## How about if split between areas? 2014--2023
## Hike in labor issues in Oct? Mar education makes sense, but ...
p <- petition %>%
  filter(year_petitioned > 2013 & year_petitioned < 2024) %>%
  ggplot(aes(x = month_petitioned, fill = area, colour = area)) +
  geom_bar() +
  facet_wrap(~area, ncol = 5) +
  scale_fill_brewer(palette = "Set3") +
  scale_colour_brewer(palette = "Set3") +
  theme_bw() +
  scale_x_discrete(breaks = c("Mar", "Jun", "Sep", "Dec")) +
  xlab("Month Petitioned") +
  ylab("Count")
p + theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 3))
## pdf_default(p) + theme(legend.position = "bottom") +
##   guides(fill = guide_legend(nrow = 3))
ggsave(here("fig", "petition_monthly_by_area.pdf"), width = 12, height = 5.5)

## Weekdays --------------------------------------------------------------------
## Just frequency plot without any fills
## Activities are significantly higher during weekdays, not weekend
p <- ggplot(data = petition, aes(x = wday_petitioned)) +
  geom_bar(colour = "#C6DBEF", fill = "#C6DBEF") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  xlab("Weekday Petitioned") +
  ylab("Count")
p
## pdf_default(p)
ggsave(here("fig", "petition_all_weekdays.pdf"), width = 8, height = 5)

## How about if split between years? 2014--2023
## Same inverse-U curve, except 2018 on Sundays??
p <- petition %>%
  filter(year_petitioned > 2013 & year_petitioned < 2024) %>%
  ggplot(aes(x = wday_petitioned)) +
  geom_bar(colour = "#C6DBEF", fill = "#C6DBEF") +
  facet_wrap(~year_petitioned, ncol = 5) +
  theme_bw() +
  scale_x_discrete(breaks = c("Sun", "Tue", "Thu", "Sat")) +
  xlab("Weekday Petitioned") +
  ylab("Count")
p
## pdf_default(p)
ggsave(here("fig", "petition_weekdays_by_year.pdf"), width = 8, height = 5)

# Tidytext =====================================================================
## extract nouns to create unique(feature list)
voca <- unlist(sapply(petition$bodytext, extractNoun, USE.NAMES = FALSE))
tb.voca <- sort(table(voca), decreasing = TRUE)
tb.voca[1:10]

## a more precise preprocessing with KoNLP
voca.df <- petition %>%
  unnest_tokens(pos, bodytext, SimplePos09)

## extract 용언 (불용어 제거)
voca.df.n <- voca.df %>%
  filter(str_detect(pos, "/n")) %>%
  mutate(pos_cleaned = str_remove(pos, "/.*$"))

## 어미 통일
voca.df.p <- voca.df %>%
  filter(str_detect(pos, "/p")) %>%
  mutate(pos_cleaned = str_replace_all(pos, "/.*$", "다"))

## n + p
voca.df.out <- bind_rows(voca.df.n, voca.df.p) %>%
  filter(nchar(pos_cleaned) > 1) %>%
  filter(nchar(pos_cleaned) < 10) %>%
  select(title, area, year, month, pos_cleaned)

tb.voca <- table(voca.df.out$pos_cleaned)
voca.ko <- names(tb.voca)
tb.voca.sorted <- sort(tb.voca, decreasing = TRUE)
tb.voca.sorted[1:10]


# Tentative rules for further preprocessing ====================================
## 1. special characters (underbar, emoji, semicolon, hyphen, comma, ...) ------
voca.df.out$pos_cleaned <- str_replace_all(
  string = voca.df.out$pos_cleaned, pattern = "[[:punct:]]", replace = ""
)
voca.df.out <- distinct(voca.df.out)
voca.df.out <- voca.df.out %>% filter(pos_cleaned != "")

voca.df.out <- voca.df.out %>%
  filter(!grepl("\\^|<|>|~", pos_cleaned))

## 2. remove words that contain numbers (dates, counts) ------------------------
voca.df.out <- voca.df.out %>%
  filter(!grepl("[0-9]", pos_cleaned))

tb.voca <- table(voca.df.out$pos_cleaned)
voca.ko <- names(tb.voca)
tb.voca.sorted <- sort(tb.voca, decreasing = TRUE)
tb.voca.sorted[1:10]
