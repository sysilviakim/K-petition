source(here::here("R", "utilities.R"))

# Load all data, wrangle, bind, and save =======================================
petition_list <- seq(2002, 2023) %>%
  set_names(., .) %>%
  map(
    ~ {
      temp <- read.csv(
        here(paste0("data/tidy/pub_petition_content_", .x, ".csv"))
      ) %>%
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

# What is the distribution of the number of petitions per year? --> already done
# What is the distribution of the length of petitions? =========================

body_nchar <- petition %>%
  select(body_problem, body_proposal, body_expectation, year) %>%
  unite("body", contains("body"), sep = " ", na.rm = TRUE) %>%
  mutate(body_nchar = nchar(body))

## Median value: 502 characters, max 246,122(!)
summary(body_nchar$body_nchar)

## Draw the distribution over all years ----------------------------------------
p <- body_nchar %>%
  ggplot(aes(x = body_nchar)) +
  geom_histogram() +
  labs(x = "Number of Characters", y = "Frequency (1,000 Petitions)") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = function(x) x / 1000) +
  theme_bw()
p
## pdf_default(p)
ggsave(here("fig", "petition_length_distribution.pdf"), width = 5, height = 3)

## Logged version because it's very skewed
p <- body_nchar %>%
  ggplot(aes(x = log(body_nchar))) +
  geom_histogram() +
  labs(x = "Number of Characters (Logged)", y = "Frequency (1,000 Petitions)") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = function(x) x / 1000) +
  theme_bw()
p
## pdf_default(p)
ggsave(here("fig", "petition_length_dist_logged.pdf"), width = 5, height = 3)

## Average values over years? --------------------------------------------------
p <- body_nchar %>%
  group_by(year) %>%
  summarise(mean_nchar = median(body_nchar)) %>%
  ggplot(aes(x = year, y = mean_nchar)) +
  geom_col() +
  labs(x = "Year", y = "Average Number of Characters") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
## pdf_default(p)
ggsave(here("fig", "petition_length_avg_over_years.pdf"), width = 5, height = 3)

# Is the response rate increasing over time? ===================================

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
