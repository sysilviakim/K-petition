## Studying KoNLP with petitions data

source(here::here("R", "utilities.R"))

library(KoNLP)
petition <- read.csv("data/tidy/pub_petition_content_2012.csv")
names(petition)[7] <- c("bodytext")

petition$year <- substr(petition$date_petitioned,1,4)
petition$month <- substr(petition$date_petitioned,6,7)

table(petition$area)

summary.df <- petition %>%
    group_by(area,month) %>%
    summarise(count=n())

ggplot(data=summary.df, aes(x=month, y=count, fill=area)) +
    geom_bar(stat="identity", position="dodge") +
    theme_bw()
## need to install fonts to display hangeul

## tidytext
## extract nouns to create unique(feature list)
voca <- unlist(sapply(petition$bodytext, extractNoun, USE.NAMES=FALSE))
tb.voca <- sort(table(voca),decreasing=TRUE)
tb.voca[1:10]

## a more precise preprocessing with KoNLP
voca.df <- petition %>%
    unnest_tokens(pos, bodytext, SimplePos09)

## extract 용언 (불용어 제거)
voca.df.n <- voca.df %>%
    filter(str_detect(pos, "/n")) %>%
    mutate(pos_cleaned = str_remove(pos,"/.*$"))

## 어미 통일
voca.df.p <- voca.df %>%
    filter(str_detect(pos, "/p")) %>%
    mutate(pos_cleaned = str_replace_all(pos,"/.*$","다"))

## n + p
voca.df.out <- bind_rows(voca.df.n, voca.df.p) %>%
    filter(nchar(pos_cleaned) > 1) %>%
    filter(nchar(pos_cleaned) < 10) %>%
    select(title,area,year,month,pos_cleaned)
    
tb.voca <- table(voca.df.out$pos_cleaned)
voca.ko <- names(tb.voca)
tb.voca.sorted <- sort(tb.voca, decreasing=TRUE)
tb.voca.sorted[1:10]


## ----------------------------------------- ##
## tentative rules for further preprocessing ##
## ----------------------------------------- ##
## 1. special characters (underbar, emoji, semicolon, hyphen, comma, period)
voca.df.out$pos_cleaned <- str_replace_all(string=voca.df.out$pos_cleaned, pattern="[[:punct:]]", replace="")
voca.df.out <- distinct(voca.df.out)
voca.df.out <- voca.df.out %>% filter(pos_cleaned != "")

voca.df.out <- voca.df.out %>%
    filter(!grepl("\\^|<|>|~",pos_cleaned))

## 2. remove words that contain numbers (dates, counts)
voca.df.out <- voca.df.out %>%
    filter(!grepl("[0-9]", pos_cleaned))

tb.voca <- table(voca.df.out$pos_cleaned)
voca.ko <- names(tb.voca)
tb.voca.sorted <- sort(tb.voca, decreasing=TRUE)
tb.voca.sorted[1:10]
