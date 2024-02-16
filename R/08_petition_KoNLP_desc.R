## Descriptive analysis of petitions data with KoNLP
## "Fri Feb 16 15:57:10 2024"

source(here::here("R", "utilities.R"))

library(KoNLP)

## merge 2002 - 2012 data
years <- 2002:2012
file.names <- paste0("data/tidy/petition_konlp",years,".rds")
petition.df <- as_tibble(map_dfr(file.names, readRDS))
nrow(petition.df) ## 865235

## further preprocessing
## special characters (underbar, emoji, semicolon, hyphen, comma, period)
petition.df <- petition.df %>%
    filter(!grepl("\\^|<|>|~",pos_cleaned))

## remove words that contain numbers
petition.df <- petition.df %>%
    filter(!str_detect(pos_cleaned, "\\d"))

##-- caution: argumentative rules begin --##
## remove words that contain foreign languages
petition.df <- petition.df %>%
    filter(str_detect(pos_cleaned, "^[가-힣]+$"))

## remove verbs: 하다, 되다, 있다, 없다, 않다... or 해요, 데요,
## words that ends with 다
verb.df <- petition.df %>%
    mutate( last_chr = substr(pos_cleaned,nchar(pos_cleaned),nchar(pos_cleaned)) ) %>%
    filter(last_chr %in% c("다","요")) %>%
    select(-last_chr)

tb.verb <- sort(table(verb.df$pos_cleaned), decreasing=TRUE)
verb.ko <- names(tb.verb) ## 5153

petition.df <- petition.df %>%
    mutate( last_chr = substr(pos_cleaned,nchar(pos_cleaned),nchar(pos_cleaned)) ) %>%
    filter(!last_chr %in% c("다","요")) %>%
    select(-last_chr)
##-- caution: argumentative rules end --##

## create feature space
tb.voca <- table(petition.df$pos_cleaned)
voca.ko <- names(tb.voca)
tb.voca.sorted <- sort(tb.voca, decreasing=TRUE)
tb.voca.sorted[1:50]

## top 10 words for each year and area
areas <- unique(petition.df$area)
areas.eng <- c("Labor & Environment", "Finance & Consumers", "Admin & Security",
               "Legislation", "Transport & Agriculture", "Education & Culture",
               "Industry & Science", "Foreign Relations", "Social Safety",
               "Food & Medicine", "Misc")

top10.df <- petition.df %>%
        group_by(area,year) %>%
        count(pos_cleaned) %>%
        arrange(desc(n)) %>%
        top_n(10)

top10.area.df <- petition.df %>%
        group_by(area) %>%
        count(pos_cleaned) %>%
        arrange(desc(n)) %>%
        top_n(10)

top10.year.df <- petition.df %>%
        group_by(year) %>%
        count(pos_cleaned) %>%
        arrange(desc(n)) %>%
        top_n(10)

## word cloud
library(wordcloud2)
library(htmlwidgets)
wc.df <- as.data.frame(tb.voca)
threshold <- quantile(wc.df$Freq,0.99)
wc.df <- wc.df %>%
    filter(Freq > threshold)
wordcloud2(wc.df) %>%
    saveWidget("output/wc.pdf", selfcontained = TRUE)

wordcloud2(wc.df,figPath="data/kor_penin.png",size=1.5) ## doesn't work...




## basic Topic Models
## create document feature matrix
library(quanteda)
petition.dfm <- petition.df %>%
    select(title,pos_cleaned) %>%
    table()
dim(petition.dfm)
petition.dfm[1:10,1:10]


