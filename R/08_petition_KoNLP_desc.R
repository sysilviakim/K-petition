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

## group by words in each petition
petition.df <- petition.df %>%
    count(title,pos_cleaned) %>%
    group_by(title) %>%
    mutate(total = sum(n)) %>%
    ungroup()
nrow(petition.df) ## 438617

## another clean-up
petition.df <- petition.df %>%
    mutate(title = str_replace_all(title,"\\s+", " ")) %>% ## remove extra white spaces
    mutate(title = str_trim(title,side="both")) ## remove white space at the beginning and end


## basic topic model
## compute tf-idf
petition.df <- petition.df %>%
    bind_tf_idf(pos_cleaned, title, n)

quantile(petition.df$tf_idf)
##          0%          25%          50%          75%         100% 
##0.0009080714 0.0329332617 0.0631300368 0.1232115592 9.4872900578

## extremely common/uncommon words
common_terms <- petition.df %>%
    filter(tf_idf < 0.025) %>%
    select(pos_cleaned)
## common, but not safe to remove

rare_terms <- petition.df %>%
    filter(tf_idf > 5) %>%
    select(pos_cleaned)
## safe to remove



## pick up from here..ㅜㅜ




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
    saveWidget("output/wc.pdf")

wordcloud2(wc.df,figPath="data/kor_penin.png",size=1.5) ## doesn't work...


