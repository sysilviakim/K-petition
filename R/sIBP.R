## fits sIBP for feature extraction
## create DFM
## normalize
## split into training and test
library(tidyverse)
library(texteffect)

## source(here::here("R", "utilities.R"))

## this code runs topic models to generate topic vectors for petitions
petition_df <- read_csv("data/tidy/pub_petition_corrected.csv") ## original text with space correction
petition_lm <- read_csv("data/tidy/pub_petition_corrected_lm.csv") ## lemmatized text with space correction

## limit scope to post 2013
idx <- petition_df$year >= 2013
petition_df <- petition_df[idx,]
petition_lm <- petition_lm[idx,]

## petition_df$lemm <- petition_lm

type <- "lemmatized"
## type <- "original"
if(type == "lemmatized") petition <- petition_lm ## use lemmatized text
if(type == "original") petition <- petition_df ## use original text

petition$id <- 1:nrow(petition)
petition$title <- petition_df$title
petition$date <- petition_df$date_petitioned
petition$area <- petition_df$area
petition$branch <- petition_df$branch
petition$date_answered <- petition_df$date_answered
petition$date_implemented <- petition_df$date_implemented

petition$date_to_answer <- petition$date_answered - petition$date
petition$date_to_implement <- petition$date_implemented - petition$date

rm(list="petition_df") ## memory saving

## note: duplicate titles!
petition <- petition %>%
    mutate(title = paste0(id,":",title))

petition_dfm <- petition %>%
    unnest_tokens(words,token="ngrams",n=1,corrected_bodytext) %>%
    dplyr::count(title,words)

## some pruning
## 0. special characters, one-character words, stopwords
## remove stopwords
stopwords <- read_csv("data/kiwipiepy_stopwords.csv")
stopwords <- stopwords$Stopword
petition_dfm <- petition_dfm %>%
    filter(!words %in% stopwords)

## remove special characters, numbers
petition_dfm <- petition_dfm %>%
    mutate(words = gsub("[^가-힣\\s]", "", words)) %>%
    filter(words != "")

## examine
idx <- sample(1:nrow(petition),5)
petition_dfm %>% slice(idx)

## one-character words
petition_dfm <- petition_dfm %>%
    mutate(nchar = nchar(words))

## 788826 occasions, 1742 unique occasions
onechar <- petition_dfm %>% filter(nchar == 1) %>% dplyr::select(words)
onechar %>% slice(1:10)

## 1. TF-IDF
petition_dfm <- petition_dfm %>%
    bind_tf_idf(words, title, n)

lower <- quantile(petition_dfm$tf_idf)[2]
upper <- quantile(petition_dfm$tf_idf)[4]

## low TF-IDF: low term frequency, high document frequency
petition_dfm %>%
    filter(tf_idf < lower) %>%
    dplyr::select(words) %>% slice(1:50)
## 1 가족  
## 2 공무원
## 3 교육  
## 4 국가  
## 5 글    
## 6 기업  
## 7 나    
## 8 느끼  
## 9 늘어나
##10 다르  

## high TF-IDF: high term frequency, low document frequency
petition_dfm %>%
    filter(tf_idf > upper) %>%
    dplyr::select(words) %>% slice(1:50)
## 1 답        
## 2 대지      
## 3 등기      
## 4 매각      
## 5 명의      
## 6 물의      
## 7 부동산    
## 8 사례      
## 9 소유      
##10 소유권드기

## mid-level TF-IDF (25% ~ 75% quantile)
petition_dfm <- petition_dfm %>%
    filter(tf_idf < upper & tf_idf > lower)

## ---------- ##
##  fit sIBP  ##
## create X: normalize DFM
## create Y: response delay?
## split sample

## create Y
## use response time
Y <- petition %>%
    dplyr::select(title,date_to_answer) %>%
    filter(!is.na(date_to_answer))

## create X
petition_dfm <- petition_dfm %>%
    filter(title %in% Y$title)

Y <- Y %>%
    filter(title %in% unique(petition_dfm$title))

dfm <- cast_dfm(petition_dfm,
                document="title",
                term="words",
                value="n")

idx <- match(rownames(dfm),Y$title)
Y <- Y[idx,]

## further pruning
## remove words that appear only once in the data
## remove documents that only have one word
word_count <- Matrix::colSums(dfm)
one_time_words <- word_count[word_count == 1]
dfm <- dfm[,word_count > 1]
doc_count <- Matrix::rowSums(dfm)
dfm <- dfm[doc_count > 1,]
Y <- Y[doc_count > 1,]

Y <- Y %>%
    mutate(date_to_answer = as.numeric(date_to_answer))

## fit sIBP
## split sample (use 50% as training set)
train_ind <- sample(1:nrow(dfm), size = 0.5*nrow(dfm), replace = FALSE)

## try range of parameters
## alpha: A parameter that influences how common the treatments are. When alpha is large, the treatments are common.
## sigmasq.n: A parameter determining the variance of the word counts conditional on the treatments. When sigmasq.n is large, the treatments must explain most of the variation in X.
sibp_out <- sibp(X=dfm,Y=Y$date_to_answer,K=2,
                 alpha=2,sigmasq.n=0.8,train.ind=train_ind)

## failed..! memory limit reached
## further pruning required..?! try with more recent years?

## grid search to find optimal model specification
sibp_out <- sibp_param_search(X=dfm,Y=Y$date_to_answer,K=2,
                              alphas=c(2,4),sigmasq.ns=c(0.8,1),train.ind=train_ind)

## Qualitatively look at the top candidates
sibp_top_words(sibp_out[["4"]][["0.8"]][[1]], colnames(dfm), 10, verbose = TRUE)
sibp_top_words(sibp_out[["4"]][["1"]][[1]], colnames(dfm), 10, verbose = TRUE)

## Select the most interest treatments to investigate
sibp_fit <- sibp_out[["4"]][["0.8"]][[1]]

## Estimate the AMCE using the test set
amce<-sibp_amce(sibp_fit, X, Y)
## Plot 95% confidence intervals for the AMCE of each treatment
sibp_amce_plot(amce)
