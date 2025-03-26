## fits sIBP for feature extraction
## create DFM
## normalize
## split into training and test
library(tidyverse)
library(texteffect)
library(tidytext)

## source(here::here("R", "utilities.R"))

## this code runs topic models to generate topic vectors for petitions
petition_df <- read_csv("data/tidy/pub_petition_corrected.csv") ## original text with space correction
petition_lm <- read_csv("data/tidy/pub_petition_corrected_lm.csv") ## lemmatized text with space correction

## limit scope to post 2013 for consistency in DGP
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

## subset down to one or a few areas
K <- 5
areas <- unique(petition$area)
areas_eng <- c("Land_Infra_Agric_Marine",
               "Admin_Security",
               "Industry_Communication_Sci",
               "Food_Drug",
               "Labor_Environ",
               "Food_Health_Family",
               "Education_Culture",
               "Finance_Consumer",
               "Misc",
               "Military_Foreign_NK",
               "Law_Judiciary")
for(i in 1:length(areas)){
    petition_sub <- petition %>%
        filter(area == areas[i])

    ## create Y
    ## use response time
    Y <- petition_sub %>%
        dplyr::select(title,date_to_answer) %>%
        mutate(date_to_answer = as.numeric(date_to_answer)) %>%
        filter(!is.na(date_to_answer))

    ## create X
    petition_dfm_X <- petition_dfm %>%
        filter(title %in% Y$title)

    Y <- Y %>%
        filter(title %in% unique(petition_dfm_X$title))

    dfm <- cast_dfm(petition_dfm_X,
                    document="title",
                    term="words",
                    value="n")

    idx <- match(rownames(dfm),Y$title)
    Y <- Y[idx,]

    ## address missing caused by matching
    na <- which(is.na(Y$date_to_answer))
    if(length(na) > 0) dfm <- dfm[-na,];Y <- Y[-na,]

    ## further pruning
    ## keep words that appear at least once in the document
    ## keep documents that have at least one word
    word_count <- Matrix::colSums(dfm)
    word_threshold <- 30

    ##sample_words <- colnames(dfm)[word_count < word_threshold]
    ##sample_words[ sample(1:length(sample_words),20) ]

    ##sample_words <- colnames(dfm)[word_count > word_threshold]
    ##sample_words[ sample(1:length(sample_words),20) ]

    ##one_time_words <- word_count[word_count == 1]
    dfm <- dfm[,word_count > word_threshold]
    ## doc_count <- Matrix::rowSums(dfm)
    ## dfm <- dfm[doc_count > 1,]
    ## Y <- Y[doc_count > 1,]

    ## fit sIBP
    ## sibp takes data frame objects
    X <- quanteda::convert(dfm,to="data.frame")
    y <- Y$date_to_answer

    ## add covariates
    ## 1. branch
    ## 2. year
    ## branch and year as binary variables
    petition_sub <- petition_sub %>%
        mutate(area = str_replace_all(areas[i],"/","_"))
    petition_cov <- petition_sub %>%
        mutate(year = substr(date,1,4)) %>%
        mutate(value = 1) %>%
        dplyr::select(title,year,value) %>%
        ## dplyr::select(title,area,year,value) %>%
        ## pivot_wider(names_from = area, values_from = value, values_fill = list(value = 0)) %>%
        mutate(value = 1) %>%
        pivot_wider(names_from = year, values_from = value, values_fill = list(value = 0))

    X <- X %>%
        left_join(petition_cov,by=c("doc_id"="title"))
    X <- X %>%
        dplyr::select(-doc_id)
    features <- colnames(X)

    ## some added covariates (i.e. years) are missing from X (0 columns)
    X <- X[,colSums(X) > 5]

    ## split sample (use 50% as training set)
    train_ind <- sample(1:nrow(dfm), size = 0.5*nrow(dfm), replace = FALSE)

    ## try range of parameters
    ## alpha: A parameter that influences how common the treatments are. When alpha is large, the treatments are common.
    ## sigmasq.n: A parameter determining the variance of the word counts conditional on the treatments. When sigmasq.n is large, the treatments must explain most of the variation in X.
    sibp_out <- sibp(X=X,
                     Y=y,K=K,
                     alpha=2,sigmasq.n=0.8,train.ind=train_ind)

    ## document-treatment probability matrix
    ## probability that the given document has treatment k
    Ytrain <- Y[train_ind,]
    nu_cum <- colMeans(sibp_out$nu)
    names(nu_cum) <- paste0("T",1:K)
    pdf(paste0("output/",areas_eng[i],"_doc_treat_freq.pdf"),width=2*K,height=K)
    barplot(nu_cum,main="Average Frequency of Treatment Features across Documents")
    dev.off()

    ## treatment-feature matrix: the effect of the row treatment on the column word
    detect_max <- function(x,n){
        sorted_x <- sort(x,decreasing=TRUE)
        return(unlist(sapply(1:n,function(j){which(sorted_x[j] == x)})))
    }
    phi <- sibp_out$phi
    ## extract strongest 30 features for each treatment
    feature_id <- sapply(1:K,function(j){features[ detect_max(phi[j,],30) ]})
    colnames(feature_id) <- paste0("T",1:K)
    write_csv(as_tibble(feature_id),paste0("output/",areas_eng[i],"_treat_id.csv"))

    ## Estimate the AMCE using the test set
    amce <- sibp_amce(sibp_out, X, y)
    ## Plot 95% confidence intervals for the AMCE of each treatment
    sibp_amce_plot(amce) + theme_classic()
    ggsave(paste0("output/",areas_eng[i],"_sibp_amce.pdf"))
}


## next task:
## add model evaluation to decide optimal K
## interpretation?

## t0_docs <- Ytrain$title[rowSums(nu < 0.1)]
## t1_docs <- Ytrain$title[nu[,1]>0.9]
## t2_docs <- Ytrain$title[nu[,2]>0.9]
## t3_docs <- Ytrain$title[nu[,3]>0.9]

## t0_text <- petition %>% filter(title %in% t0_docs)
## t1_text <- petition %>% filter(title %in% t1_docs)
## t2_text <- petition %>% filter(title %in% t2_docs)
## t3_text <- petition %>% filter(title %in% t3_docs)

## write_csv(t0_text,"output/t0_sample_texts.csv")
## write_csv(t1_text,"output/t1_sample_texts.csv")
## write_csv(t2_text,"output/t2_sample_texts.csv")
## write_csv(t3_text,"output/t3_sample_texts.csv")

## effect of treatment k on having given words for document i
## treat_coef <- nu[i,] %*% phi
## features[treat_coef > 0]

## K-length vector: the effect of having each treatment on the outcome
## negative value: having treatment k decreases the response delay
## positive value: having treatment k increases the response delay
## m <- sibp_out$m

## grid search to find optimal model specification
sibp_out <- sibp_param_search(X=X,Y=y,K=2,
                              alphas=c(2,4),sigmasq.ns=c(0.8,1),train.ind=train_ind,iters=1)

## Qualitatively look at the top candidates
sibp_top_words(sibp_out[["4"]][["0.8"]][[1]], colnames(dfm), 10, verbose = TRUE)
sibp_top_words(sibp_out[["4"]][["1"]][[1]], colnames(dfm), 10, verbose = TRUE)

## Select the most interest treatments to investigate
sibp_fit <- sibp_out[["4"]][["0.8"]][[1]]

## Estimate the AMCE using the test set
amce<-sibp_amce(sibp_fit, X, Y)
## Plot 95% confidence intervals for the AMCE of each treatment
sibp_amce_plot(amce)
