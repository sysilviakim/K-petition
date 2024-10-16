## toy run to test GPT API
source(here::here("R", "utilities.R"))

## this code runs topic models to generate topic vectors for petitions
petition_df <- read_csv("data/tidy/pub_petition_corrected.csv") ## original text with space correction
petition_lm <- read_csv("data/tidy/pub_petition_corrected_lm.csv") ## lemmatized text with space correction

## type <- "lemmatized"
type <- "original"
if(type == "lemmatized") petition <- petition_lm ## use lemmatized text
if(type == "original") petition <- petition_df ## use original text

petition$id <- 1:nrow(petition)
petition$title <- petition_df$title
petition$date <- petition_df$date_petitioned

## note: duplicate titles!
petition <- petition %>%
    mutate(title = paste0(id,":",title))

petition <- petition %>%
    select(id,date,title,corrected_bodytext)

petition <- petition %>%
    unnest_tokens(words,token="ngrams",n=1,corrected_bodytext) %>%
    count(title,words)

## some pruning
## 0. special characters, one-character words, stopwords
## remove stopwords
stopwords <- read_csv("data/kiwipiepy_stopwords.csv")
stopwords <- stopwords$Stopword
petition <- petition %>%
    filter(!words %in% stopwords)

## remove special characters, numbers
petition <- petition %>%
    mutate(words = gsub("[^가-힣\\s]", "", words)) %>%
    filter(words != "")

## one-character words
petition <- petition %>%
    mutate(nchar = nchar(words))

## 788826 occasions, 1742 unique occasions
onechar <- petition %>% filter(nchar == 1) %>% select(words) %>% distinct()

## further cleaning + lemmatization using KoNLP
library(KoNLP)
petition <- petition %>%
    mutate(lm_words = unlist(strsplit(SimplePos09(words)[[1]],"/"))[1])

petition <- petition %>%
  mutate(lemmatized_words = sapply(words, function(w) {
      tuple <- SimplePos09(w)
    
      cleaned_word <- unlist(lapply(tuple, function(x) {
          decomp <- str_split(x, "/")[[1]]
          terms <- ""
          if(decomp[2] == "N" | decomp[2] == "P"){
              terms <- decomp[1]
          }
      }))
  }))

sub <- petition %>% slice(1:15)

## fix this code
sub <- sub %>%
  mutate(lm_words = sapply(words, function(w) {
      tuple <- SimplePos09(w)[[1]] ## keep only the 어미, lemmatize by cleaning up what follows the 어미
    
      cleaned_word <- unlist(lapply(tuple, function(x) {
          decomp <- unlist(str_split(x, "/"))
          terms <- ""
          if(decomp[2] == "N" | decomp[2] == "P"){
              terms <- decomp[1]
          }
      }))

      return(str_trim(cleaned_word,side="both"))
  })[[1]])



## 1. TF-IDF
petition_df <- petition_df %>%
    bind_tf_idf(words, title, n)
quantile(petition_df$tf_idf)
##           0%          25%          50%          75%         100% 
##  0.000206185  0.031957845  0.064358613  0.127482995 12.247665543  
## low TF-IDF: low term frequency, high document frequency
petition_df %>%
    filter(tf_idf < 0.03) %>%
    select(words) %>% slice(1:50)
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
petition_df %>%
    filter(tf_idf > 0.12) %>%
    select(words) %>% slice(1:50)
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

## mid-level TF-IDF
petition_df <- petition_df %>%
    filter(tf_idf < 0.12 & tf_idf > 0.03)

table(petition_df$n)
## 80% of words appear only once...!

## turn into DFM
dfm <- cast_dfm(petition_df,
                document="title",
                term="words",
                value="n")

vocab <- colnames(dfm)
doc_id <- gsub("\\:.*","",rownames(dfm))

## a lot of pruning still needed!

## fit LDA
lda_out <- topicmodels::LDA(dfm,k=10)

lda_topic_words <- tidy(lda_out, matrix="beta")
lda_topic_words %>% slice(1:10)

lda_topic_words %>%
    group_by(topic) %>%
    slice_max(beta, n=10)

lda_doc_topics <- tidy(lda_out, matrix="gamma")
lda_doc_topics %>% slice(1:10)



