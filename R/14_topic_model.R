## toy run to test GPT API
source(here::here("R", "utilities.R"))

## this code runs topic models to generate topic vectors for petitions
petition_df <- read_csv("data/tidy/pub_petition_corrected.csv") ## original text with space correction
petition_lm <- read_csv("data/tidy/pub_petition_corrected_lm.csv") ## lemmatized text with space correction

petition_df$lemm <- petition_lm

type <- "lemmatized"
## type <- "original"
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

## examine
idx <- sample(1:nrow(petition),5)
petition %>% slice(idx)

## one-character words
petition <- petition %>%
    mutate(nchar = nchar(words))

## 788826 occasions, 1742 unique occasions
onechar <- petition %>% filter(nchar == 1) %>% select(words)
onechar %>% slice(1:10)

## 1. TF-IDF
petition <- petition %>%
    bind_tf_idf(words, title, n)
quantile(petition$tf_idf)

## low TF-IDF: low term frequency, high document frequency
petition %>%
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
petition %>%
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
petition <- petition %>%
    filter(tf_idf < 0.12 & tf_idf > 0.03)

table(petition$n)
## 80% of words appear only once in a document...!

## turn into DFM
dfm <- cast_dfm(petition,
                document="title",
                term="words",
                value="n")

vocab <- colnames(dfm)
doc_id <- gsub("\\:.*","",rownames(dfm))
dim(dfm)

## a lot of pruning still needed!

## fit LDA
library(topicmodels)
library(textmineR)

## k=10
lda_out10 <- LDA(dfm,k=10,control=list(seed=1234))

lda_topic_words <- tidy(lda_out10, matrix="beta")
writexl::write_xlsx(lda_topic_words %>%
          group_by(topic) %>%
          slice_max(beta, n=5),
          "data/topic_term_df_k10.xlsx")

lda_doc_topics <- tidy(lda_out10, matrix="gamma")
lda_doc_topics %>%
    group_by(topic) %>%
    slice_max(gamma,n=2)

## evaluate the model fit
sparse_mat_dfm <- as(dfm, "sparseMatrix")
beta <- lda_out10@beta
colnames(beta) <- vocab

## coherence score (0.3 ~ 0.5 acceptable)
## evaluates semantic similarity between words in a topic
## higher coherence score: coherent topics (on average)
coherence <- CalcProbCoherence(beta, sparse_mat_dfm)
mean(coherence) ## 0.05

## perpexlity
## lower perplexity means better out-of-sample prediction (generalizable)
perplexity_score <- perplexity(lda_out10) 
perplexity_score ## 2288.672

## k=15
lda_out15 <- LDA(dfm,k=15,control=list(seed=1234))

lda_topic_words <- tidy(lda_out15, matrix="beta")
writexl::write_xlsx(lda_topic_words %>%
          group_by(topic) %>%
          slice_max(beta, n=5),
          "data/topic_term_df_k15.xlsx")

sparse_mat_dfm <- as(dfm, "sparseMatrix")
beta <- lda_out15@beta
colnames(beta) <- vocab

## coherence score (0.3 ~ 0.5 acceptable)
coherence <- CalcProbCoherence(beta, sparse_mat_dfm)
mean(coherence) ## 0.04

perplexity_score <- perplexity(lda_out15) 
perplexity_score ## 2231

## metrics on increasing model complexity
eval_df <- data.frame(K=seq(5,20,by=2),
                      "Coherence"=numeric(8),
                      "Perplexity"=numeric(8))
count <- 1
for(K in seq(5,20,by=2)){
    lda_out <- LDA(dfm,k=K,control=list(seed=1234))

    sparse_mat_dfm <- as(dfm, "sparseMatrix")
    beta <- lda_out@beta
    colnames(beta) <- vocab

    coherence <- CalcProbCoherence(beta, sparse_mat_dfm)
    eval_df[count,"Coherence"] <- mean(coherence)  

    perplexity_score <- perplexity(lda_out) 
    eval_df[count,"Perplexity"] <- perplexity_score  

    count <- count+1
}
eval_df;
##   K  Coherence Perplexity
##1  5 0.03289427   2513.534
##2  7 0.04869404   2396.895
##3  9 0.05237409   2299.646
##4 11 0.04236011   2255.295
##5 13 0.04630680   2226.740
##6 15 0.04098572   2231.463
##7 17 0.03658186   2205.027
##8 19 0.03799578   2180.374

## topic coherence tends to increase with K
## perplexity tends to decrease with K
## use elbow method to determine the optimal K
par(mfrow=c(1,2))
plot(eval_df$K, eval_df$Coherence, type="o", main="Coherence")
plot(eval_df$K, eval_df$Perplexity, type="o", main="Perplexity")

## increase/decrease slows down at K=13
lda_out <- LDA(dfm,k=13,control=list(seed=1234))

lda_topic_words <- tidy(lda_out, matrix="beta")
writexl::write_xlsx(lda_topic_words %>%
          group_by(topic) %>%
          slice_max(beta, n=5),
          "data/topic_term_df_k13.xlsx")

sparse_mat_dfm <- as(dfm, "sparseMatrix")
beta <- lda_out@beta
colnames(beta) <- vocab

saveRDS(lda_out,"data/TopicK13.rds")
