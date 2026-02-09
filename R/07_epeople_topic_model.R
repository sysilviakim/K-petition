## toy run to test GPT API
source(here::here("R", "utilities.R"))

## this code runs topic models to generate topic vectors for petitions
## original text with space correction
petition_df <- read_csv(
  here("data", "tidy", "pub_petition_corrected.csv")
)
## lemmatized text with space correction
petition_lm <- read_csv(
  here("data", "tidy", "pub_petition_corrected_lm.csv")
)

## limit scope to post 2013
idx <- petition_df$year >= 2013
petition_df <- petition_df[idx, ]
petition_lm <- petition_lm[idx, ]

## petition_df$lemm <- petition_lm

type <- "lemmatized"
## type <- "original"
if (type == "lemmatized") petition <- petition_lm ## use lemmatized text
if (type == "original") petition <- petition_df ## use original text

petition$id <- 1:nrow(petition)
petition$title <- petition_df$title
petition$date <- petition_df$date_petitioned

## note: duplicate titles!
petition <- petition %>%
  mutate(title = paste0(id, ":", title))

petition <- petition %>%
  select(id, date, title, corrected_bodytext)

petition <- petition %>%
  unnest_tokens(words, token = "ngrams", n = 1, corrected_bodytext) %>%
  count(title, words)

## some pruning
## 0. special characters, one-character words, stopwords
## remove stopwords
stopwords <- read_csv(
  here("data", "kiwipiepy_stopwords.csv")
)
stopwords <- stopwords$Stopword
petition <- petition %>%
  filter(!words %in% stopwords)

## remove special characters, numbers
petition <- petition %>%
  mutate(words = gsub("[^가-힣\\s]", "", words)) %>%
  filter(words != "")

## examine
idx <- sample(1:nrow(petition), 5)
petition %>% slice(idx)

## one-character words
petition <- petition %>%
  mutate(nchar = nchar(words))

## 788826 occasions, 1742 unique occasions
onechar <- petition %>%
  filter(nchar == 1) %>%
  select(words)
onechar %>% slice(1:10)

## 1. TF-IDF
petition <- petition %>%
  bind_tf_idf(words, title, n)

lower <- quantile(petition$tf_idf)[2]
upper <- quantile(petition$tf_idf)[4]

## low TF-IDF: low term frequency, high document frequency
petition %>%
  filter(tf_idf < lower) %>%
  select(words) %>%
  slice(1:50)
## 1 가족
## 2 공무원
## 3 교육
## 4 국가
## 5 글
## 6 기업
## 7 나
## 8 느끼
## 9 늘어나
## 10 다르

## high TF-IDF: high term frequency, low document frequency
petition %>%
  filter(tf_idf > upper) %>%
  select(words) %>%
  slice(1:50)
## 1 답
## 2 대지
## 3 등기
## 4 매각
## 5 명의
## 6 물의
## 7 부동산
## 8 사례
## 9 소유
## 10 소유권드기

## mid-level TF-IDF (25% ~ 75% quantile)
petition <- petition %>%
  filter(tf_idf < upper & tf_idf > lower)

## turn into DFM
dfm <- cast_dfm(petition,
  document = "title",
  term = "words",
  value = "n"
)

vocab <- colnames(dfm)
doc_id <- gsub("\\:.*", "", rownames(dfm))
dim(dfm)

## further pruning
## remove words that appear only once in the data
## remove documents that only have one word
word_count <- Matrix::colSums(dfm)
one_time_words <- word_count[word_count == 1]
dfm <- dfm[, word_count > 1]
doc_count <- Matrix::rowSums(dfm)
dfm <- dfm[doc_count > 1, ]

## fit LDA

## k=10
lda_out10 <- LDA(dfm, k = 10, control = list(seed = 1234))

lda_topic_words <- tidy(lda_out10, matrix = "beta")
writexl::write_xlsx(
  lda_topic_words %>%
    group_by(topic) %>%
    slice_max(beta, n = 5),
  here("output", "topic_term_df_k10.xlsx")
)

lda_doc_topics <- tidy(lda_out10, matrix = "gamma")
lda_doc_topics %>%
  group_by(topic) %>%
  summarise(topic_prop = mean(gamma))
lda_doc_topics %>%
  group_by(topic) %>%
  slice_max(gamma, n = 1)

## evaluate the model fit
sparse_mat_dfm <- as(dfm, "sparseMatrix")
beta <- lda_out10@beta
colnames(beta) <- colnames(dfm)

## coherence score (0.3 ~ 0.5 acceptable)
## evaluates semantic similarity between words in a topic
## higher coherence score: coherent topics (on average)
coherence <- CalcProbCoherence(beta, sparse_mat_dfm)
mean(coherence)

## perpexlity
## lower perplexity means better out-of-sample prediction (generalizable)
perplexity_score <- perplexity(lda_out10)
perplexity_score

## k=15
lda_out15 <- LDA(dfm, k = 15, control = list(seed = 1234))

lda_topic_words <- tidy(lda_out15, matrix = "beta")
writexl::write_xlsx(
  lda_topic_words %>%
    group_by(topic) %>%
    slice_max(beta, n = 5),
  here("output", "topic_term_df_k15.xlsx")
)

sparse_mat_dfm <- as(dfm, "sparseMatrix")
beta <- lda_out15@beta
colnames(beta) <- colnames(dfm)

## coherence score (0.3 ~ 0.5 acceptable)
coherence <- CalcProbCoherence(beta, sparse_mat_dfm)
mean(coherence)

perplexity_score <- perplexity(lda_out15)
perplexity_score

## metrics on increasing model complexity
eval_df <- data.frame(
  K = seq(5, 20, by = 2),
  "Coherence" = numeric(8),
  "Perplexity" = numeric(8)
)
count <- 1
for (K in seq(5, 20, by = 2)) {
  lda_out <- LDA(dfm, k = K, control = list(seed = 1234))

  sparse_mat_dfm <- as(dfm, "sparseMatrix")
  beta <- lda_out@beta
  colnames(beta) <- colnames(dfm)

  coherence <- CalcProbCoherence(beta, sparse_mat_dfm)
  eval_df[count, "Coherence"] <- mean(coherence)

  perplexity_score <- perplexity(lda_out)
  eval_df[count, "Perplexity"] <- perplexity_score

  count <- count + 1
}
eval_df
##   K  Coherence Perplexity
## 1  5 0.03289427   2513.534
## 2  7 0.04869404   2396.895
## 3  9 0.05237409   2299.646
## 4 11 0.04236011   2255.295
## 5 13 0.04630680   2226.740
## 6 15 0.04098572   2231.463
## 7 17 0.03658186   2205.027
## 8 19 0.03799578   2180.374

## topic coherence tends to increase with K
## perplexity tends to decrease with K
## use elbow method to determine the optimal K
par(mfrow = c(1, 2))
plot(eval_df$K, eval_df$Coherence, type = "o", main = "Coherence")
plot(eval_df$K, eval_df$Perplexity, type = "o", main = "Perplexity")

## increase/decrease slows down at K=11
K <- 11
lda_out <- LDA(dfm, k = K, control = list(seed = 1234))

lda_topic_words <- tidy(lda_out, matrix = "beta")
writexl::write_xlsx(
  lda_topic_words %>%
    group_by(topic) %>%
    slice_max(beta, n = 5),
  here("output", "topic_term_df_k11.xlsx")
)

sparse_mat_dfm <- as(dfm, "sparseMatrix")
beta <- lda_out@beta

saveRDS(lda_out, here("data", "TopicK11.rds"))

## prep regression data frame
title <- rownames(dfm)
topic_df <- as_tibble(cbind(lda_out@gamma, title)) %>%
  rename_with(~ str_replace(., "V", "Topic")) %>%
  select(last_col(), everything()) %>%
  mutate(across(2:(K + 1), as.numeric))

## merge with other covariates
df <- petition_df %>%
  select(
    title, area, status, date_petitioned,
    branch, date_answered, date_implemented
  ) %>%
  mutate(id = 1:nrow(petition_df)) %>%
  mutate(title = paste0(id, ":", title)) %>%
  left_join(topic_df, by = "title")

saveRDS(
  df,
  here("data", "tidy", "pub_petition_02-23_cleaned.rds")
)
