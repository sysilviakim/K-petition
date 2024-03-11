## Descriptive analysis of petitions data with KoNLP
## "Fri Feb 16 15:57:10 2024"

source(here::here("R", "utilities.R"))
library(wordcloud2)
library(htmlwidgets)
library(topicmodels)

# Merge and process data =======================================================
years <- 2002:2023
file.names <- paste0("data/tidy/petition_konlp", years, ".rds")
petition.df <- as_tibble(map_dfr(file.names, readRDS))
nrow(petition.df) ## 865235

## further preprocessing
## special characters (underbar, emoji, semicolon, hyphen, comma, period)
petition.df <- petition.df %>%
  filter(!grepl("\\^|<|>|~", pos_cleaned))

## remove words that contain numbers
petition.df <- petition.df %>%
  filter(!str_detect(pos_cleaned, "\\d"))

## -- caution: argumentative rules begin --##
## remove words that contain foreign languages
petition.df <- petition.df %>%
  filter(str_detect(pos_cleaned, "^[가-힣]+$"))

## remove verbs: 하다, 되다, 있다, 없다, 않다... or 해요, 데요,
## words that ends with 다
verb.df <- petition.df %>%
  mutate(
    last_chr = substr(pos_cleaned, nchar(pos_cleaned), nchar(pos_cleaned))
  ) %>%
  filter(last_chr %in% c("다", "요")) %>%
  select(-last_chr)

tb.verb <- sort(table(verb.df$pos_cleaned), decreasing = TRUE)
verb.ko <- names(tb.verb) ## 5153

petition.df <- petition.df %>%
  mutate(
    last_chr = substr(pos_cleaned, nchar(pos_cleaned), nchar(pos_cleaned))
  ) %>%
  filter(!last_chr %in% c("다", "요")) %>%
  select(-last_chr)
## -- caution: argumentative rules end --##

# Word frequencies =============================================================
## top 10 words for each year and area
areas <- unique(petition.df$area)
areas.eng <- c(
  "Labor & Environment", "Finance & Consumers", "Admin & Security",
  "Legislation", "Transport & Agriculture", "Education & Culture",
  "Industry & Science", "Foreign Relations", "Social Safety",
  "Food & Medicine", "Misc"
)

top10.df <- petition.df %>%
  group_by(area, year) %>%
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
wc.df <- as.data.frame(tb.voca)
threshold <- quantile(wc.df$Freq, 0.99)
wc.df <- wc.df %>%
  filter(Freq > threshold)
wordcloud2(wc.df) %>%
  saveWidget("output/wc.pdf")

wordcloud2(wc.df, figPath = "data/kor_penin.png", size = 1.5) ## doesn't work...

# Basic topic model ============================================================
## group by words in each petition
petition.dfm <- petition.df %>%
  count(title, pos_cleaned) %>%
  group_by(title) %>%
  mutate(total = sum(n)) %>%
  ungroup()
nrow(petition.dfm) ## 438617

## another clean-up
petition.df <- petition.df %>%
  ## remove extra white spaces
  mutate(title = str_replace_all(title, "\\s+", " ")) %>%
  mutate(title = str_replace(title, "\\\"", "'")) %>% ## quotation
  mutate(title = str_replace(title, "\\\"", "'")) %>% ## quotation
  ## remove white space at the beginning and end
  mutate(title = str_trim(title, side = "both"))

petition.dfm <- petition.dfm %>%
  ## remove extra white spaces
  mutate(title = str_replace_all(title, "\\s+", " ")) %>%
  mutate(title = str_replace(title, "\\\"", "'")) %>% ## quotation
  mutate(title = str_replace(title, "\\\"", "'")) %>% ## quotation
  ## remove white space at the beginning and end
  mutate(title = str_trim(title, side = "both"))

## compute tf-idf
petition.dfm <- petition.dfm %>%
  bind_tf_idf(pos_cleaned, title, n)

quantile(petition.dfm$tf_idf)
##          0%          25%          50%          75%         100%
## 0.0009080714 0.0329332617 0.0631300368 0.1232115592 9.4872900578

## extremely common/uncommon words
common_terms <- petition.dfm %>%
  filter(tf_idf < 0.01) %>%
  select(pos_cleaned) %>%
  distinct()
## words appearing in many documents:
## 문제점, 심각, 우리나라, 낭비, 정부, 시간, 사람

rare_terms <- petition.dfm %>%
  filter(tf_idf > 3) %>%
  select(pos_cleaned) %>%
  distinct()
## words appearing in few documents: names of cities and regions, typoes

## remove rare and common terms
petition.dfm <- petition.dfm %>%
  filter(tf_idf > 0.01 & tf_idf < 3)

## append year
petition.df.idx <- petition.df %>%
  select(-pos_cleaned) %>%
  distinct()

petition.dfm <- petition.dfm %>%
  left_join(petition.df.idx, by = "title", multiple = "any")

## turn to document-term matrix
## first run for 2010 to 2012
dtm <- petition.dfm %>%
  filter(year >= 2010) %>%
  cast_dtm(document = title, term = pos_cleaned, value = n)

## fit LDA
K <- 5
lda.fit <- LDA(dtm, k = K)
print(lda.fit)

## describe topics: top 10 terms for each topic
word.prob.k <- tidy(lda.fit, matrix = "beta")

top.terms <- word.prob.k %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)

top.terms %>% filter(topic == 1)
top.terms %>% filter(topic == 2)
top.terms %>% filter(topic == 3)
top.terms %>% filter(topic == 4)
top.terms %>% filter(topic == 5)

top10.terms.mat <- matrix(NA, 10, K)
for (k in 1:5) {
  top10.terms.mat[, k] <- top.terms %>%
    filter(topic == k) %>%
    ungroup() %>%
    select(term) %>%
    unlist()
}
top10.terms.mat

## describe each document
doc.prob.k <- tidy(lda.fit, matrix = "gamma")
doc.prob.k <- doc.prob.k %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  arrange(topic, -gamma)

table(doc.prob.k$topic)


## All years
## first run for 2010 to 2012
dtm <- petition.dfm %>%
  cast_dtm(document = title, term = pos_cleaned, value = n)

## fit LDA
K <- 5
lda.fit <- LDA(dtm, k = K)

saveRDS(lda.fit, "output/lda_out_5.rds")

## describe topics: top 20 terms for each topic
word.prob.k <- tidy(lda.fit, matrix = "beta")
N <- 20
top.terms <- word.prob.k %>%
  group_by(topic) %>%
  top_n(N, beta) %>%
  arrange(topic, -beta)
topN.terms.mat <- matrix(NA, N, K)
for (k in 1:K) {
  topN.terms.mat[, k] <- top.terms %>%
    filter(topic == k) %>%
    ungroup() %>%
    select(term) %>%
    unlist()
}

doc.prob.k <- tidy(lda.fit, matrix = "gamma")
doc.prob.k <- doc.prob.k %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  arrange(topic, -gamma)

## append year
doc.prob.k <- doc.prob.k %>%
  left_join(petition.df.idx, by = c("document" = "title"), multiple = "any")

topN.terms.mat <- as.data.frame(topN.terms.mat)
colnames(topN.terms.mat) <-
  c("energy", "telecomm", "primary school", "college", "social welfare")
writexl::write_xlsx(topN.terms.mat, "output/topic_term.xlsx")

doc.prob.k <- doc.prob.k %>%
  mutate(
    topic_label = dplyr::recode(
      topic,
      "1" = "energy",
      "2" = "telecomm",
      "3" = "primary school",
      "4" = "college",
      "5" = "social welfare"
    )
  )

## distribution of topics over time
f <- ggplot(data = doc.prob.k) +
  geom_bar(aes(x = year, fill = factor(topic_label)), position = "dodge") +
  theme_bw()
ggsave("output/topic_dist_02-12.pdf", f, width = 10, height = 5)
