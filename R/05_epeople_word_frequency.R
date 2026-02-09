## Descriptive analysis of petitions data with KoNLP
## "Fri Feb 16 15:57:10 2024"

source(here::here("R", "utilities.R"))

# Merge and process data =======================================================
years <- 2002:2023
file.names <- here(
  "data", "tidy",
  paste0("petition_konlp", years, ".rds")
)
petition.df <- as_tibble(map_dfr(file.names, readRDS))
nrow(petition.df) ## 1041038

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

nrow(petition.df) ## 770121

## further pruning
## 1) 대명사 제거: 이것, 저것, 그것, 이런, 저런, 그런, 이거, 저거, 그거, 이럴, 저럴, 그럴,
petition.df <- petition.df %>%
  mutate(pos_cleaned = str_replace_all(
    pos_cleaned,
    "이것|저것|그것|이런|저런|그런|이거|저거|그거|이럴|저럴|그럴",
    ""
  ))

## other non-essentials that get attached to words
petition.df <- petition.df %>%
  mutate(pos_cleaned = str_replace_all(
    pos_cleaned,
    "같은|같이|있는|없는|않은|않는|때문|다는|밖에|라는|라고|하는|어떤|어디|언제",
    ""
  ))

petition.df <- petition.df %>% filter(pos_cleaned != "")

## 2) 조사로 끝나는 단어들 정리: 뒷편에, 웰빙시대에, 저희도, 그런점을
petition.df <- petition.df %>%
  mutate(last_char = str_sub(pos_cleaned, -1, -1)) %>%
  mutate(last_two_char = str_sub(pos_cleaned, -2, -1)) %>%
  mutate(last_three_char = str_sub(pos_cleaned, -3, -1))

josa1 <- c("은", "는", "이", "가", "에", "을", "를", "께")
josa2 <- c("에는", "에게", "에도", "에서", "게서")
josa3 <- c("에서는", "에게는", "에게도", "에게서")
petition.df %>% filter(last_three_char %in% josa3)
petition.df %>% filter(last_two_char %in% josa2)
petition.df %>% filter(last_char %in% josa1)

petition.df <- petition.df %>%
  ## 에서는, 에게는, 에게도: delete last 3 chars
  mutate(pos_cleaned = if_else(
    last_three_char %in% josa3,
    str_sub(pos_cleaned, 1, -4), pos_cleaned
  )) %>%
  ## 에는, 에게, 게는, 게도, 에도: delete last 2 chars
  mutate(pos_cleaned = if_else(
    last_two_char %in% josa2,
    str_sub(pos_cleaned, 1, -3), pos_cleaned
  )) %>%
  ## 은,는,이,가...: delete last char
  ## (relatively high incorrect removals)
  mutate(pos_cleaned = if_else(
    last_char %in% josa1,
    str_sub(pos_cleaned, 1, -2), pos_cleaned
  ))

## 3) 단수 복수 구분 제거: 민초들 -> 민초
petition.df <- petition.df %>%
  mutate(last_char = str_sub(pos_cleaned, -1, -1))

petition.df %>% filter(last_char == "들")

petition.df <- petition.df %>%
  mutate(pos_cleaned = if_else(
    last_char == "들",
    str_sub(pos_cleaned, 1, -2), pos_cleaned
  ))


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
  saveWidget(here("output", "wc.pdf"))

wordcloud2(wc.df, figPath = "data/kor_penin.png", size = 1.5) ## doesn't work...


## another clean-up
petition.df <- petition.df %>%
  ## remove extra white spaces
  mutate(title = str_replace_all(title, "\\s+", " ")) %>%
  mutate(title = str_replace(title, "\\\"", "'")) %>% ## quotation
  mutate(title = str_replace(title, "\\\"", "'")) %>% ## quotation
  ## remove white space at the beginning and end
  mutate(title = str_trim(title, side = "both"))

## group by words in each petition
petition.dfn <- petition.df %>%
  count(title, pos_cleaned) %>%
  group_by(title) %>%
  mutate(total = sum(n)) %>%
  ungroup()
nrow(petition.dfn) ## 598582
## too few overlaps, spacing problem?!

## compute tf-idf
petition.dfn <- petition.dfn %>%
  bind_tf_idf(pos_cleaned, title, n)

quantile(petition.dfn$tf_idf, na.rm = TRUE)
##          0%          25%          50%          75%         100%
## 0.002129251  0.065440287  0.154765445  2.240160670 12.079749497

pdf(here("output", "tf_idf_density.pdf"), width = 6, height = 4)
plot(density(petition.dfn$tf_idf, na.rm = TRUE), main = "TF-IDF Density Plot")
dev.off()

## words with very low tf-idf scores
## (common words appearing across many documents)
low_tfidf <- petition.dfn %>%
  filter(tf_idf < 0.1) %>%
  select(pos_cleaned) %>%
  distinct()
## infrequent words appearing in many documents:
## 고용노동, 교육, 보건복지, 여성가족, ..., 
## 추천해서, 투표하기, 서로간, 등기, 배송료, ...
## these are the majority of words

## words with too high tf-idf scores
## (likely frequent in only a few documents)
high_tfidf <- petition.dfn %>%
  filter(tf_idf > 4) %>%
  select(pos_cleaned) %>%
  distinct()
## frequent words appearing in few documents: names of cities and regions, typos

## is it okay to remove them? probably region-specific problems...

## remove words with too low or too high tf-idf scores
petition.dfn <- petition.dfn %>%
  filter(tf_idf > 0.1 & tf_idf < 4)

dim(petition.dfn) ## 299454

## append year
petition.df.idx <- petition.df %>%
  select(title, area, year, month, pos_cleaned) %>%
  distinct()

## duplicate issue: inflates the rows by about 1000
petition.dfn <- petition.dfn %>%
  left_join(petition.df.idx, by = c("title", "pos_cleaned"))

## turn to document-term matrix
## first run for 2010 to 2012
petition.dfm <- petition.dfn %>%
  filter(year >= 2010) %>%
  cast_dfm(document = title, term = pos_cleaned, value = n)

# Basic topic model ============================================================
## fit LDA
K <- 5
lda.fit <- LDA(petition.dfm, k = K)
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
petition.dfm <- petition.dfn %>%
  cast_dfm(document = title, term = pos_cleaned, value = n)

saveRDS(petition.dfm, here("data", "DFM_Aug14.rds"))

## fit LDA
K <- 5
lda.fit <- LDA(petition.dfm, k = K)

saveRDS(lda.fit, here("output", "lda_out_5.rds"))

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
writexl::write_xlsx(
  topN.terms.mat,
  here("output", "topic_term.xlsx")
)

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
  geom_bar(aes(x = year, fill = factor(topic)), position = "dodge") +
  theme_bw()
ggsave(
  here("output", "topic_dist_02-12.pdf"),
  f, width = 10, height = 5
)


## find optimal topic number
topics_range <- seq(5, 40, by = 1) ## search grid

topic_grid_search <- FindTopicsNumber(
  petition.dfm,
  topics = topics_range,
  metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "VEM",
  control = list(seed = 123),
  mc.cores = 2,
  verbose = TRUE
  ## libpath="/Library/Frameworks/R.framework/
  ##   Versions/4.3-arm64/Resources/library"
)

plot(topic_grid_search)
