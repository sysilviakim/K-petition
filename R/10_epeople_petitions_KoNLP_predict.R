## Use DFM to predict the response delay
## "Wed Jul 17 17:03:32 2024"

source(here::here("R", "utilities.R"))
library(glmnet)
library(xgboost)

# Load data ====================================================================
## load dfm
petition_dfm <- readRDS(here("data/DFM.rds"))

## load meta data
years <- 2002:2023
## this should be the path to the data in dropbox folder
file.names <- here(paste0("data/tidy/pub_petition_content_", years, ".csv"))
meta_df <- as_tibble(map_dfr(file.names, read_csv))

# Deduplicate ==================================================================
## duplicates detected
meta_df <- distinct(meta_df)
tb <- table(meta_df$title)
table(tb)
##     1      2      3      4      5      6      7      8      9     10     11
## 188461   6239   1061    359    200     74     35     23     13     13      8
##    12     13     14     15     16     17     18     19     20     21     23
##     7      4      3      5      3      2      6      1      1      2      2
##    24     25     37
##     1      1      1
tb[tb == 37]
## "아동복지예산 중앙환원을 촉구합니다."
meta_df %>% filter(title == "아동복지예산 중앙환원을 촉구합니다.")

## there are some common titles that people use (less than 5% of the petitions)
## not a lot of duplicates, ignore them for now
## to-do-list: merge title with petition-date to address duplicate titles

## for now: get rid of dup titles
dup_titles <- names(tb[tb > 1])
meta_df <- meta_df %>%
  filter(!title %in% dup_titles)

# Generate y for dfm (days to response) ========================================
meta_df <- meta_df %>%
  mutate(
    date_petitioned = as.Date(date_petitioned),
    date_answered = as.Date(date_answered)
  ) %>%
  mutate(
    ## days to response
    respond_delay = date_answered - date_petitioned,
    ## days from petition to implementation
    implement_petition_delay = date_implemented - date_petitioned,
    ## days from response to implementation
    implement_respond_delay = date_implemented - date_answered
  )

quantile(meta_df$respond_delay, na.rm = TRUE)
## Time differences in days
##   0%  25%  50%  75% 100%
##    1   17   29   39 4725
## some petitions get response after more than 10 years...!

quantile(meta_df$implement_petition_delay, na.rm = TRUE)
## Time differences in days
##   0%  25%  50%  75% 100%
##    1   17   27   33 3911

quantile(meta_df$implement_respond_delay, na.rm = TRUE)
## Time differences in days
##   0%  25%  50%  75% 100%
##    0    0    0    0  230
## most responses accompany almost immediate implementation
## probably the reason why responses are so slow?

# Merge y with dfm =============================================================
doc_name <- quanteda::docnames(petition_dfm)
words <- quanteda::featnames(petition_dfm)

## df <- as.matrix(petition_dfm)
## use sparse matrix
df <- Matrix(petition_dfm, sparse = TRUE)

col.idx <- colSums(df) > 1
df <- df[, col.idx] ## keep words that are used at least twice across petitions
words <- words[col.idx]

row.idx <- rowSums(df) != 0
df <- df[row.idx, ] ## keep petitions that are more than one word
doc_name <- doc_name[row.idx]

## save memory
rm(list = c("petition_dfm", "col.idx", "row.idx"))

doc_df <- tibble("docs" = doc_name)
meta_df <- meta_df %>%
  select(
    title, respond_delay, implement_petition_delay,
    implement_respond_delay, area,
    date_petitioned, date_answered, date_implemented
  )

doc_df <- doc_df %>%
  left_join(meta_df, by = c("docs" = "title"))

# Run ML algorithms ============================================================
## 1. LASSO regression: which words are predictive of the outcome?

## y = whether responded
responded <- as.matrix(ifelse(is.na(doc_df$respond_delay), 0, 1))
## responded <- as.matrix(!is.na(doc_df$respond_delay))
cv_lasso_out <- cv.glmnet(x = df, y = responded, alpha = 1, nfolds = 10)
opt_lambda <- cv_lasso_out$lambda.min
lasso_out_res <- glmnet(x = df, y = responded, alpha = 1, lambda = opt_lambda)
save(lasso_out_res, file = here("output", "lasso_out_res.Rda"))
coef_matrix <- coef(lasso_out_res)
coef_matrix
y1 <- sort(coef_matrix[coef_matrix[, 1] != 0, ])
length(y1)

y1_pos <- sort(coef_matrix[coef_matrix[, 1] > 0, ])
names(y1_pos[1:10])
## [1] "블리자드코리아" "집중폭우"       "국토교통"       "민원내용"
## [5] "경찰들"         "폭우"           "택시요금"       "공개수업"
## [9] "지원자격"       "국민제안"
y1_neg <- sort(coef_matrix[coef_matrix[, 1] < 0, ])
names(y1_neg[1:10])
## [1] "인사혁신"       "해양수산"       "여성가족"       "기획재정"
## [5] "문화체육관광"   "식품의약품안전" "서울특별"       "제주특별자치"
## [9] "울산광역"       "국세"
## if petitions contain specific subjects, they are more likely to get responded
## if petitions contain specific branches of the government or regions,
## they are less likely to get responded
## do not know if this is due to the nature of the petitions
## or the capacity of the branches that these petitions are directed

## y = days to response
## note: selection issue
days_to_response <- as.matrix(doc_df$respond_delay)
## transform to address skewness
ln_days_to_response <- log(days_to_response + 1)

## treat missings
NAs <- is.na(days_to_response)
cv_lasso_out <- cv.glmnet(
  x = df[!NAs, ], y = ln_days_to_response[!NAs, ], alpha = 1, nfolds = 10
)
opt_lambda <- cv_lasso_out$lambda.min
lasso_out_d2res <- glmnet(
  x = df[!NAs, ],
  y = ln_days_to_response[!NAs, ],
  alpha = 1, lambda = opt_lambda
)
save(lasso_out_d2res, file = here("output", "lasso_out_d2res.Rda"))
coef_matrix <- coef(lasso_out_d2res)
coef_matrix
y2 <- sort(coef_matrix[coef_matrix[, 1] != 0, ])
y2

y2_pos <- sort(coef_matrix[coef_matrix[, 1] > 0, ])
names(y2_pos[1:10])
## [1] "특별사법경찰관" "경찰관서"       "참조해"         "보도"
## [5] "기차"           "시민들"         "경우"           "제작"
## [9] "우리"           "지방"
y2_neg <- sort(coef_matrix[coef_matrix[, 1] < 0, ])
names(y2_neg[1:10])
## [1] "행정안전"   "국토교통"   "국민신문고"

## y = days to implement
## note: selection issue
days_to_implement <- as.matrix(doc_df$implement_petition_delay)
ln_days_to_implement <- log(days_to_implement + 1)

## treat missings
NAs <- is.na(days_to_implement)
cv_lasso_out <- cv.glmnet(
  x = df[!NAs, ], y = ln_days_to_implement[!NAs, ], alpha = 1, nfolds = 10
)
opt_lambda <- cv_lasso_out$lambda.min
lasso_out_d2imp <- glmnet(
  x = df[!NAs, ],
  y = ln_days_to_implement[!NAs, ],
  alpha = 1, lambda = opt_lambda
)
save(lasso_out_d2imp, file = here("output", "lasso_out_d2imp.Rda"))
coef_matrix <- coef(lasso_out_d2imp)
coef_matrix
y3 <- sort(coef_matrix[coef_matrix[, 1] != 0, ])
y3

y3_pos <- sort(coef_matrix[coef_matrix[, 1] > 0, ])
names(y3_pos[1:10])
## [1] "특별사법경찰관" "경찰관서"       "참조해"         "보도"
## [5] "기차"           "시민들"         "경우"           "제작"
## [9] "우리"           "지방"
y3_neg <- sort(coef_matrix[coef_matrix[, 1] < 0, ])
names(y3_neg[1:10])
## [1] "행정안전"   "국토교통"   "국민신문고"

## Sanity checks
assert_that(!identical(lasso_out_res, lasso_out_d2res))
assert_that(!identical(lasso_out_res, lasso_out_d2imp))
assert_that(!identical(lasso_out_d2res, lasso_out_d2imp))
assert_that(!identical(y1, y2))
assert_that(!identical(y1, y3))
assert_that(!identical(y2, y3))

## Intersections?
## y2/y3 coefs go in similar directions.
## On the other hand... no response or delayed response
sort(setdiff(intersect(names(y1[y1 < 0]), names(y2[y2 > 0])), "(Intercept)"))
#  [1] "가능"     "같습니"   "개발"     "겁니"     "고등학교" "납부"     "누구"     "뉴스"
#  [9] "대학"     "도움"     "만원"     "몇자"     "무궁화"   "문제점"   "바랍니"   "부모"
# [17] "사고"     "사람"     "사용"     "생각"     "소식"     "시간"     "시중"     "시행"
# [25] "실제"     "아이들"   "안녕"     "여건"     "우리나라" "있습니"   "자격증"   "전기"
# [33] "제정"     "출근"     "표시"     "하루"     "학교"     "한번"     "합니"     "현행"
# [41] "현황"

sort(setdiff(intersect(names(y1[y1 > 0]), names(y2[y2 < 0])), "(Intercept)"))
# [1] "국민신문고" "국토교통"

setdiff(
  intersect(intersect(names(y1), names(y2)), names(y3)),
  "(Intercept)"
)
#  [1] "합니"     "안녕"     "같습니"   "몇자"     "하루"     "누구"     "사용"
#  [8] "뉴스"     "소식"     "실제"     "가능"     "아이들"   "생각"     "현행"
# [15] "표시"     "우리나라" "문제점"   "한번"     "시간"     "사람"     "도움"
# [22] "대학"     "사고"     "시행"     "출근"     "개발"     "현황"

## predictive analysis
## use 70% of data as training set
samp_idx <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train_df <- df[samp_idx, ]
test_df <- df[!samp_idx, ]

## 1. whether responded
train_responded <- responded[samp_idx, ]
test_responded <- responded[!samp_idx, ]

cv_lasso_out <-
  cv.glmnet(x = train_df, y = train_responded, alpha = 1, nfolds = 10)
opt_lambda <- cv_lasso_out$lambda.min
lasso_out_res <-
  glmnet(x = train_df, y = train_responded, alpha = 1, lambda = opt_lambda)

pred_responded <- predict(lasso_out_res, s = opt_lambda, newx = train_df)
pred_out_train <- cbind(pred_responded, train_responded)

pred_responded <- predict(lasso_out_res, s = opt_lambda, newx = test_df)
pred_out_test <- cbind(pred_responded, test_responded)

pdf("output/lasso_responded.pdf", width = 10, height = 6)
par(mfrow = c(1, 2))
plot(
  pred_out_train,
  xlab = "Predicted Vals",
  ylab = "True Vals",
  main = "Whether Responded: Predict on Train Set"
)
plot(
  pred_out_test,
  xlab = "Predicted Vals",
  ylab = "True Vals",
  main = "Whether Responded: Predict on Test Set"
)
dev.off()

## 2. days to response
train_days_to_response <- log(days_to_response[samp_idx, ] + 1)
test_days_to_response <- log(days_to_response[!samp_idx, ] + 1)

NAs <- is.na(train_days_to_response)
cv_lasso_out <- cv.glmnet(
  x = train_df[!NAs, ],
  y = train_days_to_response[!NAs],
  alpha = 1, nfolds = 10
)
opt_lambda <- cv_lasso_out$lambda.min
lasso_out_d2res <- glmnet(
  x = train_df[!NAs, ],
  y = train_days_to_response[!NAs],
  alpha = 1, lambda = opt_lambda
)

pred_d2res <- predict(lasso_out_d2res, s = opt_lambda, newx = train_df)
pred_out_train <- cbind(pred_d2res, train_days_to_response)

pred_d2res <- predict(lasso_out_d2res, s = opt_lambda, newx = test_df)
pred_out_test <- cbind(pred_d2res, test_days_to_response)
pdf("output/lasso_d2r.pdf", width = 10, height = 6)
plot(
  pred_out_train,
  xlab = "Predicted Vals",
  ylab = "True Vals",
  main = "Days to Response: Predict on Train Set"
)
abline(a = 0, b = 1, col = "tomato")
plot(
  pred_out_test,
  xlab = "Predicted Vals",
  ylab = "True Vals",
  main = "Days to Response: Predict on Test Set"
)
abline(a = 0, b = 1, col = "tomato")
dev.off()

## 3. days to implement
train_days_to_implement <- log(days_to_implement[samp_idx, ] + 1)
test_days_to_implement <- log(days_to_implement[!samp_idx, ] + 1)

NAs <- is.na(train_days_to_implement)
cv_lasso_out <- cv.glmnet(
  x = train_df[!NAs, ],
  y = train_days_to_implement[!NAs],
  alpha = 1, nfolds = 10
)
opt_lambda <- cv_lasso_out$lambda.min
lasso_out_d2imp <- glmnet(
  x = train_df[!NAs, ],
  y = train_days_to_implement[!NAs],
  alpha = 1, lambda = opt_lambda
) ## all coefficients to 0

pred_d2imp <- predict(lasso_out_d2imp, s = opt_lambda, newx = train_df)
pred_out_train <- cbind(pred_d2imp, train_days_to_implement)

pred_d2imp <- predict(lasso_out_d2imp, s = opt_lambda, newx = test_df)
pred_out_test <- cbind(pred_d2imp, test_days_to_implement)
pdf("output/lasso_d2i.pdf", width = 10, height = 6)
par(mfrow = c(1, 2))
plot(
  pred_out_train,
  xlab = "Predicted Vals",
  ylab = "True Vals",
  main = "Days to Implementation: Predict on Train Set"
)
abline(a = 0, b = 1, col = "tomato")
plot(
  pred_out_test,
  xlab = "Predicted Vals",
  ylab = "True Vals",
  main = "Days to Implementation: Predict on Test Set"
)
abline(a = 0, b = 1, col = "tomato")
dev.off()

## convert to DMatrix for optimal performance
train_dmat <- xgb.DMatrix(data = train_df, label = train_responded)

## set model parameters
params <- list(
  objective = "binary:logistic",
  max_depth = 6, # Maximum depth of the trees
  eta = 0.3, # Learning rate
  nthread = 2 # Number of parallel threads to use
)

## 1. whether responded
xgb_out_res <- xgb.train(
  params = params,
  data = train_dmat,
  nrounds = 100
)
