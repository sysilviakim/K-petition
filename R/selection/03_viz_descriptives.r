# 기본 기술통계 시각화: 영역, 상태, 응답시간, 점수 분포, 상관관계

# 필요한 패키지 설치 및 로드
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(viridis)

# CSV 데이터 불러오기
petitions_data <- read.csv("evaluated_data_2024_slim.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# 데이터 전처리
petitions_data$date_petitioned <- ymd(petitions_data$date_petitioned, quiet = TRUE)
petitions_data$date_answered <- ymd(petitions_data$date_answered, quiet = TRUE)
petitions_data$date_scraped <- ymd(petitions_data$date_scraped, quiet = TRUE)
petitions_data$response_days <- as.numeric(petitions_data$date_answered - petitions_data$date_petitioned)

petitions_data <- petitions_data %>%
  mutate(
    clarity_score = ifelse(is.na(clarity_score), 0, clarity_score),
    specificity_score = ifelse(is.na(specificity_score), 0, specificity_score),
    logic_and_consistency_score = ifelse(is.na(logic_and_consistency_score), 0, logic_and_consistency_score),
    formal_completeness_score = ifelse(is.na(formal_completeness_score), 0, formal_completeness_score),
    emotionality_score = ifelse(is.na(emotionality_score), 0, emotionality_score),
    validity_and_feasibility_score = ifelse(is.na(validity_and_feasibility_score), 0, validity_and_feasibility_score)
  )

# 1. 영역별 민원 수 막대 그래프
area_count <- petitions_data %>%
  count(area) %>%
  arrange(desc(n))

ggplot(area_count, aes(x = reorder(area, n), y = n, fill = area)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "영역별 민원 수",
    x = "영역",
    y = "민원 수"
  ) +
  theme(legend.position = "none") +
  scale_fill_viridis_d()

# 2. 민원 상태별 비율 원 그래프
status_count <- petitions_data %>%
  count(status) %>%
  mutate(prop = n / sum(n) * 100)

ggplot(status_count, aes(x = "", y = n, fill = status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(
    title = "민원 상태별 비율",
    fill = "상태"
  ) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  scale_fill_viridis_d() +
  geom_text(aes(label = paste0(round(prop), "%")),
    position = position_stack(vjust = 0.5)
  )

# 3. 답변 소요 시간 히스토그램
response_time_data <- petitions_data %>%
  filter(!is.na(response_days))

ggplot(response_time_data, aes(x = response_days)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(
    title = "민원 답변 소요 일수 분포",
    x = "소요 일수",
    y = "빈도"
  )

# 4. 영역별 점수 비교 (평균)
scores_by_area <- petitions_data %>%
  group_by(area) %>%
  summarise(
    clarity = mean(clarity_score, na.rm = TRUE),
    specificity = mean(specificity_score, na.rm = TRUE),
    logic = mean(logic_and_consistency_score, na.rm = TRUE),
    completeness = mean(formal_completeness_score, na.rm = TRUE),
    emotionality = mean(emotionality_score, na.rm = TRUE),
    validity = mean(validity_and_feasibility_score, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(clarity, specificity, logic, completeness, emotionality, validity),
    names_to = "score_type",
    values_to = "average_score"
  )

ggplot(scores_by_area, aes(x = score_type, y = average_score, fill = area)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "영역별 평균 점수 비교",
    x = "점수 유형",
    y = "평균 점수"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. 점수 간의 상관관계 시각화
score_corr <- petitions_data %>%
  select(
    clarity_score, specificity_score, logic_and_consistency_score,
    formal_completeness_score, emotionality_score, validity_and_feasibility_score
  )

corr_data <- cor(score_corr)
corr_df <- as.data.frame(as.table(corr_data))
names(corr_df) <- c("Score1", "Score2", "Correlation")

ggplot(corr_df, aes(x = Score1, y = Score2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "점수 간 상관관계 히트맵") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. 각 점수 분포 밀도 그래프
scores_long <- petitions_data %>%
  select(
    clarity_score, specificity_score, logic_and_consistency_score,
    formal_completeness_score, emotionality_score, validity_and_feasibility_score
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "score_type",
    values_to = "score"
  )

# 점수 유형 이름을 더 짧게 수정
scores_long <- scores_long %>%
  mutate(score_type = case_when(
    score_type == "clarity_score" ~ "명확성",
    score_type == "specificity_score" ~ "구체성",
    score_type == "logic_and_consistency_score" ~ "논리일관성",
    score_type == "formal_completeness_score" ~ "형식완성도",
    score_type == "emotionality_score" ~ "감정적측면",
    score_type == "validity_and_feasibility_score" ~ "타당성실현가능성",
    TRUE ~ score_type
  ))

ggplot(scores_long, aes(x = score, fill = score_type)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "각 점수 유형별 분포",
    x = "점수",
    y = "밀도",
    fill = "점수 유형"
  )

# 7. 상태별 평균 점수 비교
status_scores <- petitions_data %>%
  group_by(status) %>%
  summarise(
    clarity = mean(clarity_score),
    specificity = mean(specificity_score),
    logic = mean(logic_and_consistency_score),
    completeness = mean(formal_completeness_score),
    emotionality = mean(emotionality_score),
    validity = mean(validity_and_feasibility_score)
  ) %>%
  pivot_longer(
    cols = c(clarity, specificity, logic, completeness, emotionality, validity),
    names_to = "score_type",
    values_to = "average_score"
  )

ggplot(status_scores, aes(x = score_type, y = average_score, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "민원 상태별 평균 점수 비교",
    x = "점수 유형",
    y = "평균 점수"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8. 종합 점수 계산 및 영역별 비교
petitions_data <- petitions_data %>%
  mutate(total_score = clarity_score + specificity_score + logic_and_consistency_score +
    formal_completeness_score + emotionality_score + validity_and_feasibility_score)

area_total_score <- petitions_data %>%
  group_by(area) %>%
  summarise(avg_total_score = mean(total_score))

ggplot(area_total_score, aes(x = reorder(area, avg_total_score), y = avg_total_score, fill = area)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "영역별 평균 종합 점수",
    x = "영역",
    y = "평균 종합 점수"
  ) +
  theme(legend.position = "none")
