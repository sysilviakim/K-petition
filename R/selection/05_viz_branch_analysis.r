# 기관(부처)별 분석: 민원 수, 답변 시간, 점수 비교

# 필요한 패키지 설치 및 로드
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(viridis)

# CSV 데이터 불러오기
petitions_data <- read.csv(
  "evaluated_data_2024_slim.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

# 데이터 전처리
petitions_data$date_petitioned <- ymd(
  petitions_data$date_petitioned, quiet = TRUE
)
petitions_data$date_answered <- ymd(
  petitions_data$date_answered, quiet = TRUE
)
petitions_data$date_scraped <- ymd(
  petitions_data$date_scraped, quiet = TRUE
)
petitions_data$response_days <- as.numeric(
  petitions_data$date_answered -
    petitions_data$date_petitioned
)

petitions_data <- petitions_data %>%
  mutate(
    clarity_score = ifelse(is.na(clarity_score), 0, clarity_score),
    specificity_score = ifelse(is.na(specificity_score), 0, specificity_score),
    logic_and_consistency_score = ifelse(
      is.na(logic_and_consistency_score),
      0, logic_and_consistency_score
    ),
    formal_completeness_score = ifelse(
      is.na(formal_completeness_score),
      0, formal_completeness_score
    ),
    emotionality_score = ifelse(
      is.na(emotionality_score),
      0, emotionality_score
    ),
    validity_and_feasibility_score = ifelse(
      is.na(validity_and_feasibility_score),
      0, validity_and_feasibility_score
    )
  )

area_count <- petitions_data %>%
  count(area) %>%
  arrange(desc(n))

# 1. 기관별 민원 수 및 평균 답변 시간
branch_stats <- petitions_data %>%
  group_by(branch) %>%
  summarise(
    petition_count = n(),
    avg_response_days = mean(response_days, na.rm = TRUE)
  ) %>%
  arrange(desc(petition_count)) %>%
  head(10) # 상위 10개 기관만 표시

# 기관별 민원 수
ggplot(
  branch_stats,
  aes(
    x = reorder(branch, petition_count),
    y = petition_count, fill = branch
  )
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "상위 10개 기관별 민원 수",
    x = "기관",
    y = "민원 수"
  ) +
  theme(legend.position = "none")

# 기관별 평균 답변 시간
ggplot(
  branch_stats,
  aes(
    x = reorder(branch, avg_response_days),
    y = avg_response_days, fill = branch
  )
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "상위 10개 기관별 평균 답변 소요일",
    x = "기관",
    y = "평균 답변 소요일"
  ) +
  theme(legend.position = "none")

# 2. 주요 영역별 평균 답변 소요 시간
top_areas <- area_count %>% head(5)

area_response_time <- petitions_data %>%
  filter(!is.na(response_days) & area %in% top_areas$area) %>%
  group_by(area) %>%
  summarise(avg_response_time = mean(response_days))

ggplot(
  area_response_time,
  aes(
    x = reorder(area, avg_response_time),
    y = avg_response_time, fill = area
  )
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "주요 영역별 평균 답변 소요 시간",
    x = "영역",
    y = "평균 답변 소요 일수"
  ) +
  theme(legend.position = "none") +
  scale_fill_viridis_d()

## 중앙/지방 부처 등으로 정리 필요
# 3. 부처별 평균 점수 계산
branch_scores <- petitions_data %>%
  group_by(branch) %>%
  summarise(
    avg_clarity_score = mean(clarity_score, na.rm = TRUE),
    avg_specificity_score = mean(specificity_score, na.rm = TRUE),
    avg_logic_and_consistency_score = mean(
      logic_and_consistency_score, na.rm = TRUE
    ),
    avg_formal_completeness_score = mean(
      formal_completeness_score, na.rm = TRUE
    ),
    avg_emotionality_score = mean(
      emotionality_score, na.rm = TRUE
    ),
    avg_validity_and_feasibility_score = mean(
      validity_and_feasibility_score, na.rm = TRUE
    ),
    n = n()
  ) %>%
  filter(n >= 10)

# 상위 5개 부처 선택 함수
top_n_branches <- function(df, score_col, n = 5) {
  df %>%
    arrange(desc(!!sym(score_col))) %>%
    head(n)
}

# 각 항목별 상위 5개 부처 데이터 추출
top_clarity_branches <- top_n_branches(branch_scores, "avg_clarity_score")
top_specificity_branches <- top_n_branches(
  branch_scores, "avg_specificity_score"
)
top_logic_branches <- top_n_branches(
  branch_scores, "avg_logic_and_consistency_score"
)
top_completeness_branches <- top_n_branches(
  branch_scores, "avg_formal_completeness_score"
)
top_emotionality_branches <- top_n_branches(
  branch_scores, "avg_emotionality_score"
)
top_validity_branches <- top_n_branches(
  branch_scores,
  "avg_validity_and_feasibility_score"
)

# 시각화 함수
plot_top_branches_score <- function(df, score_col, title) {
  ggplot(
    df,
    aes(
      x = reorder(branch, !!sym(score_col)),
      y = !!sym(score_col), fill = branch
    )
  ) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = title,
      x = "부처",
      y = "평균 점수"
    ) +
    theme(legend.position = "none") +
    scale_fill_viridis_d()
}

# 각 항목별 상위 5개 부처 시각화
plot_top_branches_score(
  top_clarity_branches,
  "avg_clarity_score",
  "명확성 점수 상위 5개 부처"
)
plot_top_branches_score(
  top_specificity_branches,
  "avg_specificity_score",
  "구체성 점수 상위 5개 부처"
)
plot_top_branches_score(
  top_logic_branches,
  "avg_logic_and_consistency_score",
  "논리 및 일관성 점수 상위 5개 부처"
)
plot_top_branches_score(
  top_completeness_branches,
  "avg_formal_completeness_score",
  "형식 완성도 점수 상위 5개 부처"
)
plot_top_branches_score(
  top_emotionality_branches,
  "avg_emotionality_score",
  "감정적 측면 점수 상위 5개 부처"
)
plot_top_branches_score(
  top_validity_branches,
  "avg_validity_and_feasibility_score",
  "타당성 및 실현 가능성 점수 상위 5개 부처"
)
