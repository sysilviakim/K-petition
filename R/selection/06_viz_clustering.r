# 클러스터링 및 상위 백분위 분석

# 필요한 패키지 설치 및 로드
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
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
    ),
    total_score =
      clarity_score + specificity_score +
      logic_and_consistency_score +
      formal_completeness_score +
      emotionality_score +
      validity_and_feasibility_score
  )

# 1. 종합 점수 상위 20% 청원 추출
top_percent <- 0.2
threshold_total_score <- quantile(
  petitions_data$total_score,
  1 - top_percent, na.rm = TRUE
)
top_scored_petitions <- petitions_data %>%
  filter(total_score >= threshold_total_score)

# 상위 점수 청원들의 영역별 분포
top_area_count <- top_scored_petitions %>%
  count(area) %>%
  arrange(desc(n))

ggplot(top_area_count, aes(x = reorder(area, n), y = n, fill = area)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = paste0("상위 ", round(top_percent * 100), "% 종합 점수 청원들의 영역별 분포"),
    x = "영역",
    y = "청원 수"
  ) +
  theme(legend.position = "none") +
  scale_fill_viridis_d()

# 2. 각 점수 유형별 상위 20% 청원 추출 및 표시 함수
plot_top_score_comparison <- function(score_column, score_name, data) {
  threshold <- quantile(data[[score_column]], 1 - 0.2, na.rm = TRUE)
  top_scored <- data %>%
    mutate(is_top = ifelse(!!sym(score_column) >= threshold, "상위 20%", "그 외"))

  ggplot(top_scored, aes(x = !!sym(score_column), fill = is_top)) +
    geom_density(alpha = 0.7) +
    theme_minimal() +
    labs(
      title = paste0("상위 20% ", score_name, " 점수 청원 분포"),
      x = paste0(score_name, " 점수"),
      y = "밀도",
      fill = ""
    ) +
    scale_fill_manual(values = c("상위 20%" = "salmon", "그 외" = "lightgray"))
}

plot_clarity <- plot_top_score_comparison(
  "clarity_score", "명확성", petitions_data
)
plot_specificity <- plot_top_score_comparison(
  "specificity_score", "구체성", petitions_data
)
plot_logic <- plot_top_score_comparison(
  "logic_and_consistency_score",
  "논리일관성", petitions_data
)
plot_completeness <- plot_top_score_comparison(
  "formal_completeness_score",
  "형식완성도", petitions_data
)
plot_emotionality <- plot_top_score_comparison(
  "emotionality_score",
  "감정적측면", petitions_data
)
plot_validity <- plot_top_score_comparison(
  "validity_and_feasibility_score",
  "타당성실현가능성", petitions_data
)

grid.arrange(plot_clarity, plot_specificity, plot_logic,
  plot_completeness, plot_emotionality, plot_validity,
  ncol = 2
)

# 3. 클러스터링에 사용할 점수 데이터 준비 (NA 제외)
score_data_no_na <- petitions_data %>%
  select(
    clarity_score, specificity_score, logic_and_consistency_score,
    formal_completeness_score,
    emotionality_score,
    validity_and_feasibility_score
  ) %>%
  na.omit()

# 데이터 스케일링
scaled_score_data <- scale(score_data_no_na)

# 최적의 클러스터 수 결정 (엘보우 방법 활용)
set.seed(123)
wss <- (nrow(scaled_score_data) - 1) * sum(apply(scaled_score_data, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(scaled_score_data, centers = i)$withinss)
}
plot(
  1:10, wss, type = "b",
  xlab = "Number of Clusters",
  ylab = "Within groups sum of squares"
)

# 최적의 클러스터 수 결정 후 k 값 설정 (예: 3)
k <- 3
kmeans_result <- kmeans(scaled_score_data, centers = k, nstart = 25)

# 클러스터 결과 추가
score_data_no_na <- score_data_no_na %>%
  mutate(row_id = row_number())

kmeans_result_df <- tibble(
  row_id = as.numeric(
    rownames(scaled_score_data)
  ),
  cluster = factor(kmeans_result$cluster)
)

clustered_petitions <- petitions_data %>%
  mutate(row_id = row_number()) %>%
  left_join(kmeans_result_df, by = "row_id")

# 클러스터별 점수 분포 시각화 (Parallel Coordinates Plot)
cluster_means_long <- clustered_petitions %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(
    clarity = mean(clarity_score, na.rm = TRUE),
    specificity = mean(specificity_score, na.rm = TRUE),
    logic = mean(logic_and_consistency_score, na.rm = TRUE),
    completeness = mean(formal_completeness_score, na.rm = TRUE),
    emotionality = mean(emotionality_score, na.rm = TRUE),
    validity = mean(validity_and_feasibility_score, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -cluster,
    names_to = "score_type",
    values_to = "average_score"
  )

ggplot(
  cluster_means_long,
  aes(
    x = score_type, y = average_score,
    group = cluster, color = cluster
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "클러스터별 평균 점수 비교 (Parallel Coordinates Plot)",
    x = "점수 유형",
    y = "평균 점수",
    color = "클러스터"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d()

# 클러스터별 개수 확인 (NA 제외)
table(clustered_petitions$cluster)

# 클러스터별 영역 분포
cluster_area_distribution <- clustered_petitions %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster, area) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n))

ggplot(
  cluster_area_distribution,
  aes(
    x = reorder(area, proportion),
    y = proportion, fill = cluster
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "클러스터별 영역 분포",
    x = "영역",
    y = "비율",
    fill = "클러스터"
  ) +
  scale_fill_viridis_d()
