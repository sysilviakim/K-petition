# visualization test

# 필요한 패키지 설치 및 로드
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(viridis)

# CSV 데이터 불러오기
petitions_data <- read.csv("evaluated_data_2024_slim.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
glimpse(petitions_data)
# 데이터 전처리
# 날짜 형식 변환
petitions_data$date_petitioned <- ymd(petitions_data$date_petitioned, quiet = TRUE)
petitions_data$date_answered <- ymd(petitions_data$date_answered, quiet = TRUE)
petitions_data$date_scraped <- ymd(petitions_data$date_scraped, quiet = TRUE)

# 답변 소요일수 계산
petitions_data$response_days <- as.numeric(petitions_data$date_answered - petitions_data$date_petitioned)

# NA 처리: 점수 컬럼에서 NA를 0으로 대체
petitions_data <- petitions_data %>%
    mutate(
        clarity_score = ifelse(is.na(clarity_score), 0, clarity_score),
        specificity_score = ifelse(is.na(specificity_score), 0, specificity_score),
        logic_and_consistency_score = ifelse(is.na(logic_and_consistency_score), 0, logic_and_consistency_score),
        formal_completeness_score = ifelse(is.na(formal_completeness_score), 0, formal_completeness_score),
        emotionality_score = ifelse(is.na(emotionality_score), 0, emotionality_score),
        validity_and_feasibility_score = ifelse(is.na(validity_and_feasibility_score), 0, validity_and_feasibility_score)
    )

# 영역별 민원 수 시각화
area_count <- petitions_data %>%
    count(area) %>%
    arrange(desc(n))

# 1. 영역별 민원 수 막대 그래프
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
# NA 값을 제외하고 분석
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

# 5. 첨부파일 유무에 따른 답변 시간 비교
petitions_data <- petitions_data %>%
    mutate(has_attachment = ifelse(attachment == "NA", "없음", "있음"))

attachment_response <- petitions_data %>%
    filter(!is.na(response_days)) %>%
    group_by(has_attachment) %>%
    summarise(
        avg_days = mean(response_days, na.rm = TRUE),
        median_days = median(response_days, na.rm = TRUE)
    )

ggplot(attachment_response, aes(x = has_attachment, y = avg_days, fill = has_attachment)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(
        title = "첨부파일 유무에 따른 평균 답변 소요일",
        x = "첨부파일",
        y = "평균 소요일"
    ) +
    theme(legend.position = "none")

# 6. 기관별 민원 수 및 평균 답변 시간
branch_stats <- petitions_data %>%
    group_by(branch) %>%
    summarise(
        petition_count = n(),
        avg_response_days = mean(response_days, na.rm = TRUE)
    ) %>%
    arrange(desc(petition_count)) %>%
    head(10) # 상위 10개 기관만 표시

# 기관별 민원 수
ggplot(branch_stats, aes(x = reorder(branch, petition_count), y = petition_count, fill = branch)) +
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
ggplot(branch_stats, aes(x = reorder(branch, avg_response_days), y = avg_response_days, fill = branch)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    labs(
        title = "상위 10개 기관별 평균 답변 소요일",
        x = "기관",
        y = "평균 답변 소요일"
    ) +
    theme(legend.position = "none")
branch_stats <- petitions_data %>%
    group_by(branch) %>%
    summarise(
        petition_count = n(),
        avg_response_days = mean(response_days, na.rm = TRUE)
    ) %>%
    arrange(desc(petition_count)) %>%
    head(10) # 상위 10개 기관만 표시

# 7. 점수 간의 상관관계 시각화
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

# 8. 시간 경과에 따른 민원 추이
daily_petitions <- petitions_data %>%
    count(date_petitioned)

ggplot(daily_petitions, aes(x = date_petitioned, y = n)) +
    geom_col(fill = "skyblue") +
    theme_minimal() +
    labs(
        title = "시간 경과에 따른 민원 접수 추이",
        x = "접수일",
        y = "민원 수"
    ) +
    scale_x_date(date_labels = "%m/%d", date_breaks = "1 week")

ggplot(daily_petitions, aes(x = date_petitioned, y = n)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(
        title = "일자별 민원 접수 건수",
        x = "접수일",
        y = "민원 수"
    ) +
    scale_x_date(date_labels = "%m/%d", date_breaks = "1 day")

# 9. 종합 점수 계산 및 영역별 비교
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

# 10. 각 점수 분포 밀도 그래프
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

# 11. 상태별 평균 점수 비교
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

# extra with other approaches

# 월별 민원 수 및 답변 완료율 계산
monthly_status <- petitions_data %>%
    mutate(month_petitioned = floor_date(date_petitioned, "month")) %>%
    group_by(month_petitioned) %>%
    summarise(
        total_petitions = n(),
        completed_petitions = sum(status == "답변완료", na.rm = TRUE)
    ) %>%
    mutate(completion_rate = (completed_petitions / total_petitions) * 100)

# 월별 민원 수
plot_monthly_count <- ggplot(monthly_status, aes(x = month_petitioned, y = total_petitions)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_point(color = "steelblue", size = 3) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
    theme_minimal() +
    labs(
        title = "월별 민원 접수 추이",
        x = "접수 월",
        y = "민원 수"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 월별 답변 완료율
plot_monthly_completion <- ggplot(monthly_status, aes(x = month_petitioned, y = completion_rate)) +
    geom_line(color = "forestgreen", size = 1.2) +
    geom_point(color = "forestgreen", size = 3) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
    theme_minimal() +
    labs(
        title = "월별 민원 답변 완료율",
        x = "접수 월",
        y = "답변 완료율 (%)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(plot_monthly_count, plot_monthly_completion, ncol = 1)

# 주요 영역별 평균 답변 소요 시간
top_areas <- area_count %>% head(5) # 답변이 완료된 상위 5개 영역 선정 (NA 제외)

area_response_time <- petitions_data %>%
    filter(!is.na(response_days) & area %in% top_areas$area) %>%
    group_by(area) %>%
    summarise(avg_response_time = mean(response_days))

ggplot(area_response_time, aes(x = reorder(area, avg_response_time), y = avg_response_time, fill = area)) +
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

# 종합 점수 상위 20% 청원 추출
top_percent <- 0.2
threshold_total_score <- quantile(petitions_data$total_score, 1 - top_percent, na.rm = TRUE)
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

# 각 점수 유형별 상위 20% 청원 추출 및 표시 함수
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

# 각 점수 유형별 상위 20% 청원 분포 시각화
plot_clarity <- plot_top_score_comparison("clarity_score", "명확성", petitions_data)
plot_specificity <- plot_top_score_comparison("specificity_score", "구체성", petitions_data)
plot_logic <- plot_top_score_comparison("logic_and_consistency_score", "논리일관성", petitions_data)
plot_completeness <- plot_top_score_comparison("formal_completeness_score", "형식완성도", petitions_data)
plot_emotionality <- plot_top_score_comparison("emotionality_score", "감정적측면", petitions_data)
plot_validity <- plot_top_score_comparison("validity_and_feasibility_score", "타당성실현가능성", petitions_data)

grid.arrange(plot_clarity, plot_specificity, plot_logic,
    plot_completeness, plot_emotionality, plot_validity,
    ncol = 2
)

# 클러스터링에 사용할 점수 데이터 준비 (NA 제외)
score_data_no_na <- petitions_data %>%
    select(
        clarity_score, specificity_score, logic_and_consistency_score,
        formal_completeness_score, emotionality_score, validity_and_feasibility_score
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
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# 최적의 클러스터 수 결정 후 k 값 설정 (예: 3)
k <- 3
kmeans_result <- kmeans(scaled_score_data, centers = k, nstart = 25)

# 클러스터 결과 추가
clustered_petitions <- petitions_data %>%
    left_join(
        tibble(row_id = as.numeric(rownames(score_data_no_na)), cluster = factor(kmeans_result$cluster)),
        by = join_by("row_id" == "row_id")
    )

# row_id를 생성하여 join에 활용
score_data_no_na <- score_data_no_na %>%
    mutate(row_id = row_number())

kmeans_result_df <- tibble(row_id = as.numeric(rownames(scaled_score_data)), cluster = factor(kmeans_result$cluster))

clustered_petitions <- petitions_data %>%
    mutate(row_id = row_number()) %>%
    left_join(kmeans_result_df, by = "row_id")

# 클러스터별 점수 분포 시각화 (Parallel Coordinates Plot)
cluster_means_long <- clustered_petitions %>%
    filter(!is.na(cluster)) %>% # NA가 아닌 클러스터 정보만 사용
    group_by(cluster) %>%
    summarise(
        clarity = mean(clarity_score, na.rm = TRUE),
        specificity = mean(specificity_score, na.rm = TRUE),
        logic = mean(logic_and_consistency_score, na.rm = TRUE),
        completeness = mean(formal_completeness_score, na.rm = TRUE),
        emotionality = mean(emotionality_score, na.rm = TRUE),
        validity = mean(validity_and_feasibility_score, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = -cluster, names_to = "score_type", values_to = "average_score")

ggplot(cluster_means_long, aes(x = score_type, y = average_score, group = cluster, color = cluster)) +
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

# 클러스터별 특징 (예: 영역별 분포) 추가 분석 가능
cluster_area_distribution <- clustered_petitions %>%
    filter(!is.na(cluster)) %>%
    group_by(cluster, area) %>%
    summarise(n = n()) %>%
    mutate(proportion = n / sum(n))

ggplot(cluster_area_distribution, aes(x = reorder(area, proportion), y = proportion, fill = cluster)) +
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

library(zoo)

# 월별 민원 접수 건수 이동 평균
monthly_petition_count <- petitions_data %>%
    mutate(month = floor_date(date_petitioned, "month")) %>%
    count(month) %>%
    arrange(month) %>%
    mutate(monthly_avg = rollmean(n, k = 3, fill = NA, align = "center")) # 3개월 이동 평균

ggplot(monthly_petition_count, aes(x = month)) +
    geom_line(aes(y = n, color = "월별 민원 수")) +
    geom_line(aes(y = monthly_avg, color = "3개월 이동 평균"), linewidth = 1.2) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "3 month") +
    theme_minimal() +
    labs(
        title = "월별 민원 접수 건수 및 3개월 이동 평균",
        x = "접수 월",
        y = "건수",
        color = ""
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = c("월별 민원 수" = "steelblue", "3개월 이동 평균" = "firebrick"))



## 중앙/지방 부처 등으로 정리 필요
# 부처별 평균 점수 계산
branch_scores <- petitions_data %>%
    group_by(branch) %>%
    summarise(
        avg_clarity_score = mean(clarity_score, na.rm = TRUE),
        avg_specificity_score = mean(specificity_score, na.rm = TRUE),
        avg_logic_and_consistency_score = mean(logic_and_consistency_score, na.rm = TRUE),
        avg_formal_completeness_score = mean(formal_completeness_score, na.rm = TRUE),
        avg_emotionality_score = mean(emotionality_score, na.rm = TRUE),
        avg_validity_and_feasibility_score = mean(validity_and_feasibility_score, na.rm = TRUE),
        n = n() # 부처별 민원 수 추가
    ) %>%
    filter(n >= 10) # 너무 적은 민원 수의 부처는 제외 (선택 사항)

# 상위 5개 부처 선택 함수
top_n_branches <- function(df, score_col, n = 5) {
    df %>%
        arrange(desc(!!sym(score_col))) %>%
        head(n)
}

# 각 항목별 상위 5개 부처 데이터 추출
top_clarity_branches <- top_n_branches(branch_scores, "avg_clarity_score")
top_specificity_branches <- top_n_branches(branch_scores, "avg_specificity_score")
top_logic_branches <- top_n_branches(branch_scores, "avg_logic_and_consistency_score")
top_completeness_branches <- top_n_branches(branch_scores, "avg_formal_completeness_score")
top_emotionality_branches <- top_n_branches(branch_scores, "avg_emotionality_score")
top_validity_branches <- top_n_branches(branch_scores, "avg_validity_and_feasibility_score")

# 시각화 함수
plot_top_branches_score <- function(df, score_col, title) {
    ggplot(df, aes(x = reorder(branch, !!sym(score_col)), y = !!sym(score_col), fill = branch)) +
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
plot_top_branches_score(top_clarity_branches, "avg_clarity_score", "명확성 점수 상위 5개 부처")
plot_top_branches_score(top_specificity_branches, "avg_specificity_score", "구체성 점수 상위 5개 부처")
plot_top_branches_score(top_logic_branches, "avg_logic_and_consistency_score", "논리 및 일관성 점수 상위 5개 부처")
plot_top_branches_score(top_completeness_branches, "avg_formal_completeness_score", "형식 완성도 점수 상위 5개 부처")
plot_top_branches_score(top_emotionality_branches, "avg_emotionality_score", "감정적 측면 점수 상위 5개 부처")
plot_top_branches_score(top_validity_branches, "avg_validity_and_feasibility_score", "타당성 및 실현 가능성 점수 상위 5개 부처")
