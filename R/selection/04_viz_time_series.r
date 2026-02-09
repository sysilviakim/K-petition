# 시계열 분석: 일별/월별 추이, 답변 완료율, 이동평균, 첨부파일 효과

# 필요한 패키지 설치 및 로드
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(zoo)

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

# 1. 시간 경과에 따른 민원 추이 (일별)
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

# 2. 월별 민원 수 및 답변 완료율
monthly_status <- petitions_data %>%
  mutate(month_petitioned = floor_date(date_petitioned, "month")) %>%
  group_by(month_petitioned) %>%
  summarise(
    total_petitions = n(),
    completed_petitions = sum(status == "답변완료", na.rm = TRUE)
  ) %>%
  mutate(completion_rate = (completed_petitions / total_petitions) * 100)

plot_monthly_count <- ggplot(
  monthly_status,
  aes(x = month_petitioned, y = total_petitions)
) +
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

plot_monthly_completion <- ggplot(
  monthly_status,
  aes(x = month_petitioned, y = completion_rate)
) +
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

# 3. 월별 민원 접수 건수 이동 평균
monthly_petition_count <- petitions_data %>%
  mutate(month = floor_date(date_petitioned, "month")) %>%
  count(month) %>%
  arrange(month) %>%
  # 3개월 이동 평균
  mutate(monthly_avg = rollmean(
    n, k = 3, fill = NA, align = "center"
  ))

ggplot(monthly_petition_count, aes(x = month)) +
  geom_line(aes(y = n, color = "월별 민원 수")) +
  geom_line(
    aes(y = monthly_avg, color = "3개월 이동 평균"),
    linewidth = 1.2
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 month") +
  theme_minimal() +
  labs(
    title = "월별 민원 접수 건수 및 3개월 이동 평균",
    x = "접수 월",
    y = "건수",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c(
    "월별 민원 수" = "steelblue",
    "3개월 이동 평균" = "firebrick"
  ))

# 4. 첨부파일 유무에 따른 답변 시간 비교
petitions_data <- petitions_data %>%
  mutate(has_attachment = ifelse(attachment == "NA", "없음", "있음"))

attachment_response <- petitions_data %>%
  filter(!is.na(response_days)) %>%
  group_by(has_attachment) %>%
  summarise(
    avg_days = mean(response_days, na.rm = TRUE),
    median_days = median(response_days, na.rm = TRUE)
  )

ggplot(
  attachment_response,
  aes(
    x = has_attachment, y = avg_days,
    fill = has_attachment
  )
) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "첨부파일 유무에 따른 평균 답변 소요일",
    x = "첨부파일",
    y = "평균 소요일"
  ) +
  theme(legend.position = "none")
