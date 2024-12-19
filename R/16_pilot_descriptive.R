# 241219 pilot descriptive statistics
# 50

# Load libraries
library(tidyverse)

# Load data
data <- read_csv("data/pilot/raw_data.csv")

# Descriptive statistics
# SQ1: gender
data %>%
  group_by(SQ1) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    gender = case_when(
      SQ1 == 1 ~ "남자",
      SQ1 == 2 ~ "여자"
    ),
    percentage = n / sum(n) * 100
  ) %>%
  select(gender, n, percentage)

data %>%
  mutate(
    gender = case_when(
      SQ1 == 1 ~ "남성",
      SQ1 == 2 ~ "여성"
    )
  ) %>%
  ggplot(aes(x = gender, fill = gender)) +
  geom_bar(color = "black", alpha = 0.75) +
  scale_fill_manual(values = c("남성" = "blue", "여성" = "red")) +
  labs(
    x = "성별",
    y = "응답자 수",
    title = "성별 분포"
  ) +
  theme_minimal(base_family = "AppleGothic")

# SQ2_code: age_group
data %>%
  group_by(SQ2_code) %>%
  summarise(
    n = n(),
    percentage = n / sum(n) * 100,
    .groups = "drop"
  ) %>%
  arrange(SQ2_code)

data %>%
  mutate(
    age_group = case_when(
      SQ2_code == 1 ~ "만 20~29세",
      SQ2_code == 2 ~ "만 30~39세",
      SQ2_code == 3 ~ "만 40~49세",
      SQ2_code == 4 ~ "만 50~59세",
      SQ2_code == 5 ~ "만 60세 이상"
    )
  ) %>%
  ggplot(aes(x = age_group, fill = age_group)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(
    x = "연령대",
    y = "응답자 수",
    title = "연령대 분포"
  ) +
  scale_fill_manual(values = c(
    "만 20~29세" = "lightblue",
    "만 30~39세" = "skyblue",
    "만 40~49세" = "steelblue",
    "만 50~59세" = "dodgerblue",
    "만 60세 이상" = "blue"
  )) +
  theme_minimal(base_family = "AppleGothic")

# SQ3: education
data %>%
  group_by(SQ3) %>%
  summarise(
    n = n()
  ) %>%
  arrange(SQ3)

data %>%
  mutate(
    education_level = case_when(
      SQ3 == 1 ~ "중학교 졸업 이하",
      SQ3 == 2 ~ "고등학교 졸업",
      SQ3 == 3 ~ "대학 재학 중",
      SQ3 == 4 ~ "대학 졸업",
      SQ3 == 5 ~ "대학원 재학 중",
      SQ3 == 6 ~ "대학원 졸업"
    )
  ) %>%
  ggplot(aes(x = education_level, fill = education_level)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(
    x = "학력",
    y = "응답자 수",
    title = "학력 분포"
  ) +
  scale_fill_manual(values = c(
    "중학교 졸업 이하" = "lightblue",
    "고등학교 졸업" = "skyblue",
    "대학 재학 중" = "steelblue",
    "대학 졸업" = "dodgerblue",
    "대학원 재학 중" = "navy",
    "대학원 졸업" = "blue"
  )) +
  theme_minimal(base_family = "AppleGothic")

# SQ4: region
region_data <- data %>%
  mutate(
    region = case_when(
      SQ4 == 1 ~ "서울",
      SQ4 == 2 ~ "부산",
      SQ4 == 3 ~ "대구",
      SQ4 == 4 ~ "인천",
      SQ4 == 5 ~ "광주",
      SQ4 == 6 ~ "대전",
      SQ4 == 7 ~ "울산",
      SQ4 == 8 ~ "경기",
      SQ4 == 9 ~ "강원",
      SQ4 == 10 ~ "충북",
      SQ4 == 11 ~ "충남",
      SQ4 == 12 ~ "전북",
      SQ4 == 13 ~ "전남",
      SQ4 == 14 ~ "경북",
      SQ4 == 15 ~ "경남",
      SQ4 == 16 ~ "제주"
    )
  ) %>%
  group_by(region) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

region_data %>%
  ggplot(aes(x = reorder(region, -n), y = n, fill = region)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    x = "지역",
    y = "응답자 수",
    title = "지역별 응답자 분포"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# SQ5: marital_status
data %>%
  mutate(
    marital_status = case_when(
      SQ5 == 1 ~ "기혼 (유자녀)",
      SQ5 == 2 ~ "기혼 (무자녀)",
      SQ5 == 3 ~ "미혼",
      SQ5 == 4 ~ "기타 (이혼, 사별 등)"
    )
  ) %>%
  group_by(marital_status) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

data %>%
  mutate(
    marital_status = case_when(
      SQ5 == 1 ~ "기혼 (유자녀)",
      SQ5 == 2 ~ "기혼 (무자녀)",
      SQ5 == 3 ~ "미혼",
      SQ5 == 4 ~ "기타 (이혼, 사별 등)"
    )
  ) %>%
  ggplot(aes(x = marital_status, fill = marital_status)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(
    x = "결혼 여부",
    y = "응답자 수",
    title = "결혼 여부 분포"
  ) +
  scale_fill_manual(values = c(
    "기혼 (유자녀)" = "skyblue",
    "기혼 (무자녀)" = "lightgreen",
    "미혼" = "orange",
    "기타 (이혼, 사별 등)" = "pink"
  )) +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# SQ6: children
data %>%
  mutate(
    has_children = case_when(
      SQ6 == 1 ~ "있다",
      SQ6 == 2 ~ "없다"
    )
  ) %>%
  group_by(has_children) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

data %>%
  mutate(
    has_children = case_when(
      SQ6 == 1 ~ "있다",
      SQ6 == 2 ~ "없다"
    )
  ) %>%
  ggplot(aes(x = has_children, fill = has_children)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(
    x = "자녀 유무",
    y = "응답자 수",
    title = "자녀 유무 분포"
  ) +
  scale_fill_manual(values = c(
    "있다" = "lightblue",
    "없다" = "orange"
  )) +
  theme_minimal(base_family = "AppleGothic")

# SQ7: monthly_income
data %>%
  mutate(
    monthly_income = case_when(
      SQ7 == 1 ~ "100만원 미만",
      SQ7 == 2 ~ "100만원 이상-200만원 미만",
      SQ7 == 3 ~ "200만원 이상-300만원 미만",
      SQ7 == 4 ~ "300만원 이상-400만원 미만",
      SQ7 == 5 ~ "400만원 이상-500만원 미만",
      SQ7 == 6 ~ "500만원 이상-600만원 미만",
      SQ7 == 7 ~ "600만원 이상-700만원 미만",
      SQ7 == 8 ~ "700만원 이상-1천만원 미만",
      SQ7 == 9 ~ "1천만원 이상-1천 5백만원 미만",
      SQ7 == 10 ~ "1천 5백만원 이상-2천만원 미만",
      SQ7 == 11 ~ "2천만원 이상"
    )
  ) %>%
  group_by(monthly_income) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

data %>%
  mutate(
    monthly_income = factor(case_when(
      SQ7 == 1 ~ "100만원 미만",
      SQ7 == 2 ~ "100만원 이상-200만원 미만",
      SQ7 == 3 ~ "200만원 이상-300만원 미만",
      SQ7 == 4 ~ "300만원 이상-400만원 미만",
      SQ7 == 5 ~ "400만원 이상-500만원 미만",
      SQ7 == 6 ~ "500만원 이상-600만원 미만",
      SQ7 == 7 ~ "600만원 이상-700만원 미만",
      SQ7 == 8 ~ "700만원 이상-1천만원 미만",
      SQ7 == 9 ~ "1천만원 이상-1천 5백만원 미만",
      SQ7 == 10 ~ "1천 5백만원 이상-2천만원 미만",
      SQ7 == 11 ~ "2천만원 이상"
    ), levels = c(
      "100만원 미만",
      "100만원 이상-200만원 미만",
      "200만원 이상-300만원 미만",
      "300만원 이상-400만원 미만",
      "400만원 이상-500만원 미만",
      "500만원 이상-600만원 미만",
      "600만원 이상-700만원 미만",
      "700만원 이상-1천만원 미만",
      "1천만원 이상-1천 5백만원 미만",
      "1천 5백만원 이상-2천만원 미만",
      "2천만원 이상"
    ))
  ) %>%
  ggplot(aes(x = monthly_income, fill = monthly_income)) +
  geom_bar(color = "black", alpha = 0.7) +
  labs(
    x = "월소득",
    y = "응답자 수",
    title = "월소득 분포"
  ) +
  scale_fill_manual(values = c(
    "100만원 미만" = "lightblue",
    "100만원 이상-200만원 미만" = "skyblue",
    "200만원 이상-300만원 미만" = "steelblue",
    "300만원 이상-400만원 미만" = "dodgerblue",
    "400만원 이상-500만원 미만" = "navy",
    "500만원 이상-600만원 미만" = "orange",
    "600만원 이상-700만원 미만" = "coral",
    "700만원 이상-1천만원 미만" = "pink",
    "1천만원 이상-1천 5백만원 미만" = "purple",
    "1천 5백만원 이상-2천만원 미만" = "red",
    "2천만원 이상" = "darkred"
  )) +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# SQ8: occupation
occupation_data <- data %>%
  mutate(
    occupation = case_when(
      SQ8 == 1 ~ "직장인",
      SQ8 == 2 ~ "전업주부",
      SQ8 == 3 ~ "대학생/대학원생",
      SQ8 == 4 ~ "전문직",
      SQ8 == 5 ~ "서비스/영업직",
      SQ8 == 6 ~ "농림어업",
      SQ8 == 7 ~ "자영업/개인사업",
      SQ8 == 8 ~ "공무원/교사",
      SQ8 == 9 ~ "프리랜서",
      SQ8 == 10 ~ "기술직",
      SQ8 == 11 ~ "무직/퇴직/은퇴/취업준비생",
      SQ8 == 12 ~ "기타"
    )
  ) %>%
  group_by(occupation) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n)) %>%
  {
    print(.)
    .
  }

occupation_data %>%
  ggplot(aes(x = reorder(occupation, -n), y = n, fill = occupation)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    x = "직업",
    y = "응답자 수",
    title = "직업 분포"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############################
# Q1: satisfaction
q1_stats <- data %>%
  summarise(
    mean = mean(Q1, na.rm = TRUE),
    median = median(Q1, na.rm = TRUE),
    sd = sd(Q1, na.rm = TRUE),
    min = min(Q1, na.rm = TRUE),
    max = max(Q1, na.rm = TRUE)
  ) %>%
  {
    print(.)
    .
  }

data %>%
  ggplot(aes(x = Q1)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    x = "삶의 만족도 점수",
    y = "응답자 수",
    title = "삶의 만족도 분포"
  ) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme_minimal(base_family = "AppleGothic")

# Q2: ideology
q2_stats <- data %>%
  summarise(
    mean = mean(Q2, na.rm = TRUE),
    median = median(Q2, na.rm = TRUE),
    sd = sd(Q2, na.rm = TRUE),
    min = min(Q2, na.rm = TRUE),
    max = max(Q2, na.rm = TRUE)
  ) %>%
  {
    print(.)
    .
  }

data %>%
  ggplot(aes(x = Q2)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black", alpha = 0.7) +
  labs(
    x = "이념 성향 점수",
    y = "응답자 수",
    title = "이념 성향 분포"
  ) +
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  theme_minimal(base_family = "AppleGothic")

# Q3: party_id
q3_stats <- data %>%
  mutate(
    party = case_when(
      Q3 == 1 ~ "더불어민주당",
      Q3 == 2 ~ "국민의힘",
      Q3 == 3 ~ "조국혁신당",
      Q3 == 4 ~ "개혁신당",
      Q3 == 5 ~ "진보당",
      Q3 == 6 ~ "기본소득당",
      Q3 == 7 ~ "사회민주당",
      Q3 == 8 ~ "기타"
    )
  ) %>%
  group_by(party) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n)) %>%
  {
    print(.)
    .
  }

q3_stats %>%
  ggplot(aes(x = reorder(party, -n), y = n, fill = party)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    x = "정당",
    y = "응답자 수",
    title = "정당 선호 분포"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Q4: vote
q4_stats <- data %>%
  mutate(
    candidate = case_when(
      Q4 == 1 ~ "더불어민주당 (이재명)",
      Q4 == 2 ~ "국민의힘 (윤석열)",
      Q4 == 3 ~ "정의당 (심상정)",
      Q4 == 4 ~ "국민의당 (안철수)",
      Q4 == 5 ~ "기타",
      Q4 == 6 ~ "투표하지 않음"
    )
  ) %>%
  group_by(candidate) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n)) %>%
  {
    print(.)
    .
  }

q4_stats %>%
  ggplot(aes(x = reorder(candidate, -n), y = n, fill = candidate)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    x = "후보자",
    y = "응답자 수",
    title = "20대 대통령 선거 투표 선호 분포"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Q5: trust_populism
q5_stats <- data %>%
  summarise(
    Q5_1_mean = mean(Q5_1, na.rm = TRUE),
    Q5_1_median = median(Q5_1, na.rm = TRUE),
    Q5_1_sd = sd(Q5_1, na.rm = TRUE),
    Q5_2_mean = mean(Q5_2, na.rm = TRUE),
    Q5_2_median = median(Q5_2, na.rm = TRUE),
    Q5_2_sd = sd(Q5_2, na.rm = TRUE),
    Q5_3_mean = mean(Q5_3, na.rm = TRUE),
    Q5_3_median = median(Q5_3, na.rm = TRUE),
    Q5_3_sd = sd(Q5_3, na.rm = TRUE),
    Q5_4_mean = mean(Q5_4, na.rm = TRUE),
    Q5_4_median = median(Q5_4, na.rm = TRUE),
    Q5_4_sd = sd(Q5_4, na.rm = TRUE),
    Q5_5_mean = mean(Q5_5, na.rm = TRUE),
    Q5_5_median = median(Q5_5, na.rm = TRUE),
    Q5_5_sd = sd(Q5_5, na.rm = TRUE),
    Q5_6_mean = mean(Q5_6, na.rm = TRUE),
    Q5_6_median = median(Q5_6, na.rm = TRUE),
    Q5_6_sd = sd(Q5_6, na.rm = TRUE)
  ) %>%
  {
    print(.)
    .
  }

data %>%
  ggplot(aes(x = Q5_1)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black", alpha = 0.7) +
  labs(
    x = "Q5_1 응답 점수",
    y = "응답자 수",
    title = "Q5_1: 한국 사회는 신뢰가 높은 사회이다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

data %>%
  ggplot(aes(x = Q5_2)) +
  geom_histogram(binwidth = 1, fill = "coral", color = "black", alpha = 0.7) +
  labs(
    x = "Q5_2 응답 점수",
    y = "응답자 수",
    title = "Q5_2: 한국 사회는 공정과 원칙이 지켜지는 사회이다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

data %>%
  ggplot(aes(x = Q5_3)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(
    x = "Q5_3 응답 점수",
    y = "응답자 수",
    title = "Q5_3: 한국 사회는 사회적 연대가 잘 이루어지는 사회이다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

data %>%
  ggplot(aes(x = Q5_4)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.7) +
  labs(
    x = "Q5_4 응답 점수",
    y = "응답자 수",
    title = "Q5_4: 한국 사회는 국민이 주인인 사회이다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

data %>%
  ggplot(aes(x = Q5_5)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
  labs(
    x = "Q5_5 응답 점수",
    y = "응답자 수",
    title = "Q5_5: 정책 결정은 국민들에 의해 이루어져야 한다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

data %>%
  ggplot(aes(x = Q5_6)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    x = "Q5_6 응답 점수",
    y = "응답자 수",
    title = "Q5_6: 엘리트 집단이 국민을 무시하고 있다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

# Q6: petition_usage
# collaps columns into one column
q6_long <- data %>%
  select(starts_with("Q6_")) %>%  
  pivot_longer(
    cols = starts_with("Q6_"),
    names_to = "question",
    values_to = "response"
  ) %>%
  filter(!is.na(response)) 

q6_stats <- q6_long %>%
  mutate(
    response_label = case_when(
      response == 1 ~ "국민신문고",
      response == 2 ~ "청와대 국민청원 게시판",
      response == 3 ~ "국민제안 게시판",
      response == 4 ~ "청원24",
      response == 5 ~ "국회 전자청원",
      response == 6 ~ "기타",
      response == 7 ~ "이용한 적 없음"
    )
  ) %>%
  group_by(response_label) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n)) %>%
  { print(.); . }

q6_stats %>%
  ggplot(aes(x = reorder(response_label, -n), y = n, fill = response_label)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    x = "공개 청원/민원 제도",
    y = "응답자 수",
    title = "공개 청원/민원 제도 사용 빈도"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Q7: attention_check
q7_long <- data %>%
  select(starts_with("Q7_")) %>% 
  pivot_longer(
    cols = starts_with("Q7_"),
    names_to = "question",
    values_to = "response"
  ) 

q7_grouped <- q7_long %>%
  mutate(
    respondent_id = ceiling(row_number() / length(unique(q7_long$question))) 
  ) %>%
  group_by(respondent_id) %>%
  summarise(
    contains_1 = any(response == 1),  
    contains_2 = any(response == 2)  
  ) %>%
  mutate(
    attention_pass = ifelse(contains_1 & contains_2, 1, 0)  
  )

q7_check <- q7_grouped %>%
  summarise(
    total_respondents = n(), 
    passed_attention_check = sum(attention_pass, na.rm = TRUE),  
    pass_rate = passed_attention_check / total_respondents * 100  
  ) %>%
  { print(.); . }

# Q8: usage_reason
q8_long <- data %>%
  select(starts_with("Q8_")) %>%  # Q8 관련 열만 선택
  pivot_longer(
    cols = starts_with("Q8_"),
    names_to = "question",
    values_to = "response"
  ) %>%
  filter(!is.na(response))

q8_stats <- q8_long %>%
  mutate(
    response_label = case_when(
      response == 1 ~ "누군가의 진심 어린 청원에 도움을 주고 싶어서",
      response == 2 ~ "사회 이슈 개선에 도움이 되고 싶어서",
      response == 3 ~ "다양한 의견이 모이면 변화가 일어날 것 같아서",
      response == 4 ~ "세상을 바꿔야 한다는 생각 때문에",
      response == 5 ~ "사회 이슈에 대한 나의 생각을 적극적으로 표현하고 싶어서",
      response == 6 ~ "기타"
    )
  ) %>%
  group_by(response_label) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n)) %>%
  { print(.); . }

q8_stats %>%
  ggplot(aes(x = reorder(response_label, -n), y = n, fill = response_label)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    x = "이유",
    y = "응답자 수",
    title = "공개 청원/민원 제도를 활용한 주요 이유"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    axis.text.x = element_text(angle = 10, hjust = 0.6),
    legend.position = "none"  
  )

# Q9: usage_experience
q9_stats <- data %>%
  summarise(
    Q9_1_mean = mean(Q9_1, na.rm = TRUE),
    Q9_1_median = median(Q9_1, na.rm = TRUE),
    Q9_1_sd = sd(Q9_1, na.rm = TRUE),
    Q9_2_mean = mean(Q9_2, na.rm = TRUE),
    Q9_2_median = median(Q9_2, na.rm = TRUE),
    Q9_2_sd = sd(Q9_2, na.rm = TRUE),
    Q9_3_mean = mean(Q9_3, na.rm = TRUE),
    Q9_3_median = median(Q9_3, na.rm = TRUE),
    Q9_3_sd = sd(Q9_3, na.rm = TRUE),
    Q9_4_mean = mean(Q9_4, na.rm = TRUE),
    Q9_4_median = median(Q9_4, na.rm = TRUE),
    Q9_4_sd = sd(Q9_4, na.rm = TRUE),
    Q9_5_mean = mean(Q9_5, na.rm = TRUE),
    Q9_5_median = median(Q9_5, na.rm = TRUE),
    Q9_5_sd = sd(Q9_5, na.rm = TRUE)
  ) %>%
  { print(.); . }

data %>%
  ggplot(aes(x = Q9_1)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black", alpha = 0.7) +
  labs(
    x = "Q9_1 응답 점수",
    y = "응답자 수",
    title = "Q9_1: 많은 사람들과 함께 공감하고 소통한다는 것이 보기 좋았다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

data %>%
  ggplot(aes(x = Q9_2)) +
  geom_histogram(binwidth = 1, fill = "coral", color = "black", alpha = 0.7) +
  labs(
    x = "Q9_2 응답 점수",
    y = "응답자 수",
    title = "Q9_2: 사회적 연대가 이루어지고 있다는 것을 느꼈다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

data %>%
  ggplot(aes(x = Q9_3)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(
    x = "Q9_3 응답 점수",
    y = "응답자 수",
    title = "Q9_3: 국가가 국민과 소통하고 있다는 생각을 가지게 되었다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

data %>%
  ggplot(aes(x = Q9_4)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.7) +
  labs(
    x = "Q9_4 응답 점수",
    y = "응답자 수",
    title = "Q9_4: 특정 세력 및 정파에 의한 여론몰이가 걱정스러웠다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

data %>%
  ggplot(aes(x = Q9_5)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
  labs(
    x = "Q9_5 응답 점수",
    y = "응답자 수",
    title = "Q9_5: 청원이 지나치게 무분별하게 올라오는 듯한 느낌이 들었다."
  ) +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  theme_minimal(base_family = "AppleGothic")

# Q10: improvement
q10_stats <- data %>%
  group_by(Q10) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    response_label = case_when(
      Q10 == 1 ~ "청원/민원 처리 과정의 투명성 강화",
      Q10 == 2 ~ "답변의 신속성과 실효성 향상",
      Q10 == 3 ~ "청원/민원 등록 기준 강화 (무분별한 청원 방지)",
      Q10 == 4 ~ "사용자 편의를 위한 플랫폼 개선 (예: UX/UI 개선)",
      Q10 == 5 ~ "홍보 강화로 국민 접근성 확대",
      Q10 == 6 ~ "기타"
    )
  ) %>%
  arrange(desc(n)) %>%
  { print(.); . }

q10_stats %>%
  ggplot(aes(x = reorder(response_label, -n), y = n, fill = response_label)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    x = "개선 사항",
    y = "응답자 수",
    title = "공개 청원/민원 제도의 개선이 필요한 주요 사항"
  ) +
  theme_minimal(base_family = "AppleGothic") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.95), 
    legend.position = "none" 
  )
