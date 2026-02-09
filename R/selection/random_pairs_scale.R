# Load packages and utility functions
source(here::here("R", "utilities.R"))

library(tidyverse)
library(writexl)
# setwd("~/Dropbox/K-petition")

# 설정값 ----------------------------------------------------------------------
N <- 1200 # 총 응답자 수
P <- 10 # 응답자당 쌍비교(pairwise comparison) 문항 수
S <- 5 # 응답자당 스케일(scale) 평가 문항 수
pet_id <- 1:65 # 청원 ID 목록

set.seed(1234) # 재현 가능한 난수 생성

# 함수: 개별 응답자에게 쌍비교 10쌍 + 스케일 5개 할당 -------------------------
make_assignments <- function(pid = pet_id, P = 10, S = 5) {
  # 쌍비교용 쌍 10개 생성 (중복 없이)
  pair_sample <- replicate(P, sample(pid, 2))
  pair_sample <- apply(pair_sample, 2, sort)
  pair_df <- as_tibble(t(pair_sample)) %>%
    rename(Pair_A = V1, Pair_B = V2)

  # 스케일 평가용 청원 5개: 쌍비교에 사용되지 않은 청원 중에서만 샘플링
  used_pets <- unique(c(pair_df$Pair_A, pair_df$Pair_B))
  remaining <- setdiff(pid, used_pets)

  if (length(remaining) < S) {
    stop("사용되지 않은 청원이 5개 미만입니다. 스케일 문항을 배정할 수 없습니다.")
  }

  scale_pets <- sample(remaining, S)

  list(pair_df = pair_df, scale = scale_pets)
}

# 모든 응답자에 대해 무작위 배정 수행 ------------------------------------------
assignments <- map(1:N, ~ make_assignments())

# 쌍비교 배정 결과 정리 -------------------------------------------------------
pairwise_df <- map2_dfr(
  assignments, 1:N,
  ~ mutate(.x$pair_df, Respondent_id = .y)
)

# 스케일 배정 결과 정리 -------------------------------------------------------
scale_df <- map2_dfr(
  assignments, 1:N,
  ~ tibble(
    Respondent_id = .y,
    Petition_scale = .x$scale
  )
)

# 스케일 문항에 1~5 순서 부여
scale_df <- scale_df %>%
  group_by(Respondent_id) %>%
  mutate(Scale_order = row_number()) %>%
  ungroup()

# Excel 파일로 저장 -----------------------------------------------------------
writexl::write_xlsx(
  list(
    pairwise_assignments = pairwise_df, # 시트1: 쌍비교 문항
    scale_assignments = scale_df # 시트2: 스케일 문항
  ),
  path = paste0(
    "data/screenshots/main/final/",
    "공개제안_random_assignments_10pair_5scale.xlsx"
  )
)

# ===========================
library(showtext)
showtext_auto()

# 청원이 쌍비교에서 등장한 횟수 분포 시각화
pair_count <- pairwise_df %>%
  pivot_longer(
    cols = c(Pair_A, Pair_B),
    names_to = "pair_role",
    values_to = "Petition_ID"
  ) %>%
  count(Petition_ID, name = "pairwise_count")

# 시각화
ggplot(pair_count, aes(x = as.factor(Petition_ID), y = pairwise_count)) +
  geom_col() +
  labs(
    title = "청원별 쌍비교 등장 횟수",
    x = "청원 ID",
    y = "등장 횟수"
  ) +
  theme_minimal()

# ===========================
# 청원이 스케일 문항에 등장한 횟수 시각화

scale_count <- scale_df %>%
  count(Petition_scale, name = "scale_count") %>%
  rename(Petition_ID = Petition_scale)

# 시각화
ggplot(scale_count, aes(x = as.factor(Petition_ID), y = scale_count)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "청원별 스케일 문항 등장 횟수",
    x = "청원 ID",
    y = "등장 횟수"
  ) +
  theme_minimal()

# ===========================
# 전체 청원 등장 총합 비교 (쌍비교 + 스케일)

total_dist <- full_join(pair_count, scale_count, by = "Petition_ID") %>%
  replace_na(list(pairwise_count = 0, scale_count = 0)) %>%
  mutate(total_count = pairwise_count + scale_count)

# 시각화
ggplot(total_dist, aes(x = as.factor(Petition_ID), y = total_count)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "청원별 총 등장 횟수 (쌍비교 + 스케일)",
    x = "청원 ID",
    y = "총 등장 횟수"
  ) +
  theme_minimal()
