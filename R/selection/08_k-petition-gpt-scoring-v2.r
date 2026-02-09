# k-petition gpt-scoring
# 필요한 라이브러리 로드
library(tidyverse)
library(httr)
library(jsonlite)

# load data
# from dropbox!
data <- read_csv("pub_petition_content_2024_encoding.csv")

# Rename columns to English
colnames(data) <- c(
  "title", "area", "attachment", "status", "date_petitioned", "date_answered",
  "branch", "current_issues", "improvement_plan", "expected_effect",
  "review_content", "date_scraped", "date_implemented", "implementation_result"
)

# current_issues, improvement_plan, expected_effect 세 열을 합쳐서 bodytext_vector 생성
bodytext_vector <- data %>%
  mutate(
    combined_text = paste(
      "[현황 및 문제점]", current_issues,
      "\n\n[개선방안]", improvement_plan,
      "\n\n[기대효과]", expected_effect,
      sep = "\n\n"
    )
  ) %>%
  pull(combined_text)

# OpenAI API 키 및 엔드포인트 설정
api_key <- "---------------" # 실제 API 키로 변경
api_url <- "https://api.openai.com/v1/chat/completions"

# 평가 함수 정의
evaluate_bodytext <- function(text) {
  # 평가 기준을 포함한 프롬프트 (한국어 버전)
  prompt <- paste(
    "당신은 공공 제안을 평가하는 전문가입니다.",
    "아래 민원 내용을 꼼꼼하게 정독한 후, 평가 기준에 따라 신중하게 평가해 주세요.",
    "각 항목에 대해 1점(매우 낮음)부터 5점(매우 높음)까지 실제로 차별적인 점수를 부여해 주세요.",
    "모든 항목을 3점이나 4점에 집중시키지 말고, 민원 내용의 질에 따라 높은 점수와 낮은 점수를 명확히 구분해 주세요.",
    "응답은 반드시 다음 형식의 JSON으로 제공해 주세요.",
    "각 항목에는 점수(score: 1~5)와 그에 대한",
    "간결하면서도 구체적인 평가 이유(reason)를 포함해야 합니다.",
    "\n예시:\n",
    "{\n",
    "  \"clarity\": {\n",
    "    \"score\": 3,\n",
    "    \"reason\": \"핵심 주장은 확인되지만 전체 문장의 구조가 혼란스러워 이해에 어려움이 있습니다.\"\n",
    "  },\n",
    "  \"specificity\": {\n",
    "    \"score\": 2,\n",
    "    \"reason\": \"제안이 너무 추상적이며 구체적인 사례나 수치 제시가 부족합니다.\"\n",
    "  },\n",
    "  ...(각 항목 모두 포함)...\n",
    "}\n",
    "※ 'reason'은 각 항목의 점수를 정당화할 수 있도록, 평가 기준 중 어떤 측면이 문제되었는지를 짧고 명확하게 서술해 주세요.",
    "\n\n[평가 기준]",
    "\n1. 명확성 (clarity):",
    "- 핵심 주장이 분명하게 전달되는가?",
    "- 문장이 이해하기 쉬운가?",
    "- 전문 용어나 어려운 표현이 필요한 경우, 충분히 설명되었는가?",
    "\n2. 구체성 (specificity):",
    "- 주장이나 사실 관계가 구체적이고 명확한가?",
    "- 사례, 수치, 문서 등의 객관적 자료가 제시되었는가?",
    "- 자료의 출처가 신뢰 가능하며 진위 여부가 분명한가?",
    "- 요청 또는 해결책이 구체적으로 제시되었는가?",
    "- 해당 주장이 왜 중요한지를 충분히 설명하고 있는가?",
    "\n3. 논리성 및 일관성 (logic_and_consistency):",
    "- 주장과 근거 사이에 논리적 연결이 있는가?",
    "- 문장이 자연스럽게 연결되며 흐름이 일관적인가?",
    "- 불필요한 정보나 반복이 없는가?",
    "\n4. 형식적 완성도 (formal_completeness):",
    "- 띄어쓰기, 맞춤법 등 문법 오류는 없는가?",
    "- 문법과 표현이 올바르게 사용되었는가?",
    "- 글의 구조가 논리적으로 정리되어 있는가?",
    "\n5. 감정적 요소 (emotionality):",
    "- 감정적 표현이 과도하지 않고 민원의 목적을 해치지 않는가?",
    "- 욕설, 비방, 인신공격 등의 표현 없이 정중하고 건설적으로 작성되었는가?",
    "\n6. 타당성 및 실시가능성 (validity_and_feasibility):",
    "- 제안 내용이 현실적이고 타당한가?",
    "- 예산, 인력, 법·제도, 기술 등 제약을 고려했을 때 실시 가능성이 있는가?",
    "- 제안이 효과적이고 지속 가능한 해결책을 포함하고 있는가?",
    "- 제안이 형평성과 사회적 합의에 부합하는가?",
    "- 문제의 시급성과 긴급성을 고려하였는가?",
    "- 해당 기관의 권한과 업무 범위 내에서 해결 가능한 사안인가?",
    "\n\n[민원 내용]",
    text
  )

  # API 요청
  response <- POST(
    url = api_url,
    add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(
      model = "gpt-4o-mini-2024-07-18", # for consistency
      messages = list(
        list(role = "system", content = "당신은 공공 정책 제안(민원)을 평가하는 AI입니다."),
        list(role = "user", content = prompt)
      ),
      max_tokens = 1000,
      temperature = 0
    ), auto_unbox = TRUE),
    encode = "json"
  )

  # 실패 처리
  if (http_error(response)) {
    warning("API request failed. Status code: ", status_code(response))
    return(NA)
  }

  # 응답 파싱
  content_json <- content(response, as = "parsed", simplifyVector = FALSE)

  # GPT 응답 문자열 추출
  gpt_output <- tryCatch(
    {
      if (!is.null(content_json$choices) && length(content_json$choices) > 0) {
        content_json$choices[[1]]$message$content
      } else {
        warning("Unexpected API response format.")
        return(NA)
      }
    },
    error = function(e) {
      warning("Failed to extract GPT output.")
      return(NA)
    }
  )

  if (is.na(gpt_output) || gpt_output == "") {
    warning("GPT output is empty or NA.")
    return(NA)
  }

  # ```json 코드 블록 제거
  gpt_output_clean <- gsub("^```json\\s*|\\s*```$", "", gpt_output)


  # JSON 파싱
  parsed_result <- tryCatch(
    {
      fromJSON(gpt_output_clean, simplifyDataFrame = TRUE)
    },
    # 첨부파일만 있고 내용이 없는 경우
    error = function(e) {
      warning("Failed to parse GPT JSON. Using fallback response.")
      list(
        clarity = list(score = 0, reason = "attachment only"),
        specificity = list(score = 0, reason = "attachment only"),
        logic_and_consistency = list(score = 0, reason = "attachment only"),
        formal_completeness = list(score = 0, reason = "attachment only"),
        emotionality = list(score = 0, reason = "attachment only"),
        validity_and_feasibility = list(score = 0, reason = "attachment only")
      )
    }
  )

  return(parsed_result)
}

# 1. 디렉토리 생성
dir.create("gpt_evaluation_results_v2", showWarnings = FALSE)

# 2. 평가 수행 + 매건 저장
for (i in seq(1, length(bodytext_vector))) { # 첫 번째 데이터부터 시작
  file_path <- file.path(
    "gpt_evaluation_results_v2",
    paste0("eval_", i, ".json")
  )

  if (file.exists(file_path)) {
    next # 이미 저장된 경우 건너뜀
  }

  text <- bodytext_vector[[i]]
  if (is.na(text) || text == "") {
    warning(paste("Skipping empty bodytext at index", i))
    next
  }

  Sys.sleep(1) # API rate limit 대응

  result <- evaluate_bodytext(text)

  # JSON 형식으로 저장
  write_json(result, file_path, pretty = TRUE, auto_unbox = TRUE)
}

# 3. 저장된 JSON을 모두 읽어 평가결과 DataFrame으로 변환
files <- list.files("gpt_evaluation_results_v2", full.names = TRUE)
evaluation_df <- map_dfr(files, function(f) {
  res <- tryCatch(fromJSON(f), error = function(e) {
    return(NULL)
  })

  if (is.null(res)) {
    return(tibble(
      clarity_score = NA, clarity_reason = NA,
      specificity_score = NA, specificity_reason = NA,
      logic_and_consistency_score = NA, logic_and_consistency_reason = NA,
      formal_completeness_score = NA, formal_completeness_reason = NA,
      emotionality_score = NA, emotionality_reason = NA,
      validity_and_feasibility_score = NA, validity_and_feasibility_reason = NA
    ))
  }

  tibble(
    clarity_score = res$clarity$score,
    specificity_score = res$specificity$score,
    logic_and_consistency_score = res$logic_and_consistency$score,
    formal_completeness_score = res$formal_completeness$score,
    emotionality_score = res$emotionality$score,
    validity_and_feasibility_score = res$validity_and_feasibility$score,
    clarity_reason = res$clarity$reason,
    specificity_reason = res$specificity$reason,
    logic_and_consistency_reason = res$logic_and_consistency$reason,
    formal_completeness_reason = res$formal_completeness$reason,
    emotionality_reason = res$emotionality$reason,
    validity_and_feasibility_reason = res$validity_and_feasibility$reason
  )
})

# 4. 원본 데이터와 결합 및 전체 저장
result <- bind_cols(data[1:nrow(evaluation_df), ], evaluation_df)
write_excel_csv(result, "evaluated_data_2024_v2.csv")

# 5. 슬림 버전 저장 (리뷰 텍스트 등 제외)
result2 <- result %>%
  select(
    title, area, attachment, status, date_petitioned,
    date_answered, branch, date_scraped,
    clarity_score, specificity_score,
    logic_and_consistency_score, formal_completeness_score,
    emotionality_score, validity_and_feasibility_score
  )
write_excel_csv(result2, "evaluated_data_2024_v2_slim.csv")


# make a distribution plot
result %>%
  select(
    clarity_score, specificity_score, logic_and_consistency_score,
    formal_completeness_score,
    emotionality_score,
    validity_and_feasibility_score
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "criteria",
    values_to = "score"
  ) %>%
  ggplot(aes(x = score)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~criteria) +
  labs(
    title = "Distribution of Evaluation Scores",
    x = "Score",
    y = "Count"
  ) +
  theme_minimal()
