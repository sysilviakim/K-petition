# k-petition test R file

library(tidyverse)

# load data
data <- read_csv("pub_petition_corrected.csv")

glimpse(data)

# select first 100 rows of the data and save it as a new data frame
data_10 <- data %>% slice(154001:154010)

glimpse(data_10)

# 필요한 라이브러리 로드
library(httr)
library(jsonlite)

# bodytext 컬럼 추출
bodytext_vector <- data_10$bodytext

# OpenAI API 키 및 엔드포인트 설정
api_key <- "API_KEY" # 실제 API 키로 변경
api_url <- "https://api.openai.com/v1/chat/completions"

# 단일 텍스트를 GPT API에 보내 평가하는 함수
evaluate_bodytext <- function(text) {
    # 평가 기준을 포함한 프롬프트 (한국어 버전)
    prompt <- paste(
        "당신은 공공 제안을 평가하는 전문가입니다.",
        "다음 기준을 바탕으로 해당 제안을 평가하고, 각 기준에 대해 1점(매우 낮음)부터 5점(매우 높음)까지 점수를 부여해 주세요.",
        "JSON 형식으로 결과를 제공해 주세요. 추가 평가 사유 등을 결과물로 제공하지 말고, 평가 기준에 따른 점수만 부탁드립니다.",
        "\n\n**평가 기준:**",
        "- 효과성: 이 제안이 실제로 실행 가능하고, 긍정적인 영향을 미칠 가능성이 얼마나 높은가? (실시 가능성을 포함)",
        "- 구체성: 제안의 내용이 구체적으로 서술되었으며, 실행 방안이 명확한가?",
        "- 감정: 제안이 감정적으로 격앙되거나 공격적인 표현을 포함하고 있는가? (낮을수록 공격적이지 않음)",
        "- 감성적 표현: 제안이 논리적 근거보다 연민이나 감정에 호소하는 경향이 있는가?",
        "- 문법/맞춤법: 제안의 문법과 맞춤법이 올바르고 문장이 명확한가?",
        "\n\n**공개 제안 내용:**",
        text
    )

    # API 요청 전송
    response <- POST(
        url = api_url,
        add_headers(
            Authorization = paste("Bearer", api_key),
            `Content-Type` = "application/json"
        ),
        body = toJSON(list(
            model = "gpt-4",
            messages = list(
                list(role = "system", content = "당신은 공공 정책 제안(민원)을 평가하는 AI입니다."),
                list(role = "user", content = prompt)
            ),
            max_tokens = 300
        ), auto_unbox = TRUE),
        encode = "json"
    )

    # API 응답 에러 처리
    if (http_error(response)) {
        warning("API request failed. Status code: ", status_code(response))
        return(NA) # 에러 발생 시 NA 반환
    }

    # 응답 내용 파싱
    content <- content(response, as = "parsed", simplifyVector = TRUE)

    # 응답 내용 확인
    if (!is.list(content) || is.null(content$choices) || length(content$choices) == 0) {
        warning("Unexpected API response format: ", content)
        return(NA)
    }

    # GPT 응답을 JSON으로 변환
    gpt_output <- tryCatch(
        {
            content_json <- content(response, as = "parsed", simplifyVector = FALSE) # Ensure it's a list
            print(content_json) # Debugging: Print raw parsed JSON response

            # Check if the "choices" field exists and has expected structure
            if (!is.null(content_json$choices) && length(content_json$choices) > 0) {
                content_json$choices[[1]]$message$content
            } else {
                warning("Unexpected API response format: ", content_json)
                return(NA)
            }
        },
        error = function(e) {
            warning("Failed to extract GPT output: ", e$message)
            return(NA)
        }
    )

    # Parse the GPT output into JSON
    parsed_result <- tryCatch(
        fromJSON(gpt_output, simplifyDataFrame = TRUE),
        error = function(e) {
            warning("Failed to parse GPT output: ", gpt_output)
            return(NA)
        }
    )

    # 평가 결과 반환
    return(parsed_result)
}

# 모든 bodytext에 대해 평가 실행
evaluations <- map(data_10$bodytext, function(single_text) {
    if (is.na(single_text) || single_text == "") {
        warning("Skipping empty or NA bodytext.")
        return(NA)
    }
    Sys.sleep(1) # 1초 지연
    evaluate_bodytext(single_text) # 한 항목씩 전달
})

# 평가 결과를 데이터프레임으로 변환
evaluation_df <- bind_rows(evaluations)

# 기존 데이터에 평가 점수 결합
data_10 <- bind_cols(data_10, evaluation_df)

# 결과 저장
write_excel_csv(data_10, "evaluated_data_10.csv")
