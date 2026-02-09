source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Output from script #17 GPT scoring
gpt <- read_csv(here("data", "tidy", "evaluated_data_2024_final.csv")) %>%
  filter(!is.na(clarity_specificity_score)) %>%
  ## 43 rows with reasons as "attachment only"?
  filter(
    branch %in% c(
      "국토교통부", "보건복지부", "교육부", "행정안전부", "환경부",
      "경찰청", "고용노동부", "문화체육관광부", "농림축산식품부", "기획재정부",
      "법무부", "국방부", "산업통상자원부", "산림청", "식품의약품안전처",
      "국가보훈부", "여성가족부", "국세청", "소방청", "중소벤처기업부",
      "금융위원회", "인사혁신처", "과학기술정보통신부", "저출산고령사회위원회",
      "외교부", "해양수산부", "병무청", "공정거래위원회", "질병관리청",
      "방송통신위원회", "농촌진흥청", "국가유산청", "통일부", "방위사업청",
      "조달청", "국가교육위원회", "국민권익위원회", "개인정보보호위원회",
      "관세청", "해양경찰청", "우주항공청", "기상청", "대검찰청", "특허청",
      "행정중심복합도시건설청", "재외동포청", "국가인권위원회",
      "원자력안전위원회", "국무조정실", "국무총리비서실", "대통령비서실",
      "문화재청", "새만금개발청"
      ## 공단 제외
      ## 지방자치단체 제외
      ## 교육청 제외
    )
  )

## Four areas of scoring
## clarity and specificity
## logic and consistency
## tone and manner
## validity and feasibility

## 1. 내용의 명확성 및 구체성
## --- 핵심 주장과 요청이 분명하고 이해하기 쉬운가?
## --- 구체적인 사실, 사례, 자료가 제시되었는가?
## --- 필요한 경우, 용어나 배경 설명이 충분한가?

## 2. 논리성과 일관성
## --- 주장과 근거 사이에 논리적 연결이 있는가?
## --- 문장과 내용의 전개가 자연스럽고 일관적인가?

## 3. 적절한 표현과 태도
## --- 감정적 표현이 과도하지 않고 목적에 부합하는가?
## --- 비방, 욕설, 인신공격 없이 정중하게 작성되었는가?
## --- 문법, 맞춤법 등 기본적인 표현이 적절한가?

## 4. 타당성과 실현 가능성
## --- 제안이 현실적이며 예산, 법, 제도 등 여건상 실행 가능한가?
## --- 문제의 시급성, 효과성, 형평성을 고려했는가?
## --- 해당 기관의 권한과 업무 범위 내에서 처리 가능한 사안인가?
## --- 개인적 이익이 아닌 공익에 부합하는가?

## Distribution
prop(gpt, "clarity_specificity_score")
prop(gpt, "logic_consistency_score")
prop(gpt, "tone_manner_score")
prop(gpt, "validity_feasibility_score")

# Wrangle data =================================================================
## Create binary variable for each area of assessment --------------------------
## whether score is high or low (1-5 likert scale, with occasional zeros)
gpt <- gpt %>%
  mutate(
    clarity = case_when(
      clarity_specificity_score >= 4 ~ 1,
      clarity_specificity_score < 4 ~ 0
    ),
    clarity_label = case_when(
      clarity == 0 ~ "unclear",
      clarity == 1 ~ "clear"
    ),
    logic = case_when(
      logic_consistency_score >= 4 ~ 1,
      logic_consistency_score < 4 ~ 0
    ),
    logic_label = case_when(
      logic == 0 ~ "illogical",
      logic == 1 ~ "logical"
    ),
    tone = case_when(
      tone_manner_score >= 4 ~ 1,
      tone_manner_score < 4 ~ 0
    ),
    tone_label = case_when(
      tone == 0 ~ "untoned",
      tone == 1 ~ "toned"
    ),
    validity = case_when(
      validity_feasibility_score >= 4 ~ 1,
      validity_feasibility_score < 4 ~ 0
    ),
    validity_label = case_when(
      validity == 0 ~ "invalid",
      validity == 1 ~ "valid"
    )
  ) %>%
  ## Now split into 2^6 combinations, depending on whether each area has
  ## a high or a low score
  mutate(
    ## Create a new variable that is the combination of all six areas
    combination = as.factor(
      paste0(
        as.character(clarity),
        as.character(logic),
        as.character(tone),
        as.character(validity)
      )
    ),
    label = as.factor(
      paste(clarity_label, logic_label, tone_label, validity_label, sep = "-")
    )
  ) %>%
  select(combination, label, title, area, contains("reason"), everything()) %>%
  mutate(
    text = paste(
      title, current_issues, improvement_plan, expected_effect,
      sep = "\n\n"
    )
  )

prop(gpt, "clarity") ## 56.9% (w/ prompt change + filtering)
prop(gpt, "logic") ## 20.1%
prop(gpt, "tone") ## 86.8%
prop(gpt, "validity") ## 49.1%

## Create binary 4-digit patterns with 0-1 -------------------------------------
pattern01 <- c(
  "0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
  "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"
)

# Check observations for unique patterns/frequencies ===========================
## 14 patterns (2 missing)
## 13 if limited to 10 or more observations
sort(table(gpt$combination), decreasing = TRUE)
length(table(gpt$combination))

## Missing patterns ------------------------------------------------------------
pattern01[!pattern01 %in% gpt$combination]
## 1100, 1101

# Random selection =============================================================
set.seed(123)
gpt_sample <- gpt %>%
  group_by(combination) %>%
  ## randomly select 10 rows
  slice_sample(n = 10) %>%
  select(combination, title, contains("reason"), everything()) %>%
  arrange(combination)

# View(gpt)
# View(gpt_sample)

# Topic selection ==============================================================
set.seed(123)
gpt_sample <- gpt %>%
  arrange(combination) %>%
  ## Total 400 after filtering
  ## 167 부동산 only
  ## 249 연금 only
  filter(grepl("부동산|연금", text)) %>%
  group_by(text) %>%
  ## Deleting duplicates, 363 observations
  slice_head(n = 1) %>%
  group_by(combination) %>%
  ## randomly select 20 rows
  slice_sample(n = 20) %>%
  select(combination, label, title, contains("reason"), everything()) %>%
  arrange(combination)

gpt_housing <- gpt %>%
  filter(grepl("부동산", text)) %>%
  filter(
    title == "가로주택 정비사업 분할소송을 중단해주세요" |
      title == "부동산 거래 사고방지 제안" |
      grepl("건설업 부정당업자 추석 명절 사면 청구", title) |
      title == "공인중개사법 중개대상물 소재지" |
      grepl("부동산 광고 올리는게 점점 복잡해져서", title) |
      grepl("3D 프린팅", title) |
      title == "서민용 신규 오피스텔 잔금 대출, 규제 풀어주세요" |
      title == "전세 계약 시 개인 임대인의 보증 보험 가입 의무화 방안" |
      title == "은행 대출 규제 완화" |
      title == "공공 일반분양 무주택요건 강화가 필요합니다." |
      title == "재개발, 재건축에서 국민아파트 공급 확대 정책(규제 혁파)" |
      grepl("민생토론", title) |
      title == "전세 사기 방지를 위한 부동산 투기 억제 법안 제안" |
      title == "생활안정자금 추가약정서 위반에 따른 소명요청" |
      title == "전세사기피해자 지원(재산세 감면) 개선요청" |
      title == "청약에서 40·50대는 버림받은 세대인가?(가점제도 개선)" |
      grepl("부동산 거래계약서 개인정보", title) |
      grepl("8.8 부동산대책 중 매입임대주택 활성화 방안", title)
  )
nrow(gpt_housing)

write_xlsx(
  gpt_housing,
  path = here("data", "공개제안_부동산.xlsx")
)

gpt_pension <- gpt %>%
  filter(grepl("연금", text)) %>%
  filter(
    title == "노령연금 지급 대상자 변경" |
      title == "국민연금 고갈 방지를 위한 제안입니다" |
      title == "기초연금 개선(안)" |
      title == "출산 기여 연금제 제도 도입" |
      grepl("기초연금 신청시 자영업자 고용보험가입자에 대한", title) |
      title == "기초연금 지급대상자 확대 요청" |
      title == "국민연금을 개인이 관리할 수 있도록 해주세요" |
      title == "국민연금 고갈에 대한 문제 해결에 대한 방안" |
      title == "퇴직연금(DC형) 금융기관 이전의 불편함 해소 요청"
  )
nrow(gpt_pension)

write_xlsx(
  gpt_pension,
  path = here("data", "공개제안_연금.xlsx")
)

# Manual selection =============================================================
full_df <- c("배달", "킥보드", "연금", "부동산", "사교육", "저출산") %>%
  set_names(., .) %>%
  map(
    ~ here("data", paste0("공개제안_", .x, ".xlsx")) %>%
      read_xlsx() %>%
      mutate(no = row_number()) %>%
      select(no, everything())
  ) %>%
  bind_rows(.id = "category") %>%
  arrange(category, no) %>%
  mutate(
    topic = NA,
    SK = NA,
    BK = NA,
    KY = NA
  ) %>%
  select(
    category, no, topic,
    combo = combination, label,
    SK, BK, KY, title, everything()
  )

write_xlsx(
  full_df,
  path = here("data", "공개제안_전체.xlsx")
)
