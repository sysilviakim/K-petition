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


## create a full text variable
## (title + current issues + improvement plan + expected effect)
gpt <- gpt %>%
  mutate(text = paste(
    title, current_issues,
    improvement_plan, expected_effect,
    sep = "\n"
  ))

## issue-specific subsets based on keywords
emotional <- gpt %>%
  filter(grepl(text, pattern = "짜증"))

## handpick emotional + alpha petitions
emotional_df <- emotional[c(2, 8, 17, 26, 7, 9, 25), ]
openxlsx::write.xlsx(emotional_df, "emotional_samples.xlsx")

china <- gpt %>%
  filter(grepl(text, pattern = "중국"))

## other dimensions to examine
## 1. personal vs not personal
## personal: 자기 또는 지인의 직접적 경험에 기반한 처원, 자기에게 일어난 일 구체적으로
## not personal: 자기와 직접적으로 관련 없는 일, 국가적 일이나 철학적/일반론적 사건에 대한 청원

## 2. national vs local (political vs practical?)
## national: 국가적 이슈, 높은 확장성, 하지만 낮은 구체성 (i.e. 국가경제, 외교, ...)
## local: 특정 지역에 대한 이슈, 낮은 확장성, 하지만 높은 구체성

## gpt[4951,]
## "세금으로 무너지는 대한민국이 될 것!!!!"
## (long petition text about economic hardship,
## property taxes, self-employed struggles,
## real estate collapse concerns, and
## calls for property tax reform)

## some toy examples
national_df <- rbind(
  china[1, ],
  china[4, ],
  china[6, ],
  china[18, ],
  china[19, ],
  china[21, ],
  emotional[7, ]
)
openxlsx::write.xlsx(national_df, "national_samples.xlsx")

personal_df <- rbind(
  emotional[15, ],
  emotional[21, ],
  emotional[9, ]
)
openxlsx::write.xlsx(personal_df, "personal_samples.xlsx")


## assign two issues to each
## Kyusik
lowbirth <- gpt %>%
  filter(grepl(text, pattern = "출산율"))
education <- gpt %>%
  filter(grepl(text, pattern = "사교육"))

## BK
delivery <- gpt %>%
  filter(grepl(text, pattern = "배달"))
rider <- gpt %>%
  filter(grepl(text, pattern = "자전거")) ## 교통, 자전거, 킥보드

## Seo-young
real <- gpt %>%
  filter(grepl(text, pattern = "부동산"))
pension <- gpt %>%
  filter(grepl(text, pattern = "연금"))
