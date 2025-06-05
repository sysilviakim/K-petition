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

prop(gpt, "clarity")      ## 56.9% (w/ prompt change + filtering)
prop(gpt, "logic")        ## 20.1%
prop(gpt, "tone")         ## 86.8%
prop(gpt, "validity")     ## 49.1%

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

View(gpt)
View(gpt_sample)

# Topic selection ==============================================================
gpt_sample <- gpt %>%
  select(combination, title, contains("reason"), everything()) %>%
  arrange(combination) %>%
  ## Total 400 after filtering
  filter(grepl("부동산|연금", text)) %>%
  group_by(text) %>%
  ## Deleting duplicates, 363 observations
  slice_head(n = 1) %>%
  group_by(combination) %>%
  ## randomly select 10 rows
  slice_sample(n = 10) %>%
  select(combination, title, contains("reason"), everything()) %>%
  arrange(combination)
