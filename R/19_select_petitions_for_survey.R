source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Output from script #17 GPT scoring
gpt <- read_csv(here("data", "tidy", "evaluated_data_2024_v2.csv")) %>%
  filter(!is.na(clarity_score)) %>%
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

## Six areas of scoring
## clarity
## specificity
## logic/consistency
## formal completeness
## emotionality
## validity and feasibility

## Distribution
prop(gpt, "clarity_score")
prop(gpt, "specificity_score")
prop(gpt, "logic_and_consistency_score")
prop(gpt, "formal_completeness_score")
prop(gpt, "emotionality_score")
prop(gpt, "validity_and_feasibility_score")

# Wrangle data =================================================================
## Create binary variable for each area of assessment --------------------------
## whether score is high or low (1-5 likert scale, with occasional zeros)
gpt <- gpt %>%
  mutate(
    clarity = case_when(
      clarity_score >= 4 ~ 1,
      clarity_score < 4 ~ 0
    ),
    clarity_label = case_when(
      clarity == 0 ~ "unclear",
      clarity == 1 ~ "clear"
    ),
    specificity = case_when(
      specificity_score >= 4 ~ 1,
      specificity_score < 4 ~ 0
    ),
    specificity_label = case_when(
      specificity == 0 ~ "unspecific",
      specificity == 1 ~ "specific"
    ),
    logic = case_when(
      logic_and_consistency_score >= 4 ~ 1,
      logic_and_consistency_score < 4 ~ 0
    ),
    logic_label = case_when(
      logic == 0 ~ "illogical",
      logic == 1 ~ "logical"
    ),
    completeness = case_when(
      formal_completeness_score >= 4 ~ 1,
      formal_completeness_score < 4 ~ 0
    ),
    completeness_label = case_when(
      completeness == 0 ~ "incomplete",
      completeness == 1 ~ "complete"
    ),
    emotion = case_when(
      emotionality_score >= 4 ~ 1,
      emotionality_score < 4 ~ 0
    ),
    emotion_label = case_when(
      emotion == 0 ~ "emotional",
      emotion == 1 ~ "unemotional"
    ),
    validity = case_when(
      validity_and_feasibility_score >= 4 ~ 1,
      validity_and_feasibility_score < 4 ~ 0
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
        as.character(specificity),
        as.character(logic),
        as.character(completeness),
        as.character(emotion),
        as.character(validity)
      )
    ),
    label = as.factor(
      paste0(
        clarity_label, "-", specificity_label, "-",
        logic_label, "-", completeness_label, "-",
        emotion_label, "-", validity_label
      )
    )
  ) %>%
  select(combination, label, title, area, contains("reason"), everything())

prop(gpt, "clarity")      ## 58.9% ---> 47.7% (w/ prompt change + filtering)
prop(gpt, "specificity")  ## 38.2% ---> 19.0%
prop(gpt, "logic")        ## 81.1% ---> 61.4%
prop(gpt, "completeness") ##  8.6% ---> 18.4%
prop(gpt, "emotion")      ##  2.2% --->  3.1%
prop(gpt, "validity")     ## 76.8% ---> 44.1%

## Create binary 6-digit patterns with 0-1 -------------------------------------
pattern01 <- c(
  "000000", "000001", "000010", "000011", "000100", "000101",
  "000110", "000111", "001000", "001001", "001010", "001011",
  "001100", "001101", "001110", "001111", "010000", "010001",
  "010010", "010011", "010100", "010101", "010110", "010111",
  "011000", "011001", "011010", "011011", "011100", "011101",
  "011110", "011111", "100000", "100001", "100010", "100011", 
  "100100", "100101", "100110", "100111", "101000", "101001",
  "101010", "101011", "101100", "101101", "101110", "101111",
  "110000", "110001", "110010", "110011", "110100", "110101",
  "110110", "110111", "111000", "111001", "111010", "111011",
  "111100", "111101", "111110", "111111"
)

# Check observations for unique patterns/frequencies ===========================
## 39 patterns overall (even with bins of 1 observation)
sort(table(gpt$combination), decreasing = TRUE)
length(table(gpt$combination))

## Missing patterns ------------------------------------------------------------
pattern01[!pattern01 %in% gpt$combination]

## Frequencies>10 --------------------------------------------------------------
## 24 patterns
gpt %>%
  group_by(combination) %>%
  summarise(n = n()) %>%
  filter(n > 10) %>%
  arrange(desc(n))

## Low frequency patterns (sanity check) ---------------------------------------
gpt %>%
  group_by(combination) %>%
  filter(n() <= 5) %>%
  select(combination, everything())

## If we keep low frequency patterns,
## selection of petitions will be
1 * 7 + 2 * (39 - 7) ## 71

## If limit to at least 10 in the category,
24 * 2

# gpt %>%
#   filter(emotionality_score == 4) %>%
#   .$emotionality_reason %>%
#   table() %>%
#   as.data.frame() %>%
#   arrange(desc(Freq)) %>%
#   View()

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
