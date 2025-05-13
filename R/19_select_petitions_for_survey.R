source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Output from script #17 GPT scoring
gpt <- read_csv(here("data", "tidy", "evaluated_data_2024_v2.csv")) %>%
  filter(!is.na(clarity_score))
  ## 43 rows with reasons as "attachment only"?

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
    specificity = case_when(
      specificity_score >= 4 ~ 1,
      specificity_score < 4 ~ 0
    ),
    logic = case_when(
      logic_and_consistency_score >= 4 ~ 1,
      logic_and_consistency_score < 4 ~ 0
    ),
    completeness = case_when(
      formal_completeness_score >= 4 ~ 1,
      formal_completeness_score < 4 ~ 0
    ),
    emotion = case_when(
      emotionality_score >= 4 ~ 1,
      emotionality_score < 4 ~ 0
    ),
    validity = case_when(
      validity_and_feasibility_score >= 4 ~ 1,
      validity_and_feasibility_score < 4 ~ 0
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
    )
  )

prop(gpt, "clarity")      ## 58.9% ---> 47.1% (w/ prompt change)
prop(gpt, "specificity")  ## 38.2% ---> 19.3%
prop(gpt, "logic")        ## 81.1% ---> 60.8%
prop(gpt, "completeness") ##  8.6% ---> 18.1%
prop(gpt, "emotion")      ##  2.2% --->  3.1%
prop(gpt, "validity")     ## 76.8% ---> 43.1%

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
## 41 patterns overall (even with bins of 1 observation)
table(gpt$combination)
length(table(gpt$combination))

## Missing patterns ------------------------------------------------------------
pattern01[!pattern01 %in% gpt$combination]
#  [1] "001000" "001100" "001101" "010000" "010100" "011000" "011100" "011101"
#  [9] "011110" "011111" "100000" "100001" "100100" "100101" "101000" "101100"
# [17] "101101" "110000" "110001" "110100" "110101" "111000" "111100"

## Frequencies>5 ---------------------------------------------------------------
## 31 patterns
gpt %>%
  group_by(combination) %>%
  summarise(n = n()) %>%
  filter(n > 5) %>%
  arrange(desc(n))

## Top 5
## "101011" clear, unspecific, logical, not formal, unemotional, valid
## "000010" unclear, unspecific, illogical, not formal, unemotional, invalid
## "000110" unclear, unspecific, illogical, formal, unemotional, invalid
## "101010" clear, unspecific, logical, not formal, unemotional, invalid
## "001010" unclear, unspecific, logical, not formal, unemotional, invalid

## Low frequency patterns (sanity check) ---------------------------------------
gpt %>%
  group_by(combination) %>%
  filter(n() <= 5) %>%
  select(combination, everything()) %>%
  View()

## If we keep low frequency patterns,
## selection of petitions will be
1 * 5 + 2 * (41 - 5) ## 77

## If limit to at least 10 in the category,
28 * 2

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

View(gpt_sample)
