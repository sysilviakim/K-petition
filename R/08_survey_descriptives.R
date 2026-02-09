# Setup ========================================================================
source(here::here("R", "utilities.R"))
fname <- here(
  stri_trans_nfc(
    "data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx"
  )
)

## Theta constraints -----------------------------------------------------------
## item 1: 1011. clear, weak logic, well mannered, valid
## item 15: 0000. unclear, illogical, untoned, invalid
## item 49: 0101. unclear, logical, untoned, valid
## 1st dim: clarity and manner
## 2nd dim: logic and validity
theta_constraints <- list(
  item.1 = list(1, 2),
  item.1 = list(2, 2),
  item.15 = list(1, -2),
  item.15 = list(2, -2),
  item.49 = list(1, "-"),
  item.49 = list(2, "+")
)

## 성x연령 균등할당
## 30대 남 123, 40대 여 123, 나머지 전부 122

# Load data ====================================================================
df_list <- list(
  raw = read_xlsx(
    stri_trans_nfc(fname),
    sheet = "Raw"
  ),
  label = read_xlsx(
    stri_trans_nfc(fname),
    sheet = "Label"
  ),
  open = read_xlsx(
    stri_trans_nfc(fname),
    sheet = "Open"
  ),
  questions = read_xlsx(
    stri_trans_nfc(fname),
    sheet = stri_trans_nfc("변수 가이드")
  ) %>%
    rename(
      varname = stri_trans_nfc("변수명"),
      question = stri_trans_nfc("변수 내용")
    )
)
df <- df_list$raw

# Petition post characteristics ================================================
post_types <- here(
  stri_trans_nfc(
    "data/screenshots/공개제안_tidy_url_appended.xlsx"
  )
) %>%
  read_xlsx() %>%
  select(id, category, comb_fin, text)

## comb_fin:
## 1. clarity, specificity
## 2. logic, consistency
## 3. tone, manner
## 4. validity, feasibility
post_types <- post_types %>%
  mutate(
    clarity_specificity = substr(comb_fin, 1, 1),
    logic_consistency = substr(comb_fin, 2, 2),
    tone_manner = substr(comb_fin, 3, 3),
    validity_feasibility = substr(comb_fin, 4, 4)
  ) %>%
  rename(item = id) %>%
  mutate(item = as.character(item))

# Pairwise comparison data =====================================================
pwc_df <- map_dfr(1:8, function(i) {
  df %>%
    select(
      NO,
      !!sym(paste0("Q13_gCode", i, "_1")),
      !!sym(paste0("Q13_gCode", i, "_2")),
      !!sym(paste0("Q13_", i))
    ) %>%
    rename(
      Item1 = !!sym(
        paste0("Q13_gCode", i, "_1")
      ),
      Item2 = !!sym(
        paste0("Q13_gCode", i, "_2")
      ),
      Choice = !!sym(paste0("Q13_", i))
    )
})

pwc_df <- pwc_df %>%
  rowwise() %>%
  mutate(
    Item1 = paste0("item.", Item1),
    Item2 = paste0("item.", Item2),
    Choice = c(Item1, Item2)[Choice]
  ) %>%
  ungroup()
pwc_df <- as.data.frame(pwc_df)

# Functions ====================================================================
survey_rename <- function(x, wrangle = TRUE) {
  out <- x %>%
    rename(
      gender = SQ1,
      age = SQ2_1,
      age_range = SQ2_2,
      edu = SQ3,
      residence = SQ4,
      married = SQ5,
      kids = SQ6,
      income = SQ7,
      occupation = SQ8,
      occupation_etc = SQ8_etc,
      livelihood = SQ9,
      life = Q1,
      ideology = Q2,
      party = Q3,
      party_etc = Q3_etc,
      pres22 = Q4,
      pres22_etc = Q4_etc,
      pres25 = Q5,
      pres25_etc = Q5_etc
    )

  if (wrangle) {
    out <- out %>%
      mutate(
        ## Option 5: 400만원 이상-500만원 미만
        ## 2025 기준 3인 가족 중위소득 = 500만원
        median_income = factor(
          ifelse(income > 5, 1, 0),
          levels = c(0, 1),
          labels = c(
            "Below Median", "Above Median"
          )
        ),
        ## Party: Q3 codes
        ## 1=DPK, 2=PPP, 3=JIP, 4=Jinbo,
        ## 5=Reform, 6=BI, 7=SD, 8=Other
        party_group = case_when(
          party == 2 ~ "PPP",
          party %in% c(1, 3:7) ~ "Opposition",
          TRUE ~ "Other"
        ),
        party_group = factor(
          party_group,
          levels = c(
            "PPP", "Opposition", "Other"
          )
        ),
        ## Populist: Q6_7 >= 4
        populist = Q6_7 >= 4
      )
  }

  return(out)
}

stats_summ_create <- function(out) {
  theta_summ <- left_join(
    irt_summ("theta1", out) %>%
      as.data.frame() %>%
      rownames_to_column(var = "item") %>%
      rename_with(
        ~ paste0("theta1_", .), -item
      ) %>%
      mutate(
        item = gsub("theta1.", "", item)
      ),
    irt_summ("theta2", out) %>%
      as.data.frame() %>%
      rownames_to_column(var = "item") %>%
      rename_with(
        ~ paste0("theta2_", .), -item
      ) %>%
      mutate(
        item = gsub("theta2.", "", item)
      )
  ) %>%
    mutate(item = gsub("item.", "", item)) %>%
    left_join(
      post_types %>% select(-text),
      by = "item"
    )

  gamma_summ <- irt_summ("gamma", out) %>%
    as.data.frame() %>%
    rownames_to_column(var = "respondent") %>%
    mutate(
      respondent = gsub("gamma.", "", respondent)
    ) %>%
    left_join(
      df %>%
        select(
          NO,
          SQ1, SQ2_1, SQ2_2, SQ3, SQ4,
          SQ5, SQ6, SQ7, SQ8, SQ8_etc,
          SQ9, Q1, Q2, Q3, Q3_etc,
          Q4, Q4_etc, Q5, Q5_etc, Q6_7
        ) %>%
        mutate(respondent = as.character(NO)),
      by = "respondent"
    ) %>%
    survey_rename()

  list(theta = theta_summ, gamma = gamma_summ)
}

# Survey weights ===============================================================
## Map survey codes to demo_weight groups
## SQ1: 1=M, 2=F
## SQ2_2: 1=20s, 2=30s, 3=40s, 4=50s, 5=60+
age_map <- c(
  "1" = "20-29", "2" = "30-39",
  "3" = "40-49", "4" = "50-59",
  "5" = "60+"
)

df <- df %>%
  mutate(
    wt_gender = ifelse(SQ1 == 1, "M", "F"),
    wt_age = age_map[as.character(SQ2_2)]
  ) %>%
  left_join(
    demo_weight,
    by = c(
      "wt_gender" = "gender",
      "wt_age" = "age_group"
    )
  ) %>%
  rename(wt = weight)

# Attention ====================================================================
time_cols <- paste0("q13_q14_time_", 1:8)
attention_df <- df %>%
  select(NO, all_of(time_cols)) %>%
  pivot_longer(
    -NO,
    names_to = "pair",
    values_to = "seconds"
  ) %>%
  mutate(
    pair = factor(
      gsub("q13_q14_time_", "Pair ", pair),
      levels = paste("Pair", 1:8)
    )
  )

p_attn <- ggplot(
  attention_df,
  aes(x = seconds)
) +
  geom_histogram(bins = 50, fill = "gray60") +
  facet_wrap(~pair, ncol = 4) +
  xlab("Response Time (Seconds)") +
  ylab("Count") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))

ggsave(
  here("fig", "attention_time.pdf"),
  plot = p_attn, width = 10, height = 5
)

# Demographics =================================================================
df_renamed <- survey_rename(df)

## Summary table
demo_table <- df_renamed %>%
  transmute(
    Gender = ifelse(
      gender == 1, "Male", "Female"
    ),
    `Age Range` = case_when(
      age_range == 1 ~ "20-29",
      age_range == 2 ~ "30-39",
      age_range == 3 ~ "40-49",
      age_range == 4 ~ "50-59",
      age_range == 5 ~ "60+"
    ),
    Education = case_when(
      edu <= 2 ~ "Middle school or below",
      edu == 3 ~ "High school",
      edu == 4 ~ "University",
      edu >= 5 ~ "Graduate"
    ),
    Income = median_income,
    Party = party_group,
    Ideology = case_when(
      ideology <= 2 ~ "Progressive",
      ideology %in% 3:5 ~ "Moderate",
      ideology >= 6 ~ "Conservative"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  count(Variable, Category) %>%
  group_by(Variable) %>%
  mutate(
    Pct = round(n / sum(n) * 100, 1)
  ) %>%
  ungroup() %>%
  arrange(
    factor(
      Variable,
      levels = c(
        "Gender", "Age Range",
        "Education", "Income",
        "Party", "Ideology"
      )
    ),
    Category
  )

demo_xtable <- xtable(
  demo_table,
  caption = "Survey Respondent Demographics",
  label = "tab:demographics"
)
print_xtable <- capture.output(
  print(
    demo_xtable,
    include.rownames = FALSE,
    booktabs = TRUE,
    file = here("tab", "demographics.tex")
  )
)

# Choice frequency =============================================================
choice_freq <- pwc_df %>%
  filter(!is.na(Choice)) %>%
  count(Choice, name = "n_chosen") %>%
  mutate(
    item = gsub("item.", "", Choice)
  ) %>%
  left_join(
    post_types %>% select(-text),
    by = "item"
  ) %>%
  arrange(desc(n_chosen))

## Also count how often each item appeared
appear_freq <- bind_rows(
  pwc_df %>%
    select(item = Item1) %>%
    mutate(item = gsub("item.", "", item)),
  pwc_df %>%
    select(item = Item2) %>%
    mutate(item = gsub("item.", "", item))
) %>%
  count(item, name = "n_shown")

choice_freq <- choice_freq %>%
  left_join(appear_freq, by = "item") %>%
  mutate(win_rate = n_chosen / n_shown)

p_choice <- ggplot(
  choice_freq,
  aes(
    x = reorder(item, win_rate),
    y = win_rate,
    fill = comb_fin
  )
) +
  geom_col() +
  coord_flip() +
  xlab("Petition ID") +
  ylab("Win Rate") +
  scale_fill_viridis_d(
    name = "Quality Code", end = 0.9
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  here("fig", "choice_frequency.pdf"),
  plot = p_choice, width = 7, height = 9
)
