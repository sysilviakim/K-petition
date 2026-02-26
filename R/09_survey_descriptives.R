# Setup ========================================================================
source(here::here("R", "08_survey_wrangling.R"))

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

## Variable groups -------------------------------------------------------------
petition_vars <- c(
  "diff_quality", "diff_nchar", "diff_nword", "diff_nsent", "diff_avg_sent",
  "diff_clarity", "diff_logic", "diff_manner", "diff_validity", "same_category"
)

## Nested additive blocks (Option A)
## M1: demographics
## M2: + socioeconomic
## M3: + political
## M4: + attitudes / trust
m1_resp <- c(
  "female", "age_group", "edu4",
  "seoul", "married", "has_young_child",
  "log_time"
)
m2_resp <- c(
  m1_resp, "median_income", "subj_class3"
)
m3_resp <- c(
  m2_resp, "ideo3", "ppp",
  "voted_yoon_2022", "voted_lee_2025"
)
m4_resp <- c(
  m3_resp, "populist", "anti_elitist",
  "instit_trust_high"
)

# Petition post characteristics ================================================
post_types <- here::here(
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

# Text features (shared by scripts 14, 16) =====================================
text_features <- post_types %>%
  mutate(
    nchar = nchar(text),
    nword = str_count(text, "\\S+"),
    nsent = str_count(text, "[.!?。]") + 1,
    avg_sent_len = nword / nsent,
    quality_sum = as.numeric(
      substr(comb_fin, 1, 1)
    ) + as.numeric(
      substr(comb_fin, 2, 2)
    ) + as.numeric(
      substr(comb_fin, 3, 3)
    ) + as.numeric(
      substr(comb_fin, 4, 4)
    )
  ) %>%
  select(
    item, category, comb_fin,
    clarity_specificity, logic_consistency,
    tone_manner, validity_feasibility,
    quality_sum, nchar, nword, nsent,
    avg_sent_len
  )

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

# Attention ====================================================================
attention_df <- df %>%
  select(NO, all_of(time_vars)) %>%
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
  geom_histogram(bins = 50, fill = ACCENT) +
  facet_wrap(~pair, ncol = 4) +
  xlab("Response Time (Seconds)") +
  ylab("Count") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))

pdf(here::here("fig", "attention_time.pdf"), width = 10, height = 5)
print(p_attn)
dev.off()

# Demographics =================================================================
demo_table <- df %>%
  transmute(
    Gender = ifelse(
      female == 1, "Female", "Male"
    ),
    `Age Range` = as.character(age_group),
    Education = as.character(edu4),
    Income = as.character(median_income),
    Party = as.character(pid3),
    Ideology = as.character(ideo3)
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

save_xtable(
  demo_table,
  caption = "Survey Respondent Demographics",
  label = "tab:demographics",
  file = "demographics.tex"
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

pdf(here::here("fig", "choice_frequency.pdf"), width = 7, height = 9)
print(p_choice)
dev.off()
