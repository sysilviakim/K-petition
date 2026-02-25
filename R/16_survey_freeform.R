source(here::here("R", "08_survey_wrangling.R"))

# Q16: freeform responses ======================================================
## "공개 청원 및 민원 제도에 대해 귀하가 느끼는
##  문제점이나 한계는 무엇입니까?"
## (What problems or limitations do you perceive
##  regarding the public petition system?)
q16 <- df %>%
  select(
    NO, female, age_group, edu4, income, ideo3,
    pid, any_petition_exp, petition_positive,
    petition_negative, populist, instit_trust
  ) %>%
  left_join(
    df_list$open %>%
      select(NO, Q16) %>%
      mutate(Q16 = stri_trans_nfc(Q16)),
    by = "NO"
  ) %>%
  mutate(
    nchar = nchar(Q16),
    nword = stri_count_words(Q16)
  )

# Response length stats ========================================================
len_stats <- q16 %>%
  summarise(
    n = n(),
    mean_nchar = mean(nchar, na.rm = TRUE),
    median_nchar = median(nchar, na.rm = TRUE),
    sd_nchar = sd(nchar, na.rm = TRUE),
    max_nchar = max(nchar, na.rm = TRUE),
    mean_nword = mean(nword, na.rm = TRUE),
    median_nword = median(nword, na.rm = TRUE)
  )

# Identify non-substantive responses ===========================================
## Common filler: "없음", "없다", "모름", "모르겠다", etc.
filler_pat <- paste0(
  "^(없음|없다|없습니다|모름|모르겠다|",
  "모르겠습니다|모르겠음|딱히 없음|특별히 없음|",
  "잘 모르겠다|잘 모르겠습니다|글쎄요|",
  "잘 모름|없어요|없는 것 같다|",
  "없는것같다|없는것 같다|없는 것같다|",
  "없는거 같다|모르겠어요|x|X|XX|xx|",
  "특이사항 없음|해당없음|해당 없음|",
  "없는듯|없는 듯|",
  "없을것 같다|없을 것 같다|",
  "없을것같다|잘모르겠다|잘모름|",
  "딱히없음|특별히없음|특이사항없음|",
  "없다고 생각|생각없음|\\.|,|\\s*)$"
)

q16 <- q16 %>%
  mutate(
    substantive = !stri_detect_regex(
      stri_trim_both(Q16), filler_pat
    )
  )

n_substantive <- sum(q16$substantive, na.rm = TRUE)
n_filler <- sum(!q16$substantive, na.rm = TRUE)

# Tokenize (space-based) =======================================================
## Korean stopwords
kr_stop <- c(
  "있다", "없다", "하다", "되다", "이다", "않다",
  "것", "수", "등", "더", "좀", "잘", "안",
  "그", "이", "저", "때", "함", "또", "및",
  "위해", "대해", "대한", "통해", "있는",
  "없는", "하는", "되는", "같다", "같은",
  "생각", "생각이", "느낌", "부분", "점",
  "것이", "것을", "것도", "것이다",
  "있음", "없음", "하고", "되고",
  "라고", "다고", "에서", "으로",
  "하는것", "하는것이", "있는것", "있는것이",
  "같습니다", "합니다", "됩니다", "입니다",
  "있습니다", "않습니다", "같다고",
  "때문에", "정도", "하지", "않는"
)

tokens <- q16 %>%
  filter(substantive) %>%
  select(NO, Q16) %>%
  unnest_tokens(word, Q16, token = "words") %>%
  filter(
    nchar(word) >= 2,
    !word %in% kr_stop,
    !stri_detect_regex(word, "^[0-9]+$")
  )

word_freq <- tokens %>%
  count(word, sort = TRUE)

# Thematic coding via keywords =================================================
themes <- tribble(
  ~theme_en, ~theme_kr, ~keywords,
  "Low effectiveness",
  "실효성 부족",
  "실효|효과|반영|처리|해결|무시|형식|답변|변화|개선|
   소용|의미|결과|아무|쓸모|달라지|바뀌",
  "Lack of awareness",
  "인지도 부족",
  "홍보|인지|알리|접근|모르|몰랐|인식|알려|
   존재|관심|참여|알지",
  "Transparency issues",
  "투명성 부족",
  "투명|공개|결과|공정|공정성|비공개|
   알 수|진행|확인|추적",
  "Procedural complexity",
  "절차 복잡",
  "절차|복잡|어렵|불편|시스템|접근성|
   사용|이용|편의|과정|까다|간소",
  "Manipulation / abuse",
  "악용 및 여론몰이",
  "여론|악용|정치|동원|세력|조작|편향|
   이용당|선동|특정|정파|정당|이념",
  "Indiscriminate petitions",
  "무분별한 청원",
  "무분별|남용|사소|무의미|장난|도배|
   아무나|오남용|쓸데없|허위|가짜|거짓",
  "Slow response",
  "느린 대응",
  "느리|오래|시간|지연|늦|속도|빠르|
   신속|즉각|기다|답변.*늦|처리.*늦",
  "Distrust in government",
  "정부 불신",
  "불신|신뢰|믿|정부|국가|관료|공무원|
   기관|제도|권력|무책임|방관"
)

## Clean up keyword patterns (remove whitespace/newlines)
themes <- themes %>%
  mutate(
    keywords = stri_replace_all_regex(
      keywords, "\\s+", ""
    )
  )

## Tag each response with themes
q16_themes <- q16 %>%
  filter(substantive)

for (i in seq_len(nrow(themes))) {
  q16_themes <- q16_themes %>%
    mutate(
      !!themes$theme_en[i] := as.numeric(
        stri_detect_regex(Q16, themes$keywords[i])
      )
    )
}

theme_cols <- themes$theme_en
theme_summary <- q16_themes %>%
  summarise(across(all_of(theme_cols), sum)) %>%
  pivot_longer(
    everything(),
    names_to = "theme",
    values_to = "n"
  ) %>%
  mutate(
    pct = round(100 * n / n_substantive, 1)
  ) %>%
  arrange(desc(n)) %>%
  left_join(
    themes %>% select(theme_en, theme_kr),
    by = c("theme" = "theme_en")
  )

# Theme frequency figure =======================================================
p_theme <- theme_summary %>%
  mutate(
    label = paste0(theme, "\n(", theme_kr, ")"),
    label = fct_reorder(label, n)
  ) %>%
  ggplot(aes(x = label, y = pct)) +
  geom_col(fill = viridis(1, begin = 0.4)) +
  coord_flip() +
  labs(
    x = NULL,
    y = "% of substantive responses"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

pdf(here("fig", "q16_theme_frequency.pdf"), width = 8, height = 5)
p_theme
dev.off()

# Top words figure =============================================================
p_words <- word_freq %>%
  head(30) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = viridis(1, begin = 0.6)) +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

pdf(here("fig", "q16_top_words.pdf"), width = 7, height = 7)
p_words
dev.off()

# Theme by demographics ========================================================
theme_by_group <- function(data, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),
      across(
        all_of(theme_cols),
        ~ round(100 * mean(.x, na.rm = TRUE), 1)
      ),
      .groups = "drop"
    )
}

theme_by_age <- theme_by_group(q16_themes, "age_group")
theme_by_gender <- theme_by_group(
  q16_themes %>% mutate(
    gender = ifelse(female == 1, "Female", "Male")
  ),
  "gender"
)
theme_by_ideo <- theme_by_group(q16_themes, "ideo3")
theme_by_exp <- theme_by_group(
  q16_themes %>% mutate(
    petition_exp = ifelse(
      any_petition_exp == 1, "Yes", "No"
    )
  ),
  "petition_exp"
)

# Response length figure =======================================================
p_length <- q16 %>%
  ggplot(aes(x = nchar)) +
  geom_histogram(
    binwidth = 10,
    fill = viridis(1, begin = 0.5),
    color = "white"
  ) +
  geom_vline(
    xintercept = median(q16$nchar),
    linetype = "dashed"
  ) +
  labs(
    x = "Response length (characters)",
    y = "Count"
  ) +
  theme_minimal()

pdf(here("fig", "q16_response_length.pdf"), width = 6, height = 4)
p_length
dev.off()

# Summary table ================================================================
tab_theme <- theme_summary %>%
  select(theme, theme_kr, n, pct) %>%
  setNames(c("Theme", "Theme (KR)", "N", "\\%"))

save_xtable(
  tab_theme,
  caption = paste0(
    "Thematic distribution of Q16 responses ",
    "(N = ", n_substantive,
    " substantive out of ", nrow(q16), ")"
  ),
  label = "tab:q16_themes",
  file = "q16_themes.tex",
  sanitize = TRUE
)
