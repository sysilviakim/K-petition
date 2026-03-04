## project: LLM analysis of open-endend survey responses to Korea's e-petition

## Rscript may launch in the C locale; set UTF-8 so Korean paths resolve.
Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(tidyverse)
library(tidytext)
## texteffect loads MASS, which shadows dplyr::select.  Load it only after
## utilities.R has finished (where dplyr::select is still needed).

# Setup ========================================================================
## utilities.R loads MCMCpack → MASS (masks dplyr::select) and plyr (masks
## dplyr::mutate, summarise, group_by, …) before re-attaching dplyr via
## library(tidyverse).  Because library(tidyverse) on an already-loaded
## tidyverse only re-attaches the meta-package—not dplyr itself—the individual
## dplyr verbs remain buried below plyr/MASS on the search path.
## Pinning the affected dplyr functions in .GlobalEnv (position 0, searched
## before any package) ensures the right versions are used inside source().
filter <- dplyr::filter
select <- dplyr::select
mutate <- dplyr::mutate
rename <- dplyr::rename
summarise <- dplyr::summarise
summarize <- dplyr::summarize
group_by <- dplyr::group_by
arrange <- dplyr::arrange
count <- dplyr::count
source(here::here("R", "utilities.R"))
## Do NOT rm() the .GlobalEnv pins — they must remain for the entire script
## so that dplyr verbs win over plyr/MASS throughout (not just inside source()).
fname <- here(
  stri_trans_nfc(
    "data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx"
  )
)

# Load data ====================================================================
## Korean filenames cause encoding issues in readxl on some systems;
## copy to a temp path with an ASCII name before reading.
file.copy(fname, "/tmp/survey_sibp.xlsx", overwrite = TRUE)
df_list <- list(
  raw = read_xlsx("/tmp/survey_sibp.xlsx", sheet = "Raw"),
  label = read_xlsx("/tmp/survey_sibp.xlsx", sheet = "Label"),
  open = read_xlsx("/tmp/survey_sibp.xlsx", sheet = "Open"),
  questions = read_xlsx(
    "/tmp/survey_sibp.xlsx",
    sheet = stri_trans_nfc("변수 가이드")
  ) |>
    ## setNames avoids plyr::rename / MASS::select conflicts entirely.
    ## The sheet has exactly 2 columns in this order: 변수명, 변수 내용.
    setNames(c("varname", "question"))
)
df_meta <- df_list$raw
df_meta <- df_meta %>%
  select(SQ1, SQ2_1, SQ2_2, SQ3, SQ4, SQ5, SQ6, SQ7, SQ8, SQ8_etc, SQ9, Q1, Q2, Q3, Q3_etc, Q4, Q4_etc, Q5, Q5_etc, starts_with("Q8"))
df_meta <- df_meta %>%
  mutate(open = df_list$open$Q16)

## task:
## df_meta$open contains responses to an open-ended survey question asking
## how respondents feel about Korea's e-petition in general.
## Read the responses and classify them into categories based on their
## substantive contents, feelings, etc.

# Classification ===============================================================
## Define keyword patterns for each category.
## Each pattern is a regex (Korean morpheme fragments) that signals a category.
## Order matters: earlier rules win when a response matches multiple categories.

category_rules <- tribble(
  ~category_id, ~category_en, ~category_kr, ~pattern,

  ## 9. No opinion / Don't know  – must be checked FIRST (short non-answers)
  9L, "No opinion / Don't know", "의견 없음·모름",
  "^(없음|없습니다|모르겠|모릅니다|잘\\s*모르|특별히|딱히|잘\\s*모름|특이사항|글쎄|잘\\s*모르겠|해당없|관심없|생각없|별다른|없어요|없네요|없다$|몰라|모름|경험없|이용.*않아|해보지|해\\s*본\\s*적|없는것 같|없는거 같|없는\\s*것\\s*같|관심이 없|신경.*안|잘은 모르|모르겠습니다|없음\\.|해당 없|의견 없|^\\.$|^\\s*$|안녕|그냥|그래서|서술형|문제점\\s*없|문제.*없|^\\s*\\.\\s*$)",

  ## 8. Positive / Supportive
  8L, "Positive / Supportive", "긍정적",
  "(긍정|좋은|좋아요|좋다|좋습니다|잘\\s*되|잘\\s*운영|만족|괜찮|훌륭|감사|좋은\\s*제도|필요한\\s*제도|바람직|현재.*좋|긍적적|응원|유익|잘\\s*하고)",

  ## 2. Frivolous / Emotional / Low-quality petitions (incl. petition overload)
  2L, "Frivolous / Emotional petitions", "무분별한 청원",
  "(무분별|감정적|감정에\\s*호소|화풀이|분풀이|억지|떼[^거]|막무가내|무지성|쓸데없|허무맹랑|터무니없|헛소리|비합리|성의없|장난|도배|악용|남용|악성|사소한|하찮|시비|어처구니|어이없|말도.*안.*되|황당|똥글|의미없는|쓸모없|비이성|과격|비방|욕설|너무.*많[은다이]|많이.*올라|남발|난무|합리적이지.*않|아무거나|주먹구구|타당하지|타당.*않|찌질|이슈몰이|불필요한.*민원|지나치|한계가 있|과도하|불필요한.*청원)",

  ## 7. Personal / Selfish petitions
  7L, "Personal / Selfish petitions", "개인적 민원",
  "(개인적|사적인|이기적|개인.*불만|개인.*불편|개인.*이익|사리사욕|이해관계|본인.*억울|사익|개인$|극히.*개인|사적\\s*감정|개인의\\s*감정|자기.*불만|개인적인.*민원|개인적인.*사안|개인.*불평|본인.*위주|자기.*입장|자기만|본인들.*불편|사사로운|공익.*아닌|사적.*이득|공익.*보다.*개인|개인.*사례|피해.*본.*듯)",

  ## 10. Mob mentality / Opinion manipulation
  10L, "Mob mentality / Opinion manipulation", "여론몰이·조작",
  "(여론.*몰이|여론.*조작|떼거지|우르르|몰리는|선동|편향|정치.*이용|정치적|좌파|우파|특정\\s*정당|민주당|국민의힘|정파|진영|이념|목소리.*큰|조작|맹목|부화뇌동)",

  ## 1. Lack of effectiveness / Implementation doubt
  1L, "Lack of effectiveness", "실효성 부족",
  "(실효성|실효|효과.*없|해결.*안|해결.*못|해결.*되지|해결이 안|반영.*안|반영.*되지|반영.*못|무시|무용|소용없|소용.*없|무의미|보여주기|형식적|허울|방치|유명무실|힘.*없|귀에.*경|소귀.*경|소리.*없|실현.*불가|효과.*찾기|답\\s*없|안\\s*들어|제대로.*안|외면|묵살|실현.*안|실현.*못|흐지부지|변화.*없|개선.*안|개선.*없|개선.*못|실질.*없|의문이다|의문$|반영될지|정책에.*반영|들어주는지|실행.*안|바뀌지.*않|결론.*없|수리.*드물|미처리|보여지기|처리.*안|기대하기.*어|결과.*기대|원하는.*결과|이어지지|안이어진|못하고|해결점|해결책|실행되어지지)",

  ## 12. Feasibility / Unrealistic proposals
  12L, "Feasibility / Unrealistic proposals", "실현 가능성 부족",
  "(실현.*가능|현실.*가능|현실성|비현실|실현불가|가능성.*낮|가능성.*없|이상적|추상적|구체적.*부족|구체성|현실화.*어|현실적.*부족|현실적이지|탁상공론|공허|실행가능|역량.*부족|역량.*한계|비전문|전문가가.*아닌|전문성.*부족|일반.*역량|일반인.*한계|미흡한.*부분|대안.*부족|수용.*한계|수용.*어|모든.*해결.*어)",

  ## 3. Transparency / Feedback deficiency
  3L, "Transparency / Feedback deficiency", "투명성·피드백 부족",
  "(투명|피드백|팔로우|후속|결과.*모르|결과.*알.*수|진행.*모르|진행.*알.*수|어떻게.*되었|공개.*안|공개.*부족|처리.*결과|시행.*여부|시행.*모르|답변.*없|회신.*없|확인.*어|알\\s*수\\s*없|알수없|알수가 없|불투명|공개성|진행여부|진행.*여부|되었는지|개선.*되었|실현되는지|개선여부|궁금|추적)",

  ## 4. Slow processing / Bureaucratic delays
  4L, "Slow processing / Bureaucratic delays", "처리 속도·관료적 지연",
  "(느리|늦|속도|지연|오래.*걸|시간.*걸|시간.*부족|신속|빠르|빨리|시간이.*많이|처리.*늦|처리.*느|처리.*오래|답변.*지연|답변.*늦|답변.*오래|시일)",

  ## 5. Need for filtering / Guidelines / Format
  5L, "Need for filtering / Guidelines", "필터링·가이드라인 필요",
  "(필터|가이드|양식|형식|기준|포맷|규격|체계|분류|검증|심사|선별|걸러|걸름|스크리닝|사전.*검토|사전.*필터|일정.*형식|표준화|규정|틀.*없|틀이.*없|명확한.*기준|중구난방|들쑥날쑥)",

  ## 6. Readability / Communication issues
  6L, "Readability / Communication issues", "가독성·전달력 부족",
  "(가독|읽기|이해.*어|이해.*힘|복잡|전달.*부족|전달력|내용.*어|명확.*않|명확하지|알아보기.*어|논리.*부족|근거.*부족|설득력|표현.*부족|어휘|문체|논조|글쓰기|글.*어|내용.*복잡|핵심.*잃|소통.*한계|전달.*어|전달하기|정확.*전달|논리적.*근거|의견.*다[르를]|일률적|공감.*다|단순.*의견|공정성)",

  ## 13. Civil servant burden / Institutional constraints
  13L, "Civil servant burden / Institutional constraints", "공무원 부담·제도적 한계",
  "(공무원|담당자|인력|부서|기관|행정|관료|업무.*과다|업무.*과|업무량|처리.*어|처리.*힘|인당|전담|예산|법제화|국회|제도.*한계|제도적|법적.*한계|권한.*부족|떠넘기|돌려막기|절차.*간소|일관성.*미흡|정권|현장.*넘기|정부.*의지)",

  ## 11. Need for more participation / Awareness
  11L, "Need for more participation / Awareness", "참여·홍보 필요",
  "(참여|홍보|활성화|관심.*필요|알려|인지.*부족|알지.*못|모르는.*사람|이용.*적|접근성|사용.*쉽|편리|편의|접하기|접해보|알려져|인식.*부족|관심.*가져|적극.*참여|아는.*사람.*앎|번거|접근.*어)",

  ## 14. Other — catch-all (empty pattern, never matched by regex)
  14L, "Other", "기타", "^$"
)

## ---- helper: classify a single response ------------------------------------
classify_response <- function(txt, rules) {
  if (is.na(txt)) {
    return(9L)
  } # NA → No opinion
  txt <- str_squish(trimws(txt))
  if (nchar(txt) == 0) {
    return(9L)
  }

  ## Short non-answers: <= 5 chars and matches common fillers
  if (nchar(txt) <= 5 &&
    str_detect(txt, regex("없음|없다|모름|글쎄|없어|모르|좋아", ignore_case = TRUE))) {
    ## Check if positive first
    if (str_detect(txt, regex("좋아|좋다|좋음", ignore_case = TRUE))) {
      return(8L)
    }
    return(9L)
  }

  for (i in seq_len(nrow(rules))) {
    pat <- rules$pattern[i]
    if (pat == "^$") next # skip catch-all
    if (str_detect(txt, regex(pat, ignore_case = TRUE))) {
      return(rules$category_id[i])
    }
  }
  return(14L) # Other
}

## ---- apply classification ---------------------------------------------------
df_meta <- df_meta %>%
  mutate(
    category_id = map_int(open, ~ classify_response(.x, category_rules))
  ) %>%
  left_join(
    category_rules %>% select(category_id, category_en, category_kr),
    by = "category_id"
  )

# Summary ======================================================================
cat("\n========== Classification Summary ==========\n\n")

## Frequency table
freq_tbl <- df_meta %>%
  count(category_id, category_en, category_kr, name = "n") %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

print(as.data.frame(freq_tbl), right = FALSE, row.names = FALSE)
cat("\nTotal classified:", sum(freq_tbl$n), "\n")

## Random sample of 5 from each category for sanity-checking
cat("\n========== Random Samples per Category ==========\n\n")
set.seed(42)
samples <- df_meta %>%
  filter(!is.na(open)) %>%
  group_by(category_id, category_en) %>%
  mutate(.grp_n = n()) %>%
  slice_sample(n = 5) %>%
  ungroup() %>%
  select(category_id, category_en, open)

for (cid in sort(unique(samples$category_id))) {
  sub <- samples %>% filter(category_id == cid)
  cat(sprintf(
    "--- [%d] %s ---\n", cid,
    unique(sub$category_en)
  ))
  for (j in seq_len(nrow(sub))) {
    cat(sprintf("  • %s\n", sub$open[j]))
  }
  cat("\n")
}

# Save outputs =================================================================
dir.create(here("output"), showWarnings = FALSE)

## Full classified data
write_csv(
  df_meta %>% select(open, category_id, category_en, category_kr),
  here("output", "openended_classified.csv")
)

## Summary table
write_csv(freq_tbl, here("output", "openended_summary.csv"))

cat("\nOutputs saved to:\n")
cat("  ", here("output", "openended_classified.csv"), "\n")
cat("  ", here("output", "openended_summary.csv"), "\n")

# Demographic Analysis =========================================================
cat("\n\n========== Demographic Analysis ==========\n\n")

## ---- Label demographic variables -------------------------------------------
df_meta <- df_meta %>%
  mutate(
    ## SQ1: Gender
    gender = factor(SQ1, levels = 1:2, labels = c("Male", "Female")),

    ## SQ2_1: Age → age groups
    age_group = cut(
      SQ2_1,
      breaks = c(19, 29, 39, 49, 59, Inf),
      labels = c("20s", "30s", "40s", "50s", "60+"),
      right = TRUE
    ),

    ## SQ3: Education
    education = factor(
      as.integer(gsub("\\).*", "", SQ3)),
      levels = 1:6,
      labels = c(
        "Middle school or less",
        "High school",
        "In college",
        "College grad",
        "In grad school",
        "Grad school"
      )
    ),
    ## Collapse to 3 levels for cleaner analysis
    edu3 = fct_collapse(
      education,
      "HS or less" = c("Middle school or less", "High school"),
      "College"    = c("In college", "College grad"),
      "Grad+"      = c("In grad school", "Grad school")
    ),

    ## SQ5: Marital status
    marital = factor(
      as.integer(gsub("\\).*", "", SQ5)),
      levels = 1:4,
      labels = c(
        "Married (children)",
        "Married (no children)",
        "Single",
        "Other"
      )
    ),

    ## SQ7: Household income (monthly, pre-tax)
    income_raw = as.integer(gsub("\\).*", "", SQ7)),
    income = fct_collapse(
      factor(income_raw),
      "< 200"     = c("1", "2"),
      "200-400"   = c("3", "4"),
      "400-700"   = c("5", "6", "7"),
      "700-1000"  = c("8"),
      "1000+"     = c("9", "10", "11")
    ),

    ## SQ8: Occupation
    occupation = factor(
      as.integer(gsub("\\).*", "", SQ8)),
      levels = 1:12,
      labels = c(
        "Office worker", "Homemaker", "Student",
        "Professional", "Service/Sales", "Agriculture",
        "Self-employed", "Civil servant/Teacher", "Freelancer",
        "Technical", "Unemployed/Retired", "Other"
      )
    ),

    ## Q2: Political ideology (1 = very progressive, 7 = very conservative)
    ideology = factor(
      Q2,
      levels = 1:7,
      labels = c(
        "Very progressive", "Progressive", "Lean progressive",
        "Moderate",
        "Lean conservative", "Conservative", "Very conservative"
      )
    ),
    ## Collapse to 3 levels
    ideo3 = fct_collapse(
      ideology,
      "Progressive" = c("Very progressive", "Progressive", "Lean progressive"),
      "Moderate" = "Moderate",
      "Conservative" = c("Lean conservative", "Conservative", "Very conservative")
    )
  )

## ---- Exclude "No opinion" and "Other" for cleaner demographic patterns ------
df_sub <- df_meta %>% filter(!category_id %in% c(9L, 14L))

## ---- Chi-squared tests for each demographic × category ----------------------
demo_vars <- c(
  "gender", "age_group", "edu3", "marital",
  "income", "occupation", "ideo3"
)
demo_labels <- c(
  "Gender", "Age Group", "Education", "Marital Status",
  "Income", "Occupation", "Political Ideology"
)

chi_results <- tibble()

for (k in seq_along(demo_vars)) {
  dv <- demo_vars[k]
  dl <- demo_labels[k]

  ## Build contingency table
  tbl <- table(df_sub[[dv]], df_sub$category_en)
  ## Drop levels with 0 observations
  tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0]

  chi <- chisq.test(tbl, simulate.p.value = TRUE, B = 5000)

  chi_results <- bind_rows(chi_results, tibble(
    variable = dl,
    chi_sq = round(chi$statistic, 2),
    df = NA_integer_,
    p_value = round(chi$p.value, 4),
    sig = case_when(
      chi$p.value < 0.001 ~ "***",
      chi$p.value < 0.01 ~ "**",
      chi$p.value < 0.05 ~ "*",
      chi$p.value < 0.10 ~ ".",
      TRUE ~ ""
    )
  ))

  cat(sprintf(
    "\n--- %s (chi-sq = %.2f, p = %.4f %s) ---\n",
    dl, chi$statistic, chi$p.value,
    chi_results$sig[nrow(chi_results)]
  ))

  ## Column-proportion table (% within each demographic group)
  prop_tbl <- prop.table(tbl, margin = 1) * 100
  print(round(prop_tbl, 1))
  cat("\n")
}

## ---- Print chi-squared summary table ----------------------------------------
cat("\n========== Chi-squared Test Summary ==========\n")
cat("(Responses in 'No opinion' and 'Other' excluded)\n\n")
print(as.data.frame(chi_results), row.names = FALSE)

## ---- Visualisation: proportion heatmaps per demographic var -----------------
library(scales)

make_heatmap <- function(df, demo_var, demo_label) {
  ## Compute proportions within each demographic group
  plot_df <- df %>%
    filter(!is.na(.data[[demo_var]])) %>%
    count(.data[[demo_var]], category_en) %>%
    group_by(.data[[demo_var]]) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()

  ggplot(plot_df, aes(
    x = category_en,
    y = .data[[demo_var]],
    fill = prop
  )) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.0f%%", prop * 100)),
      size = 2.5, color = "black"
    ) +
    scale_fill_gradient(
      low = "white", high = "#2166AC",
      labels = percent_format()
    ) +
    labs(
      title = paste("Response Category by", demo_label),
      subtitle = paste0(
        "(Excluding 'No opinion' & 'Other'; ",
        "chi-sq p = ",
        chi_results %>%
          filter(variable == demo_label) %>%
          pull(p_value), ")"
      ),
      x = NULL, y = demo_label, fill = "Proportion"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 40, hjust = 1, size = 7),
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "right"
    )
}

## Generate all heatmaps
plots <- map2(demo_vars, demo_labels, ~ make_heatmap(df_sub, .x, .y))

## Save each figure as a separate PDF
fig_names <- c(
  "gender", "age_group", "education",
  "marital", "income", "occupation", "ideology"
)
dir.create(here("output", "fig"), showWarnings = FALSE)
walk2(plots, fig_names, function(p, nm) {
  out <- here("output", "fig", paste0("openended_", nm, ".pdf"))
  ggsave(out, plot = p, width = 14, height = 6)
  cat("  Saved:", out, "\n")
})
cat("\nDemographic heatmaps saved to output/fig/\n")

## ---- Save chi-sq summary ---------------------------------------------------
write_csv(chi_results, here("output", "openended_chisq_summary.csv"))
cat(
  "Chi-squared summary saved to:",
  here("output", "openended_chisq_summary.csv"), "\n"
)

## ---- Save full classified data WITH demographics ----------------------------
write_csv(
  df_meta %>%
    select(
      open, category_id, category_en, category_kr,
      gender, age_group, edu3, marital, income, occupation, ideo3
    ),
  here("output", "openended_classified_with_demographics.csv")
)
cat(
  "Full classified data with demographics saved to:",
  here("output", "openended_classified_with_demographics.csv"), "\n"
)
