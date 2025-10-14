source(here::here("R", "utilities.R"))

# Data filtering for survey ====================================================
df_full <- df <- seq(2021, 2024) %>%
  set_names(., .) %>%
  map_dfr(
    ~ here("data", "tidy", paste0("pub_petition_content_", .x, ".csv")) %>%
      read_csv() %>%
      mutate(year = .x)
  )

df <- df_full %>%
  ## 40,840 to 3,530 observations
  filter(branch == "보건복지부") %>%
  ## 233 observations + 난임/임신 should have been another keyword
  filter(grepl("출생|출산|육아|난임|임신", title)) %>%
  ## This is conditional on having been answered by Jan 2024 for 2021--2023
  ## and by Nov 2024 for 2024
  ## Nothing in 제안실현/제안추진 in 2024
  filter(status == "답변완료" | status == "제안실현" | status == "제안추진")

## No petitions were accepted ever, 2021-2024
table(df$year, df$status)

## 저출산고령사회위원회 only has 6 petitions in 2023
## and 28 petitions in 2024 by that account
## so leaving out (none of them were accepted)

## Were *any* ever accepted? 2.3% 
df_full %>%
  filter(status == "제안실현" | status == "제안추진") %>%
  .$branch %>%
  table() %>%
  sort(decreasing = TRUE)

## Mostly local petitions, esp. 창원, 울산, 김포
## Within central branches, very skewed distribution of topic

## Export dataframe
## Preserve Korean UTF-8
df %>%
  readr::write_excel_csv(
    here("data", "tidy", "pub_petition_content_birth_2021-2024.csv")
  )

# Manually evaluated for 2024 ==================================================
evaluated <- bind_rows(
  readxl::read_excel(
    here("data/tidy/pub_petition_content_birth_2021-2024_SK_evaluated.xlsx")
  ) %>%
    mutate(eval = "SK"),
  readxl::read_excel(
    here("data/tidy/pub_petition_content_birth_2021-2024_KY_evaluated.xlsx")
  ) %>%
    mutate(eval = "KY")
) %>%
  filter(year == 2024 | eval == "KY") %>%
  filter(!is.na(`personal?`)) %>%
  select(
    eval, date_petitioned, title,
    `personal?`, `emotional?`, `good writing?`, `feasible?`, `concrete?`,
    `현황 및 문제점`, `개선방안`, `기대효과`
  ) %>%
  group_by(title, `현황 및 문제점`) %>%
  filter(n() > 1) %>%
  arrange(title, eval)

## Delete duplicates
evaluated <- evaluated[!duplicated(evaluated), ]

## Completely agree: 18 pairs
evaluated <- evaluated %>%
  mutate(
    personal_agree = case_when(
      `personal?`[[1]] == `personal?`[[2]] ~ 1,
      TRUE ~ 0
    ),
    emotional_agree = case_when(
      `emotional?`[[1]] == `emotional?`[[2]] ~ 1,
      TRUE ~ 0
    ),
    good_writing_agree = case_when(
      `good writing?`[[1]] == `good writing?`[[2]] ~ 1,
      TRUE ~ 0
    ),
    feasible_agree = case_when(
      `feasible?`[[1]] == `feasible?`[[2]] ~ 1,
      TRUE ~ 0
    ),
    concrete_agree = case_when(
      `concrete?`[[1]] == `concrete?`[[2]] ~ 1,
      TRUE ~ 0
    ),
    agree =
      personal_agree + emotional_agree + good_writing_agree +
        feasible_agree + concrete_agree
  ) %>%
  select(-matches("_agree")) %>%
  select(agree, everything())

round(prop.table(table(evaluated$agree)) * 100, digits = 1)

## Distribution of features
round(prop.table(table(evaluated$`personal?`)) * 100, digits = 1)
round(prop.table(table(evaluated$`emotional?`)) * 100, digits = 1)
round(prop.table(table(evaluated$`good writing?`)) * 100, digits = 1)
round(prop.table(table(evaluated$`feasible?`)) * 100, digits = 1)
round(prop.table(table(evaluated$`concrete?`)) * 100, digits = 1)

round(prop.table(table(evaluated$`personal?`, evaluated$eval)) * 100, digits = 1)
round(prop.table(table(evaluated$`emotional?`, evaluated$eval)) * 100, digits = 1)
round(prop.table(table(evaluated$`good writing?`, evaluated$eval)) * 100, digits = 1)
round(prop.table(table(evaluated$`feasible?`, evaluated$eval)) * 100, digits = 1)
round(prop.table(table(evaluated$`concrete?`, evaluated$eval)) * 100, digits = 1)

## Binning choices: emotional X concrete
binned <- evaluated %>%
  mutate(
    bin = case_when(
      `emotional?` == 1 & `concrete?` == 1 ~ "emotional and concrete",
      `emotional?` == 1 & `concrete?` == 0 ~ "emotional and abstract",
      `emotional?` == 0 & `concrete?` == 1 ~ "dry and concrete",
      `emotional?` == 0 & `concrete?` == 0 ~ "dry and abstract"
    )
  ) %>%
  select(bin, agree, everything()) %>%
  filter(agree >= 4)

binned %>%
  filter(agree == 5) %>%
  select(-agree, -date_petitioned) %>%
  select(-contains("personal"), -contains("feasible"), everything()) %>%
  View()

## 국가의 존립이 달린 혼인률 --> grifting, leaving out of sample
## 인구감소및저출산 방한 문제 --> disappeared?
## 출산율과 유관가치관 전환 --> needs a youtube video, so leaving out of sample
## 저출산대책 --> disappeared
## 출산장려 캠페인 복지부에서 함께해요!! --> grifting
## 출산정책 --> disappeared
## Coded bad writing, but reasonably good grammar
## --- 저출산에 아기를 갖기 위한
## --- 출산 기여 연금제
## --- 출산율을 높이기 위한 제안
## --- 출산직 공무원
## --- and more

## 2024 manually chosen ---> +2023 manually added

## dry - abstract - bad grammar (2) ---> (3)
## dry - abstract - good grammar (3) ---> (3)
## dry - concrete - bad grammar (0) ---> (1)
## dry - concrete - good grammar (2) ---> (2)
## emotional - abstract - bad grammar (3) ---> (3)
## emotional - abstract - good grammar (0) ---> (2)
## emotional - concrete - bad grammar (0) ---> (4)
## emotional - concrete - good grammar (3) ---> (4)

## Final selection for pilot
pilot <- df_full %>%
  select(
    title, date_petitioned, `현황 및 문제점`, `개선방안`, `기대효과`, attachment
  ) %>%
  mutate(
    file = case_when(
      title == "여성장애인 출산에 대한 다양한 서비스 정보 제공" ~ 
        "dry-abstract-badgrammar-01",
      grepl("출산정책 : 아기가 갖고 싶은데 않되는 산모", title) ~ 
        "dry-abstract-badgrammar-02",
      grepl("저출산 고령화 문제를 해결하기 위해 ", title) ~ 
        "dry-abstract-goodgrammar-01",
      grepl("고출산을위해", title) ~ 
        "dry-abstract-goodgrammar-02",
      grepl("복지출산급여제도 신", title) ~ 
        "dry-concrete-badgrammar-01",
      grepl("출산및 인구 장려책", title) ~ 
        "dry-concrete-badgrammar-02",
      grepl("아기환영", title) ~ 
        "dry-concrete-goodgrammar-01",
      grepl("일과 육아의 조화", title) ~ 
        "dry-concrete-goodgrammar-02",
      grepl("인구감소및저출산 출산지원금", title) ~ 
        "emotional-abstract-badgrammar-01",
      grepl("출생신고대행도우미", title) ~ 
        "emotional-abstract-badgrammar-02",
      grepl("임신육아종합포털아이사랑", title) ~ 
        "emotional-abstract-goodgrammar-01",
      grepl("난임부부지원 지자체 6개월 제한", title) ~ 
        "emotional-abstract-goodgrammar-02",
      grepl("난임부부지원정책 사각지대", title) ~ 
        "emotional-concrete-badgrammar-01",
      grepl("저출산 정책 현실적이고", title) ~ 
        "emotional-concrete-badgrammar-02",
      grepl("난임비 시술지원비 관련 제안드립니다", title) ~ 
        "emotional-concrete-goodgrammar-01",
      grepl("장애아 출산율을 낮출 수 있도록", title) ~ 
        "emotional-concrete-goodgrammar-02"
    )
  ) %>%
  filter(!is.na(file))

pilot <- pilot[!duplicated(pilot), ]
nrow(pilot)

pilot %>%
  writexl::write_xlsx(here("data", "tidy", "pub_petition_pair_pilot.xlsx"))

# For thermometer scaling 2024 =================================================
df_scale <- df_full %>%
  filter(branch == "보건복지부") %>%
  filter(year == 2024) %>%
  mutate(
    n1 = nchar(`현황 및 문제점`),
    n2 = nchar(`개선방안`),
    n3 = nchar(`기대효과`)
  ) %>%
  filter(status != "제안심사")

## Not worth keeping
df_scale %>%
  filter(n1 < 10) %>%
  View()

## Deleting anomalies
df_scale <- df_scale %>%
  filter(n1 >= 10) %>%
  filter(
    !(
      `현황 및 문제점` %in% c(
        "첨부자료 참고 바랍니다.", "첨부파일있습니다", "첨부와 같습니다.",
        "첨부파일 있음", "첨부물 참조", "첨부파일 참고"
      )
    )
  ) %>%
  filter(
    !(`개선방안` %in% c("첨부파일 참고", "첨부파일을 참고해주세요"))
  ) %>%
  filter(!(title == "제안은 아니고 문의입니다 (죄송)"))

df_scale %>%
  filter(n2 < 10) %>%
  View()

df_scale %>%
  filter(n3 < 10) %>%
  View()

## 790 rows. For pilot, sample
set.seed(1203)
df_scale_sampled <- df_scale %>%
  sample_n(10)
View(df_scale_sampled)

df_scale_sampled %>%
  select(-area, -branch, -n1, -n2, -n3, -status, -date_implemented) %>%
  select(-year, -`실시 결과`, -date_scraped, -`검토내용`, -date_answered) %>%
  select(-attachment, everything()) %>%
  writexl::write_xlsx(
    here("data", "tidy", "pub_petition_scale_health_2024.xlsx")
  )
