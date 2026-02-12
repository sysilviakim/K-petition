# Setup ========================================================================
source(here::here("R", "utilities.R"))
fname <- here(
  stri_trans_nfc(
    "data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx"
  )
)

## 성x연령 균등할당
## 30대 남 123, 40대 여 123, 나머지 전부 122

time_vars <- paste0("q13_q14_time_", 1:8)

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

# Data wrangling ===============================================================
df <- df %>%
  mutate(
    ## SQ1. 귀하의 성별은 무엇입니까? 1 = 남자, 2 = 여자
    female = as.numeric(SQ1 == 2),
    ## SQ2_2, SQ2. 귀하의 연령은 어떻게 되십니까? - 연령대
    ## SQ2_1, SQ2. 귀하의 연령은 어떻게 되십니까? - 나이
    age = SQ2_1,
    age_group = factor(
      SQ2_2,
      levels = seq(5),
      labels = c("20-29", "30-39", "40-49", "50-59", "60+")
    ),
    ## SQ3. 귀하의 최종 학력은 어떻게 되십니까?
    edu = factor(
      SQ3,
      levels = seq(6),
      labels = c(
        "Middle school or below",
        "High school",
        "Some college",
        "College grad",
        "Some postgrad",
        "Postgrad"
      )
    ),
    edu4 = factor(
      case_when(
        SQ3 <= 2 ~ "High school",
        SQ3 == 3 ~ "Some college",
        SQ3 == 4 | SQ3 == 5 ~ "College grad",
        SQ3 == 6 ~ "Postgrad"
      ),
      levels = c("High school", "Some college", "College grad", "Postgrad")
    ),
    ## SQ4. 현재 살고 계신 곳은 어느 지역입니까? (17개 시도)
    province = SQ4,
    seoul = as.numeric(SQ4 == 1),
    ## SQ5. 귀하의 결혼 여부는 무엇입니까?
    ## 1 = 기혼(유자녀), 2 = 기혼(무자녀), 3 = 미혼, 4 = 기타(이혼, 사별 등)
    ## in hindsight, this was a bad design (if divorced, kids? can't tell)
    married = as.numeric(SQ5 %in% c(1, 2)),
    ## SQ6. 귀하는 영유아 자녀(초등학교에 입학하지 않은 자녀)가 있으십니까?
    has_young_child = as.numeric(SQ6 == 1),
    ## SQ7. 귀하의 지난 1년 동안 세금 납부(공제) 전의
    ## 월평균 총 가구 소득은 얼마입니까?
    income = factor(
      SQ7,
      levels = seq(11),
      labels = c(
        "Under 1 mill. KRW", "1-2 mill. KRW", "2-3 mill. KRW",
        "3-4 mill. KRW", "4-5 mill. KRW", "5-6 mill. KRW",
        "6-7 mill. KRW", "7-10 mill. KRW", "10-15 mill. KRW",
        "15-20 mill. KRW", "Over 20 mill. KRW"
      )
    ),
    ## 2025 년 기준 중위소득
    ## 1인: 2,392,013원
    ## 2인: 3,932,658원
    ## 3인: 5,025,353원
    ## 4인: 6,097,773원
    median_income = factor(
      ifelse(SQ7 > 5, "Above Median", "Below Median"),
      levels = c("Below Median", "Above Median")
    ),
    ## SQ8. 귀하의 직업군을 선택해주세요
    occupation = factor(
      SQ8,
      levels = seq(12),
      ## 1) 직장인
      ## 2) 전업주부
      ## 3) 대학생/대학원생
      ## 4) 전문직
      ## 5) 서비스/영업직
      ## 6) 농림어업
      ## 7) 자영업/개인사업
      ## 8) 공무원/교사
      ## 9) 프리랜서
      ## 10) 기술직
      ## 11) 무직/퇴직/은퇴/취업준비생
      ## 12) 기타
      labels = c(
        "Employee", "Homemaker", "Student", "Professional",
        "Service/Sales", "Agriculture/Fishing", "Self-employed",
        "Public sector/Teacher", "Freelancer", "Technical",
        "Unemployed/Retired", "Other"
      )
    ),
    ## SQ9. 우리 국민들의 생활수준을 상, 중상, 중, 중하, 하의 다섯 단계로
    ## 나눈다면 귀하의 생활수준은 어디에 속한다고 생각하십니까?
    subj_class = factor(
      SQ9,
      levels = seq(5),
      labels = c("Upper", "Upper-middle", "Middle", "Lower-middle", "Lower")
    ),
    subj_class3 = factor(
      case_when(
        SQ9 %in% c(1, 2) ~ "Upper",
        SQ9 == 3 ~ "Middle",
        SQ9 %in% c(4, 5) ~ "Lower"
      ),
      levels = c("Lower", "Middle", "Upper")
    ),
    ## Q1. 현재의 전반적인 생활을 고려할 때 귀하의 삶에 어느 정도 만족하십니까?
    ## 0-10 slider bar
    life_satisfaction = Q1,
    ## Q2. 귀하의 이념성향이 어떠하다고 생각하십니까?
    ideo7 = factor(
      Q2,
      levels = seq(7),
      labels = c(
        ## CES convention
        "Very liberal", "Liberal", "Somewhat liberal",
        "Middle of the road", "Somewhat conservative",
        "Conservative", "Very conservative"
      )
    ),
    ideo3 = factor(
      case_when(
        Q2 %in% c(1, 2, 3) ~ "liberal",
        Q2 == 4 ~ "moderate",
        Q2 %in% c(5, 6, 7) ~ "conservative"
      ),
      levels = c("liberal", "moderate", "conservative")
    ),
    ideology = Q2,
    ## Q3. 다음 중 지지하거나 조금이라도 더 호감이 가는 정당은 무엇입니까?
    ## 현재 국회의원 의석 수 대로 보여드리겠습니다
    pid = factor(
      Q3,
      levels = seq(8),
      ## 1) 더불어민주당
      ## 2) 국민의힘
      ## 3) 조국혁신당
      ## 4) 진보당
      ## 5) 개혁신당
      ## 6) 기본소득당
      ## 7) 사회민주당
      ## 8) 기타
      labels = c(
        "Democratic Party of Korea (더불어민주당)",
        "People Power Party (국민의힘)",
        "Rebuilding Korea Party (조국혁신당)",
        "Progressive Party (진보당)",
        "Reform Party (개혁신당)",
        "Basic Income Party (기본소득당)",
        "Social Democratic Party (사회민주당)",
        "Other"
      )
    ),
    dpk = as.numeric(Q3 == 1),
    ppp = as.numeric(Q3 == 2),
    right_wing = case_when(
      Q3 == 2 | Q3 == 5 ~ 1,
      TRUE ~ 0
    ),
    pid3 = factor(
      case_when(
        Q3 == 2 ~ "PPP",
        Q3 %in% c(1, 3, 4, 6, 7) ~ "Opposition",
        TRUE ~ "Other"
      ),
      levels = c("PPP", "Opposition", "Other")
    ),
    ## Q4. 제20대 대통령 선거(2022년)에서 귀하가 투표한 정당 및 후보자명은
    ## 무엇입니까?
    ## 1) 더불어민주당 (이재명)
    ## 2) 국민의힘 (윤석열)
    ## 3) 정의당 (심상정)
    ## 4) 국민의당 (안철수)
    ## 5) 기타
    ## 6) 투표하지 않음
    vote2022 = factor(
      Q4,
      levels = seq(6),
      labels = c(
        "Democratic Party of Korea (이재명)",
        "People Power Party (윤석열)",
        "Justice Party (심상정)",
        "People's Party (안철수)",
        "Other",
        "Did not vote"
      )
    ),
    voted_yoon_2022 = as.numeric(Q4 == 2),
    ## Q5. 제21대 대통령 선거(2025년)에서 귀하가 투표한 정당 및 후보자명은
    ## 무엇입니까?
    ## 1) 더불어민주당 (이재명)
    ## 2) 국민의힘 (김문수)
    ## 3) 개혁신당 (이준석)
    ## 4) 민주노동당(권영국)
    ## 5) 기타
    ## 6) 투표하지 않음
    vote2025 = factor(
      Q5,
      levels = seq(6),
      labels = c(
        "Democratic Party of Korea (이재명)",
        "People Power Party (김문수)",
        "Reform Party (이준석)",
        "Democratic Labor Party (권영국)",
        "Other",
        "Did not vote"
      )
    ),
    voted_lee_2025 = as.numeric(Q5 == 1),
    ## Q6_1	Q6. [동의도] 1) 한국 사회는 신뢰가 높은 사회이다.
    social_trust_1 = Q6_1,
    ## Q6_2	Q6. [동의도] 2) 한국 사회는 공정과 원칙이 지켜지는 사회이다.
    social_trust_2 = Q6_2,
    ## Q6_3	Q6. [동의도] 3) 한국 사회는 사회적 연대가 잘 이루어지는 사회이다.
    social_trust_3 = Q6_3,
    ## Q6_4	Q6. [동의도] 4) 한국 사회는 국민이 주인인 사회이다.
    social_trust_4 = Q6_4,
    social_trust = (Q6_1 + Q6_2 + Q6_3 + Q6_4) / 4,
    social_trust_high = as.numeric(social_trust > 3),
    ## Q6_5	Q6. [동의도] 5) 전문가나 지식인의 의견보다는
    ##                      보통 사람들의 지혜를 믿는 편이 좋다.
    ## 5-point Likert scale: 1 = 전혀 동의하지 않음, 5 = 매우 동의함
    populism = Q6_5,
    populist = as.numeric(Q6_5 >= 4),
    ## Q6_6	Q6. [동의도] 6) 정치는 결국 국민과 권력자들 사이의
    ##                      싸움이라고 생각한다.
    anti_elite_1 = Q6_6,
    ## Q6_7	Q6. [동의도] 7) 한국 사회에서는 엘리트 집단이 자신들의 이익을 위해
    ##            사회를 운영하고 있으며, 일반 국민들의 목소리는 무시되고 있다.
    anti_elite_2 = Q6_7,
    populism_broad = (Q6_5 + Q6_6 + Q6_7) / 3,
    populist_broad = as.numeric(populism_broad > 3),
    ## Q7: 귀하는 다음의 대한민국 주요 정치 제도 및 기관에 대해서
    ## 얼마나 신뢰하고 계십니까? 5-point Likert scale
    ## 1 = 전혀 신뢰하지 않음, 5 = 매우 신뢰함
    trust_president = Q7_1,
    trust_national_assembly = Q7_2,
    trust_constitutional_court = Q7_3,
    trust_supreme_court = Q7_4,
    trust_election_admin = Q7_5,
    trust_prosecutors = Q7_6,
    trust_central_bank = Q7_7,
    instit_trust = (Q7_1 + Q7_2 + Q7_3 + Q7_4 + Q7_5 + Q7_6 + Q7_7) / 7,
    instit_trust_high = as.numeric(instit_trust > 3),
    ## Q8. 지난 3년간 다음 공개 청원/민원 제도 사용 경험
    ## Check-all-that-apply: non-NA = selected
    ## 1) 국민신문고
    used_epeople = as.numeric(!is.na(Q8_1)),
    ## 2) 청와대 국민청원 게시판
    used_bluehouse = as.numeric(!is.na(Q8_2)),
    ## 3) 국민제안 게시판
    ## 그동안 국민제안을 이용해 주셔서 감사합니다.
    ## 본 홈페이지는 2025년 4월 23일부터 서비스가 종료됩니다.
    used_withpeople = as.numeric(!is.na(Q8_3)),
    ## 4) 청원24
    used_petition24 = as.numeric(!is.na(Q8_4)),
    ## 5) 국회 전자청원
    used_assembly = as.numeric(!is.na(Q8_5)),
    ## 6) 기타
    used_other_platform = as.numeric(!is.na(Q8_6)),
    ## 7) 이용한 적 없음
    never_used_petition = as.numeric(!is.na(Q8_7)),
    any_petition_exp = as.numeric(
      !is.na(Q8_1) | !is.na(Q8_2) |
        !is.na(Q8_3) | !is.na(Q8_4) |
        !is.na(Q8_5)
    ),
    ## Q9. 귀하가 가장 효과적이라고 생각하는 제도는 무엇입니까?
    ## 설문조사 문항을 집중해서 읽고 계시다는 것을 보여주시기 위해서
    ## 국민신문고와 청와대 국민청원 게시판 두 개를 선택해 주십시오.
    attention_check = as.numeric((Q9_1 == 1 & Q9_2 == 2)),
    ## Q10. 공개 청원/민원 제도를 활용한 주요 이유를 선택해주세요
    ## Check-all-that-apply
    ## 1) 누군가의 진심 어린 청원에 도움을 주고 싶어서
    reason_personal_help = as.numeric(!is.na(Q10_1)),
    ## 2) 사회 이슈 개선에 도움이 되고 싶어서
    reason_society_improve = as.numeric(!is.na(Q10_2)),
    ## 3) 다양한 의견이 모이면 변화가 일어날 것 같아서
    reason_possible_change = as.numeric(!is.na(Q10_3)),
    ## 4) 세상을 바꿔야 한다는 생각 때문에
    reason_civic_duty = as.numeric(!is.na(Q10_4)),
    ## 5) 사회 이슈에 대한 나의 생각을 적극적으로
    ##    표현하고 싶어서
    reason_my_voice = as.numeric(!is.na(Q10_5)),
    ## 6) 기타
    reason_other = as.numeric(!is.na(Q10_6)),
    ## Q11. 공개 청원 및 민원에 대해
    ## 다음 문장에 대해 귀하의 동의 정도를 선택해주세요
    ## 5-point Likert: 1 = 전혀 동의하지 않음, 5 = 매우 동의함
    ## 1) 많은 사람들과 함께 공감하고 소통한다는 것이 보기 좋았다
    petition_empathy = Q11_1,
    ## 2) 사회적 연대가 이루어지고 있다는 것을 느꼈다
    petition_solidarity = Q11_2,
    ## 3) 국가가 국민과 소통하고 있다는 생각을 가지게 되었다
    petition_govt_comm = Q11_3,
    ## 4) 특정 세력 및 정파에 의한 여론몰이가 걱정스러웠다
    petition_manipulation = Q11_4,
    ## 5) 청원이 지나치게 무분별하게 올라오는 듯한 느낌이 들었다
    petition_indiscriminate = Q11_5,
    petition_positive = (Q11_1 + Q11_2 + Q11_3) / 3,
    petition_negative = (Q11_4 + Q11_5) / 2,
    ## Q12. 귀하가 생각하기에 공개 청원 및 민원 제도의 개선이 필요한
    ## 주요 사항은 무엇입니까?
    ## 1) 청원/민원 처리 과정의 투명성 강화
    ## 2) 답변의 신속성과 실효성 향상
    ## 3) 청원/민원 등록 기준 강화 (무분별한 청원 방지)
    ## 4) 사용자 편의를 위한 플랫폼 개선 (예: UX/UI 개선)
    ## 5) 홍보 강화로 국민 접근성 확대
    ## 6) 기타
    petition_improvement = factor(
      Q12,
      levels = seq(6),
      labels = c(
        "Transparency",
        "Responsiveness",
        "Stricter registration",
        "Platform UX/UI",
        "Public awareness",
        "Other"
      )
    ),
    ## Response time per respondent
    median_time = apply(
      select(., all_of(time_vars)), 1, median
    ),
    log_time = log(median_time + 1)
  )

# Survey weights ===============================================================
## Map survey codes to demo_weight groups
## SQ1: 1=M, 2=F
## SQ2_2: 1=20s, 2=30s, 3=40s, 4=50s, 5=60+
df <- df %>%
  mutate(
    wt_gender = ifelse(female == 0, "M", "F"),
    wt_age = age_group
  ) %>%
  left_join(
    demo_weight,
    by = c(
      "wt_gender" = "gender",
      "wt_age" = "age_group"
    )
  ) %>%
  rename(wt = weight)
