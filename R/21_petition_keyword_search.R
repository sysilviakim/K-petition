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


## create a full text variable (title + current issues + improvement plan + expected effect)
gpt <- gpt %>%
    mutate(text = paste(title, current_issues, improvement_plan, expected_effect, sep="\n"))

## issue-specific subsets based on keywords
emotional <- gpt %>%
    filter(grepl(text,pattern="짜증"))

## handpick emotional + alpha petitions
emotional_df <- emotional[c(2,8,17,26,7,9,25),]
openxlsx::write.xlsx(emotional_df,"emotional_samples.xlsx")

china <- gpt %>%
    filter(grepl(text,pattern="중국"))

## other dimensions to examine
## 1. personal vs not personal
## personal: 자기 또는 지인의 직접적 경험에 기반한 처원, 자기에게 일어난 일 구체적으로
## not personal: 자기와 직접적으로 관련 없는 일, 국가적 일이나 철학적/일반론적 사건에 대한 청원

## 2. national vs local (political vs practical?)
## national: 국가적 이슈, 높은 확장성, 하지만 낮은 구체성 (i.e. 국가경제, 외교, ...)
## local: 특정 지역에 대한 이슈, 낮은 확장성, 하지만 높은 구체성

## gpt[4951,] "세금으로 무너지는 대한민국이 될 것!!!!\n코로나 발생시점 부터 개인 자영업자들이 고통당하는 모습을 보아왔다. 그 이후로는 러시아 푸틴의 전쟁발발로 인한 인플레이션으로 인해, 시진핑의 군비확장으로 인한 대중국 제제로 인해 발생하는 국내의 수출입 축소로 인한 경제위축이라는 거시경제의 여건이 악화되어가고 출산율 감소, 노령인구 증가로 인해 사방으로 경제적인 어려움이 가중되고 가고 이러한 경제환경 속에 풀뿌리라고 할 수 있는 자영업자의 고통은 불보듯 뻔한 것이다. 정상적인 영업이 안되면 각종 부채와 부채의 이자로 인해 이중, 삼중의 고통과 엮여있는 가족구성원들의 고통은 누가 감당할 것인가?\n\n얼마전 지인을 만나려고 일산 화정을 가는 도중에 행신을 지나치게 되었는데, 전에는 상권이 그나마 좋았는데 이제는 마치 영화촬영장과 같이 상가는 다 텅텅비었고 인적도 없어서 본인 마저도 밤에는 못다니겠다라는 생각이 들 정도였다. 뿐만 아니라 상권이 좋다는 홍대권에도 불과 몇 백미터만 벗어나면 빈 공실이 상당히 있을 정도로 자영업자들의 현 상황을 대변해 준다.\n\n\n그렇다면 이 문제가 과연 자영업자에게만 한정된 문제인가?\n\n나오지 않으면 임대인은 무지막지한 재산세와 건강보험료 그리고 자신들의 생계가 막혀버린다.\n\n이런 상황이 조금만 더 지나면 이러한 매물이 경매에 우후죽순 나올 것이며 이는 전국적인 부동산 폭락으로 이어질 것임은\n\n명약관화한 결과인 것이다.\n\n\n월급쟁이, 공무원, \n\n세비받아 먹는 분들이야 나랑 뭔 상관이냐는 생각으로  '있는 놈'의 돈 뜯어먹기의 사상으로 살 터이지만\n\n이렇게 대내외적으로 악화일로로는 결국 국가신용도가 하락할 것이고\n\n그렇다면 대한민국이란 배에 탄 모두는 한꺼번에 침몰할 것이다.\n\n\n내가 사는 지역에는 해당 구청장이 플랭카드를 곳곳에 갖다 붙여놓았다.\n\n'무리한 임대료 상승은 임대인과 임차인의 공멸의 지름길'(?)\n\n\n이 시점에서 공멸의 원흉은 임대인이 아니라, '무리한 세금을 징수하는 국가나 지자체'라고 생각한다.\n\n\n재산세만 해도 임대료의 (1/6)을 띁어간다. 소득세도 상당히 뜯어간다. 건강보험료도 상당히 뜯어간다.\n\n임대료는 연 5%이상 올리지 못하게 묶어놓았다. 최종적으로는 상속세, 증여세로 거반 다 뜯어간다.\n\n국가와 지자체가 다 훑어먹고 있다고 생각한다.\n\n이걸 대한민국은 자본주의 국가라고 부르는가?\n\n\n나는 자동차도 굴리지 못한다. \n\n자동차 굴릴 돈도 없지만 기름값도 주차비도 감당이 안되고, 자동차 보유로 인한 건강보험료도 무섭다.\n\n그래서 자전거를 타고 다니며 5000원짜리, 7000원짜리 점심을 먹는다.\n\n국가와 지자체가  나에게 해준게 무엇이 있다고 1년에 수 천만원씩 재산세를 뜯어가는가?\n\n회계상으로도 10년이면 감가상각처리되어 가격이 \"0원\"처리 시키는데\n\n45년된 건물에 수 십만원씩 재산세를 물리고 있다.\n\n\n이렇게 돈은 국가나 지자체에서 거반 다 뜯어가면서,  임대인과 임차인의 공멸을 말할 자격이 있나?\n\n\n이건 세금이 아니라 '조폭'이란 생각마저 개인적으로 든다. 이걸 조세저항이라고 부른다.\n\n그러니 돈 많은 사람들이 외국으로 이민가는 게 바로 이렇기 때문이다.\n임대인, 임차인 공멸을 논한 상황이 아니다.\n\n대한민국 전체가 공멸할 상황이다.\n\n\n앞서 언급한 대로 국가와 지자체에서 상당부분 소득을 뜯어가니\n\n자영업자들에게 뿌려질 돈이 없다.  경제가 돌아갈 마중물이 없다.\n\n\n재산세(?) 현금화 되지 않은 재산이 무슨 의미가 있나? \n\n그 재산이 1조짜리라 할지라도 소득은 1억이 나오면 1억짜리인 것이다.\n\n물론 그 재산을 처분해서 1조 현금화 되면 그때는 그에 따라 양도세를 내면 된다.\n\n\n재산세를 폐지하고 거기서 창출되는 소득으로 세금이 부과되어야 한다. 건강보험료도 마찬가지이다.\n\n( OOO이란 인간, 20억짜리 약을 건강보험 적용시켜서, 모든 사람들에게 보험료 부담을 가중시켰다.)\n그러면 재산세로 뜯긴 돈이 소비로 이어지면서 경제가 살아날 것이라고 생각한다."

## some toy examples
national_df <- rbind(china[1,],
                  china[4,],
                  china[6,],
                  china[18,],
                  china[19,],
                  china[21,],
                  emotional[7,])
openxlsx::write.xlsx(national_df,"national_samples.xlsx")

personal_df <- rbind(emotional[15,],
                  emotional[21,],
                  emotional[9,])
openxlsx::write.xlsx(personal_df,"personal_samples.xlsx")


## assign two issues to each
## Kyusik
lowbirth <- gpt %>% 
    filter(grepl(text,pattern="출산율"))
education <- gpt %>%
    filter(grepl(text,pattern="사교육"))

## BK
delivery <- gpt %>% 
    filter(grepl(text,pattern="배달"))
rider <- gpt %>%
    filter(grepl(text,pattern="자전거")) ## 교통, 자전거, 킥보드

## Seo-young
real <- gpt %>% 
    filter(grepl(text,pattern="부동산"))
pension <- gpt %>% 
    filter(grepl(text,pattern="연금"))
