source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Output from manually selected petition subset
pet <- read_excel("data/screenshots/main/공개제안_전체.xlsx")

## remove petitions that all three reviewers didn't evaluate
pet <- pet %>%
    filter(!is.na(SK) | !is.na(BK) | !is.na(KY))

## final camb by majority vote (GenAI, SK, BK, KY)
maj_vote <- function(x){
    out <- sort(table(x),decreasing=TRUE)
    if(all(out==1)){ ## all votes diff
        out <- "no majority"
    }
    else if(any(out>1)){ ## choose majority
        out <- names(out)[1]
    }
    return(out)
}

pet <- pet %>%
    rowwise() %>%
    mutate(comb_fin = maj_vote(c(combo,SK,BK,KY))) %>%
    ungroup()

## 7 cases that didn't meet majority consensus
## manual filling
pet$comb_fin[c(2,3,5,10,28,29,61)] <- c("1000","1000","0011","1010","1000","1011","1000")

## tabulate
tb <- table(pet$comb_fin,pet$category)
tb;
##
##         배달   부동산   사교육  연금   저출산   킥보드
##  0000    0      3      0    1      2      2
##  0001    1      0      0    1      0      1
##  0010    1      1      2    4      3      0
##  0011    1      0      0    0      0      0
##  0100    0      0      2    0      0      0
##  0101    0      0      0    0      1      1
##  0110    0      0      0    1      1      0
##  0111    0      1      0    0      1      0
##  1000    3      0      1    0      2      2
##  1010    0      5      1    1      1      0
##  1011    2      4      1    0      2      1
##  1110    0      2      0    1      0      0
##  1111    0      1      2    0      2      0

## generate all pairwise comparisons within each category, across combinations
pet_tidy <- pet %>%
    mutate(id = 1:nrow(pet)) %>%
    select(id,category,comb_fin,area,title,date_petitioned,text)

write_xlsx(pet_tidy,"data/screenshots/main/공개제안_tidy.xlsx")

## 배달
get_pairs_df <- crossing(df1 = pet_tidy %>% filter(category == "배달"), df2 = pet_tidy %>% filter(category == "배달")) %>%
    filter(df1$id < df2$id, df1$comb_fin != df2$comb_fin)
pairs_delivery_df <- tibble(category="배달","id1"=get_pairs_df$df1$id,"id2"=get_pairs_df$df2$id,"comb1"=get_pairs_df$df1$comb_fin,"comb2"=get_pairs_df$df2$comb_fin)

## 부동산
get_pairs_df <- crossing(df1 = pet_tidy %>% filter(category == "부동산"), df2 = pet_tidy %>% filter(category == "부동산")) %>%
    filter(df1$id < df2$id, df1$comb_fin != df2$comb_fin)
pairs_apt_df <- tibble(category="부동산","id1"=get_pairs_df$df1$id,"id2"=get_pairs_df$df2$id,"comb1"=get_pairs_df$df1$comb_fin,"comb2"=get_pairs_df$df2$comb_fin)

## 사교육
get_pairs_df <- crossing(df1 = pet_tidy %>% filter(category == "사교육"), df2 = pet_tidy %>% filter(category == "사교육")) %>%
    filter(df1$id < df2$id, df1$comb_fin != df2$comb_fin)
pairs_edu_df <- tibble(category="사교육","id1"=get_pairs_df$df1$id,"id2"=get_pairs_df$df2$id,"comb1"=get_pairs_df$df1$comb_fin,"comb2"=get_pairs_df$df2$comb_fin)

## 연금
get_pairs_df <- crossing(df1 = pet_tidy %>% filter(category == "연금"), df2 = pet_tidy %>% filter(category == "연금")) %>%
    filter(df1$id < df2$id, df1$comb_fin != df2$comb_fin)
pairs_pension_df <- tibble(category="연금","id1"=get_pairs_df$df1$id,"id2"=get_pairs_df$df2$id,"comb1"=get_pairs_df$df1$comb_fin,"comb2"=get_pairs_df$df2$comb_fin)

## 저출산
get_pairs_df <- crossing(df1 = pet_tidy %>% filter(category == "저출산"), df2 = pet_tidy %>% filter(category == "저출산")) %>%
    filter(df1$id < df2$id, df1$comb_fin != df2$comb_fin)
pairs_birth_df <- tibble(category="저출산","id1"=get_pairs_df$df1$id,"id2"=get_pairs_df$df2$id,"comb1"=get_pairs_df$df1$comb_fin,"comb2"=get_pairs_df$df2$comb_fin)

## 킥보드
get_pairs_df <- crossing(df1 = pet_tidy %>% filter(category == "킥보드"), df2 = pet_tidy %>% filter(category == "킥보드")) %>%
    filter(df1$id < df2$id, df1$comb_fin != df2$comb_fin)
pairs_kick_df <- tibble(category="킥보드","id1"=get_pairs_df$df1$id,"id2"=get_pairs_df$df2$id,"comb1"=get_pairs_df$df1$comb_fin,"comb2"=get_pairs_df$df2$comb_fin)
    
pairs_df <- pairs_delivery_df %>%
    bind_rows(pairs_apt_df, pairs_edu_df, pairs_pension_df, pairs_birth_df, pairs_kick_df)

write_xlsx(pairs_df,"data/screenshots/main/공개제안_pairs.xlsx")
