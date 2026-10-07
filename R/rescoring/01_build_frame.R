source(here::here("R", "rescoring", "rescoring_utils.R"))

# Scoring frame for the 2024 corpus
# 1. Keep the proposals addressed to central government agencies (6,726 rows),
#    using the same branch filter as the 2025 gpt-4o-mini scoring.
# 2. Give each proposal a stable id (pid = row number, the index of eval_<pid>.json
#    in the 2025 run) and a SHA-1 hash of the body text sent to the models.
# 3. Attach the 2025 gpt-4o-mini scores by reading eval_<pid>.json for each pid.
#    Joining by pid, not by file order, is what keeps scores on the right proposal.

# Frame ========================================================================
cols <- c(
  "title", "area", "attachment", "status", "date_petitioned", "date_answered",
  "branch", "current_issues", "improvement_plan", "expected_effect",
  "review_content", "date_scraped", "date_implemented", "implementation_result"
)

branches <- c(
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
)

frame <- read_csv(
  source_csv,
  col_types = cols(.default = col_character()), name_repair = "minimal"
) %>%
  set_names(cols) %>%
  filter(branch %in% branches) %>%
  mutate(pid = row_number(), .before = 1) %>%
  mutate(
    body_sha1 = build_body(current_issues, improvement_plan, expected_effect) %>%
      sha1() %>%
      as.character()
  )
stopifnot(nrow(frame) == 6726)

write_excel_csv(frame, frame_csv, na = "")

# 2025 gpt-4o-mini scores, joined by pid =======================================
gpt4omini <- map_dfr(frame$pid, function(p) {
  j <- read_json(file.path(gpt4omini_dir, paste0("eval_", p, ".json")))
  tibble(pid = p) %>%
    bind_cols(as_tibble(c(
      set_names(map(dims, ~ as.integer(j[[.x]]$score)), paste0(dims, "_score")),
      set_names(map(dims, ~ j[[.x]]$reason), paste0(dims, "_reason"))
    ))) %>%
    select(pid, as.vector(rbind(paste0(dims, "_score"), paste0(dims, "_reason"))))
})

frame %>%
  left_join(gpt4omini, by = "pid") %>%
  write_excel_csv(realigned_csv, na = "")
