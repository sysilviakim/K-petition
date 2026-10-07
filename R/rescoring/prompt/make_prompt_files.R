# Writes the prompt files in this folder. Byte-exact reconstruction of the prompt used in the
# 2025 gpt-4o-mini scoring (7_final_gpt_scoring.r, 2025-06-10).
# The original builds the prompt with base::paste(), whose default sep is a single space.
# We reproduce that call verbatim with a placeholder in place of the petition body.
build_prompt <- function(text) {
    paste(
        "당신은 공공 제안을 평가하는 전문가입니다.",
        "아래 민원 내용을 평가 기준에 따라 신중하게 검토하고, 항목별로 1점(매우 낮음)부터 5점(매우 높음)까지 차별화된 점수를 부여해 주세요.",
        "모든 항목에 대해 점수(score)와 간결하면서도 구체적인 평가 사유(reason)를 JSON 형식으로 작성해 주세요.",
        "특히 3점이나 4점에 점수가 집중되지 않도록 유의하며, 민원 내용의 질에 따라 높은 점수와 낮은 점수를 명확히 구분해 주세요.",
        "",
        "반드시 다음 형식의 JSON으로 응답해 주세요:",
        "\n예시:\n",
        "{\n",
        "  \"clarity_specificity\": {\n",
        "    \"score\": 4,\n",
        "    \"reason\": \"핵심 주장이 명확하며, 사례가 간결하게 제시되어 있어 이해하기 쉬움.\"\n",
        "  },\n",
        "  \"logic_consistency\": {\n",
        "    \"score\": 2,\n",
        "    \"reason\": \"근거가 단편적이고 주장과의 연결이 약함. 문장 간 흐름도 부자연스러움.\"\n",
        "  },\n",
        "  \"tone_manner\": {\n",
        "    \"score\": 5,\n",
        "    \"reason\": \"정중하고 객관적인 표현으로 민원을 작성했으며, 감정적 표현이 배제됨. 문법 오류도 없음.\"\n",
        "  },\n",
        "  \"validity_feasibility\": {\n",
        "    \"score\": 3,\n",
        "    \"reason\": \"제안은 타당하나, 관련 예산이나 권한 범위에 대한 고려가 부족함.\"\n",
        "  }\n",
        "}",
        "\n\n※ 참고: 다음은 낮은 점수(1점)의 예시입니다.",
        "{\n",
        "  \"tone_manner\": {\n",
        "    \"score\": 1,\n",
        "    \"reason\": \"욕설과 비방이 포함되어 있으며, 인신공격적 표현으로 인해 민원으로서 부적절함.\"\n",
        "  }\n",
        "}",
        "\n\n[민원 평가 기준]",
        "\n1. 내용의 명확성 및 구체성 (clarity_specificity)",
        "- 민원의 핵심 주장과 요청사항이 명확하고 이해하기 쉬운지 평가해 주세요.",
        "- 구체적인 사실, 사례, 또는 자료가 제시되었는지 확인해 주세요.",
        "- 필요한 경우 용어나 배경 설명이 충분한지도 고려해 주세요.",
        "\n2. 논리성과 일관성 (logic_consistency)",
        "- 주장과 근거 사이에 논리적인 연결이 있는지 평가해 주세요.",
        "- 문장과 내용의 흐름이 자연스럽고 일관적인지 살펴봐 주세요.",
        "\n3. 적절한 표현과 태도 (tone_manner)",
        "- 감정적 표현이 과도하지 않고, 정중하고 목적에 부합하는 방식으로 작성되었는지 확인해 주세요.",
        "- 비방, 욕설, 인신공격이 없는지 검토하고, 문법과 맞춤법이 적절한지도 고려해 주세요.",
        "\n4. 타당성과 실현 가능성 (validity_feasibility)",
        "- 제안이 현실적이고 실행 가능한지, 예산·법·제도 등의 여건상 가능성이 있는지를 판단해 주세요.",
        "- 문제의 시급성, 효과성, 형평성을 고려하고 있는지 살펴봐 주세요.",
        "- 해당 기관의 권한과 업무 범위 내에서 처리 가능한 사안인지 검토해 주세요.",
        "- 제안이 개인 이익이 아닌 공익에 기여하는지를 판단해 주세요.",
        "\n\n[민원 본문]",
        text
    )
}
# combined_text template from 7_final_gpt_scoring.r lines 36-45 (paste with sep = "\n\n")
build_body <- function(current_issues, improvement_plan, expected_effect) {
    paste(
        "[현황 및 문제점]", current_issues,
        "\n\n[개선방안]", improvement_plan,
        "\n\n[기대효과]", expected_effect,
        sep = "\n\n"
    )
}
PLACEHOLDER <- "{{PETITION_BODY}}"
out_dir <- here::here("R", "rescoring", "prompt")
p <- build_prompt(PLACEHOLDER)
writeLines(p, file.path(out_dir, "prompt_4dim_exact_ko.txt"), useBytes = TRUE, sep = "")
writeLines("당신은 공공 정책 제안(민원)을 평가하는 AI입니다.", file.path(out_dir, "system_message_exact_ko.txt"), useBytes = TRUE, sep = "")
writeLines(build_body("{{current_issues}}", "{{improvement_plan}}", "{{expected_effect}}"), file.path(out_dir, "body_template_exact_ko.txt"), useBytes = TRUE, sep = "")
cat("prompt nchar:", nchar(p), " bytes:", nchar(p, type = "bytes"), "\n")
