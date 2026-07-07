#!/usr/bin/env python3
"""
LLM-based multi-label classification of open-ended survey responses (Q16) about
Korea's e-petition system, with a regex baseline ported from openended.R for an
agreement/validation check.

Pipeline:
  1. Load Q16 responses from the survey workbook.
  2. Classify each response with an LLM into the 14-category codebook
     (multi-label; one "primary" label flagged for comparability with regex).
  3. Re-run the original regex classifier (single-label) in Python.
  4. Write the merged classification + an LLM-vs-regex agreement report.

Outputs (under output/):
  openended_llm_labels.jsonl       cache (one JSON object per response NO)
  openended_llm_classified.csv     NO, response, primary, all_labels, regex, rationale
  openended_llm_summary.csv        primary-label frequency table
  openended_agreement.csv          LLM-primary vs regex agreement + Cohen's kappa

Feeds paper/open.tex (via code/analyze_openended.R):
  openended_llm_classified.csv -> Table 1 (tab:openended), Table 3 (tab:chisq), Figures 1 & 4-6
  Cohen's kappa (~0.57) and the "Other" 23.6%->3.6% comparison -> Sec. 3.2 footnote & Appendix B

Usage:
  python3 code/llm_classify.py --limit 40      # pilot on first 40 responses
  python3 code/llm_classify.py                 # full run (cached; safe to re-run)
  python3 code/llm_classify.py --model gpt-4.1 # override model

The OpenAI key is read from $OPENAI_API_KEY, else from the file given by
--key-file (default: the project's BK_api_key.txt).
"""

import argparse
import json
import os
import re
import sys
from pathlib import Path

import pandas as pd

# --------------------------------------------------------------------------- #
# Paths
# --------------------------------------------------------------------------- #
ROOT = Path(__file__).resolve().parent.parent
SURVEY_XLSX = ROOT / "data" / "main" / "공개 청원 및 민원에 대한 인식조사(1,222's).xlsx"
OUT_DIR = ROOT / "output"
CACHE = OUT_DIR / "openended_llm_labels.jsonl"
DEFAULT_KEY_FILE = ROOT / "BK_api_key.txt"  # fallback only; $OPENAI_API_KEY takes precedence

# --------------------------------------------------------------------------- #
# Codebook (IDs kept identical to the regex scheme in openended.R)
# --------------------------------------------------------------------------- #
CODEBOOK = {
    1:  ("Lack of effectiveness",
         "Doubts the system produces real change; petitions ignored, symbolic, not reflected in policy, ineffective (실효성 없음, 무시, 보여주기, 유명무실)."),
    2:  ("Frivolous / Emotional petitions",
         "Petitions are indiscriminate, emotional, baseless, abusive, excessive in number, venting/tantrums (무분별, 감정적, 억지, 무지성, 남발, 도배)."),
    3:  ("Transparency / Feedback deficiency",
         "Cannot tell the outcome or progress; no feedback, results not disclosed (결과 모름, 피드백 없음, 불투명, 진행 여부 모름)."),
    4:  ("Slow processing / Bureaucratic delays",
         "Processing or responses are slow, delayed, take too long (느림, 지연, 오래 걸림, 답변 지연)."),
    5:  ("Need for filtering / Guidelines",
         "Wants screening, standards, formats, vetting, classification before petitions are posted (필터링, 가이드라인, 양식, 기준, 심사, 선별)."),
    6:  ("Readability / Communication issues",
         "Petitions hard to read/understand; weak logic, weak evidence, poor expression, unclear (가독성, 이해 어려움, 논리 부족, 근거 부족, 전달력)."),
    7:  ("Personal / Selfish petitions",
         "Petitions pursue private/individual interest rather than the public good (개인적, 사적, 이기적, 사익, 공익 아님)."),
    8:  ("Positive / Supportive",
         "Positive, satisfied, supportive; a good/necessary system, works well (긍정, 좋다, 만족, 바람직, 필요한 제도)."),
    9:  ("No opinion / Don't know",
         "Non-answer: blank, don't know, no particular thought, not applicable, no experience (없음, 모름, 잘 모르겠다, 관심 없음). Use ALONE."),
    10: ("Mob mentality / Opinion manipulation",
         "Bandwagon, mobilization, manipulation, political/partisan instrumentalization, agitation (여론몰이, 조작, 선동, 정치적 이용, 편향, 진영)."),
    11: ("Need for more participation / Awareness",
         "Wants more participation, promotion, awareness, accessibility, ease of use (참여 필요, 홍보, 활성화, 접근성, 인지 부족)."),
    12: ("Feasibility / Unrealistic proposals",
         "Proposals unrealistic, abstract, not feasible, lacking concreteness/expertise (비현실적, 실현 가능성 낮음, 추상적, 탁상공론, 전문성 부족)."),
    13: ("Civil servant burden / Institutional constraints",
         "Burden on officials/agencies; institutional, legal, budget, manpower, authority limits; political will (공무원 부담, 인력, 행정, 제도적 한계, 예산, 권한 부족)."),
    14: ("Other",
         "Substantive opinion that fits none of 1-13. Use ALONE, only when no other label applies."),
}

SUBSTANTIVE_IDS = [i for i in CODEBOOK if i not in (9, 14)]

SYSTEM_PROMPT = (
    "You are an expert Korean-language survey coder. You classify open-ended "
    "responses to the question: \"In general, how do you feel about Korea's "
    "public petition and civil-complaint system? Describe your thoughts and "
    "feelings freely.\" Respondents are ordinary citizens; answers are short, "
    "colloquial Korean and may raise more than one concern.\n\n"
    "Assign every applicable category from the codebook (multi-label). Rules:\n"
    "- Categories 9 (No opinion) and 14 (Other) are exclusive: if you use "
    "either, it must be the ONLY label.\n"
    "- Use 9 only for genuine non-answers (blank, \"모름\", \"없음\", "
    "\"관심 없음\", no experience).\n"
    "- Use 14 only for a substantive opinion that truly fits none of 1-13.\n"
    "- Otherwise assign one or more of categories 1-13.\n"
    "- 'primary' is the single most salient category id for the response.\n"
    "- 'rationale' is a brief (<=12 words) English justification.\n"
    "Return strictly the requested JSON."
)

CODEBOOK_TEXT = "\n".join(
    f"{i}. {name}: {desc}" for i, (name, desc) in CODEBOOK.items()
)

# --------------------------------------------------------------------------- #
# Regex classifier — ported verbatim from code/openended.R category_rules
# (priority order matters: first match wins; 9 checked first, 14 catch-all)
# --------------------------------------------------------------------------- #
REGEX_RULES = [
    (9,  r"^(없음|없습니다|모르겠|모릅니다|잘\s*모르|특별히|딱히|잘\s*모름|특이사항|글쎄|잘\s*모르겠|해당없|관심없|생각없|별다른|없어요|없네요|없다$|몰라|모름|경험없|이용.*않아|해보지|해\s*본\s*적|없는것 같|없는거 같|없는\s*것\s*같|관심이 없|신경.*안|잘은 모르|모르겠습니다|없음\.|해당 없|의견 없|^\.$|^\s*$|안녕|그냥|그래서|서술형|문제점\s*없|문제.*없|^\s*\.\s*$)"),
    (8,  r"(긍정|좋은|좋아요|좋다|좋습니다|잘\s*되|잘\s*운영|만족|괜찮|훌륭|감사|좋은\s*제도|필요한\s*제도|바람직|현재.*좋|긍적적|응원|유익|잘\s*하고)"),
    (2,  r"(무분별|감정적|감정에\s*호소|화풀이|분풀이|억지|떼[^거]|막무가내|무지성|쓸데없|허무맹랑|터무니없|헛소리|비합리|성의없|장난|도배|악용|남용|악성|사소한|하찮|시비|어처구니|어이없|말도.*안.*되|황당|똥글|의미없는|쓸모없|비이성|과격|비방|욕설|너무.*많[은다이]|많이.*올라|남발|난무|합리적이지.*않|아무거나|주먹구구|타당하지|타당.*않|찌질|이슈몰이|불필요한.*민원|지나치|한계가 있|과도하|불필요한.*청원)"),
    (7,  r"(개인적|사적인|이기적|개인.*불만|개인.*불편|개인.*이익|사리사욕|이해관계|본인.*억울|사익|개인$|극히.*개인|사적\s*감정|개인의\s*감정|자기.*불만|개인적인.*민원|개인적인.*사안|개인.*불평|본인.*위주|자기.*입장|자기만|본인들.*불편|사사로운|공익.*아닌|사적.*이득|공익.*보다.*개인|개인.*사례|피해.*본.*듯)"),
    (10, r"(여론.*몰이|여론.*조작|떼거지|우르르|몰리는|선동|편향|정치.*이용|정치적|좌파|우파|특정\s*정당|민주당|국민의힘|정파|진영|이념|목소리.*큰|조작|맹목|부화뇌동)"),
    (1,  r"(실효성|실효|효과.*없|해결.*안|해결.*못|해결.*되지|해결이 안|반영.*안|반영.*되지|반영.*못|무시|무용|소용없|소용.*없|무의미|보여주기|형식적|허울|방치|유명무실|힘.*없|귀에.*경|소귀.*경|소리.*없|실현.*불가|효과.*찾기|답\s*없|안\s*들어|제대로.*안|외면|묵살|실현.*안|실현.*못|흐지부지|변화.*없|개선.*안|개선.*없|개선.*못|실질.*없|의문이다|의문$|반영될지|정책에.*반영|들어주는지|실행.*안|바뀌지.*않|결론.*없|수리.*드물|미처리|보여지기|처리.*안|기대하기.*어|결과.*기대|원하는.*결과|이어지지|안이어진|못하고|해결점|해결책|실행되어지지)"),
    (12, r"(실현.*가능|현실.*가능|현실성|비현실|실현불가|가능성.*낮|가능성.*없|이상적|추상적|구체적.*부족|구체성|현실화.*어|현실적.*부족|현실적이지|탁상공론|공허|실행가능|역량.*부족|역량.*한계|비전문|전문가가.*아닌|전문성.*부족|일반.*역량|일반인.*한계|미흡한.*부분|대안.*부족|수용.*한계|수용.*어|모든.*해결.*어)"),
    (3,  r"(투명|피드백|팔로우|후속|결과.*모르|결과.*알.*수|진행.*모르|진행.*알.*수|어떻게.*되었|공개.*안|공개.*부족|처리.*결과|시행.*여부|시행.*모르|답변.*없|회신.*없|확인.*어|알\s*수\s*없|알수없|알수가 없|불투명|공개성|진행여부|진행.*여부|되었는지|개선.*되었|실현되는지|개선여부|궁금|추적)"),
    (4,  r"(느리|늦|속도|지연|오래.*걸|시간.*걸|시간.*부족|신속|빠르|빨리|시간이.*많이|처리.*늦|처리.*느|처리.*오래|답변.*지연|답변.*늦|답변.*오래|시일)"),
    (5,  r"(필터|가이드|양식|형식|기준|포맷|규격|체계|분류|검증|심사|선별|걸러|걸름|스크리닝|사전.*검토|사전.*필터|일정.*형식|표준화|규정|틀.*없|틀이.*없|명확한.*기준|중구난방|들쑥날쑥)"),
    (6,  r"(가독|읽기|이해.*어|이해.*힘|복잡|전달.*부족|전달력|내용.*어|명확.*않|명확하지|알아보기.*어|논리.*부족|근거.*부족|설득력|표현.*부족|어휘|문체|논조|글쓰기|글.*어|내용.*복잡|핵심.*잃|소통.*한계|전달.*어|전달하기|정확.*전달|논리적.*근거|의견.*다[르를]|일률적|공감.*다|단순.*의견|공정성)"),
    (13, r"(공무원|담당자|인력|부서|기관|행정|관료|업무.*과다|업무.*과|업무량|처리.*어|처리.*힘|인당|전담|예산|법제화|국회|제도.*한계|제도적|법적.*한계|권한.*부족|떠넘기|돌려막기|절차.*간소|일관성.*미흡|정권|현장.*넘기|정부.*의지)"),
    (11, r"(참여|홍보|활성화|관심.*필요|알려|인지.*부족|알지.*못|모르는.*사람|이용.*적|접근성|사용.*쉽|편리|편의|접하기|접해보|알려져|인식.*부족|관심.*가져|적극.*참여|아는.*사람.*앎|번거|접근.*어)"),
]


def regex_classify(txt):
    """Single-label regex classifier (port of openended.R::classify_response)."""
    if txt is None or (isinstance(txt, float) and pd.isna(txt)):
        return 9
    t = re.sub(r"\s+", " ", str(txt)).strip()
    if not t:
        return 9
    if len(t) <= 5 and re.search(r"없음|없다|모름|글쎄|없어|모르|좋아", t):
        if re.search(r"좋아|좋다|좋음", t):
            return 8
        return 9
    for cid, pat in REGEX_RULES:
        if re.search(pat, t):
            return cid
    return 14


# --------------------------------------------------------------------------- #
# Data loading
# --------------------------------------------------------------------------- #
def load_responses():
    wb = pd.read_excel(SURVEY_XLSX, sheet_name="Open")
    df = wb[["NO", "Q16"]].copy()
    df["NO"] = df["NO"].astype(int)
    df["response"] = df["Q16"].astype("string")
    return df[["NO", "response"]]


# --------------------------------------------------------------------------- #
# LLM classification
# --------------------------------------------------------------------------- #
RESULT_SCHEMA = {
    "type": "json_schema",
    "json_schema": {
        "name": "petition_codes",
        "strict": True,
        "schema": {
            "type": "object",
            "additionalProperties": False,
            "properties": {
                "results": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "additionalProperties": False,
                        "properties": {
                            "index": {"type": "integer"},
                            "labels": {
                                "type": "array",
                                "items": {"type": "integer", "minimum": 1, "maximum": 14},
                            },
                            "primary": {"type": "integer", "minimum": 1, "maximum": 14},
                            "rationale": {"type": "string"},
                        },
                        "required": ["index", "labels", "primary", "rationale"],
                    },
                }
            },
            "required": ["results"],
        },
    },
}


def make_client(key_file):
    from openai import OpenAI
    key = os.environ.get("OPENAI_API_KEY")
    if not key and key_file and Path(key_file).exists():
        key = Path(key_file).read_text().strip()
    if not key:
        sys.exit("No OpenAI API key found (set $OPENAI_API_KEY or --key-file).")
    return OpenAI(api_key=key)


def classify_batch(client, model, batch):
    """batch: list of (index, text). Returns dict index -> {labels, primary, rationale}."""
    listing = "\n".join(f"[{i}] {t}" for i, t in batch)
    user = (
        "Codebook:\n" + CODEBOOK_TEXT +
        "\n\nClassify each numbered response. Return one result object per "
        "response, echoing its index.\n\nResponses:\n" + listing
    )
    resp = client.chat.completions.create(
        model=model,
        temperature=0,
        messages=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": user},
        ],
        response_format=RESULT_SCHEMA,
    )
    data = json.loads(resp.choices[0].message.content)
    out = {}
    for r in data["results"]:
        labels = sorted(set(int(x) for x in r["labels"]))
        primary = int(r["primary"])
        # enforce exclusivity of 9 and 14
        if 9 in labels:
            labels, primary = [9], 9
        elif 14 in labels and len(labels) > 1:
            labels = [x for x in labels if x != 14]
            if primary == 14:
                primary = labels[0]
        if primary not in labels and labels:
            primary = labels[0]
        out[int(r["index"])] = {
            "labels": labels, "primary": primary,
            "rationale": r.get("rationale", ""),
        }
    return out


def load_cache():
    done = {}
    if CACHE.exists():
        for line in CACHE.read_text().splitlines():
            if line.strip():
                obj = json.loads(line)
                done[int(obj["NO"])] = obj
    return done


def append_cache(obj):
    with CACHE.open("a") as f:
        f.write(json.dumps(obj, ensure_ascii=False) + "\n")


# --------------------------------------------------------------------------- #
# Agreement
# --------------------------------------------------------------------------- #
def cohen_kappa(a, b):
    import numpy as np
    cats = sorted(set(a) | set(b))
    idx = {c: i for i, c in enumerate(cats)}
    n = len(a)
    m = np.zeros((len(cats), len(cats)))
    for x, y in zip(a, b):
        m[idx[x], idx[y]] += 1
    po = np.trace(m) / n
    pe = (m.sum(0) * m.sum(1)).sum() / (n * n)
    return (po - pe) / (1 - pe) if pe < 1 else 1.0, po


# --------------------------------------------------------------------------- #
# Main
# --------------------------------------------------------------------------- #
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--model", default="gpt-4.1-mini")
    ap.add_argument("--limit", type=int, default=None, help="classify only first N (pilot)")
    ap.add_argument("--batch", type=int, default=20)
    ap.add_argument("--key-file", default=str(DEFAULT_KEY_FILE))
    args = ap.parse_args()

    OUT_DIR.mkdir(exist_ok=True)
    df = load_responses()
    if args.limit:
        df = df.head(args.limit).copy()

    # regex baseline (always, for everyone)
    df["regex"] = df["response"].map(regex_classify)

    # LLM: classify only the not-yet-cached
    done = load_cache()
    todo = [(int(r.NO), (r.response if isinstance(r.response, str) else ""))
            for r in df.itertuples() if int(r.NO) not in done]
    if todo:
        client = make_client(args.key_file)
        print(f"Classifying {len(todo)} responses with {args.model} "
              f"({len(done)} cached) ...", flush=True)
        for i in range(0, len(todo), args.batch):
            chunk = todo[i:i + args.batch]
            # index batch positionally to keep prompt short, map back via NO
            pos = [(j, t) for j, (_no, t) in enumerate(chunk)]
            res = classify_batch(client, args.model, pos)
            for j, (no, t) in enumerate(chunk):
                r = res.get(j, {"labels": [14], "primary": 14, "rationale": "parse-miss"})
                obj = {"NO": no, "response": t, **r, "model": args.model}
                append_cache(obj)
                done[no] = obj
            print(f"  {min(i+args.batch, len(todo))}/{len(todo)}", flush=True)
    else:
        print("All responses already cached; skipping API calls.")

    # assemble
    df["primary"] = df["NO"].map(lambda n: done[n]["primary"] if n in done else None)
    df["all_labels"] = df["NO"].map(
        lambda n: "|".join(map(str, done[n]["labels"])) if n in done else "")
    df["rationale"] = df["NO"].map(lambda n: done[n].get("rationale", "") if n in done else "")
    df["primary_en"] = df["primary"].map(lambda c: CODEBOOK.get(c, ("?",))[0])
    df["regex_en"] = df["regex"].map(lambda c: CODEBOOK.get(c, ("?",))[0])

    df.to_csv(OUT_DIR / "openended_llm_classified.csv", index=False)

    # primary-label frequency
    freq = (df.groupby(["primary", "primary_en"]).size()
            .reset_index(name="n").sort_values("n", ascending=False))
    freq["pct"] = (freq["n"] / freq["n"].sum() * 100).round(1)
    freq.to_csv(OUT_DIR / "openended_llm_summary.csv", index=False)

    # multi-label coverage: how many responses got >1 substantive label
    n_multi = df["all_labels"].map(lambda s: len([x for x in s.split("|") if x]) > 1).sum()

    # agreement vs regex
    k, po = cohen_kappa(list(df["primary"]), list(df["regex"]))

    print("\n========== LLM primary-label frequency ==========")
    print(freq.to_string(index=False))
    print(f"\nMulti-label responses (>1 label): {n_multi} / {len(df)} "
          f"({100*n_multi/len(df):.1f}%)")
    print(f"\nLLM-vs-regex primary agreement: {100*po:.1f}%   Cohen's kappa: {k:.3f}")

    # agreement detail
    agr = (df.assign(agree=df["primary"] == df["regex"])
           .groupby(["regex_en"]).agg(n=("agree", "size"),
                                      agree=("agree", "mean")).reset_index())
    agr["agree_pct"] = (agr["agree"] * 100).round(1)
    agr.drop(columns="agree").to_csv(OUT_DIR / "openended_agreement.csv", index=False)

    print(f"\nWrote: {OUT_DIR}/openended_llm_classified.csv")
    print(f"       {OUT_DIR}/openended_llm_summary.csv")
    print(f"       {OUT_DIR}/openended_agreement.csv")
    print(f"       {CACHE}  (cache)")


if __name__ == "__main__":
    main()
