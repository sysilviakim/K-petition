# K-Petition — Code & Data Guide

This folder holds the analysis for the **open-ended / citizen co-assessment paper**
(`paper/open.tex`, *Who Co-Assesses? Cohort Divides in the Evaluation of South
Korea's E-Petition System*). It frames e-petitions as a case of citizen
coproduction and shows that the co-assessment of petition quality is cohort-structured.
It shares a survey and petition corpus with the companion paper `paper/quality.tex`
(*Style Over Substance*, pairwise-IRT), which lives in the same `paper/` directory.

Both papers draw on one 2024 survey of 1,222 South Korean adults. This paper uses
two of its modules: the **open-ended question (Q16)** about the petition system,
and the **feeling-thermometer module (Q15)** in which each respondent rated five
real petitions 0–100.

---

## 1. The pipeline at a glance

```
                         data/main/…survey…xlsx   data/공개제안_tidy_url_appended.xlsx
                                   │                          │
        ┌──────────────────────────┼──────────────────────────┼───────────────┐
        │                          │                          │               │
  llm_classify.py            llm_frame.py            (companion LLM quality scores:
  (Q16 → 14 categories)      (Q16 → frame)            /Users/BKmac/Dropbox/K-petition/
        │                          │                  data/tidy/evaluated_data_2024_final.csv)
        ▼                          ▼                          │
  openended_llm_*.csv      openended_frame_classified.csv     │
        │                          │                          │
        ▼                          ▼                          ▼
  analyze_openended.R  ◄───────────┘            analyze_thermometer.R
  (opinion × demographics,                      (thermometer regressions:
   chi-sq + Cramér V, frame-by-age)              quality, bridge, adjudication, frame)
        │                                                     │
        ▼                                                     ▼
  output/*.csv, paper/fig/openended_*.pdf       output/*, paper/fig/*, tab/thermo_*.tex
```

**Run order:** `llm_classify.py` and `llm_frame.py` first (they produce the
classified CSVs), then `analyze_openended.R`, then `analyze_thermometer.R`
(it reads `output/openended_analysis_data.csv` and the frame CSV).

---

## 2. What each script does

### Active scripts (this paper)

| File | Language | What it does | Key outputs |
|---|---|---|---|
| **`code/llm_classify.py`** | Python | Classifies the 1,222 open-ended Q16 responses into the 14-category codebook with an LLM (OpenAI `gpt-4.1-mini`, multi-label, one "primary" label). Also runs a transparent **regex baseline** and reports LLM-vs-regex agreement (Cohen's κ ≈ 0.57). | `output/openended_llm_classified.csv`, `…_summary.csv`, `…_agreement.csv`, `…_labels.jsonl` (cache) |
| **`code/llm_frame.py`** | Python | Codes each Q16 response by its **object of evaluation** — government institution / citizen-content platform / both / neither (sentiment-independent). This is the paper's central object-of-evaluation (frame) construct. | `output/openended_frame_classified.csv`, `openended_frame.jsonl` (cache) |
| **`code/analyze_openended.R`** | R | Demographic analysis of the LLM labels: frequency table, multi-label coverage, **χ² tests (Monte Carlo) with Cramér's V**, per-demographic heatmaps, and the **frame-by-age gradient** (gov:citizen ratio, logistic test, category-grouping proxy, and the `frame_by_age.pdf` figure). | `output/openended_llm_freq.csv`, `…_multilabel.csv`, `…_chisq.csv`, `…_analysis_data.csv`; `paper/fig/openended_*.pdf`, `paper/fig/frame_by_age.pdf` |
| **`code/analyze_thermometer.R`** | R | All thermometer regressions on the 6,110 respondent-petition ratings. Layer 1 (ratings ~ quality dims), Layer 2 (bridge: slope by opinion type), Layer 3 (demographics), plus the **Adjudication + Frame** section: standardized effect size, age shape (linear/quadratic), within-person standardization, class/party slope nulls, the **frame-slope test**, and the eliminated accounts (usage, institutional trust, acquiescence, Q10 individualism). | `output/thermo_model_frame.rds`, `output/thermo_quality_slope_by_opinion.csv`; `paper/fig/thermo_bridge_slopes.pdf`; `tab/thermo_layer{1,2,3}.tex` |

> The `tab/thermo_layer{1,2,3}.tex` files are auto-generated full-model tables
> (via `modelsummary`) kept for checking. The paper itself uses **hand-formatted
> inline tables** in `open.tex` (`tab:thermo_quality`, `tab:bridge`,
> `tab:thermo_demo`, `tab:adjudicate`), so editing those tables means editing
> `open.tex`, not these files.

### Legacy / superseded scripts (kept for the record, **not used** by `open.tex`)

| File | Status |
|---|---|
| `code/openended.R` | Original standalone **regex** classification + χ² + heatmaps. Superseded by the LLM pipeline; the regex baseline now lives inside `llm_classify.py`. (Depends on the companion project's `R/utilities.R`.) |
| `code/sIBP_response.R` | The supervised Indian Buffet Process (sIBP) analysis of the thermometer module that produced an **earlier draft's** results. The sIBP approach was **dropped** from `open.tex`; kept for reference. Needs `texteffect` (R), `kiwipiepy` (Python), and companion-project paths. |
| `code/sIBP.R` | Exploratory sIBP on the full public-petition corpus with *response delay* as the outcome. Never part of either current paper. |

---

## 3. Data inputs

| Path | Used by | Contents |
|---|---|---|
| `data/main/공개 청원 및 민원에 대한 인식조사(1,222's).xlsx` | all active scripts | The 2024 survey (sheets: `Raw`, `Label`, `Open`, 변수 가이드). Q15 = thermometer, Q16 = open-ended; demographics SQ1–SQ9; Q7 trust, Q8 platform use, Q10 motives, Q11 impressions. |
| `data/공개제안_tidy_url_appended.xlsx` | `analyze_thermometer.R` | The 65 petition stimuli: `id`, `category` (6 topics), `comb_fin` (4-bit expert quality code = Clarity, Logic, Tone, Validity), `text`. |
| `/Users/BKmac/Dropbox/K-petition/data/tidy/evaluated_data_2024_final.csv` | `analyze_thermometer.R` | Continuous LLM quality scores (clarity/logic/tone/validity) per petition, matched by `title`. Lives in the **companion project folder**. |

---

## 4. Requirements

- **R** (≥ 4.3) with: `tidyverse`, `readxl`, `here`, `stringi`, `scales`,
  `fixest`, `modelsummary`. (Legacy sIBP scripts also need `texteffect`,
  `quanteda`, `tidytext`.)
- **Python 3** with: `openai`, `pandas`, `openpyxl`.
- **OpenAI API key** for the two `.py` scripts: read from `$OPENAI_API_KEY`, else
  from the file passed via `--key-file` (default
  `/Users/BKmac/Dropbox/K-petition/BK_api_key.txt`).

Example runs:
```bash
python3 code/llm_classify.py            # full; --limit 40 for a pilot
python3 code/llm_frame.py
Rscript code/analyze_openended.R
Rscript code/analyze_thermometer.R
```

---

## 5. Reproducibility notes

- The two `.py` scripts **cache** results to `output/*.jsonl`; re-running skips
  already-coded responses, so the published numbers are pinned to the current
  cached run. Delete the `.jsonl` to re-code from scratch.
- LLM coding is **not bit-for-bit deterministic** across model versions, so
  exact replication of the classification requires the cached files (kept in
  `output/`). The downstream R analyses are fully deterministic given those files.
- Korean filenames are copied to ASCII `/tmp` paths inside the scripts before
  reading (a `readxl`/encoding workaround).

## 6. Open items

- **Human validation of the frame coding** (inter-coder κ on a hand-coded
  subsample) is planned but not yet done — the one outstanding robustness check
  for the central object-of-evaluation (frame) construct.
- Two replacement citations are Korean-language works; the reference list needs a
  CJK-capable build or transliteration at production (the manuscript body is
  Korean-free and compiles in standard pdfLaTeX).
