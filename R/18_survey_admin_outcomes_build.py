# Builds petition-level administrative outcome datasets for the R&R analysis
# (PSJ reviewers R1.6 / R4.4): links the 65 survey stimulus petitions and the
# full 2024 evaluated corpus to administrative outcomes (status, answer dates).
#
# Inputs:
#   data/screenshots/공개제안_tidy_url_appended.xlsx  (65 stimulus petitions)
#   data/tidy/evaluated_data_2024_final.csv           (2024 corpus w/ LLM scores)
#   output/theta_medians.csv                          (posterior median thetas,
#                                                      from mcmc_out.rds; see
#                                                      R/18_survey_admin_outcomes.R)
# Outputs:
#   data/tidy/admin_outcomes_65.csv
#   data/tidy/admin_outcomes_corpus2024.csv
#
# Matching: stimulus petitions match the corpus exactly on (title,
# date_petitioned). Seven stimulus petitions have multiple corpus rows with
# identical title/date; these are duplicate submissions with near-identical
# text (token overlap > 0.94) and identical status, differing only by a few
# days in date_answered. We disambiguate by maximum token overlap; the choice
# is immaterial to the results.
#
# Usage: python3 R/18_survey_admin_outcomes_build.py   (from project root)

import csv
import re
import unicodedata
from collections import defaultdict
from datetime import date

import openpyxl

STIM_XLSX = "data/screenshots/공개제안_tidy_url_appended.xlsx"
CORPUS_CSV = "data/tidy/evaluated_data_2024_final.csv"
THETA_CSV = "output/theta_medians.csv"
OUT_65 = "data/tidy/admin_outcomes_65.csv"
OUT_CORPUS = "data/tidy/admin_outcomes_corpus2024.csv"


def norm(s):
    return unicodedata.normalize("NFC", (s or "").strip())


def toks(s):
    return set(re.findall(r"\S+", norm(s)))


def parse_scrape(ds):
    return date(int(ds[:4]), int(ds[4:6]), int(ds[6:8]))


# ---- load inputs -------------------------------------------------------------
wb = openpyxl.load_workbook(STIM_XLSX, read_only=True)
rows = list(wb.active.iter_rows(values_only=True))
stim = [dict(zip(rows[0], r)) for r in rows[1:]]

with open(CORPUS_CSV, encoding="utf-8-sig") as f:
    corpus = list(csv.DictReader(f))

theta = {}
with open(THETA_CSV) as f:
    for r in csv.DictReader(f):
        theta[int(r["item"])] = (float(r["theta1"]), float(r["theta2"]))

idx = defaultdict(list)
for r in corpus:
    idx[norm(r["title"])].append(r)

# ---- 65 stimulus petitions ---------------------------------------------------
out = []
for s in stim:
    t, dp = norm(s["title"]), str(s["date_petitioned"])[:10]
    cands = [x for x in idx[t] if x["date_petitioned"] == dp]
    if len(cands) > 1:
        st = toks(s["text"])

        def sim(c):
            ct = toks(
                " ".join(
                    [c["current_issues"], c["improvement_plan"], c["expected_effect"]]
                )
            )
            return len(st & ct) / max(len(ct), 1)

        cands.sort(key=sim, reverse=True)
    c = cands[0]
    iid = int(s["id"])
    comb = str(s["comb_fin"]).zfill(4)
    da = c["date_answered"] if c["date_answered"] not in ("", "NA") else ""
    d0 = date.fromisoformat(dp)
    days = (date.fromisoformat(da) - d0).days if da else ""
    censor_days = (parse_scrape(c["date_scraped"]) - d0).days
    out.append(
        {
            "item": iid,
            "category": norm(s["category"]),
            "clarity": int(comb[0]),
            "logic": int(comb[1]),
            "manner": int(comb[2]),
            "validity": int(comb[3]),
            "quality_sum": sum(int(x) for x in comb),
            "presentation": int(comb[0]) + int(comb[2]),
            "substance": int(comb[1]) + int(comb[3]),
            "theta1": theta[iid][0],
            "theta2": theta[iid][1],
            "status": c["status"],
            "answered": 1 if da else 0,
            "days_to_answer": days,
            "censor_days": censor_days,
            "time": days if da else censor_days,
            "date_petitioned": dp,
            "nchar": len(norm(s["text"])),
        }
    )

with open(OUT_65, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(out[0].keys()))
    w.writeheader()
    w.writerows(out)
print(f"wrote {OUT_65}: {len(out)} rows, {sum(o['answered'] for o in out)} answered")

# ---- full 2024 corpus (LLM-scored) -------------------------------------------
score_cols = [
    "clarity_specificity_score",
    "logic_consistency_score",
    "tone_manner_score",
    "validity_feasibility_score",
]
seen = set()
outc = []
for r in corpus:
    key = (r["title"], r["date_petitioned"], r["current_issues"][:80])
    if key in seen:
        continue
    seen.add(key)
    sc = [r[c] for c in score_cols]
    if any(s in ("", "NA") for s in sc):
        continue
    try:
        d0 = date.fromisoformat(r["date_petitioned"])
    except ValueError:
        continue
    da = r["date_answered"]
    answered = 1 if da not in ("", "NA") else 0
    if answered:
        t = (date.fromisoformat(da) - d0).days
    else:
        t = (parse_scrape(r["date_scraped"]) - d0).days
    if t < 0:
        continue
    body = " ".join(
        [r["current_issues"], r["improvement_plan"], r["expected_effect"]]
    )
    outc.append(
        {
            "clarity": sc[0],
            "logic": sc[1],
            "manner": sc[2],
            "validity": sc[3],
            "status": r["status"],
            "answered": answered,
            "time": t,
            "area": r["area"],
            "nchar": len(body),
            "date_petitioned": r["date_petitioned"],
        }
    )

with open(OUT_CORPUS, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(outc[0].keys()))
    w.writeheader()
    w.writerows(outc)
print(f"wrote {OUT_CORPUS}: {len(outc)} rows")
