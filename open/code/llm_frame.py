#!/usr/bin/env python3
"""
LLM frame classification of open-ended responses (Q16): does the respondent
construe the e-petition system as a GOVERNMENT INSTITUTION (judged on how the
state processes/responds) or as a CITIZEN-CONTENT PLATFORM (judged on the
quality/nature of what citizens submit)?  Directly operationalizes the
object-of-evaluation (frame) construct, replacing the hand-grouped category proxy.

Unlike the 14-category coding, this works on ALL responses including positive
ones ("good that the government listens" = government; "good that people can
voice opinions" = citizen).

Outputs (output/):
  openended_frame.jsonl          cache (one JSON object per NO)
  openended_frame_classified.csv NO, response, frame, rationale

Feeds paper/open.tex:
  via code/analyze_openended.R   -> Figure 2 (fig:frame_by_age)
  via code/analyze_thermometer.R -> frame-slope test (Sec. 5.5.1) + the object-of-evaluation
                                    row of Table 6 (tab:adjudicate)
  illustrative quotes are hand-picked into Table 2 (tab:frame_examples) and Table 9 (tab:frame_samples)

Usage:
  python3 code/llm_frame.py --limit 40   # pilot
  python3 code/llm_frame.py              # full run (cached)
"""
import argparse, json, os, sys
from pathlib import Path
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
SURVEY_XLSX = ROOT / "data" / "main" / "공개 청원 및 민원에 대한 인식조사(1,222's).xlsx"
OUT_DIR = ROOT / "output"
CACHE = OUT_DIR / "openended_frame.jsonl"
DEFAULT_KEY_FILE = ROOT / "BK_api_key.txt"  # fallback only; $OPENAI_API_KEY takes precedence

SYSTEM_PROMPT = (
    "You are an expert Korean-language survey coder. Respondents answered an "
    "open-ended question about Korea's public petition / civil-complaint system "
    "(e-petitions). Your task is to identify the respondent's IMPLICIT MENTAL "
    "MODEL of what the system IS — the object they are evaluating — not whether "
    "their view is positive or negative.\n\n"
    "Assign one frame:\n"
    "- 'government': construes the system as a GOVERNMENT INSTITUTION. Focuses on "
    "what the state does with petitions — responsiveness, effectiveness, whether "
    "petitions are reflected in policy, processing speed, transparency/feedback, "
    "official conduct or burden, the government listening/communicating. The "
    "evaluative object is the state's handling.\n"
    "- 'citizen': construes the system as a CITIZEN-CONTENT PLATFORM — a space "
    "where ordinary people post opinions/petitions. Focuses on the inputs: the "
    "quality, sincerity, or nature of what citizens submit (frivolous, emotional, "
    "selfish, mob-driven, unrealistic, poorly written), or the need to filter/"
    "curate that content, or the value of citizens having a voice. The evaluative "
    "object is the citizens and their submissions.\n"
    "- 'both': clearly invokes both the state's handling AND the citizen content.\n"
    "- 'neither': non-answer, 'don't know', blank, or content that reveals no "
    "frame (e.g., access/awareness comments unrelated to either object).\n\n"
    "Judge by the OBJECT of evaluation, independent of sentiment. 'It's great "
    "that the government actually responds' = government. 'It's good ordinary "
    "people can raise their voice' = citizen. 'Too many frivolous petitions' = "
    "citizen. 'Petitions are ignored and change nothing' = government. "
    "Return strictly the requested JSON; 'rationale' <= 10 English words."
)

RESULT_SCHEMA = {
    "type": "json_schema",
    "json_schema": {
        "name": "frame_codes", "strict": True,
        "schema": {
            "type": "object", "additionalProperties": False,
            "properties": {
                "results": {
                    "type": "array",
                    "items": {
                        "type": "object", "additionalProperties": False,
                        "properties": {
                            "index": {"type": "integer"},
                            "frame": {"type": "string",
                                      "enum": ["government", "citizen", "both", "neither"]},
                            "rationale": {"type": "string"},
                        },
                        "required": ["index", "frame", "rationale"],
                    },
                }
            },
            "required": ["results"],
        },
    },
}


def load_responses():
    wb = pd.read_excel(SURVEY_XLSX, sheet_name="Open")
    df = wb[["NO", "Q16"]].copy()
    df["NO"] = df["NO"].astype(int)
    df["response"] = df["Q16"].astype("string")
    return df[["NO", "response"]]


def make_client(key_file):
    from openai import OpenAI
    key = os.environ.get("OPENAI_API_KEY")
    if not key and key_file and Path(key_file).exists():
        key = Path(key_file).read_text().strip()
    if not key:
        sys.exit("No OpenAI API key found.")
    return OpenAI(api_key=key)


def classify_batch(client, model, batch):
    listing = "\n".join(f"[{i}] {t}" for i, t in batch)
    resp = client.chat.completions.create(
        model=model, temperature=0,
        messages=[{"role": "system", "content": SYSTEM_PROMPT},
                  {"role": "user", "content": "Classify each numbered response:\n\n" + listing}],
        response_format=RESULT_SCHEMA,
    )
    data = json.loads(resp.choices[0].message.content)
    return {int(r["index"]): {"frame": r["frame"], "rationale": r.get("rationale", "")}
            for r in data["results"]}


def load_cache():
    done = {}
    if CACHE.exists():
        for line in CACHE.read_text().splitlines():
            if line.strip():
                o = json.loads(line); done[int(o["NO"])] = o
    return done


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--model", default="gpt-4.1-mini")
    ap.add_argument("--limit", type=int, default=None)
    ap.add_argument("--batch", type=int, default=20)
    ap.add_argument("--key-file", default=str(DEFAULT_KEY_FILE))
    args = ap.parse_args()

    OUT_DIR.mkdir(exist_ok=True)
    df = load_responses()
    if args.limit:
        df = df.head(args.limit).copy()

    done = load_cache()
    todo = [(int(r.NO), (r.response if isinstance(r.response, str) else ""))
            for r in df.itertuples() if int(r.NO) not in done]
    if todo:
        client = make_client(args.key_file)
        print(f"Frame-coding {len(todo)} responses with {args.model} "
              f"({len(done)} cached)...", flush=True)
        for i in range(0, len(todo), args.batch):
            chunk = todo[i:i + args.batch]
            pos = [(j, t) for j, (_n, t) in enumerate(chunk)]
            res = classify_batch(client, args.model, pos)
            with CACHE.open("a") as f:
                for j, (no, t) in enumerate(chunk):
                    r = res.get(j, {"frame": "neither", "rationale": "parse-miss"})
                    o = {"NO": no, "response": t, **r, "model": args.model}
                    f.write(json.dumps(o, ensure_ascii=False) + "\n")
                    done[no] = o
            print(f"  {min(i+args.batch, len(todo))}/{len(todo)}", flush=True)
    else:
        print("All responses already frame-coded; skipping API calls.")

    df["frame"] = df["NO"].map(lambda n: done[n]["frame"] if n in done else None)
    df["frame_rationale"] = df["NO"].map(lambda n: done[n].get("rationale", "") if n in done else "")
    df.to_csv(OUT_DIR / "openended_frame_classified.csv", index=False)

    counts = df["frame"].value_counts()
    print("\n=== Frame distribution (all responses) ===")
    for k, v in counts.items():
        print(f"  {k:11} {v:5}  {100*v/len(df):.1f}%")
    print(f"\nWrote {OUT_DIR}/openended_frame_classified.csv and {CACHE}")


if __name__ == "__main__":
    main()
