#!/usr/bin/env python3
"""Write the viewer's data: site/data/<year>.json, one file per year, loaded on demand.

    python3 scripts/site/data.py          # write
    python3 scripts/site/data.py --check  # exit 1 if site/data is stale

Inputs (all in scripts/site/data/):
  solutions.json       the solutions' source (extract.py; never puzzle inputs)
  titles.json          puzzle titles (titles.py)
  critiques.json       a short critique of each solution, drafted by a language model from its
                       code and spot-checked against it (the model is recorded per entry)
  quaalude-names.json  quaalude names with their types, from quaalude's own site data
                       (scripts/site/usage.py there), used to explain names in 2024–2025 code
"""
import json
import sys
from pathlib import Path

from hs import highlight

HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
OUT = HERE.parents[1] / "site" / "data"


def load(name):
    return json.loads((DATA / name).read_text())


def quaalude_links():
    out = {}
    for n in load("quaalude-names.json")["names"]:
        sig = n["signature"].split(" default ", 1)[0]
        typ = sig.split("::", 1)[1].strip() if "::" in sig else sig
        out[n["name"]] = f'{n["name"]} :: {typ}  (quaalude, {n["module"].removeprefix("Quaalude.")})'
    return out


def render():
    sols, titles, crit = load("solutions.json"), load("titles.json"), load("critiques.json")
    links = quaalude_links()
    years = {}
    for s in sols["solutions"]:
        key = f'{s["year"]}/{s["day"]}'
        c = crit.get(key, {})
        years.setdefault(s["year"], []).append({
            "day": s["day"], "title": titles[key], "path": s["path"], "era": s["era"],
            "lines": s["lines"], "code_lines": s["code_lines"], "imports": s["imports"], "uses": s["uses"],
            "approach": c.get("approach", ""), "critique": c.get("critique", ""), "tags": c.get("tags", []),
            "model": c.get("model", ""),
            "html": highlight(s["code"].rstrip(), links if s["era"] == "quaalude" else None),
        })
    files = {f"{y}.json": json.dumps({"year": y, "days": days}, ensure_ascii=False, separators=(",", ":")) + "\n"
             for y, days in years.items()}
    index = [{"year": y, "era": days[0]["era"], "days": [[d["day"], d["title"], d["code_lines"], d["tags"]] for d in days]}
             for y, days in years.items()]
    files["index.json"] = json.dumps(index, ensure_ascii=False, separators=(",", ":")) + "\n"
    return files


def main():
    files = render()
    stale = [n for n, t in files.items() if not (OUT / n).exists() or (OUT / n).read_text() != t]
    if "--check" in sys.argv:
        if stale:
            sys.exit(f"site/data is stale ({', '.join(stale[:4])}…): run scripts/site/data.py")
        return
    OUT.mkdir(parents=True, exist_ok=True)
    for n, t in files.items():
        (OUT / n).write_text(t)
    print(f"wrote {len(files)} files to site/data")


if __name__ == "__main__":
    main()
