#!/usr/bin/env python3
"""Collect every solution (and the shared modules they use) into data/solutions.json.

    python3 scripts/site/extract.py

Reads only Haskell source from the repository. Puzzle inputs (*/input/) are never read or
published: Advent of Code asks that inputs are not redistributed.
"""
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DATA = Path(__file__).resolve().parent / "data"
WORDS = {2015: "Fifteen", 2016: "Sixteen", 2017: "Seventeen", 2018: "Eighteen", 2019: "Nineteen", 2020: "Twenty"}
LAST_DAY = {2025: 12}

# How each year's code was written: its Prelude and helpers.
ERAS = {
    **{y: "plain" for y in range(2015, 2021)},
    2021: "relude", 2022: "relude", 2023: "adlude", 2024: "quaalude", 2025: "quaalude",
}


def solution_path(year, day):
    if year <= 2020:
        return ROOT / "2015-2020" / "src" / f"Twenty{WORDS[year]}" / f"Day{day}.hs"
    return ROOT / str(year) / "src" / f"Day{day}.hs"


def shared_modules():
    out = {}
    for f in [*(ROOT / "2015-2020" / "src").glob("*.hs"), ROOT / "2015-2020/src/TwentyNineteen/Intcode.hs",
              *(ROOT / "2021" / "src" / "Helper").glob("*.hs"), *(ROOT / "2022" / "src" / "Helper").glob("*.hs")]:
        if f.is_file():
            out[str(f.relative_to(ROOT))] = f.read_text()
    return out


def stats(code):
    lines = code.splitlines()
    body = [l for l in lines if l.strip() and not l.strip().startswith("--")]
    imports = [l for l in lines if l.startswith("import ")]
    return {"lines": len(lines), "code_lines": len(body), "imports": len(imports)}


def main():
    sols = []
    for year in range(2015, 2026):
        for day in range(1, LAST_DAY.get(year, 25) + 1):
            p = solution_path(year, day)
            if not p.is_file():
                raise SystemExit(f"missing {p.relative_to(ROOT)}")
            code = p.read_text()
            uses = sorted(set(re.findall(r"^import\s+(?:qualified\s+)?([A-Z][\w.]*)", code, re.M)))
            sols.append({"year": year, "day": day, "path": str(p.relative_to(ROOT)), "era": ERAS[year],
                         "uses": uses, **stats(code), "code": code})
    DATA.mkdir(exist_ok=True)
    (DATA / "solutions.json").write_text(json.dumps({"solutions": sols, "shared": shared_modules()},
                                                    indent=1, ensure_ascii=False) + "\n")
    print(f"{len(sols)} solutions")


if __name__ == "__main__":
    main()
