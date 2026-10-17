#!/usr/bin/env python3
"""Fetch puzzle titles from adventofcode.com, once, into data/titles.json.

    python3 scripts/site/titles.py

Only the public puzzle page's <h2> title is read; puzzle text and inputs are never stored.
Requests are throttled (one every two seconds), skip titles already recorded, and identify
this script with a User-Agent, as the site's automation guidelines ask.
"""
import html
import json
import re
import time
import urllib.request
from pathlib import Path

OUT = Path(__file__).resolve().parent / "data" / "titles.json"
UA = "github.com/harryaskham/advent scripts/site/titles.py (fetches puzzle titles once)"
DAYS = {y: range(1, 26) for y in range(2015, 2025)} | {2025: range(1, 13)}


def main():
    titles = json.loads(OUT.read_text()) if OUT.exists() else {}
    for year, days in DAYS.items():
        for day in days:
            key = f"{year}/{day}"
            if key in titles:
                continue
            req = urllib.request.Request(f"https://adventofcode.com/{year}/day/{day}", headers={"User-Agent": UA})
            with urllib.request.urlopen(req, timeout=30) as r:
                page = r.read().decode("utf-8", "replace")
            m = re.search(r"<h2>--- Day \d+: (.*?) ---</h2>", page)
            if not m:
                raise SystemExit(f"no title on {key}")
            titles[key] = html.unescape(m.group(1))
            OUT.write_text(json.dumps(titles, indent=1, ensure_ascii=False) + "\n")
            print(key, titles[key], flush=True)
            time.sleep(2)


if __name__ == "__main__":
    main()
