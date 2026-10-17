"""The viewer's drawings, from data/solutions.json."""
import json
import statistics
from pathlib import Path

from diagrams import Canvas, T

DATA = Path(__file__).resolve().parent / "data"
METRICS = {"sans": 1.0, "mono": 0.6}
ERA_KIND = {"plain": "muted", "relude": "b", "adlude": "c", "quaalude": "accent"}
ERA_LABEL = {"plain": "plain Haskell", "relude": "Relude + helpers", "adlude": "adlude", "quaalude": "quaalude"}


def lines():
    sols = json.loads((DATA / "solutions.json").read_text())["solutions"]
    years = sorted({s["year"] for s in sols})
    w, h, left, right, top, bottom = 900, 360, 56, 20, 46, 300
    top_val = 300
    c = Canvas("lines", w, h, "Lines of code in each solution, by year",
               "A dot for every solution, placed by year and by its number of lines of code (not counting blank lines "
               "or comments), with each year's median marked. 2015 to 2020 are plain Haskell, with medians of 37 to 74 "
               "lines; 2021 and 2022 use Relude and helper modules; 2023 uses adlude, a custom prelude; 2024 and 2025 "
               "use quaalude, with medians of 21 and 15 lines.")
    step = (w - left - right) / len(years)
    y_of = lambda v: bottom - (min(v, top_val) / top_val) * (bottom - top)
    for v in range(0, top_val + 1, 50):
        y = y_of(v)
        c.add("under", f'<line class="life" x1="{left}" x2="{w - right}" y1="{y:.1f}" y2="{y:.1f}"/>')
        c.add("labels", T(left - 8, y + 4, str(v), 11, "sub", "end", mono=True))
    c.add("labels", T(left - 8, top - 16, "lines", 11, "sub", "end", mono=True))
    for i, year in enumerate(years):
        cx = left + step * (i + 0.5)
        ys = [s for s in sols if s["year"] == year]
        era = ys[0]["era"]
        kind = ERA_KIND[era]
        for s in ys:
            jitter = ((s["day"] * 37) % 25 - 12) * step / 34
            c.add("nodes", f'<g class="k-{kind}"><circle class="node solid" cx="{cx + jitter:.1f}" cy="{y_of(s["code_lines"]):.1f}" '
                           f'r="3.2" opacity="0.75"><title>{year} day {s["day"]}: {s["code_lines"]} lines</title></circle></g>')
        med = statistics.median(s["code_lines"] for s in ys)
        my = y_of(med)
        c.add("over", f'<g class="k-ink"><line class="edge" x1="{cx - step * 0.36:.1f}" x2="{cx + step * 0.36:.1f}" '
                      f'y1="{my:.1f}" y2="{my:.1f}" stroke-width="2.5"/></g>')
        c.add("labels", T(cx + step * 0.4, my + 4, f"{med:g}", 11.5, "lbl", "start", mono=True, halo=True))
        c.add("labels", T(cx, bottom + 22, str(year), 12.5, "sub", "middle", mono=True))
    x = left
    for era, label in ERA_LABEL.items():
        c.add("labels", f'<g class="k-{ERA_KIND[era]}"><circle class="node solid" cx="{x + 5}" cy="{h - 18}" r="5"/></g>')
        c.add("labels", T(x + 15, h - 14, label, 12, "sub", "start"))
        x += 34 + len(label) * 7.2
    c.add("labels", f'<g class="k-ink"><line class="edge" x1="{x}" x2="{x + 22}" y1="{h - 18}" y2="{h - 18}" stroke-width="2.5"/></g>')
    c.add("labels", T(x + 30, h - 14, "median", 12, "sub", "start"))
    return c.svg()


FIGS = {"lines": lines}
