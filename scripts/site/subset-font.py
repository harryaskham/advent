#!/usr/bin/env python3
"""Subset JuliaMono (OFL) to the characters this site uses, for code full of Unicode maths.

    python3 scripts/site/subset-font.py          # write site/assets/fonts/juliamono-*.woff2
    python3 scripts/site/subset-font.py --check  # fail if the site uses a glyph the subset lacks

The full font is several megabytes; the subset covers ASCII, Latin-1 and every character found
in site/ (HTML, CSS, JSON, JS), so it stays small and every symbol renders in one typeface.
The font comes from nixpkgs (julia-mono), the same release each time via the pinned revision.
Writing WOFF2 needs fontTools with Brotli; one way to get both:

    nix shell --impure --expr 'with import (builtins.getFlake "github:NixOS/nixpkgs/c043004d1c6985732bcc1cbc5a9c9aecbbb4e0f0") {};
      python3.withPackages (p: [p.fonttools p.brotli])' -c python3 scripts/site/subset-font.py
"""
import subprocess
import sys
from pathlib import Path

from fontTools import subset
from fontTools.ttLib import TTFont

HERE = Path(__file__).resolve().parent
SITE = HERE.parents[1] / "site"
FONTS = SITE / "assets" / "fonts"
NIXPKGS = "github:NixOS/nixpkgs/c043004d1c6985732bcc1cbc5a9c9aecbbb4e0f0"
WEIGHTS = {"regular": "JuliaMono-Regular.ttf", "bold": "JuliaMono-Bold.ttf"}


def used_chars():
    chars = {chr(c) for c in range(0x20, 0x7f)} | {chr(c) for c in range(0xa0, 0x100)}
    for f in SITE.rglob("*"):
        if f.suffix in (".html", ".css", ".json", ".js") and f.is_file():
            chars |= set(f.read_text())
    return {c for c in chars if ord(c) >= 0x20}


def source_dir():
    out = subprocess.run(["nix", "build", "--no-link", "--print-out-paths", f"{NIXPKGS}#julia-mono"],
                         capture_output=True, text=True, check=True).stdout.strip()
    return Path(out) / "share" / "fonts" / "truetype"


def main():
    chars = used_chars()
    if "--check" in sys.argv:
        f = FONTS / "juliamono.txt"
        if not f.exists():
            sys.exit("subset font missing: run scripts/site/subset-font.py")
        cmap = {ord(c) for c in f.read_text()}
        src = TTFont(source_dir() / WEIGHTS["regular"])
        available = set(src.getBestCmap())
        missing = sorted(c for c in chars if ord(c) in available and ord(c) not in cmap)
        if missing:
            sys.exit(f"subset lacks {len(missing)} used glyph(s): {''.join(missing[:40])} (run subset-font.py)")
        print(f"font subset covers all {len(chars)} characters the site uses")
        return
    src = source_dir()
    FONTS.mkdir(parents=True, exist_ok=True)
    for weight, name in WEIGHTS.items():
        font = TTFont(src / name)
        keep = [ord(c) for c in chars if ord(c) in font.getBestCmap()]
        opts = subset.Options()
        opts.flavor = "woff2"
        opts.layout_features = ["kern", "liga", "calt", "ccmp", "locl", "mark", "mkmk"]
        opts.name_IDs = ["*"]
        opts.notdef_outline = True
        s = subset.Subsetter(opts)
        s.populate(unicodes=keep)
        s.subset(font)
        out = FONTS / f"juliamono-{weight}.woff2"
        font.flavor = "woff2"
        font.save(out)
        print(f"{out.relative_to(SITE)}: {len(keep)} glyphs, {out.stat().st_size // 1024} KB")
    # The characters covered, for --check (reading WOFF2 back would need Brotli too).
    covered = sorted(c for c in chars if ord(c) in TTFont(src / WEIGHTS["regular"]).getBestCmap())
    (FONTS / "juliamono.txt").write_text("".join(covered))


if __name__ == "__main__":
    main()
