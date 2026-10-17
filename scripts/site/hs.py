"""A small Haskell highlighter: source text → HTML spans (kw, ty, st, nu, cm, op, pr).

Deterministic and dependency-free, enough for display: keywords, types and constructors,
strings and characters, numbers, comments (line and nested block), operators and pragmas.
Optionally wraps names found in `link` (name -> title) in <abbr> so a reader can see what a
library name means.
"""
import html
import re

KEYWORDS = {"module", "where", "import", "qualified", "as", "hiding", "let", "in", "case", "of", "if", "then",
            "else", "do", "data", "type", "newtype", "class", "instance", "deriving", "family", "forall", "mdo",
            "infixl", "infixr", "infix", "pattern", "via", "stock", "anyclass"}

TOKEN = re.compile(r"""
  (?P<pr>\{-\#.*?\#-\})
| (?P<cm>\{-.*?-\}|--(?![!#$%&*+./<=>?@\\^|~:])[^\n]*)
| (?P<st>"(?:[^"\\\n]|\\.)*")
| (?P<ch>'(?:[^'\\\n]|\\.[^']*)')
| (?P<nu>\b\d[\d_]*(?:\.\d+)?(?:e-?\d+)?\b)
| (?P<id>[A-Za-z_\u0370-\u03ff\u0400-\u04ff\u2100-\u214f\U0001d400-\U0001d7ff][\w'\u2080-\u209f\u00b2\u00b3\u00b9\u2070-\u207f]*)
| (?P<op>[^\s\w()\[\],;`"']+)
| (?P<ws>\s+)
| (?P<other>.)
""", re.X | re.S)


def highlight(src, link=None):
    link = link or {}
    out = []
    for m in TOKEN.finditer(src):
        kind, text = m.lastgroup, m.group()
        esc = html.escape(text)
        if kind in ("ws", "other"):
            out.append(esc)
            continue
        if kind == "id":
            if text in KEYWORDS:
                cls = "kw"
            elif text[0].isupper():
                cls = "ty"
            else:
                cls = None
        elif kind == "ch":
            cls = "st"
        else:
            cls = kind
        inner = f'<span class="{cls}">{esc}</span>' if cls else esc
        if text in link:
            inner = f'<abbr title="{html.escape(link[text], quote=True)}">{inner}</abbr>'
        out.append(inner)
    return "".join(out)
