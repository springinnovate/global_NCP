"""Inventory every number stated in the paper's prose, for the numbers ledger.

Reads docs/manuscript/paper_draft_5service.qmd and writes
docs/manuscript/numbers_inventory.csv: one row per number, with line, section and the sentence it
appears in. Skips YAML header, code chunks, HTML-only author notes, image/link targets, the
References section, citation years ("et al., 2019"), and the study-period years 1992/2020.

This is step 1 of the numbers audit; the ledger (numbers_ledger.csv) adds source, recomputed value
and status per claim, and verify_paper_numbers.py recomputes them from the data.
"""

import csv
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
QMD = ROOT / "docs" / "manuscript" / "paper_draft_5service.qmd"
OUT = ROOT / "docs" / "manuscript" / "numbers_inventory.csv"

NUM = re.compile(r"(?<![\w.])[−-]?\d[\d,]*(?:\.\d+)?\s*(?:%|×|x\b|million|billion|M\b)?")
CITE_YEAR = re.compile(r"(?:et al\.,?|&\s*\w+,?|\w+,)\s*(?:19|20)\d\d[a-z]?")


def prose_lines(text):
    lines = text.splitlines()
    in_yaml = lines and lines[0].strip() == "---"
    in_code = in_note = in_refs = False
    section = ""
    for i, line in enumerate(lines, 1):
        s = line.strip()
        if in_yaml:
            if i > 1 and s == "---":
                in_yaml = False
            continue
        if s.startswith("```"):
            in_code = not in_code
            continue
        if in_code:
            continue
        if s.startswith("::: {.content-visible when-format=\"html\"}"):
            in_note = True
            continue
        if in_note:
            if s == ":::":
                in_note = False
            continue
        if s.startswith("#"):
            section = s.lstrip("#").split("{")[0].strip()
            in_refs = section.lower().startswith("references")
            continue
        if in_refs or not s or s.startswith(">"):
            continue
        yield i, section, line


def main():
    rows = []
    for i, section, line in prose_lines(QMD.read_text(encoding="utf-8")):
        clean = re.sub(r"!\[.*?\]\(.*?\)", lambda m: m.group(0).split("](")[0], line)  # drop image paths
        clean = re.sub(r"\]\([^)]*\)", "]", clean)                                      # drop link targets
        clean = re.sub(r"\{[#.][^}]*\}", "", clean)                                     # drop attributes
        cites = [m.span() for m in CITE_YEAR.finditer(clean)]
        for sent in re.split(r"(?<=[.;:!?])\s+(?=[A-Z(*])", clean):
            for m in NUM.finditer(sent):
                tok = m.group(0).strip()
                bare = tok.rstrip("%×xMmilonb ").replace(",", "")
                if bare in ("1992", "2020") or not re.search(r"\d", tok):
                    continue
                pos = clean.find(sent) + m.start()
                if any(a <= pos < b for a, b in cites):
                    continue
                rows.append({"id": len(rows) + 1, "line": i, "section": section,
                             "value": tok, "sentence": sent.strip()[:400]})
    with open(OUT, "w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["id", "line", "section", "value", "sentence"])
        w.writeheader()
        w.writerows(rows)
    by_sec = {}
    for r in rows:
        by_sec[r["section"]] = by_sec.get(r["section"], 0) + 1
    print(f"{len(rows)} numbers -> {OUT.relative_to(ROOT)}")
    for s, n in by_sec.items():
        print(f"  {n:4d}  {s}")


if __name__ == "__main__":
    main()
