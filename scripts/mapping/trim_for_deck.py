"""Deck copies of paper figures with their white margins cut off (2026-10-09).

Paper figures carry wide white margins that make them small on a slide. This writes
docs/presentations/images/<name>_tight.png for each figure given, cropped to its content plus a
small pad. The paper keeps using the originals.

Usage: .venv/Scripts/python.exe scripts/mapping/trim_for_deck.py outputs/plots/.../figure.png [...]
"""
import sys
from pathlib import Path

from PIL import Image, ImageChops

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "docs" / "presentations" / "images"
PAD = 12


def trim(path):
    im = Image.open(path).convert("RGB")
    mask = ImageChops.difference(im, Image.new("RGB", im.size, (255, 255, 255))).convert("L")
    box = mask.point(lambda p: 255 if p > 12 else 0).getbbox()
    box = (max(0, box[0] - PAD), max(0, box[1] - PAD), min(im.width, box[2] + PAD), min(im.height, box[3] + PAD))
    out = im.crop(box)
    dest = OUT / (Path(path).stem + "_tight.png")
    out.save(dest)
    print(f"{path}: {im.size} -> {out.size} -> {dest.relative_to(ROOT)}")


if __name__ == "__main__":
    for p in sys.argv[1:]:
        trim(ROOT / p if not Path(p).is_absolute() else Path(p))
