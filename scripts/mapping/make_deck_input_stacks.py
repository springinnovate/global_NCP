"""Draw the deck's input layers as oblique stacks of rasters (2026-10-08).

Reads the flat maps written by scripts/mapping/make_deck_input_layers.R and writes one stack per
input group to docs/presentations/images/: stack_services.png, stack_landcover.png,
stack_socioeconomic.png. Each map is squashed and sheared into a parallelogram and the layers are
offset upward, top layer last, with its name beside it.

Usage: .venv/Scripts/python.exe scripts/mapping/make_deck_input_stacks.py
"""
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "docs" / "presentations" / "images" / "input_layers"
OUT = ROOT / "docs" / "presentations" / "images"

STACKS = {
    "stack_services.png": [("svc_nature_access.png", "Nature access"),
                           ("svc_pollination.png", "Pollination"),
                           ("svc_sed_export.png", "Sediment export"),
                           ("svc_n_export.png", "Nitrogen export")],
    "stack_landcover.png": [("lc_2020.png", "2020"),
                            ("lc_1992.png", "1992")],
    "stack_socioeconomic.png": [("soc_gini.png", "Gini"),
                                ("soc_hdi.png", "HDI"),
                                ("soc_gdp.png", "GDP"),
                                ("soc_population.png", "Population")],
}

MAP_W = 900          # flat map width before the transform
SQUASH = 0.42        # vertical scale of each layer
SHEAR = 0.55         # horizontal shift of the top edge, as a share of the squashed height
STEP = 0.55          # vertical offset between layers, as a share of the squashed height
LABEL_W = 0           # layer names go in the slide text, in the same top-to-bottom order


def font(size):
    for name in ("segoeui.ttf", "arial.ttf"):
        try:
            return ImageFont.truetype(name, size)
        except OSError:
            continue
    return ImageFont.load_default()


def oblique(img):
    """Squash and shear a flat map into a parallelogram on a transparent background."""
    img = img.convert("RGBA").resize((MAP_W, int(MAP_W * img.height / img.width)), Image.LANCZOS)
    ImageDraw.Draw(img).rectangle([0, 0, img.width - 1, img.height - 1], outline=(150, 150, 150, 255), width=3)
    w, h = img.size
    hc = int(h * SQUASH)
    s = int(hc * SHEAR / SQUASH * 0.5)
    # inverse map: output (u, v) -> source (u - s * (1 - v / hc), v / SQUASH)
    data = (1, s / hc, -s, 0, 1 / SQUASH, 0)
    return img.transform((w + s, hc), Image.AFFINE, data, resample=Image.BICUBIC,
                         fillcolor=(255, 255, 255, 0))


def stack(layers, out):
    tiles = [oblique(Image.open(SRC / f)) for f, _ in layers]
    tw, th = tiles[0].size
    step = int(th * STEP)
    n = len(tiles)
    canvas = Image.new("RGBA", (tw + LABEL_W, th + step * (n - 1) + 10), (255, 255, 255, 0))
    draw = ImageDraw.Draw(canvas)
    f = font(34)
    # bottom layer first; layer i sits step * i higher than the bottom one
    for i, (tile, (_, label)) in enumerate(zip(tiles, layers)):
        y = step * (n - 1 - i)
        shadow = Image.new("RGBA", tile.size, (0, 0, 0, 0))
        shadow.putalpha(tile.getchannel("A").point(lambda a: int(a * 0.18)))
        canvas.alpha_composite(shadow, (8, y + 8))
        canvas.alpha_composite(tile, (0, y))
        if LABEL_W:
            draw.text((tw + 18, y + th // 2 - 20), label, font=f, fill=(51, 51, 51, 255))
    canvas.save(OUT / out)
    print("wrote", OUT / out)


if __name__ == "__main__":
    for out, layers in STACKS.items():
        stack(layers, out)
