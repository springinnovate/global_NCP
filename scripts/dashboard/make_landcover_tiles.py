"""Map tiles (XYZ PNG, web mercator) of the 1992 and 2020 land cover maps, for the explorer's land cover tab.

Source: the original ESA CCI (1992, v2.0.7) and C3S (2020, v2.1.1) maps, 300 m, from the data package
(data/raw/LandCovers/esa_cci/*.nc), not landcover_gl_1992.tif (mislabelled: a copy of the 1992 N export).
Step 1 copies each year's lccs_class layer to a tiled GeoTIFF with overviews built by the most common
class (mode), so zoomed-out tiles show the dominant class of the area they cover. Step 2 writes two tile
sets per year: the full ESA legend (its official colours) and the 9 classes of analysis/LC_change.qmd
(flooded forest counts as forest). Water and no data are transparent (the basemap shows water), so tiles
over open ocean are not written. Existing tiles are skipped: a stopped run resumes where it left off.

Usage: .venv/bin/python scripts/dashboard/make_landcover_tiles.py [max_zoom] [workers]
Needs: LC_SRC_DIR (folder with the two .nc files) or the repo's data/raw/LandCovers/esa_cci
Writes: data/processed/dashboard/lc_src/esa_<year>.tif,
        data/processed/dashboard/tiles/lc_{esa,9class}_<year>/{z}/{x}/{y}.png
"""
import math
import os
import sys
import time
from multiprocessing import Pool
from pathlib import Path

import numpy as np
import rasterio
from rasterio.enums import Resampling
from rasterio.transform import from_bounds
from rasterio.warp import reproject, transform_bounds
from rasterio.windows import Window, from_bounds as win_from_bounds
from PIL import Image

ROOT = Path(__file__).resolve().parents[2]
SRC = Path(os.environ.get("LC_SRC_DIR", ROOT / "data" / "raw" / "LandCovers" / "esa_cci"))
FILES = {1992: "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992-v2.0.7cds.nc", 2020: "C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"}
DASH = ROOT / "data" / "processed" / "dashboard"
TIF = DASH / "lc_src"
OUT = DASH / "tiles"
R = 20037508.342789244

# ESA CCI land cover legend (ESACCI-LC-Legend.csv): class -> RGB
ESA_RGB = {
    10: (255, 255, 100), 11: (255, 255, 100), 12: (255, 255, 0), 20: (170, 240, 240), 30: (220, 240, 100),
    40: (200, 200, 100), 50: (0, 100, 0), 60: (0, 160, 0), 61: (0, 160, 0), 62: (170, 200, 0), 70: (0, 60, 0),
    71: (0, 60, 0), 72: (0, 80, 0), 80: (40, 80, 0), 81: (40, 80, 0), 82: (40, 100, 0), 90: (120, 130, 0),
    100: (140, 160, 0), 110: (190, 150, 0), 120: (150, 100, 0), 121: (120, 75, 0), 122: (150, 100, 0),
    130: (255, 180, 50), 140: (255, 220, 210), 150: (255, 235, 175), 151: (255, 200, 100), 152: (255, 210, 120),
    153: (255, 235, 175), 160: (0, 120, 90), 170: (0, 150, 120), 180: (0, 220, 130), 190: (195, 20, 0),
    200: (255, 245, 215), 201: (220, 220, 220), 202: (255, 245, 215), 220: (255, 255, 255)}
# ESA class -> 9 classes (analysis/LC_change.qmd, reclass_9_class_tbl); 8 = water is not drawn
TO9 = {**{c: 1 for c in (10, 11, 12, 20, 30, 40)},
       **{c: 2 for c in (50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90, 100, 160, 170)},
       **{c: 3 for c in (110, 120, 121, 122, 130)}, **{c: 4 for c in (140, 150, 151, 152, 153)},
       180: 5, 190: 6, 200: 7, 201: 7, 202: 7, 210: 8, 220: 9}
C9_RGB = {1: (246, 224, 94), 2: (45, 106, 46), 3: (216, 179, 101), 4: (201, 185, 154), 5: (79, 179, 169),
          6: (192, 57, 43), 7: (239, 227, 196), 9: (190, 215, 235)}


def lut(scheme):
    """256-entry RGBA lookup from ESA class code."""
    t = np.zeros((256, 4), dtype=np.uint8)
    for c in range(256):
        if scheme == "esa" and c in ESA_RGB:
            t[c] = (*ESA_RGB[c], 255)
        elif scheme == "9class" and TO9.get(c) in C9_RGB:
            t[c] = (*C9_RGB[TO9[c]], 255)
    return t


def prepare(year):
    """lccs_class -> tiled, compressed GeoTIFF in EPSG:4326 with mode overviews (once per year)."""
    out = TIF / f"esa_{year}.tif"
    if out.exists():
        return out
    TIF.mkdir(parents=True, exist_ok=True)
    tmp = out.with_suffix(".tmp.tif")
    t0 = time.time()
    with rasterio.open(f"NETCDF:{SRC / FILES[year]}:lccs_class") as src:
        prof = dict(driver="GTiff", width=src.width, height=src.height, count=1, dtype="uint8", nodata=0,
                    crs="EPSG:4326", transform=from_bounds(-180, -90, 180, 90, src.width, src.height),
                    tiled=True, blockxsize=512, blockysize=512, compress="deflate", predictor=1, BIGTIFF="YES")
        with rasterio.open(tmp, "w", **prof) as dst:
            step = 2025   # the NetCDF chunk height
            for row in range(0, src.height, step):
                h = min(step, src.height - row)
                w = Window(0, row, src.width, h)
                dst.write(src.read(1, window=w), 1, window=w)
        with rasterio.open(tmp, "r+") as dst:
            dst.build_overviews([2, 4, 8, 16, 32, 64, 128, 256], Resampling.mode)
    tmp.rename(out)
    print(f"{year}: {out.name} written with overviews in {time.time() - t0:.0f}s", flush=True)
    return out


def tile_bounds(z, x, y):
    size = 2 * R / 2 ** z
    return (-R + x * size, R - (y + 1) * size, -R + (x + 1) * size, R - y * size)


_s = {}


def init(year, schemes):
    _s.update(src=rasterio.open(TIF / f"esa_{year}.tif"), year=year, luts={s: lut(s) for s in schemes})


def render(job):
    z, x, y = job
    outs = {s: OUT / f"lc_{s}_{_s['year']}" / str(z) / str(x) / f"{y}.png" for s in _s["luts"]}
    todo = [s for s, p in outs.items() if not p.exists() and not p.with_suffix(".empty").exists()]
    if not todo:
        return 0
    src = _s["src"]
    b = tile_bounds(z, x, y)
    lon0, lat0, lon1, lat1 = transform_bounds("EPSG:3857", "EPSG:4326", *b, densify_pts=21)
    w = win_from_bounds(lon0, lat0, lon1, lat1, transform=src.transform).round_offsets().round_lengths()
    w = Window(w.col_off - 1, w.row_off - 1, w.width + 2, w.height + 2).intersection(Window(0, 0, src.width, src.height))
    # read at about twice the tile size; GDAL serves decimated reads from the mode overviews
    oh, ow = min(w.height, 512), min(w.width, 512)
    a = src.read(1, window=w, out_shape=(oh, ow), resampling=Resampling.nearest)
    if not (a != 0).any():
        for s in todo:
            outs[s].parent.mkdir(parents=True, exist_ok=True); outs[s].with_suffix(".empty").touch()
        return 0
    wt = rasterio.windows.transform(w, src.transform) * rasterio.Affine.scale(w.width / ow, w.height / oh)
    dst = np.zeros((256, 256), dtype=np.uint8)
    reproject(a, dst, src_transform=wt, src_crs=src.crs, src_nodata=0, dst_transform=from_bounds(*b, 256, 256),
              dst_crs="EPSG:3857", dst_nodata=0, resampling=Resampling.nearest)
    n = 0
    for s in todo:
        rgba = _s["luts"][s][dst]
        outs[s].parent.mkdir(parents=True, exist_ok=True)
        if rgba[..., 3].any():
            Image.fromarray(rgba, "RGBA").quantize(colors=64, method=Image.Quantize.FASTOCTREE).save(outs[s], optimize=True)
            n += 1
        else:
            outs[s].with_suffix(".empty").touch()   # remembered, so a resumed run skips it
    return n


def tiles(z):
    n = 2 ** z
    def ty(lat):
        return int((1 - math.asinh(math.tan(math.radians(lat))) / math.pi) / 2 * n)
    return [(x, y) for x in range(n) for y in range(max(0, ty(85.05)), min(n - 1, ty(-85.05)) + 1)]


def main(max_zoom, workers):
    schemes = ("esa", "9class")
    for year in FILES:
        prepare(year)
    for year in FILES:
        t0 = time.time()
        jobs = [(z, x, y) for z in range(max_zoom + 1) for x, y in tiles(z)]
        with Pool(workers, initializer=init, initargs=(year, schemes)) as pool:
            written = 0
            for i, k in enumerate(pool.imap_unordered(render, jobs, chunksize=32)):
                written += k
                if i % 20000 == 0:
                    print(f"{year}: {i} of {len(jobs)} tile positions, {written} tiles written, "
                          f"{time.time() - t0:.0f}s", flush=True)
        print(f"{year}: done, {written} new tiles (z0-{max_zoom}), {time.time() - t0:.0f}s", flush=True)
    for d in sorted(OUT.glob("lc_*")):
        pngs = list(d.rglob("*.png"))
        print(f"{d.name}: {len(pngs)} tiles, {sum(p.stat().st_size for p in pngs) / 1e6:.0f} MB", flush=True)


if __name__ == "__main__":
    main(int(sys.argv[1]) if len(sys.argv) > 1 else 9, int(sys.argv[2]) if len(sys.argv) > 2 else 12)
