"""Render two context layers for the Philippines SWY interactive map: WWF-SIPA's land cover and a
shaded-relief DEM. They aren't model outputs; they sit next to the QF/B layers so the spatial
pattern of the gap can be read against land cover and terrain.

- Land cover: `ph_baseline_lulc_wgs84.tif` (WWF-SIPA's 12-class LULC, already warped to WGS84 on
  the model grid by the 09l run), mode-resampled for display (categorical: never average), same
  class colors as the scatter plots (18/19/20).
- DEM: `dem_srtmgl3_ph_snapped.tif` (the DEM every Becky-inputs run uses), hypsometric color ramp
  blended with a hillshade. Sea-level/nodata pixels transparent.

Both also get a coarse lookup grid so a map click can report the land-cover class and elevation
at that point next to QF/B.

Output: data/swy/philippines/comparison_maps/context_layers.json, context_lookup.json
(read by 11_build_output_map_html.py).
"""
import base64
import io
import json
import os

import numpy as np
import rasterio
from matplotlib.colors import LightSource, LinearSegmentedColormap
from PIL import Image
from rasterio.enums import Resampling

OUT_DIR = "data/swy/philippines/comparison_maps"
LULC_PATH = "data/swy/philippines/workspace_becky_inputs_90m_ndvi_perpixel_kc/ph_baseline_lulc_wgs84.tif"
DEM_PATH = "data/swy/philippines/inputs/dem_srtmgl3_ph_snapped.tif"
MAX_DIM = 1600
LOOKUP_MAX_DIM = 500

LULC_CLASSES = {
    1: "Annual Crop", 2: "Brush/Shrubs", 3: "Built-up", 4: "Closed Forest", 5: "Fishpond",
    6: "Grassland", 7: "Inland Water", 8: "Mangrove Forest", 9: "Marshland/Swamp",
    10: "Open Forest", 11: "Open/Barren", 12: "Perennial Crop",
}
LULC_COLORS = {
    1: "#e6ab02", 2: "#a6761d", 3: "#666666", 4: "#1b9e77", 5: "#377eb8",
    6: "#66a61e", 7: "#1f78b4", 8: "#7570b3", 9: "#984ea3", 10: "#4daf4a",
    11: "#e7298a", 12: "#d95f02",
}

# hypsometric tints: lowland green -> yellow -> brown -> grey -> near-white summits
DEM_STOPS = [(0.0, "#2e7d32"), (0.08, "#8bc34a"), (0.2, "#fff176"), (0.4, "#d7a15b"),
             (0.65, "#8d5b3a"), (0.85, "#9e9e9e"), (1.0, "#fafafa")]
DEM_CMAP = LinearSegmentedColormap.from_list("hypso", DEM_STOPS)


def _read(path, max_dim, resampling):
    with rasterio.open(path) as src:
        scale = min(1.0, max_dim / max(src.width, src.height))
        out_h, out_w = int(src.height * scale), int(src.width * scale)
        arr = src.read(1, out_shape=(out_h, out_w), resampling=resampling)
        cell_deg = abs(src.transform.a) / scale
        return arr, src.bounds, src.nodata, cell_deg


def _png_b64(rgba):
    buf = io.BytesIO()
    Image.fromarray(rgba, mode="RGBA").save(buf, format="PNG", optimize=True)
    return base64.b64encode(buf.getvalue()).decode("ascii")


def _lookup(arr, invalid, bounds):
    arr = arr.astype(np.float32)
    arr[invalid] = np.nan
    return {"width": arr.shape[1], "height": arr.shape[0],
            "bounds": [bounds.left, bounds.bottom, bounds.right, bounds.top],
            "b64": base64.b64encode(arr.tobytes()).decode("ascii")}


def render_lulc():
    arr, bounds, nodata, _ = _read(LULC_PATH, MAX_DIM, Resampling.mode)
    rgba = np.zeros(arr.shape + (4,), dtype=np.uint8)
    for code, hex_color in LULC_COLORS.items():
        m = arr == code
        rgba[m, :3] = [int(hex_color[i:i + 2], 16) for i in (1, 3, 5)]
        rgba[m, 3] = 255
    counts = {code: int((arr == code).sum()) for code in LULC_CLASSES}
    total = sum(counts.values())
    items = "".join(
        f'<span class="swy-cat"><span class="swy-cat-swatch" style="background:{LULC_COLORS[c]};"></span>'
        f'{LULC_CLASSES[c]} ({100 * counts[c] / total:.1f}%)</span>'
        for c in sorted(LULC_CLASSES, key=lambda c: -counts[c])
    )
    legend = (f'<div class="swy-legend-row"><strong>Land cover (WWF-SIPA, 12 classes)</strong></div>'
              f'<div class="swy-cat-grid">{items}</div>')
    layer = {"id": "context_lulc", "var": "LULC", "label": "Land cover (WWF-SIPA)",
             "b64": _png_b64(rgba), "bounds": [[bounds.bottom, bounds.left], [bounds.top, bounds.right]],
             "legend_html": legend}

    small, sb, _, _ = _read(LULC_PATH, LOOKUP_MAX_DIM, Resampling.mode)
    lookup = _lookup(small, ~np.isin(small, list(LULC_CLASSES)), sb)
    return layer, lookup


def render_dem():
    arr, bounds, nodata, cell_deg = _read(DEM_PATH, MAX_DIM, Resampling.average)
    arr = arr.astype("float64")
    invalid = (arr == nodata) | (arr <= 0) | ~np.isfinite(arr)
    vmax = float(np.percentile(arr[~invalid], 99.9))
    elev = np.where(invalid, 0, arr)

    cell_m = cell_deg * 111_000
    ls = LightSource(azdeg=315, altdeg=45)
    shaded = ls.shade(elev, cmap=DEM_CMAP, vmin=0, vmax=vmax, blend_mode="soft",
                      vert_exag=3, dx=cell_m, dy=cell_m)
    rgba = (shaded * 255).astype(np.uint8)
    rgba[invalid, 3] = 0

    stops = ", ".join(f"{c} {p * 100:.0f}%" for p, c in DEM_STOPS)
    legend = (f'<div class="swy-legend-row"><span class="swy-legend-swatch" style="background:'
              f'linear-gradient(to right, {stops});"></span><span>Elevation (SRTMGL3, shaded relief): '
              f'0 &ndash; {vmax:,.0f} m</span></div>')
    layer = {"id": "context_dem", "var": "DEM", "label": "Elevation (SRTM DEM, shaded relief)",
             "b64": _png_b64(rgba), "bounds": [[bounds.bottom, bounds.left], [bounds.top, bounds.right]],
             "legend_html": legend}

    small, sb, _, _ = _read(DEM_PATH, LOOKUP_MAX_DIM, Resampling.average)
    small = small.astype("float64")
    lookup = _lookup(small, (small == nodata) | (small <= 0), sb)
    return layer, lookup


def main():
    lulc_layer, lulc_lookup = render_lulc()
    dem_layer, dem_lookup = render_dem()
    with open(os.path.join(OUT_DIR, "context_layers.json"), "w") as f:
        json.dump([lulc_layer, dem_layer], f)
    with open(os.path.join(OUT_DIR, "context_lookup.json"), "w") as f:
        json.dump({"LULC": lulc_lookup, "DEM": dem_lookup, "lulc_names": LULC_CLASSES}, f)
    for l in (lulc_layer, dem_layer):
        print(f"{l['id']}: b64_len={len(l['b64'])}")
    print(f"done -- see {OUT_DIR}/context_layers.json")


if __name__ == "__main__":
    main()
