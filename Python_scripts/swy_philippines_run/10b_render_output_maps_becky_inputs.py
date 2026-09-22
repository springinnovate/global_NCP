"""Render the NCP Kc/CN run's Philippines SWY output rasters as viridis PNG overlays for the
interactive map in docs/reports/swy_status_report.qmd. "NCP Kc/CN run" = this project's
isolated-variable run: WWF-SIPA's own inputs (LULC, climate stack), this project's own Kc/CN
substituted in (see `swy_methods.qmd`'s comparison section for the full terminology note).

Mirrors `swy_borneo_run/10_render_output_maps.py` exactly in method — same reasoning applies here:
Leaflet's ImageOverlay draws a raster as a simple rectangle between two lat/lon corners without
reprojecting pixel data. The Philippines AOI spans roughly 4.6-21 degrees latitude, further from
the equator than Borneo — Web Mercator's cos(latitude) distortion is small but not as negligible
as Borneo's case; accepted here anyway for consistency with the existing map and because this is
an illustrative overlay, not a measurement tool.

Outputs base64-encoded PNGs + bounds to data/swy/philippines/workspace_becky_inputs/output_maps/,
then `11_build_output_map_html.py` reads that (plus the comparison maps' own output) to build one
combined embeddable HTML fragment.
"""
import base64
import json
import os

import matplotlib.cm as cm
import numpy as np
import rasterio
from rasterio.enums import Resampling

WORKSPACE = "data/swy/philippines/workspace_becky_inputs_90m_snapped"
OUT_DIR = os.path.join(WORKSPACE, "output_maps")
MAX_DIM = 1600

LAYERS = [
    # QF deliberately dropped 2026-09-15 night: redundant with the comparison map
    # (_output_map_ph_comparison.html), which shows it properly — side-by-side against the
    # WWF-SIPA baseline, not in isolation. AET and L_sum stay here since neither is in that
    # comparison at all (the WWF-SIPA baseline output never saved either one).
    {
        "id": "aet",
        "label": "NCP Kc/CN run — Actual evapotranspiration (AET), mm",
        "path": os.path.join(WORKSPACE, "intermediate_outputs", "aet_ph_becky_inputs_90m_snapped.tif"),
        "clip_percentile": None,
        # AET's own nodata sentinel doesn't cover the legitimate zero-value ocean pixels seen in
        # the raw stats pull (docs/swy/research_notes.md, 2026-09-14 evening) — mask those out
        # of the map too so they don't render as dark-purple "valid zero" ocean.
        "mask_below": 0.001,
    },
    {
        "id": "l_sum",
        "label": "NCP Kc/CN run — Local recharge sum (L_sum), mm — capped at p99 to show the real anomaly",
        "path": os.path.join(WORKSPACE, "L_sum_ph_becky_inputs_90m_snapped.tif"),
        "clip_percentile": 99,
    },
]


def render_layer(layer):
    with rasterio.open(layer["path"]) as src:
        scale = min(1.0, MAX_DIM / max(src.width, src.height))
        out_h, out_w = int(src.height * scale), int(src.width * scale)
        # Resampling.average, not nearest: at this decimation ratio (~7-12x for the whole-
        # Philippines extent), nearest-neighbor picks one raw pixel per display cell and skips the
        # rest -- for a field with real per-pixel texture (CN/soil-group-driven), that aliases into
        # a salt-and-pepper/moire pattern that looks like a data artifact but isn't one (confirmed
        # 2026-09-18: the underlying full-resolution raster is smooth, only this display decimation
        # wasn't). average properly box-filters every source pixel into each display cell instead.
        arr = src.read(1, out_shape=(out_h, out_w), resampling=Resampling.average)
        bounds = src.bounds
        nodata = src.nodata

    mask = np.zeros(arr.shape, dtype=bool)
    if nodata is not None:
        mask |= np.isclose(arr, nodata)
    mask |= ~np.isfinite(arr)
    if layer.get("mask_below") is not None:
        mask |= arr < layer["mask_below"]

    valid = arr[~mask]
    vmin = float(valid.min())
    vmax = (
        float(np.percentile(valid, layer["clip_percentile"]))
        if layer["clip_percentile"]
        else float(valid.max())
    )

    normed = np.clip((arr - vmin) / (vmax - vmin), 0, 1)
    rgba = (cm.viridis(normed) * 255).astype(np.uint8)
    rgba[mask, 3] = 0

    from PIL import Image

    os.makedirs(OUT_DIR, exist_ok=True)
    png_path = os.path.join(OUT_DIR, f"{layer['id']}.png")
    Image.fromarray(rgba, mode="RGBA").save(png_path)

    with open(png_path, "rb") as f:
        b64 = base64.b64encode(f.read()).decode("ascii")

    return {
        "id": layer["id"],
        "label": layer["label"],
        "b64": b64,
        "bounds": [[bounds.bottom, bounds.left], [bounds.top, bounds.right]],
        "vmin": vmin,
        "vmax": vmax,
    }


def main():
    results = [render_layer(layer) for layer in LAYERS]
    os.makedirs(OUT_DIR, exist_ok=True)
    with open(os.path.join(OUT_DIR, "layers.json"), "w") as f:
        json.dump(results, f)
    for r in results:
        print(f"{r['id']}: bounds={r['bounds']} range=[{r['vmin']:.1f}, {r['vmax']:.1f}] b64_len={len(r['b64'])}")


if __name__ == "__main__":
    main()
