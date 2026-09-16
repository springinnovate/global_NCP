"""Render a WWF-SIPA-baseline vs. NCP-Kc/CN-run comparison map for the Philippines SWY test.

Terminology, fixed 2026-09-15 (was "her"/"our", flagged as unprofessional for a shared technical
document): **WWF-SIPA baseline** = the original run using WWF-SIPA's own Kc/CN parametrization
(named for her own `.ini` section header, `wwf_PH_baseline_historical_climate`, and the shared
folder name). **NCP Kc/CN run** = this project's isolated-variable run — same WWF-SIPA inputs
(LULC, climate stack), this project's own Kc/CN substituted in (matches the `ncp_kc_cn` suffix
already used in this run's own output filenames).

The WWF-SIPA baseline output lives in
`data/swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_historical_climate/` — shared with
this project earlier, not previously compared against pixel-for-pixel. This script renders QF and
B from both runs on a **shared color scale per variable** (WWF-SIPA QF and NCP QF use the same
vmin/vmax; same for B) so the map is actually visually comparable, not two independently
auto-scaled images that happen to look similar. AET and L_sum stay solo layers (the WWF-SIPA
baseline output doesn't include AET or L_sum as saved rasters, only
QF/B/B_sum/L_avail/L_sum_avail).

Both rasters are already EPSG:4326 (confirmed 2026-09-14 — no reprojection needed for the Leaflet
overlay), but at different native resolutions (WWF-SIPA's 30m/0.00028deg vs. NCP's ~90m/0.00083deg,
since the WWF-SIPA run used TARGET_PIXEL_SIZE=30 explicitly and the NCP run inherited the DEM's
own ~90m SRTMGL3 resolution) — a real, separate finding from the numeric comparison, not something
this script tries to reconcile.

Outputs to `data/swy/philippines/comparison_maps/`: `layers.json` (the rendered display images,
as before) and `lookup.json` (a separate, coarser numeric grid per raster — baseline and NCP,
per variable — for the click-to-compare popup; nodata cells are NaN). Then
`11_build_output_map_html.py` builds one combined embeddable HTML fragment from both (plus
`10b_render_output_maps_becky_inputs.py`'s own AET/L_sum layers).
"""
import base64
import json
import os

import matplotlib.cm as cm
import numpy as np
import rasterio
from rasterio.enums import Resampling

OUT_DIR = "data/swy/philippines/comparison_maps"
MAX_DIM = 1600
# Separate, coarser grid for the click-to-compare popup lookup — doesn't need full display
# resolution, and keeping it small keeps the embedded page size sane (raw float32, not PNG-
# compressed, so it costs a lot more per pixel than the display image does).
LOOKUP_MAX_DIM = 500

OUR_WORKSPACE = "data/swy/philippines/workspace_becky_inputs"
HER_WORKSPACE = (
    "data/swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_historical_climate"
)

PAIRS = [
    {
        "var": "QF",
        "label": "Annual quickflow (QF), mm",
        "baseline_path": os.path.join(HER_WORKSPACE, "QF_wwf_PH_baseline_historical_climate.tif"),
        "ncp_path": os.path.join(OUR_WORKSPACE, "QF_ph_becky_inputs_ncp_kc_cn.tif"),
        "clip_percentile": None,
    },
    {
        "var": "B",
        "label": "Baseflow (B), mm",
        "baseline_path": os.path.join(HER_WORKSPACE, "B_wwf_PH_baseline_historical_climate.tif"),
        "ncp_path": os.path.join(OUR_WORKSPACE, "B_ph_becky_inputs_ncp_kc_cn.tif"),
        "clip_percentile": None,
    },
]


def _read_downsampled(path, max_dim=MAX_DIM):
    with rasterio.open(path) as src:
        scale = min(1.0, max_dim / max(src.width, src.height))
        out_h, out_w = int(src.height * scale), int(src.width * scale)
        arr = src.read(1, out_shape=(out_h, out_w), resampling=Resampling.nearest)
        bounds = src.bounds
        nodata = src.nodata
    mask = np.zeros(arr.shape, dtype=bool)
    if nodata is not None:
        mask |= np.isclose(arr, nodata)
    mask |= ~np.isfinite(arr)
    return arr, mask, bounds


def _build_lookup(path):
    """A coarse, click-lookup-only grid: nodata/invalid cells become NaN so JS can just check
    Number.isNaN() rather than tracking a parallel mask array."""
    arr, mask, bounds = _read_downsampled(path, max_dim=LOOKUP_MAX_DIM)
    arr = arr.astype(np.float32)
    arr[mask] = np.nan
    return {
        "width": arr.shape[1],
        "height": arr.shape[0],
        "bounds": [bounds.left, bounds.bottom, bounds.right, bounds.top],
        "b64": base64.b64encode(arr.tobytes()).decode("ascii"),
    }


def render_pair(pair):
    baseline_arr, baseline_mask, baseline_bounds = _read_downsampled(pair["baseline_path"])
    ncp_arr, ncp_mask, ncp_bounds = _read_downsampled(pair["ncp_path"])

    all_valid = np.concatenate([baseline_arr[~baseline_mask], ncp_arr[~ncp_mask]])
    vmin = float(all_valid.min())
    vmax = (
        float(np.percentile(all_valid, pair["clip_percentile"]))
        if pair["clip_percentile"]
        else float(all_valid.max())
    )

    results = []
    for run_id, run_label, arr, mask, bounds in [
        ("baseline", "WWF-SIPA baseline", baseline_arr, baseline_mask, baseline_bounds),
        ("ncp", "NCP Kc/CN run", ncp_arr, ncp_mask, ncp_bounds),
    ]:
        normed = np.clip((arr - vmin) / (vmax - vmin), 0, 1)
        rgba = (cm.viridis(normed) * 255).astype(np.uint8)
        rgba[mask, 3] = 0

        from PIL import Image

        os.makedirs(OUT_DIR, exist_ok=True)
        layer_id = f"{pair['var']}_{run_id}"
        png_path = os.path.join(OUT_DIR, f"{layer_id}.png")
        Image.fromarray(rgba, mode="RGBA").save(png_path)
        with open(png_path, "rb") as f:
            b64 = base64.b64encode(f.read()).decode("ascii")

        results.append(
            {
                "id": layer_id,
                "var": pair["var"],
                "run_id": run_id,
                "label": f"{run_label} — {pair['label']}",
                "b64": b64,
                "bounds": [[bounds.bottom, bounds.left], [bounds.top, bounds.right]],
                "vmin": vmin,
                "vmax": vmax,
                "shared_scale_note": f"shared {vmin:.1f}–{vmax:.1f} scale with its counterpart",
            }
        )

    lookup = {
        "var": pair["var"],
        "label": pair["label"],
        "baseline": _build_lookup(pair["baseline_path"]),
        "ncp": _build_lookup(pair["ncp_path"]),
    }
    return results, lookup


def main():
    all_results = []
    all_lookups = []
    for pair in PAIRS:
        results, lookup = render_pair(pair)
        all_results.extend(results)
        all_lookups.append(lookup)
    os.makedirs(OUT_DIR, exist_ok=True)
    with open(os.path.join(OUT_DIR, "layers.json"), "w") as f:
        json.dump(all_results, f)
    with open(os.path.join(OUT_DIR, "lookup.json"), "w") as f:
        json.dump(all_lookups, f)
    for r in all_results:
        print(f"{r['id']}: range=[{r['vmin']:.1f}, {r['vmax']:.1f}] b64_len={len(r['b64'])}")
    for l in all_lookups:
        for run_id in ("baseline", "ncp"):
            g = l[run_id]
            print(f"lookup {l['var']}/{run_id}: {g['width']}x{g['height']} b64_len={len(g['b64'])}")


if __name__ == "__main__":
    main()
