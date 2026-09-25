"""Render the Negron Juarez forest-Kc test run's QF/B as standalone viridis overlays for the
interactive map, same rendering pattern as `10b_render_output_maps_becky_inputs.py` (AET/L_sum).

These are standalone (no WWF-SIPA comparison/click-popup, unlike 10c's QF/B pair) -- added to the
map's layer-toggle list only, same tier as AET/L_sum, so this run's actual output can be inspected
visually without rebuilding the shared-scale comparison machinery for what's still a test run.

**Applies the same "valid in both datasets" comparison mask 10c uses for QF_baseline/QF_ncp**
(fixed 2026-09-23, was missing initially -- the user noticed this layer showing visibly more area
than QF_baseline/QF_ncp on the map, traced to this script only masking the raster's own nodata,
not the cross-dataset validity mask the comparison layers use). The numbers already reported
(`15_negronjuarez_comparison.py`) already used this mask -- only the map display was inconsistent.

Output: data/swy/philippines/workspace_becky_inputs_90m_negronjuarez_kc/output_maps/layers.json,
merged into the combined map by 11_build_output_map_html.py.
"""
import base64
import json
import os

import matplotlib.cm as cm
import numpy as np
import rasterio
from rasterio.enums import Resampling
from rasterio.warp import reproject

WORKSPACE = "data/swy/philippines/workspace_becky_inputs_90m_negronjuarez_kc"
OUT_DIR = os.path.join(WORKSPACE, "output_maps")
MASK_DIR = "data/swy/philippines/comparison_maps"
MAX_DIM = 1600

LAYERS = [
    {
        "id": "qf_negronjuarez",
        "var": "qf",
        "label": "Negron Juarez forest-Kc test — Quickflow (QF), mm",
        "path": os.path.join(WORKSPACE, "QF_ph_negronjuarez_forest_kc.tif"),
        "clip_percentile": 99,
    },
    {
        "id": "b_negronjuarez",
        "var": "b",
        "label": "Negron Juarez forest-Kc test — Baseflow (B), mm",
        "path": os.path.join(WORKSPACE, "B_ph_negronjuarez_forest_kc.tif"),
        "clip_percentile": 99,
    },
]


def render_layer(layer):
    with rasterio.open(layer["path"]) as src:
        scale = min(1.0, MAX_DIM / max(src.width, src.height))
        out_h, out_w = int(src.height * scale), int(src.width * scale)
        # average, not nearest -- see 10b's own note: nearest-neighbor at this decimation ratio
        # aliases real fine-scale texture into a moire pattern that looks like a data artifact.
        arr = src.read(1, out_shape=(out_h, out_w), resampling=Resampling.average)
        bounds = src.bounds
        nodata = src.nodata
        dst_transform = src.transform * src.transform.scale(
            src.width / out_w, src.height / out_h
        )
        dst_crs = src.crs

    mask = np.zeros(arr.shape, dtype=bool)
    if nodata is not None:
        mask |= np.isclose(arr, nodata)
    mask |= ~np.isfinite(arr)

    # Same "valid in both datasets" mask 10c applies to QF_baseline/QF_ncp -- without this, this
    # layer shows more area than its counterparts, not because the data differs, but because this
    # script was only checking the raster's own nodata, not the cross-dataset validity mask.
    mask_path = os.path.join(MASK_DIR, f"{layer['var']}_valid_mask.tif")
    with rasterio.open(mask_path) as msrc:
        mask_arr = np.zeros(arr.shape, dtype="float32")
        reproject(
            source=rasterio.band(msrc, 1), destination=mask_arr,
            src_transform=msrc.transform, src_crs=msrc.crs,
            dst_transform=dst_transform, dst_crs=dst_crs,
            resampling=Resampling.nearest, src_nodata=msrc.nodata, dst_nodata=0,
        )
    mask |= mask_arr != 1

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
