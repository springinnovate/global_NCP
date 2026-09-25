"""Map layers for the tree-crop Kc test run (09o), added to the interactive map as an optional
test: the run's baseflow on the same color scale as the other B layers, and the change from our
current run (09o - 09n, mm/yr) on a diverging scale.

The tree-crop Kc (07i) only changes Perennial Crop and barely moves its mean (0.990 -> 0.985),
but individual pixels shift baseflow by about -85 to +128 mm/yr; the change layer shows where.
Kept separate from 10c so the test layers are easy to drop from the shared map: remove
test_layers.json (or skip this script) and rebuild with 11.

Output: data/swy/philippines/comparison_maps/test_layers.json (read by 11_build_output_map_html.py)
"""
import base64
import io
import json
import os

import matplotlib.cm as cm
import numpy as np
import rasterio
from matplotlib.colors import LinearSegmentedColormap
from PIL import Image
from rasterio.enums import Resampling

OUT_DIR = "data/swy/philippines/comparison_maps"
CURRENT_B = "data/swy/philippines/workspace_becky_inputs_90m_paddy_kc/B_ph_paddy_kc.tif"
TREE_B = "data/swy/philippines/workspace_becky_inputs_90m_tree_crops_kc/B_ph_tree_crops_kc.tif"
MASK_PATH = os.path.join(OUT_DIR, "b_valid_mask.tif")
MAX_DIM = 1600
CHANGE_LIMIT = 150  # mm/yr, symmetric color range for the change layer
MIN_VISIBLE_CHANGE = 5  # mm/yr; smaller changes stay transparent

# diverging: red (decrease) - neutral gray - blue (increase)
DIVERGING = LinearSegmentedColormap.from_list(
    "change", [(0.0, "#c0392b"), (0.5, "#d9d9d6"), (1.0, "#2a78d6")])


def _read(path):
    with rasterio.open(path) as src:
        scale = min(1.0, MAX_DIM / max(src.width, src.height))
        shape = (int(src.height * scale), int(src.width * scale))
        arr = src.read(1, out_shape=shape, resampling=Resampling.average).astype("float64")
        if src.nodata is not None:
            arr[np.isclose(arr, src.nodata)] = np.nan
        return arr, src.bounds, shape


def _png(rgba):
    buf = io.BytesIO()
    Image.fromarray(rgba, mode="RGBA").save(buf, format="PNG", optimize=True)
    return base64.b64encode(buf.getvalue()).decode("ascii")


def main():
    with open(os.path.join(OUT_DIR, "layers.json")) as f:
        b_layer = next(l for l in json.load(f) if l["var"] == "B")
    vmin, vmax = b_layer["vmin"], b_layer["vmax"]

    tree, bounds, shape = _read(TREE_B)
    current, _, _ = _read(CURRENT_B)
    with rasterio.open(MASK_PATH) as src:
        valid = src.read(1, out_shape=shape, resampling=Resampling.nearest) == 1
    leaflet_bounds = [[bounds.bottom, bounds.left], [bounds.top, bounds.right]]

    # 1. tree-crop run baseflow, same scale as the other B layers
    invalid = ~valid | ~np.isfinite(tree)
    rgba = (cm.viridis(np.clip((np.nan_to_num(tree) - vmin) / (vmax - vmin), 0, 1)) * 255).astype(np.uint8)
    rgba[invalid, 3] = 0
    tree_layer = {
        "id": "test_B_tree", "var": "B", "label": "Test: our run with tree-crop Kc on perennial crop — Baseflow (B), mm",
        "b64": _png(rgba), "bounds": leaflet_bounds,
        "legend_html": ('<div class="swy-legend-row"><span class="swy-legend-swatch" style="background:'
                        'linear-gradient(to right, #440154, #3b528b, #21918c, #5ec962, #fde725);"></span>'
                        f'<span>Baseflow, tree-crop Kc test: {vmin:.1f} &ndash; {vmax:.1f} mm (same scale as the other B layers)</span></div>'),
    }

    # 2. change from the current run
    change = tree - current
    hidden = ~valid | ~np.isfinite(change) | (np.abs(change) < MIN_VISIBLE_CHANGE)
    rgba = (DIVERGING(np.clip((np.nan_to_num(change) + CHANGE_LIMIT) / (2 * CHANGE_LIMIT), 0, 1)) * 255).astype(np.uint8)
    rgba[hidden, 3] = 0
    change_layer = {
        "id": "test_B_change", "var": "B", "label": "Test: change in baseflow from tree-crop Kc (mm/yr)",
        "b64": _png(rgba), "bounds": leaflet_bounds,
        "legend_html": ('<div class="swy-legend-row"><span class="swy-legend-swatch" style="background:'
                        'linear-gradient(to right, #c0392b, #d9d9d6, #2a78d6);"></span>'
                        f'<span>Change in baseflow, tree-crop Kc minus current run: &minus;{CHANGE_LIMIT} to +{CHANGE_LIMIT} mm/yr '
                        f'(red: less, blue: more; changes under {MIN_VISIBLE_CHANGE} mm/yr transparent). '
                        'Only perennial crop changes Kc; routed flow carries some of the change downslope.</span></div>'),
    }
    shown = ~hidden
    print(f"change layer: {shown.sum():,} visible display pixels; "
          f"p5/p95 of visible change: {np.percentile(change[shown], 5):.0f} / {np.percentile(change[shown], 95):.0f} mm/yr")

    with open(os.path.join(OUT_DIR, "test_layers.json"), "w") as f:
        json.dump([tree_layer, change_layer], f)
    print(f"written {OUT_DIR}/test_layers.json")


if __name__ == "__main__":
    main()
