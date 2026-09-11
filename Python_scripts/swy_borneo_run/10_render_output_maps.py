"""Render SWY Borneo output rasters as viridis PNG overlays for the interactive map in
docs/reports/swy_status_report.qmd.

Leaflet's ImageOverlay draws a raster as a simple rectangle between two lat/lon corners without
reprojecting pixel data — that's fine here since our rasters are already EPSG:4326 and Borneo
sits close to the equator (-4.5 to 7.5 degrees latitude), where Web Mercator's cos(latitude)
distortion is negligible. No reprojection step needed, just downsampling + colormap + alpha.

Outputs base64-encoded PNGs + bounds to data/borneo_lulc/output_maps/, which
build_report_map_html.py then reads to build the embeddable HTML.
"""
import base64
import json
import os

import matplotlib.cm as cm
import numpy as np
import rasterio
from rasterio.enums import Resampling

OUT_DIR = "data/borneo_lulc/output_maps"
MAX_DIM = 1600  # downsample target on the long side — keeps embedded PNG size reasonable

LAYERS = [
    {
        "id": "qf",
        "label": "Annual quickflow (QF), mm",
        "path": "data/swy_borneo_workspace/QF_borneo_2020_test.tif",
        "nodata_extra": [-119988.0],
        "clip_percentile": None,  # QF's real range is already reasonable, no capping needed
    },
    {
        "id": "aet",
        "label": "Actual evapotranspiration (AET), mm",
        "path": "data/swy_borneo_workspace/intermediate_outputs/aet_borneo_2020_test.tif",
        "nodata_extra": [-119988.0],
        "clip_percentile": None,
    },
    {
        "id": "l_sum",
        "label": "Local recharge sum (L_sum), mm — capped at p99 to show the real anomaly",
        "path": "data/swy_borneo_workspace/L_sum_borneo_2020_test.tif",
        "nodata_extra": [],
        "clip_percentile": 99,  # cap so the ~4.6% blown-up pixels don't wash out the real signal
    },
]


def render_layer(layer):
    with rasterio.open(layer["path"]) as src:
        scale = min(1.0, MAX_DIM / max(src.width, src.height))
        out_h, out_w = int(src.height * scale), int(src.width * scale)
        # nearest, not average: averaging blends nodata sentinels into neighboring valid
        # pixels during the decimated read (confirmed — produced impossible negative AET
        # values), since GDAL's average resampler doesn't reliably respect nodata here
        arr = src.read(1, out_shape=(out_h, out_w), resampling=Resampling.nearest)
        bounds = src.bounds
        nodata = src.nodata

    mask = np.zeros(arr.shape, dtype=bool)
    if nodata is not None:
        mask |= np.isclose(arr, nodata)
    for v in layer["nodata_extra"]:
        mask |= np.isclose(arr, v)
    mask |= ~np.isfinite(arr)

    valid = arr[~mask]
    vmin = float(valid.min())
    vmax = float(np.percentile(valid, layer["clip_percentile"])) if layer["clip_percentile"] else float(valid.max())

    normed = np.clip((arr - vmin) / (vmax - vmin), 0, 1)
    rgba = (cm.viridis(normed) * 255).astype(np.uint8)
    rgba[mask, 3] = 0  # transparent nodata — basemap shows through

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
