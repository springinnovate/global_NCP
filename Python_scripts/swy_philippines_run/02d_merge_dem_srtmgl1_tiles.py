"""Merge the two AppEEARS SRTMGL1 (30m) DEM tiles (north/south, downloaded manually from the
AppEEARS website — programmatic bundle download hit the same 403 as task submission, so this
project's usual `download_bundle()` helper doesn't apply here) into one continuous DEM covering
the full Philippines AOI.

Why two tiles: a single request for the whole AOI's bounding box at SRTMGL1's native 30m
resolution exceeded AppEEARS' per-request size cap by 21.2% (SRTMGL3 at 90m fit fine at the same
footprint — 30m is ~9x the pixel count). Split at the bbox's midpoint latitude with a 0.1-degree
overlap (~11km) to avoid a seam gap at the merge boundary — see
`02c_fetch_dem_srtmgl1_becky_inputs.py`'s sibling request-JSON-building step for the exact split.

AppEEARS gives both tiles' bundles the exact same generic filename
(`SRTMGL1_NC.003_SRTMGL1_DEM_doy2000042000000_aid0001.tif`) regardless of task name — the browser
just appended " (1)" to the second download. Disambiguated by actual raster bounds (not filename
or task order): the smaller-bottom-coordinate one is south, confirmed via `rasterio` before
renaming. Already sorted into `IN_DIR` as `srtmgl1_dem_{south,north}.tif` — this script just
merges those two explicit files.

Output: `srtmgl1_dem_ph_merged.tif`, the path `09b_run_swy_ph_becky_inputs.py` now points at.
Uses `rasterio.merge` rather than `osgeo.gdal` deliberately — this project's local `.venv` (used
for steps 1-8 of this pipeline) has `rasterio` but not the standalone `osgeo` bindings, which are
only present inside the Docker image steps 9+ run in. Doing the merge locally with `rasterio`
avoids needing Docker just for this one step.
"""
import os

import rasterio
from rasterio.merge import merge

IN_DIR = "data/swy/philippines/inputs/dem_srtmgl1_becky_inputs"
OUT_PATH = os.path.join(IN_DIR, "srtmgl1_dem_ph_merged.tif")
TILE_PATHS = [
    os.path.join(IN_DIR, "srtmgl1_dem_south.tif"),
    os.path.join(IN_DIR, "srtmgl1_dem_north.tif"),
]


def main():
    for p in TILE_PATHS:
        if not os.path.exists(p):
            raise FileNotFoundError(f"expected tile not found: {p}")
    print("Merging tiles:")
    for p in TILE_PATHS:
        print(f"  {p}")

    srcs = [rasterio.open(p) for p in TILE_PATHS]
    mosaic, out_transform = merge(srcs)
    out_meta = srcs[0].meta.copy()
    out_meta.update(
        {
            "height": mosaic.shape[1],
            "width": mosaic.shape[2],
            "transform": out_transform,
            "compress": "lzw",
        }
    )
    with rasterio.open(OUT_PATH, "w", **out_meta) as dst:
        dst.write(mosaic)
    for s in srcs:
        s.close()

    with rasterio.open(OUT_PATH) as f:
        print(f"\nDone — {OUT_PATH}")
        print(f"  size: {f.width} x {f.height}")
        print(f"  pixel size (deg): {f.transform.a}, {f.transform.e}")
        print(f"  bounds: {f.bounds}")


if __name__ == "__main__":
    main()
