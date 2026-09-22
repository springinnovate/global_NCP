"""Build an explicit "valid in both datasets" comparison mask, per variable (QF, B).

Why: found 2026-09-18 that NCP's own B raster has ~8.16 million NaN pixels (~25% of the domain)
forming a clean ring around every coastline, absent from QF -- introduced at the routing/
accumulation step, not upstream (AET and L are both valid at these exact locations). Every
downstream comparison script was silently dropping these pixels via a plain `np.isfinite()` check,
undocumented. Per the user's own framing: "we should create a mask with the maximum extent in which
both datasets have valid pixels, that is the only valid comparison" -- this script builds that mask
explicitly, once, so every downstream script (map rendering, scatter/stats) reads the same file
instead of each recomputing its own ad hoc validity logic.

Requires the NCP grid to be exactly nested inside WWF-SIPA's grid (verified by
`02e_snap_dem_to_baseline_grid.py` -- run against `workspace_becky_inputs_90m_snapped`, not the
older `_rainfix` workspace, which is NOT nested and would make the "keep only 9/9" logic below
meaningless). Because the grid is exactly 3x3-nested, reprojecting WWF-SIPA's binary valid mask onto
the NCP grid via area-averaging produces exact values in {0, 1/9, ..., 9/9} -- no ambiguity. Only
cells at a full 9/9 (every one of the 9 underlying 30m pixels valid) count as baseline-valid here --
the strict reading of "maximum extent where both datasets have valid pixels," not a partial-credit
threshold.

Outputs: data/swy/philippines/comparison_maps/{qf,b}_valid_mask.tif -- uint8, 1 = valid for
comparison, 0 = excluded, on the exact NCP grid.
"""
import os

import numpy as np
import rasterio
from rasterio.windows import Window

DATA_ROOT = os.environ.get("SWY_DATA_ROOT", "/data")

NCP_WORKSPACE = os.path.join(DATA_ROOT, "swy/philippines/workspace_becky_inputs_90m_snapped")
NCP_SUFFIX = "ph_becky_inputs_90m_snapped"
BASELINE_WORKSPACE = os.path.join(
    DATA_ROOT, "swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_historical_climate"
)
OUT_DIR = os.path.join(DATA_ROOT, "swy/philippines/comparison_maps")

PAIRS = {
    "qf": {
        "baseline_path": os.path.join(
            BASELINE_WORKSPACE, "QF_wwf_PH_baseline_historical_climate.tif"
        ),
        "ncp_path": os.path.join(NCP_WORKSPACE, f"QF_{NCP_SUFFIX}.tif"),
    },
    "b": {
        # Masking-corrected baseline B (her own nodata flag misses ~55M ocean pixels her QF
        # raster's nodata flag correctly excludes) -- same file every other script in this
        # pipeline already uses, not the raw shared one.
        "baseline_path": os.path.join(
            OUT_DIR, "B_wwf_PH_baseline_historical_climate_masked.tif"
        ),
        "ncp_path": os.path.join(NCP_WORKSPACE, f"B_{NCP_SUFFIX}.tif"),
    },
}

NEST_TOLERANCE = 1e-6  # fraction of a pixel; must be ~0, confirmed by 02e for this workspace's DEM


def _assert_grid_nested(ncp_path, baseline_path):
    with rasterio.open(baseline_path) as b:
        b_origin_x, b_origin_y = b.transform.c, b.transform.f
        b_pixel_x, b_pixel_y = b.res
    with rasterio.open(ncp_path) as n:
        n_origin_x, n_origin_y = n.transform.c, n.transform.f
        n_pixel_x, n_pixel_y = n.res
    off_x = (n_origin_x - b_origin_x) / n_pixel_x
    off_y = (n_origin_y - b_origin_y) / n_pixel_y
    frac_x = off_x - round(off_x)
    frac_y = off_y - round(off_y)
    assert abs(frac_x) < NEST_TOLERANCE and abs(frac_y) < NEST_TOLERANCE, (
        f"grid NOT nested for {ncp_path} vs {baseline_path}: "
        f"fractional offset x={frac_x}, y={frac_y} -- did you point this at the "
        f"grid-snapped workspace (workspace_becky_inputs_90m_snapped), not an older one?"
    )


def build_mask(var, baseline_path, ncp_path, row_chunk=1000):
    _assert_grid_nested(ncp_path, baseline_path)

    with rasterio.open(ncp_path) as ncp_src:
        ncp_shape = ncp_src.shape
        ncp_transform = ncp_src.transform
        ncp_crs = ncp_src.crs
        ncp = ncp_src.read(1)
        ncp_nodata = ncp_src.nodata

    ncp_valid = np.isfinite(ncp)
    if ncp_nodata is not None:
        ncp_valid &= ~np.isclose(ncp, ncp_nodata)
    del ncp

    n_rows, n_cols = ncp_shape

    # Chunked block-average instead of a full in-memory reproject() -- WWF-SIPA's native raster
    # is ~2.8 billion pixels (~59813x46780); loading it whole (even as uint8, plus GDAL's own warp
    # working memory) was enough to get this container OOM-killed (exit 137) once the still-running
    # 30m job's own memory use is counted too. Since the grid is confirmed exactly 3x3-nested (see
    # 02e_snap_dem_to_baseline_grid.py's own hard assertion), each NCP pixel maps to an EXACT,
    # known 3x3 block of WWF-SIPA pixels -- a plain numpy reshape+mean block-reduction gives the
    # identical answer to an area-weighted reproject here, without GDAL's warp overhead, and lets
    # this process one row-strip at a time.
    with rasterio.open(baseline_path) as b_src:
        b_transform = b_src.transform
        b_nodata = b_src.nodata
        row_offset_wwf = round((b_transform.f - ncp_transform.f) / b_src.res[1])
        col_offset_wwf = round((ncp_transform.c - b_transform.c) / b_src.res[0])
        assert row_offset_wwf >= 0 and col_offset_wwf >= 0, (
            "NCP grid extends outside WWF-SIPA's raster -- unexpected, check inputs"
        )

        valid_fraction = np.zeros(ncp_shape, dtype="float32")
        for r0 in range(0, n_rows, row_chunk):
            r1 = min(r0 + row_chunk, n_rows)
            window = Window(
                col_offset_wwf, row_offset_wwf + 3 * r0, n_cols * 3, (r1 - r0) * 3
            )
            strip = b_src.read(1, window=window)
            strip_valid = np.isfinite(strip)
            if b_nodata is not None:
                strip_valid &= ~np.isclose(strip, b_nodata)
            strip_valid = strip_valid.astype("float32")
            blocked = strip_valid.reshape(r1 - r0, 3, n_cols, 3)
            valid_fraction[r0:r1, :] = blocked.mean(axis=(1, 3))

    # exact grid nesting -> valid_fraction should only ever take values in {0, 1/9, ..., 9/9};
    # a strict >0.999 (not ==1.0) absorbs harmless floating-point noise from the averaging itself.
    baseline_fully_valid = valid_fraction > 0.999

    mask = (baseline_fully_valid & ncp_valid).astype("uint8")

    n_baseline_full = baseline_fully_valid.sum()
    n_ncp_valid = ncp_valid.sum()
    n_both = mask.sum()
    print(f"{var}: baseline-fully-valid={n_baseline_full:,} ncp-valid={n_ncp_valid:,} "
          f"both(final mask)={n_both:,} "
          f"(dropped {n_ncp_valid - n_both:,} ncp-valid pixels not fully baseline-valid)")

    out_path = os.path.join(OUT_DIR, f"{var}_valid_mask.tif")
    profile = {
        "driver": "GTiff",
        "height": ncp_shape[0],
        "width": ncp_shape[1],
        "count": 1,
        "dtype": "uint8",
        "crs": ncp_crs,
        "transform": ncp_transform,
        "nodata": 255,
        "compress": "LZW",
    }
    with rasterio.open(out_path, "w", **profile) as dst:
        dst.write(mask, 1)
    print(f"written to {out_path}")


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    for var, paths in PAIRS.items():
        build_mask(var, paths["baseline_path"], paths["ncp_path"])


if __name__ == "__main__":
    main()
