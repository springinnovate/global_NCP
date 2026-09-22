"""Rebuild the Philippines DEM on a grid that nests exactly inside WWF-SIPA's own 30m grid.

Why: `inspring`'s `seasonal_water_yield.execute()` derives its ENTIRE output grid (pixel size and
origin-snapping both) from whatever raster is passed as `dem_raster_path` --
`align_and_resize_raster_stack(..., pixel_size=<DEM's own pixel size>, raster_align_index=<DEM's
index in the input list>)`, confirmed by reading that source directly (not assumed). So instead of
patching `inspring`, handing it a pre-warped DEM on the exact grid we want is sufficient.

Found 2026-09-18: the NCP run's own 90m grid and WWF-SIPA's 30m grid share the same CRS (EPSG:4326)
and a clean 3:1 pixel-size ratio, but their origins are offset by a non-integer fraction of a 90m
pixel (~0.91 pixel, ~22m on the ground) -- confirmed by direct transform math, not visual guessing.
`12_scatter_comparison.py`'s existing `rasterio.warp.reproject()` with area-averaging already
handles that offset correctly (verified by reproducing its exact reported numbers independently),
but the user wants the grids to actually BE nested -- every 90m pixel exactly 9 whole 30m pixels,
zero fractional overlap -- not just correctly-averaged despite being offset.

Method: snap the AOI's bounding box (already correctly built from WWF-SIPA's own valid-data
footprint by `01_build_aoi_from_rich_mask.py` -- confirmed this session via a decimated valid-mask
check, not rebuilt here) outward to the nearest exact multiple of 3 WWF-SIPA pixels, measured from
her own raster's origin. Then warp the already-fetched SRTMGL3 DEM onto that exact grid.

Output: data/swy/philippines/inputs/dem_srtmgl3_ph_snapped.tif -- point `09e`'s
`dem_raster_path` at this file, nowhere else needs to change.
"""
import math
import os

import geopandas as gpd
import rasterio
from osgeo import gdal

DATA_ROOT = os.environ.get("SWY_DATA_ROOT", "/data")

BASELINE_QF = os.path.join(
    DATA_ROOT,
    "swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_historical_climate",
    "QF_wwf_PH_baseline_historical_climate.tif",
)
AOI_PATH = os.path.join(DATA_ROOT, "swy/philippines/inputs/ph_aoi.gpkg")
SRC_DEM = os.path.join(DATA_ROOT, "swy/philippines/inputs/dem_srtmgl3/srtmgl3_dem_ph.tif")
OUT_DEM = os.path.join(DATA_ROOT, "swy/philippines/inputs/dem_srtmgl3_ph_snapped.tif")

NEST_FACTOR = 3  # NCP pixel = 3x WWF-SIPA pixel (90m vs 30m), confirmed exact this session


def main():
    with rasterio.open(BASELINE_QF) as src:
        wwf_origin_x, wwf_origin_y = src.transform.c, src.transform.f
        wwf_pixel_x, wwf_pixel_y = src.res
        assert src.crs.to_epsg() == 4326, f"expected EPSG:4326, got {src.crs}"

    ncp_pixel_x = wwf_pixel_x * NEST_FACTOR
    ncp_pixel_y = wwf_pixel_y * NEST_FACTOR

    gdf = gpd.read_file(AOI_PATH)
    aoi_west, aoi_south, aoi_east, aoi_north = gdf.total_bounds

    def snap(value, origin, pixel, round_fn):
        steps = round_fn((value - origin) / pixel)
        return origin + steps * pixel

    snapped_west = snap(aoi_west, wwf_origin_x, ncp_pixel_x, math.floor)
    snapped_east = snap(aoi_east, wwf_origin_x, ncp_pixel_x, math.ceil)
    snapped_south = snap(aoi_south, wwf_origin_y, ncp_pixel_y, math.floor)
    snapped_north = snap(aoi_north, wwf_origin_y, ncp_pixel_y, math.ceil)

    print(f"WWF-SIPA origin: ({wwf_origin_x}, {wwf_origin_y}), pixel: ({wwf_pixel_x}, {wwf_pixel_y})")
    print(f"AOI bounds (unsnapped): {aoi_west}, {aoi_south}, {aoi_east}, {aoi_north}")
    print(f"Snapped bounds: {snapped_west}, {snapped_south}, {snapped_east}, {snapped_north}")

    os.makedirs(os.path.dirname(OUT_DEM), exist_ok=True)
    gdal.Warp(
        OUT_DEM,
        SRC_DEM,
        dstSRS="EPSG:4326",
        outputBounds=(snapped_west, snapped_south, snapped_east, snapped_north),
        xRes=ncp_pixel_x,
        yRes=ncp_pixel_y,
        resampleAlg="bilinear",  # continuous elevation data
        creationOptions=["COMPRESS=LZW"],
    )

    # Hard verification -- must be exact (to float epsilon), not just "close". This is the same
    # check that caught the current run's ~0.91-pixel offset; it must show 0 this time.
    with rasterio.open(OUT_DEM) as src:
        new_origin_x, new_origin_y = src.transform.c, src.transform.f
        new_pixel_x, new_pixel_y = src.res
        print(f"New DEM origin: ({new_origin_x}, {new_origin_y}), pixel: ({new_pixel_x}, {new_pixel_y})")

    offset_x_pixels = (new_origin_x - wwf_origin_x) / ncp_pixel_x
    offset_y_pixels = (new_origin_y - wwf_origin_y) / ncp_pixel_y
    frac_x = offset_x_pixels - round(offset_x_pixels)
    frac_y = offset_y_pixels - round(offset_y_pixels)
    print(f"Offset from WWF-SIPA origin, in NCP (90m) pixels: x={offset_x_pixels}, y={offset_y_pixels}")
    print(f"Fractional remainder: x={frac_x}, y={frac_y}")
    assert abs(frac_x) < 1e-6, f"grid NOT nested: x offset has a {frac_x} pixel remainder"
    assert abs(frac_y) < 1e-6, f"grid NOT nested: y offset has a {frac_y} pixel remainder"
    print("VERIFIED: grid is exactly nested inside WWF-SIPA's own grid (zero fractional offset).")


if __name__ == "__main__":
    main()
