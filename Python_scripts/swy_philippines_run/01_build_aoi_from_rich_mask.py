"""Build the Philippines AOI polygon from Rich's own shared model-run output footprint.

**Revision note (2026-09-11)**: the first version of this script dissolved
`watershed_subset_files/*.gpkg` (the full hydrological routing domain `swy_global` used for this
job) and got a ~439,000 km^2 polygon — ~46% larger than the Philippines' real land area. Checked
why: that routing domain includes a ~118,000 km^2 chunk in northern Borneo (upstream contributing
basin area, needed for correct flow routing but never actually part of the retained output) — and
directly verified the output raster has **zero valid pixels** there. Kept as
`data/swy/philippines/inputs/ph_routing_domain_reference.gpkg` for reference, but it's the wrong
polygon to build a comparable AOI from.

**This version instead polygonizes the actual valid-data footprint of Rich's own output raster**
(`QF_wwf_PH_baseline_historical_climate.tif`'s non-nodata mask) — i.e., "everywhere Rich's run
actually reports a number," which is what this project's own from-scratch Philippines run needs to
cover to be genuinely comparable. The raster is huge (59813 x 46780, ~30m) — read it decimated
(GDAL overview-style decimated read, not a full-resolution load) rather than processing it at
native resolution, since an AOI polygon doesn't need pixel-perfect fidelity.

Input (already on disk, shared by Becky, not downloaded by this script):
    data/swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_historical_climate/
        QF_wwf_PH_baseline_historical_climate.tif

Output:
    data/swy/philippines/inputs/ph_aoi.gpkg
"""
import os

import geopandas as gpd
import numpy as np
import rasterio
from rasterio.features import shapes
from shapely.geometry import shape

QF_PATH = (
    "data/swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_historical_climate/"
    "QF_wwf_PH_baseline_historical_climate.tif"
)
OUT_PATH = "data/swy/philippines/inputs/ph_aoi.gpkg"

# Decimation factor for the read — native is ~30m; this targets ~600m, plenty for an AOI polygon
# (the DEM/precip/ET0 fetches downstream will use this polygon's bbox at their own native
# resolution, not this decimated grid).
DECIMATION = 20


def main():
    with rasterio.open(QF_PATH) as src:
        nodata = src.nodata
        out_shape = (src.height // DECIMATION, src.width // DECIMATION)
        band = src.read(1, out_shape=out_shape, resampling=rasterio.enums.Resampling.nearest)
        # transform scaled to match the decimated grid
        decimated_transform = src.transform * src.transform.scale(
            src.width / out_shape[1], src.height / out_shape[0]
        )
        crs = src.crs

    valid_mask = ~np.isclose(band, nodata) if nodata is not None else np.isfinite(band)
    valid_mask = valid_mask.astype("uint8")

    polygons = [
        shape(geom)
        for geom, val in shapes(valid_mask, mask=valid_mask.astype(bool), transform=decimated_transform)
        if val == 1
    ]
    assert polygons, "no valid-data polygons found — check QF_PATH and nodata handling"

    gdf = gpd.GeoDataFrame(geometry=polygons, crs=crs).to_crs(4326)
    dissolved = gdf.dissolve()
    dissolved["geometry"] = dissolved.buffer(0)

    # 405 disjoint parts produced a 5.9MB GeoJSON that AppEEARS' API rejected outright (403,
    # near-certainly a request-size/WAF limit, not a real permissions issue — confirmed by
    # checking the payload size directly). Most of that part count is polygonization noise, not
    # real islands: many parts share the *exact same* area (one decimated 600m pixel, ~0.38 km^2)
    # — single stray/misclassified pixels, not distinct landmasses. Drop parts under 2 km^2
    # (hydrologically negligible for a hydrology model regardless) to cut both part count and
    # payload size. Real small islands well above this threshold are unaffected.
    MIN_PART_AREA_KM2 = 2.0
    exploded = dissolved.explode(index_parts=False)
    area_km2 = exploded.to_crs(epsg=3857).area / 1e6
    kept = exploded[area_km2 >= MIN_PART_AREA_KM2]
    dropped_n = len(exploded) - len(kept)
    print(f"dropped {dropped_n} of {len(exploded)} parts under {MIN_PART_AREA_KM2} km^2 (polygonization noise)")

    # simplify LAST, on the already-filtered geometry — doing it before explode/union left the
    # saved file just as complex (140K vertices, still ~5.8MB), since union_all() on disjoint
    # parts doesn't inherit an earlier simplify pass cleanly. 0.01deg (~1km) is coarse but the
    # AOI's job is defining a download/model extent, not tracing an exact coastline.
    unioned = kept.union_all()
    dissolved = gpd.GeoDataFrame(geometry=[unioned], crs=4326)
    dissolved["geometry"] = dissolved.buffer(0).simplify(0.01, preserve_topology=True)

    os.makedirs(os.path.dirname(OUT_PATH), exist_ok=True)
    dissolved.to_file(OUT_PATH, driver="GPKG")

    area_km2 = dissolved.to_crs(epsg=3857).area.sum() / 1e6
    bounds = dissolved.total_bounds
    print(f"valid-footprint polygons found: {len(polygons)}")
    print(f"dissolved area (rough): {area_km2:,.0f} km^2 (real Philippines land area ~300,000 km^2)")
    print(f"bounds (WGS84): {bounds}")
    print(f"written to {OUT_PATH}")


if __name__ == "__main__":
    main()
