"""Build the Borneo AOI polygon from HydroBASINS.

Reconstruction note: `data/swy_shared_package/borneo_aoi.gpkg` was originally built
interactively (2026-09-10) and the exact bbox parameters used weren't captured in a script at
the time. This script is a faithful reconstruction using Borneo's standard geographic extent,
not a guaranteed byte-identical replay — if regenerating, verify the computed area against the
figure already validated (753,803 km^2 vs. Borneo's real ~743,330 km^2, 1.4% off) rather than
assuming this reproduces the exact original polygon vertex-for-vertex.

Uses HydroBASINS Australia/Oceania (not "Asia" — that region's bbox cuts off at ~1 degree N and
does not reach Borneo's latitude), level-6 Pfafstetter units, dissolved into one polygon and
clipped to Borneo's bbox to avoid leaking into Sulawesi/Sumatra/Java (which are geographically
close and can share basin-unit boundaries at this level).
"""
import os

import geopandas as gpd
from shapely.geometry import box

HYBAS_LEV06_PATH = "data/swy_shared_package/hydrobasins/hybas_au/hybas_au_lev06_v1c.shp"
OUT_PATH = "data/swy_shared_package/borneo_aoi.gpkg"

# Borneo's standard geographic extent (WGS84) — chosen tight enough to exclude Sulawesi
# (starts ~119.3E), Sumatra, and Java, per the "bbox-clipped to avoid leaking into
# Sulawesi/Sumatra/Java" decision already validated in this project's data dictionary.
BORNEO_BBOX = (108.5, -4.5, 119.3, 7.5)  # (minx, miny, maxx, maxy)


def main():
    bbox_geom = box(*BORNEO_BBOX)
    units = gpd.read_file(HYBAS_LEV06_PATH)
    units = units.to_crs(4326)

    intersecting = units[units.intersects(bbox_geom)]
    dissolved = intersecting.dissolve()
    clipped = gpd.clip(dissolved, bbox_geom)

    os.makedirs(os.path.dirname(OUT_PATH), exist_ok=True)
    clipped.to_file(OUT_PATH, driver="GPKG")

    area_km2 = clipped.to_crs(epsg=3857).area.sum() / 1e6  # rough equal-area check, not exact
    print(f"units selected: {len(intersecting)}")
    print(f"dissolved+clipped area (rough): {area_km2:,.0f} km^2 (real Borneo ~743,330 km^2)")
    print(f"written to {OUT_PATH}")


if __name__ == "__main__":
    main()
