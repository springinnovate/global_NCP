"""Fetch monthly net radiation (Rn) for the Philippines AOI from NASA POWER, computed via the
standard FAO-56 method, for use in Negron Juarez et al. (2008)'s tropical-forest Kc/ET model
(`ET = C1 + C2*EVI^C3*(Rn-C4)`, see docs/swy/cn_kc_biome_coverage_registry.md).

NASA POWER's regional monthly API (https://power.larc.nasa.gov/api/temporal/monthly/regional)
does not provide net radiation directly. It has two hard constraints, confirmed by testing against
the live API rather than assumed from docs: at most 1 parameter per request, and at most a 10-degree
latitude span per request. The Philippines AOI spans ~16 degrees of latitude, so this script tiles
the request in both dimensions (parameters x latitude bands) and mosaics the results.

Five parameters are pulled: ALLSKY_SFC_SW_DWN (Rs), CLRSKY_SFC_SW_DWN (Rso), T2M_MAX, T2M_MIN,
RH2M. Net radiation is then computed with FAO-56's standard formulas (ch. 3, eq. 34-39) rather
than any bespoke approach -- this is the same derivation FAO's own ETo tools use when POWER is the
only available input:
    Rns = (1 - ALBEDO) * Rs                                          [net shortwave]
    Rnl = SIGMA * mean(Tmax_K^4, Tmin_K^4) * (0.34 - 0.14*sqrt(ea))
          * (1.35 * clip(Rs/Rso, 0.3, 1.0) - 0.35)                    [net longwave]
    Rn = Rns - Rnl
ea (actual vapor pressure) is derived from RH2M and the mean saturation vapor pressure of
Tmax/Tmin, per FAO-56's own simplification for when only mean relative humidity is available
(eq. 19), since POWER's monthly product doesn't give dewpoint or min/max RH separately.

ALBEDO is set to 0.13 (tropical forest canopy), not FAO-56's 0.23 reference-grass default --
this is a real, documented choice, not a neutral default, and the number itself hasn't been
cross-checked against Philippine-specific canopy literature.

**Unit note, flagged not verified**: this script outputs Rn in both MJ/m^2/day (FAO-56's native
unit) and W/m^2 (x 11.574, the standard time-mean conversion). Negron Juarez's C4=140 threshold
constant is assumed to be in W/m^2 (physically the right order of magnitude for a tropical-forest
energy threshold; 140 MJ/m^2/day would be roughly 1620 W/m^2 instantaneous-equivalent, implausibly
high). This assumption has NOT been re-confirmed against the actual paper text in
docs/swy/literature_pdfs/ this session -- do that before trusting any Kc/ET output built on this.

**Also not yet done**: resampling onto the model's working grid (these outputs are still on
POWER's native ~1-degree grid, clipped to the AOI bbox, not warped/resampled to the DEM grid the
way et0/precip are in `07b_build_biophysical_table_becky_inputs.py`). Continuous inputs in this
pipeline have needed bilinear, not nearest, resampling every time this has come up before (see the
2026-09-17 rain-events checkerboard bug) -- don't skip that step when this gets wired into the
biophysical table.

Output: data/swy/philippines/inputs/net_radiation_power_2020/net_radiation_ph_2020_MM.tif
(one band, MJ/m^2/day) and net_radiation_ph_2020_MM_wm2.tif (W/m^2).
"""
import os

import geopandas as gpd
import numpy as np
import rasterio
import requests
from rasterio.crs import CRS
from rasterio.enums import Resampling
from rasterio.mask import mask
from rasterio.merge import merge
from rasterio.warp import reproject

AOI_PATH = "data/swy/philippines/inputs/ph_aoi.gpkg"
RAW_DIR = "data/swy/philippines/raw_downloads/power_ph_2020"
OUT_DIR = "data/swy/philippines/inputs/net_radiation_power_2020"
YEAR = 2020

PARAMETERS = [
    "ALLSKY_SFC_SW_DWN",
    "CLRSKY_SFC_SW_DWN",
    "T2M_MAX",
    "T2M_MIN",
    "RH2M",
]

ALBEDO = 0.13  # tropical forest canopy; FAO-56 reference-grass default is 0.23, deliberately not used
SIGMA = 4.903e-9  # Stefan-Boltzmann constant, MJ K^-4 m^-2 day^-1 (FAO-56 units)
MJ_PER_DAY_TO_W_PER_M2 = 11.574  # 1e6 / 86400

BASE_URL = "https://power.larc.nasa.gov/api/temporal/monthly/regional"


def _axis_tiles(lo_bound, hi_bound, max_span=9.8, min_span=2.0):
    """Split a lon or lat range into equal-sized tiles respecting POWER's regional-API limits,
    confirmed live against the API: at most 10 degrees per axis, at least 2 degrees per axis (a
    plain greedy walk can leave a tiny leftover tile below that minimum, so this splits evenly
    instead)."""
    span = hi_bound - lo_bound
    n_tiles = max(1, int(np.ceil(span / max_span)))
    edges = np.linspace(lo_bound, hi_bound, n_tiles + 1)
    return [(edges[i], edges[i + 1]) for i in range(n_tiles)]


def _fetch_one(parameter, lon_min, lon_max, lat_min, lat_max, out_path):
    if os.path.exists(out_path):
        return
    url = (
        f"{BASE_URL}?parameters={parameter}&community=AG"
        f"&longitude-min={lon_min}&longitude-max={lon_max}"
        f"&latitude-min={lat_min}&latitude-max={lat_max}"
        f"&start={YEAR}&end={YEAR}&format=NETCDF"
    )
    r = requests.get(url, timeout=120)
    r.raise_for_status()
    with open(out_path, "wb") as f:
        f.write(r.content)


def _read_monthly_stack(nc_path):
    """Returns (array[12, H, W], transform) for months 1-12, dropping POWER's trailing annual band 13."""
    with rasterio.open(nc_path) as src:
        arr = src.read(list(range(1, 13)))  # bands 1-12 = Jan-Dec; band 13 is the annual value, dropped
        return arr, src.transform, src.width, src.height


def fetch_parameter_mosaic(parameter, lon_tiles, lat_tiles):
    """Downloads every (lon tile, lat tile) combination for one parameter and mosaics them into a
    single [12,H,W] stack. POWER's regional API caps both axes at 10 degrees, confirmed live."""
    tile_paths = []
    for i, (lon_lo, lon_hi) in enumerate(lon_tiles):
        for j, (lat_lo, lat_hi) in enumerate(lat_tiles):
            out_path = os.path.join(RAW_DIR, f"{parameter}_tile{i}_{j}_{YEAR}.nc")
            print(
                f"  fetching {parameter} tile lon {lon_lo:.2f}-{lon_hi:.2f}, "
                f"lat {lat_lo:.2f}-{lat_hi:.2f}..."
            )
            _fetch_one(parameter, lon_lo, lon_hi, lat_lo, lat_hi, out_path)
            tile_paths.append(out_path)

    if len(tile_paths) == 1:
        arr, transform, width, height = _read_monthly_stack(tile_paths[0])
        return arr, transform

    # mosaic month-by-month via rasterio.merge, opening each tile fresh per month band
    month_mosaics = []
    merged_transform = None
    for month in range(1, 13):
        srcs = []
        for p in tile_paths:
            ds = rasterio.open(p)
            ds_band = rasterio.io.MemoryFile().open(
                driver="GTiff", height=ds.height, width=ds.width, count=1,
                dtype=ds.dtypes[0], crs=CRS.from_epsg(4326), transform=ds.transform,
                nodata=np.nan,
            )
            ds_band.write(ds.read(month), 1)
            srcs.append(ds_band)
        mosaic, out_transform = merge(srcs, nodata=np.nan)
        for s in srcs:
            s.close()
        month_mosaics.append(mosaic[0])
        merged_transform = out_transform
    return np.stack(month_mosaics), merged_transform


def main():
    os.makedirs(RAW_DIR, exist_ok=True)
    os.makedirs(OUT_DIR, exist_ok=True)

    aoi = gpd.read_file(AOI_PATH).to_crs(4326)
    lon_min, lat_min, lon_max, lat_max = aoi.total_bounds
    # pad slightly so the AOI isn't flush against the request bbox edge
    lon_min, lat_min, lon_max, lat_max = lon_min - 0.3, lat_min - 0.3, lon_max + 0.3, lat_max + 0.3
    lon_tiles = _axis_tiles(lon_min, lon_max)
    lat_tiles = _axis_tiles(lat_min, lat_max)
    print(f"AOI bbox: lon [{lon_min:.2f}, {lon_max:.2f}], lat [{lat_min:.2f}, {lat_max:.2f}]")
    print(f"lon tiles: {lon_tiles}")
    print(f"lat tiles: {lat_tiles}")

    stacks = {}
    transforms = {}
    for param in PARAMETERS:
        print(f"parameter: {param}")
        arr, tfm = fetch_parameter_mosaic(param, lon_tiles, lat_tiles)
        stacks[param] = arr
        transforms[param] = tfm
        print(f"  shape: {arr.shape}, transform: {tfm}")

    # POWER's own radiation and temperature/humidity parameters come from different underlying
    # source grids (SYN1DEG ~1deg for the two SW_DWN variables, MERRA2 ~0.5x0.625deg for
    # T2M/RH2M -- confirmed by the shape mismatch this produced on first run, not assumed).
    # Reproject everything onto the finest grid (by pixel count) before combining them, using
    # bilinear -- this pipeline's own established rule for continuous fields (see the 2026-09-17
    # rain-events checkerboard bug).
    ref_param = max(stacks, key=lambda p: stacks[p].shape[1] * stacks[p].shape[2])
    ref_transform = transforms[ref_param]
    ref_height, ref_width = stacks[ref_param].shape[1], stacks[ref_param].shape[2]
    print(f"reference grid: {ref_param}, shape ({ref_height}, {ref_width})")

    def _resample_to_ref(arr, src_transform):
        if arr.shape[1] == ref_height and arr.shape[2] == ref_width and src_transform == ref_transform:
            return arr
        out = np.empty((arr.shape[0], ref_height, ref_width), dtype="float32")
        reproject(
            source=arr, destination=out,
            src_transform=src_transform, src_crs=CRS.from_epsg(4326),
            dst_transform=ref_transform, dst_crs=CRS.from_epsg(4326),
            resampling=Resampling.bilinear,
        )
        return out

    for param in PARAMETERS:
        stacks[param] = _resample_to_ref(stacks[param], transforms[param])
    transform = ref_transform

    rs = stacks["ALLSKY_SFC_SW_DWN"]
    rso = stacks["CLRSKY_SFC_SW_DWN"]
    tmax = stacks["T2M_MAX"]
    tmin = stacks["T2M_MIN"]
    rh = stacks["RH2M"]

    tmax_k = tmax + 273.16
    tmin_k = tmin + 273.16
    es_tmax = 0.6108 * np.exp(17.27 * tmax / (tmax + 237.3))
    es_tmin = 0.6108 * np.exp(17.27 * tmin / (tmin + 237.3))
    es = (es_tmax + es_tmin) / 2.0
    ea = es * (rh / 100.0)

    rs_rso = np.clip(rs / rso, 0.3, 1.0)
    rnl = (
        SIGMA
        * ((tmax_k**4 + tmin_k**4) / 2.0)
        * (0.34 - 0.14 * np.sqrt(np.maximum(ea, 0)))
        * (1.35 * rs_rso - 0.35)
    )
    rns = (1 - ALBEDO) * rs
    rn_mj = rns - rnl  # MJ/m^2/day, shape [12, H, W]
    rn_w = rn_mj * MJ_PER_DAY_TO_W_PER_M2

    geoms = [g.__geo_interface__ for g in aoi.geometry]
    height, width = rn_mj.shape[1], rn_mj.shape[2]

    for month in range(1, 13):
        for suffix, data in (("", rn_mj), ("_wm2", rn_w)):
            tmp_path = os.path.join(OUT_DIR, f"_tmp_{month:02d}{suffix}.tif")
            profile = {
                "driver": "GTiff", "height": height, "width": width, "count": 1,
                "dtype": "float32", "crs": CRS.from_epsg(4326), "transform": transform,
                "nodata": np.nan,
            }
            with rasterio.open(tmp_path, "w", **profile) as tmp:
                tmp.write(data[month - 1].astype("float32"), 1)

            with rasterio.open(tmp_path) as tmpsrc:
                out_img, out_transform = mask(tmpsrc, geoms, crop=True, nodata=np.nan)
                out_meta = tmpsrc.meta.copy()
                out_meta.update({
                    "height": out_img.shape[1], "width": out_img.shape[2],
                    "transform": out_transform, "compress": "lzw",
                })
            out_path = os.path.join(OUT_DIR, f"net_radiation_ph_{YEAR}_{month:02d}{suffix}.tif")
            with rasterio.open(out_path, "w", **out_meta) as dst:
                dst.write(out_img)
            os.remove(tmp_path)
        print(f"month {month:02d} done")

    print(f"done -- see {OUT_DIR}")


if __name__ == "__main__":
    main()
