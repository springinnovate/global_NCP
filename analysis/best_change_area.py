"""Find the area with the highest overall change in provision within a region (inset placement).

Areas need at least half of their extent modelled (sediment export pixels), as in the global ranking.
Scans area centres on a 0.5-degree grid over a region given as lat_min lat_max lon_min lon_max
(e.g. the Chaco: -29 -20 -64 -59; eastern China: 20 36 105 122). Each window has the inset's
ground size (4 degrees of latitude, longitude span widened by 1/cos(latitude)). Score = mean over the four
land services of the area-weighted share of native pixels with strong change (SPC of 20% or more) in either
direction, from the native-SPC aggregates (analysis/native_spc.py).

Usage: .venv/Scripts/python.exe analysis/best_change_area.py lat_min lat_max lon_min lon_max
"""
import sys
from pathlib import Path

import numpy as np
import rasterio
from rasterio.vrt import WarpedVRT
from rasterio.windows import from_bounds
from rasterio.enums import Resampling
from rasterio.transform import from_origin

NAT = Path(__file__).resolve().parents[1] / "data" / "processed" / "native_spc"
HALF = 2.0


def bounds(lat, lon):
    hx = HALF / np.cos(np.radians(lat))
    return lon - hx, lat - HALF, lon + hx, lat + HALF


def share_geo(f, b):
    with rasterio.open(NAT / f) as r:
        w = from_bounds(*b, transform=r.transform)
        adv, fav, n = (np.nan_to_num(r.read(i, window=w, boundless=True)) for i in (3, 4, 5))
        top = r.transform.f - w.row_off * r.res[1]
    lat = top - (np.arange(n.shape[0]) + 0.5) * r.res[1]
    a = n * np.cos(np.radians(lat))[:, None]
    full = 324 * np.cos(np.radians(lat))[:, None] * np.ones_like(n)   # 18 x 18 native pixels per block
    land = a.sum() / full.sum()
    return ((((adv + fav) * a).sum() / a.sum()) if a.sum() > 0 else np.nan), land


def share_na(b, res=0.05):
    with rasterio.open(NAT / "nature_access_agg_f3.tif") as r:
        nx, ny = int(round((b[2] - b[0]) / res)), int(round((b[3] - b[1]) / res))
        with WarpedVRT(r, crs="EPSG:4326", resampling=Resampling.nearest,
                       transform=from_origin(b[0], b[3], res, res), width=nx, height=ny) as v:
            adv, fav, n = (np.nan_to_num(v.read(i)) for i in (3, 4, 5))
    return ((adv + fav) * n).sum() / n.sum() if n.sum() > 0 else np.nan


rows = []
la0, la1, lo0, lo1 = (float(x) for x in sys.argv[1:5])
for lat in np.arange(la0, la1 + 0.01, 0.5):
    for lon in np.arange(lo0, lo1 + 0.01, 0.5):
        b = bounds(lat, lon)
        g = [share_geo(f, b) for f in ("n_export_agg_f18.tif", "sed_export_agg_f18.tif", "pollination_agg_f18.tif")]
        if g[1][1] < 0.5:      # as in the global ranking: at least half of the area must be modelled land
            continue
        s = [x[0] for x in g] + [share_na(b)]
        rows.append((lat, lon, *[100 * x for x in s], 100 * np.nanmean(s)))

rows.sort(key=lambda r: -r[-1])
print("lat    lon    N_exp  Sed_exp  Poll  NatAcc  mean (strong change, either direction, % of land)")
for r in rows[:10]:
    print(f"{r[0]:5.1f} {r[1]:6.1f}  {r[2]:5.1f}  {r[3]:6.1f}  {r[4]:5.1f}  {r[5]:5.1f}   {r[6]:5.1f}")
