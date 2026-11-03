"""Rank candidate inset windows for the native-resolution change maps.

For each service, slides a square window (default 4 x 4 degrees, 1-degree steps) over the native-SPC
aggregates (analysis/native_spc.py) and scores it by the area-weighted share of native pixels with
strong change (SPC of 20% or more): adverse, favourable, and both at once (the smaller of the two).
Windows need at least half of their area valid. Prints the top windows per criterion, keeping only
windows at least 8 degrees apart, and writes them to outputs/tables/change_map_stats_native/inset_candidates.csv.

Usage: .venv/Scripts/python.exe analysis/find_change_insets.py [window_deg]
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import rasterio

ROOT = Path(__file__).resolve().parents[1]
NAT = ROOT / "data" / "processed" / "native_spc"
SERVICES = {"n_export": "n_export_agg_f18.tif", "sed_export": "sed_export_agg_f18.tif",
            "pollination": "pollination_agg_f18.tif"}   # nature access (equal-area grid) is handled below
WIN = float(sys.argv[1]) if len(sys.argv) > 1 else 4.0
STEP_DEG, BLOCK = 1.0, 0.05


def box_sum(x, k):
    c = np.pad(np.cumsum(np.cumsum(x, 0), 1), ((1, 0), (1, 0)))
    return c[k:, k:] - c[:-k, k:] - c[k:, :-k] + c[:-k, :-k]


rows = []
for svc, f in SERVICES.items():
    with rasterio.open(NAT / f) as r:
        adv, fav, n = (np.nan_to_num(r.read(i)) for i in (3, 4, 5))
        top, left = r.transform.f, r.transform.c
    lat = top - (np.arange(n.shape[0]) + 0.5) * BLOCK
    area = n * np.cos(np.radians(lat))[:, None]
    k, s = int(WIN / BLOCK), int(STEP_DEG / BLOCK)
    A = box_sum(area, k)[::s, ::s]
    full = box_sum(np.full_like(area, 1.0) * np.cos(np.radians(lat))[:, None] * 324, k)[::s, ::s]
    ADV = box_sum(adv * area, k)[::s, ::s] / np.where(A > 0, A, 1)
    FAV = box_sum(fav * area, k)[::s, ::s] / np.where(A > 0, A, 1)
    ok = A > 0.5 * full
    for crit, score in (("adverse", ADV), ("favourable", FAV), ("both", np.minimum(ADV, FAV))):
        sc = np.where(ok, score, -1).ravel()
        picked = []
        for idx in np.argsort(sc)[::-1]:
            if sc[idx] <= 0 or len(picked) == 5:
                break
            i, j = divmod(idx, ADV.shape[1])
            y0 = top - i * STEP_DEG; x0 = left + j * STEP_DEG
            cy, cx = y0 - WIN / 2, x0 + WIN / 2
            if any(abs(cy - p[0]) < 8 and abs(cx - p[1]) < 8 for p in picked):
                continue
            picked.append((cy, cx))
            rows.append(dict(service=svc, criterion=crit, lat_centre=round(cy, 1), lon_centre=round(cx, 1),
                             pct_strong_adverse=round(100 * ADV[i, j], 1), pct_strong_favourable=round(100 * FAV[i, j], 1)))

# nature access: Eckert IV, ~6 km blocks; window of about the same ground size, centres back to lon/lat
from rasterio.warp import transform as warp
with rasterio.open(NAT / "nature_access_agg_f3.tif") as r:
    adv, fav, n = (np.nan_to_num(r.read(i)) for i in (3, 4, 5))
    T, crs = r.transform, r.crs
bk = T.a
k, s = int(WIN * 111000 / bk), int(STEP_DEG * 111000 / bk)
A = box_sum(n, k)[::s, ::s]
full = box_sum(np.full_like(n, 9.0), k)[::s, ::s]
ADV = box_sum(adv * n, k)[::s, ::s] / np.where(A > 0, A, 1)
FAV = box_sum(fav * n, k)[::s, ::s] / np.where(A > 0, A, 1)
ok = A > 0.5 * full
for crit, score in (("adverse", ADV), ("favourable", FAV), ("both", np.minimum(ADV, FAV))):
    sc = np.where(ok, score, -1).ravel()
    picked = []
    for idx in np.argsort(sc)[::-1]:
        if sc[idx] <= 0 or len(picked) == 5:
            break
        i, j = divmod(idx, ADV.shape[1])
        x, y = T.c + (j * s + k / 2) * bk, T.f - (i * s + k / 2) * bk
        lon, lat = warp(crs, "EPSG:4326", [x], [y])
        cy, cx = lat[0], lon[0]
        if any(abs(cy - p[0]) < 8 and abs(cx - p[1]) < 8 for p in picked):
            continue
        picked.append((cy, cx))
        rows.append(dict(service="nature_access", criterion=crit, lat_centre=round(cy, 1), lon_centre=round(cx, 1),
                         pct_strong_adverse=round(100 * ADV[i, j], 1), pct_strong_favourable=round(100 * FAV[i, j], 1)))

# name each window by the countries that cover it (up to two, each with at least 20% of the window's land)
import geopandas as gpd
from shapely.geometry import box
countries = gpd.read_file(ROOT / "data" / "vector_basedata" / "cartographic_ee_r264_correspondence.gpkg")[["nev_name", "geometry"]]
countries = countries.to_crs("EPSG:8857")


def names(lat, lon):
    w = gpd.GeoDataFrame(geometry=[box(lon - WIN / 2, lat - WIN / 2, lon + WIN / 2, lat + WIN / 2).segmentize(0.25)],
                         crs="EPSG:4326").to_crs("EPSG:8857")
    x = gpd.overlay(countries, w, how="intersection")
    x["a"] = x.area
    x = x.groupby("nev_name")["a"].sum().sort_values(ascending=False)
    x = x[x / x.sum() >= 0.2].head(2)
    return ", ".join(x.index)


for r in rows:
    r["countries"] = names(r["lat_centre"], r["lon_centre"])

out = ROOT / "outputs" / "tables" / "change_map_stats_native" / "inset_candidates.csv"
out.parent.mkdir(parents=True, exist_ok=True)
df = pd.DataFrame(rows)
df.to_csv(out, index=False)
pd.set_option("display.width", 200)
print(df.to_string(index=False))
