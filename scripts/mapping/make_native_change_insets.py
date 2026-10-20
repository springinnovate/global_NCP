"""Change at native resolution in five windows of equal ground size (paper figure next to the global map).

Each window spans 4 degrees of latitude (about 445 km) and the same distance in longitude, so the
longitude span widens with latitude; panels are drawn at true aspect, with a 1-degree graticule and a
100 km scale bar.

Windows come from analysis/find_change_insets.py (outputs/tables/change_map_stats_native/
inset_candidates.csv): the windows with the densest strong adverse change (Sumatra, southern Amazon,
Chaco), the densest strong favourable change (Benin-Togo) and the strongest nature access loss
(northern Kazakhstan). The Chaco and southeastern China windows are the areas with the highest overall change in each region, the mean
over the four services of the share of land with strong change in either direction
(analysis/best_change_area.py). Each panel is the per-pixel SPC of the native raster (300 m; 2 km for
nature access, reprojected to lon/lat by nearest neighbour), over the full -200% to +200% range with a
square-root colour stretch so moderate changes stay visible. Orange = adverse for that service.

A locator map on top shows where the five windows are.

Usage: .venv/Scripts/python.exe scripts/mapping/make_native_change_insets.py
Writes: outputs/plots/maps/native_change_insets.png
"""
from pathlib import Path

import numpy as np
import rasterio
from rasterio.vrt import WarpedVRT
from rasterio.windows import from_bounds
from rasterio.enums import Resampling
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap, FuncNorm

ROOT = Path(__file__).resolve().parents[2]
NAT = ROOT / "data" / "processed" / "native_spc"
ORANGE, TEAL, ZERO = "#F07D00", "#009191", "#ededed"

WINDOWS = [  # (label, lat centre, lon centre)
    ("A  Sumatra (Riau-Jambi)", 0.0, 103.0),
    ("B  S Amazon (Mato Grosso-Para)", -11.0, -56.0),
    ("C  Argentine Chaco", -28.0, -62.5),
    ("D  SE China (Jiangxi-Hunan)", 26.5, 114.5),
    ("E  Benin-Togo", 9.0, 2.0),
    ("F  Northern Kazakhstan", 51.0, 76.0),
]
SERVICES = [("n_export", "Nitrogen export", True), ("sed_export", "Sediment export", True),
            ("pollination", "Pollination", False), ("nature_access", "Nature access", False)]
HALF = 2.0

# same scale and legend as the global map (make_native_change_maps.py): one ramp for all services,
# signed so that positive = adverse for that service, square-root stretch over -200..200
f = lambda x: np.sign(x) * np.sqrt(np.abs(x))
finv = lambda x: np.sign(x) * x ** 2
norm = FuncNorm((f, finv), vmin=-200, vmax=200)
CMAP = LinearSegmentedColormap.from_list("adv", [TEAL, ZERO, ORANGE])
CMAP.set_bad("white")


def bounds(lat, lon):
    hx = HALF / np.cos(np.radians(lat))
    return (lon - hx, lat - HALF, lon + hx, lat + HALF)


def read_window(svc, lat, lon):
    b = bounds(lat, lon)
    with rasterio.open(NAT / f"{svc}_spc_native.tif") as r:
        if r.crs.to_epsg() == 4326:
            w = from_bounds(*b, transform=r.transform)
            a = r.read(1, window=w, boundless=True, fill_value=-32768).astype(float)
        else:   # nature access: warp the window itself to a 0.018 deg (about 2 km) lon/lat grid
            from rasterio.transform import from_origin
            res = 0.018
            nx, ny = int(round((b[2] - b[0]) / res)), int(round((b[3] - b[1]) / res))
            with WarpedVRT(r, crs="EPSG:4326", resampling=Resampling.nearest,
                           transform=from_origin(b[0], b[3], res, res), width=nx, height=ny) as v:
                a = v.read(1).astype(float)
    a[a == -32768] = np.nan
    return a / 100.0, b


import os
import geopandas as gpd
from shapely.geometry import box

DATA = Path(os.environ.get("GLOBAL_NCP_DATA", ROOT / "data"))
fig = plt.figure(figsize=(13, 24))
gs = fig.add_gridspec(len(WINDOWS) + 1, len(SERVICES), height_ratios=[1.35] + [1] * len(WINDOWS),
                      hspace=0.12, wspace=0.08)
loc = fig.add_subplot(gs[0, :])
world = gpd.read_file(DATA / "vector_basedata" / "cartographic_ee_r264_correspondence.gpkg").to_crs("EPSG:8857")
world.plot(ax=loc, facecolor="#e6e6e6", edgecolor="#bdbdbd", linewidth=0.2)
boxes = gpd.GeoDataFrame(
    {"label": [w[0].split()[0] for w in WINDOWS]},
    geometry=[box(*bounds(lat, lon)).segmentize(0.5) for _, lat, lon in WINDOWS],
    crs="EPSG:4326").to_crs("EPSG:8857")
boxes.boundary.plot(ax=loc, color="#004D1E", linewidth=1.6)
for lab, g in zip(boxes["label"], boxes.geometry):
    x, y = g.bounds[2], g.bounds[3]
    loc.annotate(lab, (x, y), xytext=(3, 2), textcoords="offset points", fontsize=12,
                 fontweight="bold", color="#004D1E")
loc.set_xlim(-16000000, 16500000); loc.set_ylim(-6300000, 8300000); loc.set_axis_off()
axes = np.array([[fig.add_subplot(gs[i + 1, j]) for j in range(len(SERVICES))] for i in range(len(WINDOWS))])
for j, (svc, title, adverse_up) in enumerate(SERVICES):
    for i, (label, lat, lon) in enumerate(WINDOWS):
        ax = axes[i, j]
        a, b = read_window(svc, lat, lon)
        if np.isfinite(a).sum() == 0:
            ax.text(0.5, 0.5, "not modelled\n(north of 60 N)" if lat > 56 else "no data",
                    ha="center", va="center", transform=ax.transAxes, color="0.5", fontsize=9)
            im = None
        else:
            im = ax.imshow(a * (1 if adverse_up else -1), extent=[b[0], b[2], b[1], b[3]], cmap=CMAP, norm=norm,
                           interpolation="nearest")
        # true aspect, 1-degree graticule, coordinates on the first column, 100 km scale bar
        ax.set_xlim(b[0], b[2]); ax.set_ylim(b[1], b[3])
        ax.set_aspect(1 / np.cos(np.radians(lat)))
        xt = np.arange(np.ceil(b[0]), np.floor(b[2]) + 1, 1 if b[2] - b[0] < 5 else 2)
        yt = np.arange(np.ceil(b[1]), np.floor(b[3]) + 1, 1)
        ax.set_xticks(xt); ax.set_yticks(yt)
        ax.grid(color="0.45", linewidth=0.35, alpha=0.6, linestyle=(0, (2, 3)))
        ax.set_axisbelow(False)
        if j == 0:
            ax.set_xticklabels([f"{abs(x):.0f}°{'E' if x >= 0 else 'W'}" for x in xt], fontsize=7)
            ax.set_yticklabels([f"{abs(y):.0f}°{'N' if y >= 0 else 'S'}" for y in yt], fontsize=7)
            km = 100 / (111.32 * np.cos(np.radians(lat)))
            x0, y0 = b[0] + 0.06 * (b[2] - b[0]), b[1] + 0.06 * (b[3] - b[1])
            ax.plot([x0, x0 + km], [y0, y0], color="black", linewidth=2.5, solid_capstyle="butt")
            ax.text(x0 + km / 2, y0 + 0.04 * (b[3] - b[1]), "100 km", ha="center", fontsize=7,
                    bbox=dict(facecolor="white", edgecolor="none", alpha=0.7, pad=1))
        else:
            ax.set_xticklabels([]); ax.set_yticklabels([])
        ax.tick_params(length=2, pad=1)
        for s in ax.spines.values():
            s.set_color("0.8")
        if i == 0:
            ax.set_title(title, fontsize=12, fontweight="bold", color="#004D1E")
        if j == 0:
            ax.set_ylabel(label, fontsize=10, labelpad=4)

ticks = [-200, -50, -10, 0, 10, 50, 200]
cb = fig.colorbar(matplotlib.cm.ScalarMappable(norm=norm, cmap=CMAP), ax=axes.ravel().tolist(),
                  orientation="horizontal", fraction=0.02, pad=0.03, aspect=60, shrink=0.6, ticks=ticks)
cb.ax.set_xticklabels([str(abs(t)) for t in ticks], fontsize=9)
cb.set_label("Change (SPC, %)", fontsize=10)
cb.ax.text(0, 1.6, "Favourable", transform=cb.ax.transAxes, ha="left", fontsize=10, color=TEAL, fontweight="bold")
cb.ax.text(1, 1.6, "Adverse", transform=cb.ax.transAxes, ha="right", fontsize=10, color=ORANGE, fontweight="bold")

out = ROOT / "outputs" / "plots" / "maps" / "native_change_insets.png"
fig.savefig(out, dpi=200, bbox_inches="tight", facecolor="white")
print("wrote", out)
