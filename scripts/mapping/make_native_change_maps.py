"""Global change maps from native pixels (paper Figure 1 and the Supplement's absolute-change map).

Each display pixel (6 km, Equal Earth) shows the mean of the per-pixel SPC of the native pixels in it
(300 m; 2 km for nature access), from analysis/native_spc.py, restricted to the evaluated land
(blocks whose 10 km cell is in the cell universe, analysis/native_grid_lookup.R). The absolute variant
shows the mean per-hectare change of those pixels (2020 minus 1992). Same style as the earlier 10 km
maps: orange = adverse, teal = favourable, colour limits at the 1st/99th percentile of the display
pixels, near-zero values faded. Nitrogen and sediment export are modelled between 56 S and 60 N only.

Usage: .venv/Scripts/python.exe scripts/mapping/make_native_change_maps.py
Writes: outputs/plots/maps/global_change_native_spc.png, global_change_native_abs.png
"""
import os
from pathlib import Path

import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio
from rasterio.warp import reproject, Resampling
from rasterio.transform import from_origin
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap, Normalize

ROOT = Path(__file__).resolve().parents[2]
NAT = ROOT / "data" / "processed" / "native_spc"
DATA = Path(os.environ.get("GLOBAL_NCP_DATA", ROOT / "data"))
ORANGE, TEAL, GREEN = "#F07D00", "#009191", "#004D1E"

SERVICES = [
    ("n_export", "Nitrogen export", "n_export_agg_f18.tif", "grid_fid_005deg.tif", True, "kg N/ha/yr"),
    ("sed_export", "Sediment export", "sed_export_agg_f18.tif", "grid_fid_005deg.tif", True, "t/ha/yr"),
    ("pollination", "Pollination", "pollination_agg_f18.tif", "grid_fid_005deg.tif", False, "people fed equiv./ha"),
    ("nature_access", "Nature access", "nature_access_agg_f3.tif", "grid_fid_natacc_6km.tif", False, "access index"),
]
universe = pd.read_csv(NAT / "universe_grid_fid.csv")["grid_fid"].to_numpy()

# Equal Earth display grid, 6 km
RES = 6000.0
X0, Y0 = -17243959.0, 8392928.0
W, H = int(2 * -X0 / RES), int(2 * Y0 / RES)
DST_T = from_origin(X0, Y0, RES, RES)
CRS_EE = "EPSG:8857"

base = gpd.read_file(DATA / "vector_basedata" / "cartographic_ee_r264_correspondence.gpkg").to_crs(CRS_EE)


def layer(f, lk_name, metric):
    with rasterio.open(NAT / f) as r:
        v = r.read(1) if metric == "spc" else (r.read(9) - r.read(8)) / np.where(r.read(5) > 0, r.read(5), np.nan)
        src_t, src_crs = r.transform, r.crs
        h, w = r.height, r.width
        top = r.bounds.top
    with rasterio.open(NAT / lk_name) as lk:
        if lk.crs == src_crs and lk.height != h:   # aggregates that start at 60 N: same columns, fewer rows
            off = int(round((lk.bounds.top - top) / lk.res[1]))
            fid = lk.read(1)[off:off + h, :w]
        else:
            fid = lk.read(1)
    v = np.where(np.isin(fid, universe), v, np.nan).astype("float32")
    out = np.full((H, W), np.nan, dtype="float32")
    reproject(v, out, src_transform=src_t, src_crs=src_crs, dst_transform=DST_T, dst_crs=CRS_EE,
              resampling=Resampling.nearest, src_nodata=np.nan, dst_nodata=np.nan)
    return out


# SPC: one scale for all services (SPC is unitless and bounded), signed so that positive = adverse for
# that service, square-root stretch so moderate change stays visible next to the +-200% extremes.
from matplotlib.colors import FuncNorm
SQRT = FuncNorm((lambda x: np.sign(x) * np.sqrt(np.abs(x)), lambda x: np.sign(x) * x ** 2), vmin=-200, vmax=200)
SPC_CMAP = LinearSegmentedColormap.from_list("adv", [TEAL, "white", ORANGE])


def draw(ax, arr, adverse_up, label, unit, metric):
    base.plot(ax=ax, facecolor="#f2f2f2", edgecolor="#cccccc", linewidth=0.1)
    if metric == "spc":
        v = arr * (1 if adverse_up else -1)
        rgba = SPC_CMAP(SQRT(np.clip(v, -200, 200)))
        rgba[..., 3] = np.where(np.isfinite(v), np.minimum(np.abs(v) / 5.0, 1), 0)
    else:   # absolute change: native units differ, so each service keeps its own scale
        vals = arr[np.isfinite(arr)]
        L = float(np.max(np.abs(np.quantile(vals, [0.01, 0.99]))))
        cmap = LinearSegmentedColormap.from_list("d", [TEAL, "white", ORANGE] if adverse_up else [ORANGE, "white", TEAL])
        rgba = cmap(Normalize(-L, L)(np.clip(arr, -L, L)))
        rgba[..., 3] = np.where(np.isfinite(arr), np.minimum(np.abs(arr) / max(0.08 * L, 1e-9), 1), 0)
        sm = plt.cm.ScalarMappable(norm=Normalize(-L, L), cmap=cmap)
        cb = plt.colorbar(sm, ax=ax, orientation="horizontal", fraction=0.04, pad=0.02, aspect=40)
        cb.set_label(unit, fontsize=11); cb.ax.tick_params(labelsize=10)
    ax.imshow(rgba, extent=[X0, -X0, -Y0, Y0], interpolation="nearest", zorder=2)
    ax.set_xlim(X0, -X0); ax.set_ylim(-7000000, Y0); ax.set_axis_off()
    ax.set_title(label, fontsize=15, fontweight="bold", color=GREEN)


def shared_spc_legend(fig, axes, fontsize=11):
    """One legend for all services: magnitude of SPC, favourable to the left, adverse to the right."""
    sm = plt.cm.ScalarMappable(norm=SQRT, cmap=SPC_CMAP)
    ticks = [-200, -50, -10, 0, 10, 50, 200]
    cb = fig.colorbar(sm, ax=axes, orientation="horizontal", fraction=0.035, pad=0.03, aspect=60, shrink=0.6,
                      ticks=ticks)
    cb.ax.set_xticklabels([str(abs(t)) for t in ticks], fontsize=fontsize - 1)
    cb.set_label("Change (SPC, %)", fontsize=fontsize)
    cb.ax.text(0, 1.6, "Favourable", transform=cb.ax.transAxes, ha="left", fontsize=fontsize, color=TEAL, fontweight="bold")
    cb.ax.text(1, 1.6, "Adverse", transform=cb.ax.transAxes, ha="right", fontsize=fontsize, color=ORANGE, fontweight="bold")
    return cb


if __name__ == "__main__":
    for metric in ("spc", "abs"):
        fig, axes = plt.subplots(2, 2, figsize=(16, 9.5 if metric == "abs" else 9.0))
        for ax, (svc, label, f, lk, up, unit) in zip(axes.ravel(), SERVICES):
            draw(ax, layer(f, lk, metric), up, label, unit, metric)
        if metric == "spc":
            fig.subplots_adjust(left=0.01, right=0.99, top=0.96, bottom=0.12, wspace=0.02, hspace=0.12)
            shared_spc_legend(fig, axes.ravel().tolist())
        else:
            fig.tight_layout()
        out = ROOT / "outputs" / "plots" / "maps" / f"global_change_native_{metric}.png"
        fig.savefig(out, dpi=220, facecolor="white")
        plt.close(fig)
        print("wrote", out)
