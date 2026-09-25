"""Figures for the Köppen x crop group scoping (01_koppen_crop_coverage.py).

1. Map: dominant crop group per MapSPAM 5-arcmin cell (cells with >= 5% cropland). A plausibility
   check as much as a result: rice should show in monsoon Asia, small grains on the steppes and
   plains, tree crops in the humid tropics.
2. Map: Köppen main group of those same cropland cells.
3. Bar chart: global crop area per Köppen main group x crop group, largest first, with the
   cumulative share of global crop area as a text label on each bar.

Palette: reference categorical slots 1-6 (dataviz skill), validated (CVD pass; below-3:1 contrast
for three slots, so every bar is text-labeled and every map has a legend).

Outputs: docs/reports/swy/global_scoping_{crop_map,climate_map,cells}.png
"""
import glob
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import rasterio
from matplotlib.colors import ListedColormap
from matplotlib.patches import Patch
from rasterio.enums import Resampling

KOPPEN_PATH = "data/swy/shared/koppen_geiger/Beck_KG_V1_present_0p0083.tif"
MAPSPAM_DIR = "data/swy/shared/mapspam/raw_downloads"
CELLS_CSV = "data/swy/shared/scoping/koppen_main_crop_area.csv"
OUT_DIR = "docs/reports/swy"

CROP_GROUPS = {
    "row crops": ["MAIZ", "SORG", "MILL", "PMIL", "COTT", "SOYB", "SUNF", "GROU", "SESA", "OOIL",
                  "OFIB", "TOBA", "VEGE", "POTA", "SUGB", "TOMA", "ONIO", "CASS", "SWPO", "YAMS",
                  "ORTS", "REST"],
    "small grains": ["WHEA", "BARL", "OCER", "RAPE"],
    "tree & perennial": ["BANA", "PLNT", "CITR", "CNUT", "COCO", "COFF", "RCOF", "OILP", "RUBB",
                         "TEAS", "TEMF", "TROF"],
    "paddy rice": ["RICE"],
    "pulses": ["BEAN", "CHIC", "COWP", "LENT", "PIGE", "OPUL"],
    "sugarcane": ["SUGC"],
}
# fixed order = fixed colors (reference categorical slots 1-6)
GROUP_COLORS = {
    "row crops": "#2a78d6", "small grains": "#eb6834", "tree & perennial": "#1baf7a",
    "paddy rice": "#eda100", "pulses": "#e87ba4", "sugarcane": "#008300",
}
CLIMATE_COLORS = {  # Beck et al. (2018) legend hues for the main groups
    "A tropical": "#1f5fd6", "B dry": "#e8662c", "C temperate": "#5cb85c",
    "D continental": "#8e5ec9", "E polar": "#9e9e9e",
}
LAND_GRAY = "#e6e6e3"
TEXT = "#2b2b29"
MIN_CROP_SHARE = 0.05  # of a 5-arcmin cell's area


def cell_area_ha(shape):
    lat_edges = np.linspace(90, -90, shape[0] + 1)
    r = 6371.0088e3
    dlon = np.deg2rad(360 / shape[1])
    band = r**2 * dlon * np.abs(np.sin(np.deg2rad(lat_edges[:-1])) - np.sin(np.deg2rad(lat_edges[1:])))
    return np.repeat((band / 1e4)[:, None], shape[1], axis=1)


def load():
    groups = list(CROP_GROUPS)
    with rasterio.open(os.path.join(MAPSPAM_DIR, "RICE.tif")) as ref:
        shape = ref.shape
    stack = np.zeros((len(groups),) + shape)
    for gi, g in enumerate(groups):
        for crop in CROP_GROUPS[g]:
            with rasterio.open(os.path.join(MAPSPAM_DIR, f"{crop}.tif")) as src:
                a = src.read(1).astype("float64")
            a[~np.isfinite(a) | (a < 0)] = 0
            stack[gi] += a
    with rasterio.open(KOPPEN_PATH) as src:
        koppen = src.read(1, out_shape=shape, resampling=Resampling.mode)
    return groups, stack, koppen, shape


def style_map(ax):
    ax.set_xlim(0, 4320)
    ax.set_ylim(1800, 150)  # drop Antarctica and the high Arctic
    ax.set_xticks([])
    ax.set_yticks([])
    for s in ax.spines.values():
        s.set_visible(False)


def main():
    groups, stack, koppen, shape = load()
    total = stack.sum(axis=0)
    crop_share = total / cell_area_ha(shape)
    cropland = crop_share >= MIN_CROP_SHARE
    land = koppen > 0
    print(f"cells with >= {MIN_CROP_SHARE:.0%} cropland: {cropland.sum():,}; "
          f"crop area they hold: {total[cropland].sum() / total.sum():.1%}")
    rice_mha = stack[groups.index("paddy rice")].sum() / 1e6
    print(f"sanity: global rice physical area {rice_mha:.0f} Mha, total crop physical area "
          f"{total.sum() / 1e6:,.0f} Mha")

    # --- map 1: dominant crop group ---
    dom = np.argmax(stack, axis=0).astype(float)
    img = np.full(shape, np.nan)
    img[land] = -1
    img[cropland] = dom[cropland]
    cmap = ListedColormap([LAND_GRAY] + [GROUP_COLORS[g] for g in groups])
    fig, ax = plt.subplots(figsize=(13, 6.2), dpi=130)
    ax.imshow(img, cmap=cmap, vmin=-1.5, vmax=len(groups) - 0.5, interpolation="nearest")
    style_map(ax)
    ax.set_title("Dominant crop group by physical area (MapSPAM 2020; cells with at least 5% cropland)",
                 fontsize=12, color=TEXT, loc="left")
    handles = [Patch(color=GROUP_COLORS[g], label=g) for g in groups]
    ax.legend(handles=handles, loc="lower left", frameon=False, fontsize=9, ncol=3)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT_DIR, "global_scoping_crop_map.png"), bbox_inches="tight")
    plt.close(fig)

    # --- map 2: Köppen main group of cropland cells ---
    main_code = np.zeros(shape, dtype=int)  # 0 none, 1..5 A..E
    for code in range(1, 31):
        main_code[koppen == code] = 1 if code <= 3 else 2 if code <= 7 else 3 if code <= 16 else 4 if code <= 28 else 5
    img2 = np.full(shape, np.nan)
    img2[land] = 0
    img2[cropland & (main_code > 0)] = main_code[cropland & (main_code > 0)]
    names = list(CLIMATE_COLORS)
    cmap2 = ListedColormap([LAND_GRAY] + [CLIMATE_COLORS[n] for n in names])
    fig, ax = plt.subplots(figsize=(13, 6.2), dpi=130)
    ax.imshow(img2, cmap=cmap2, vmin=-0.5, vmax=5.5, interpolation="nearest")
    style_map(ax)
    ax.set_title("Köppen-Geiger main climate group of cropland cells (Beck et al. 2018)",
                 fontsize=12, color=TEXT, loc="left")
    ax.legend(handles=[Patch(color=CLIMATE_COLORS[n], label=n) for n in names[:4]],
              loc="lower left", frameon=False, fontsize=9, ncol=4)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT_DIR, "global_scoping_climate_map.png"), bbox_inches="tight")
    plt.close(fig)

    # --- chart 3: climate x crop cells, largest first ---
    cells = pd.read_csv(CELLS_CSV)
    cells = cells[cells.area_mha >= 1].reset_index(drop=True)
    fig, ax = plt.subplots(figsize=(10, 0.36 * len(cells) + 1.4), dpi=130)
    y = np.arange(len(cells))
    ax.barh(y, cells.area_mha, height=0.72, color=[GROUP_COLORS[g] for g in cells.crop_group],
            edgecolor="white", linewidth=2)
    for yi, row in cells.iterrows():
        ax.text(row.area_mha + 2, yi, f"{row.area_mha:.0f} Mha · cumulative {row.cumulative_share:.0%}",
                va="center", fontsize=8.5, color=TEXT)
    ax.set_yticks(y)
    ax.set_yticklabels([f"{m[2:]} · {g}" for m, g in zip(cells.main, cells.crop_group)], fontsize=9, color=TEXT)
    ax.invert_yaxis()
    ax.set_xlabel("Global crop physical area (Mha)", color=TEXT)
    ax.set_xlim(0, cells.area_mha.max() * 1.45)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    ax.grid(axis="x", color="#e2e2de", linewidth=0.8)
    ax.set_axisbelow(True)
    n80 = int((cells.cumulative_share < 0.8).sum()) + 1
    n90 = int((cells.cumulative_share < 0.9).sum()) + 1
    ax.set_title(f"Climate × crop group cells, largest first: {n80} cells cover 80% of global crop area, {n90} cover 90%",
                 fontsize=11, color=TEXT, loc="left")
    ax.legend(handles=[Patch(color=GROUP_COLORS[g], label=g) for g in groups], loc="lower right",
              frameon=False, fontsize=8.5)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT_DIR, "global_scoping_cells.png"), bbox_inches="tight")
    plt.close(fig)
    print(f"written to {OUT_DIR}/global_scoping_*.png")


if __name__ == "__main__":
    main()
