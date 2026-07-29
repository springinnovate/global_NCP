"""
Paper Supplement: Per-Service Change + Hotspot Maps
====================================================

Generates the supplement Becky requested (2026-07-21 comment on the online paper draft):
full-page maps of the fine-scale change underlying the region/income/biome aggregations, with
each service's hotspots shown alongside, two maps per page (change on top, hotspot beneath),
for both absolute and relative (pct) change.

Written in Python, not R, as a deliberate one-off exception to this project's usual R/ggplot2
mapping convention (see scripts/mapping/*.R). Reason: the hotspot rasters under
data/processed/hotspots/rasters/ have unusual internal tiling that reproducibly segfaults both
R's `terra` and `raster` packages via their GDAL bindings in this environment, confirmed via
independent testing 2026-07-24 -- the files themselves are not corrupt (rasterio reads them
cleanly), the crash is specific to this R installation's GDAL bindings. Rather than lose time
debugging the R/GDAL environment before a deadline, this script reuses the same cartographic
rules as make_faceted_maps.R (directionality-aware diverging color scale, same hex colors) so
the visual output matches the rest of the project's figures.

Rules mirrored from make_faceted_maps.R:
- Goods (increase is good): Pollination, Nature_Access, N_Ret_Ratio, Sed_Ret_Ratio,
  C_Risk_Red_Ratio
- Damages (increase is bad): N_export, Sed_export, C_Risk
- Colorblind-safe diverging pair: orange #F07D00 = decline in the service being mapped,
  teal #009191 = improvement
"""

import os
import numpy as np
import rasterio
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib.backends.backend_pdf import PdfPages

REPO = r"C:\projects\global_NCP"
CHANGE_DIR = os.path.join(REPO, "data", "processed", "service_rasters")
HOTSPOT_DIR = os.path.join(REPO, "data", "processed", "hotspots", "rasters")
OUT_PDF = os.path.join(REPO, "outputs", "plots", "maps", "paper_supplement_service_maps.pdf")

ORANGE = "#F07D00"
TEAL = "#009191"
GRAY = "#D9D9D9"

GOODS = ["pollination", "nature_access", "n_ret_ratio", "sed_ret_ratio", "c_risk_red_ratio"]
DAMAGES = ["n_export", "sed_export", "c_risk"]

# lowercase service_rasters name -> TitleCase hotspots/rasters name, and a display label
SERVICES = [
    ("pollination", "Pollination", "Pollination"),
    ("nature_access", "Nature_Access", "Nature Access"),
    ("n_export", "N_export", "Nitrogen Export"),
    ("n_ret_ratio", "N_Ret_Ratio", "Nitrogen Retention Ratio"),
    ("sed_export", "Sed_export", "Sediment Export"),
    ("sed_ret_ratio", "Sed_Ret_Ratio", "Sediment Retention Ratio"),
    ("c_risk", "C_Risk", "Coastal Risk"),
    ("c_risk_red_ratio", "C_Risk_Red_Ratio", "Coastal Risk Reduction Ratio"),
]


def direction(service_lc):
    if service_lc in GOODS:
        return "good"
    if service_lc in DAMAGES:
        return "damage"
    return "unknown"


def diverging_cmap(dir_):
    # good: low=orange(bad/decline), high=teal(improvement)
    # damage: low=teal(good/less damage), high=orange(bad/more damage)
    if dir_ == "damage":
        return mcolors.LinearSegmentedColormap.from_list("dmg", [TEAL, "white", ORANGE])
    return mcolors.LinearSegmentedColormap.from_list("good", [ORANGE, "white", TEAL])


def read_masked(path):
    with rasterio.open(path) as src:
        arr = src.read(1).astype(float)
        nodata = src.nodata
        if nodata is not None:
            arr[arr == nodata] = np.nan
        bounds = src.bounds
    return arr, bounds


def plot_page(pdf, service_label, kind_label, change_path, hotspot_path, dir_):
    change_arr, bounds = read_masked(change_path)
    extent = (bounds.left, bounds.right, bounds.bottom, bounds.top)

    valid = change_arr[~np.isnan(change_arr)]
    vmax = np.nanpercentile(np.abs(valid), 99)  # robust symmetric limit, avoid outlier blowout
    if vmax <= 0:
        vmax = max(np.nanmax(np.abs(valid)), 1e-6)
    norm = mcolors.TwoSlopeNorm(vmin=-vmax, vcenter=0, vmax=vmax)

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(8.5, 11), dpi=200)

    land_mask = (~np.isnan(change_arr)).astype(float)

    # Draw the land silhouette first, then fade near-zero change toward transparent so
    # the gray shows through exactly where there's no meaningful change -- the same way
    # the hotspot panel lets gray show through everywhere that isn't a hotspot. Without
    # this, opaque near-white "no change" pixels cover the entire land footprint and the
    # map reads as colored blobs floating with no geographic reference at all.
    ax1.imshow(land_mask, extent=extent, cmap=mcolors.ListedColormap(["white", GRAY]), vmin=0, vmax=1)
    fade_threshold = 0.08 * vmax  # values within 8% of vmax of zero fade toward transparent
    alpha = np.clip(np.abs(np.nan_to_num(change_arr, nan=0.0)) / fade_threshold, 0, 1)
    alpha = np.where(np.isnan(change_arr), 0.0, alpha)
    im1 = ax1.imshow(change_arr, extent=extent, cmap=diverging_cmap(dir_), norm=norm, alpha=alpha)
    ax1.set_title(f"{service_label} \u2014 {kind_label} change (1992\u20132020)", fontsize=13, fontweight="bold")
    ax1.set_xticks([]); ax1.set_yticks([])
    cbar1 = fig.colorbar(im1, ax=ax1, orientation="horizontal", fraction=0.04, pad=0.03, shrink=0.6)
    cbar1.set_label("Change" + (" (%)" if kind_label == "relative" else ""))

    if hotspot_path is not None and os.path.exists(hotspot_path):
        hs_arr, hs_bounds = read_masked(hotspot_path)
        hs_extent = (hs_bounds.left, hs_bounds.right, hs_bounds.bottom, hs_bounds.top)
        ax2.imshow(land_mask, extent=extent, cmap=mcolors.ListedColormap(["white", GRAY]), vmin=0, vmax=1)
        hs_display = np.where(hs_arr == 1, 1.0, np.nan)
        ax2.imshow(hs_display, extent=hs_extent, cmap=mcolors.ListedColormap([ORANGE]), vmin=0, vmax=1)
        ax2.set_title(f"{service_label} \u2014 {kind_label} hotspots (top 5% most severe decline)", fontsize=13, fontweight="bold")
    else:
        ax2.text(0.5, 0.5, "No hotspot raster available for this service/kind", ha="center", va="center")
        ax2.set_title(f"{service_label} \u2014 hotspots (not available)", fontsize=13, fontweight="bold")
    ax2.set_xticks([]); ax2.set_yticks([])

    fig.tight_layout()
    pdf.savefig(fig)
    plt.close(fig)


def main():
    os.makedirs(os.path.dirname(OUT_PDF), exist_ok=True)
    n_pages = 0
    with PdfPages(OUT_PDF) as pdf:
        for lc_name, title_name, label in SERVICES:
            dir_ = direction(lc_name)
            for kind, kind_label in [("abs", "absolute"), ("pct", "relative")]:
                change_path = os.path.join(CHANGE_DIR, f"{lc_name}_{kind}_chg.tif")
                if not os.path.exists(change_path):
                    print(f"SKIP (no change raster): {lc_name} {kind}")
                    continue
                hotspot_path = os.path.join(HOTSPOT_DIR, f"{title_name}_{kind}.tif")
                plot_page(pdf, label, kind_label, change_path, hotspot_path, dir_)
                n_pages += 1
                print(f"Rendered: {label} ({kind_label})")
    print(f"\nDone. {n_pages} pages written to {OUT_PDF}")


if __name__ == "__main__":
    main()
