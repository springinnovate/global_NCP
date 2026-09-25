"""Compare the NDVI per-pixel Kc run (09l) against WWF-SIPA's baseline AND against the previous
best run (09k, combined forest+grassland/shrub Kc), same masks and pattern as
18_combined_forest_grassland_comparison.py.

09l differs from 09k only in the Kc of the five classes that were still on the EVI-into-Kamble
regression (Annual Crop, Mangrove, Marshland/Swamp, Open/Barren, Perennial Crop -- see
07g_build_kc_ndvi_perpixel.py), so the 09k -> 09l change isolates the calibration fix. Two
questions this answers:
    1. How much of the AOI-wide B gap (09k: ratio 1.37, r 0.39) does fixing cropland et al. close?
    2. Does Closed Forest's B ratio (09k: 1.30) hold? B at a forest pixel includes water routed
       from upslope pixels, so a change in cropland Kc can move it even though forest Kc didn't change.

QF is a sanity check only -- Kc doesn't feed QF, so 09k and 09l QF should be identical.

Outputs:
    docs/reports/swy/swy_ph_ndvi_perpixel_scatter_{qf,b}.png
    data/swy/philippines/comparison_maps/ndvi_perpixel_class_comparison.csv
"""
import os

import numpy as np
import pandas as pd
import rasterio
from rasterio.warp import Resampling, reproject

RUNS = {
    "09k": ("data/swy/philippines/workspace_becky_inputs_90m_combined_forest_grassland_kc",
            "combined_forest_grassland_kc"),
    "09l": ("data/swy/philippines/workspace_becky_inputs_90m_ndvi_perpixel_kc",
            "ndvi_perpixel_kc"),
}
BASELINE = {
    "QF": "data/swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_historical_climate/"
          "QF_wwf_PH_baseline_historical_climate.tif",
    "B": "data/swy/philippines/comparison_maps/B_wwf_PH_baseline_historical_climate_masked.tif",
}
BASELINE_LULC_PATH = "data/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif"
MASK_DIR = "data/swy/philippines/comparison_maps"
OUT_CSV = os.path.join(MASK_DIR, "ndvi_perpixel_class_comparison.csv")

LULC_CLASSES = {
    1: "Annual Crop", 2: "Brush/Shrubs", 3: "Built-up", 4: "Closed Forest", 5: "Fishpond",
    6: "Grassland", 7: "Inland Water", 8: "Mangrove Forest", 9: "Marshland/Swamp",
    10: "Open Forest", 11: "Open/Barren", 12: "Perennial Crop",
}
LULC_COLORS = {
    1: "#e6ab02", 2: "#a6761d", 3: "#666666", 4: "#1b9e77", 5: "#377eb8",
    6: "#66a61e", 7: "#1f78b4", 8: "#7570b3", 9: "#984ea3", 10: "#4daf4a",
    11: "#e7298a", 12: "#d95f02",
}

N_PLOT_SAMPLE = 300_000


def _resample_to_ref(src_path, ref_path, resampling):
    with rasterio.open(ref_path) as ref:
        ref_transform, ref_crs, ref_shape = ref.transform, ref.crs, ref.shape
    with rasterio.open(src_path) as src:
        dst = np.full(ref_shape, np.nan, dtype="float64")
        reproject(
            source=rasterio.band(src, 1), destination=dst,
            src_transform=src.transform, src_crs=src.crs,
            dst_transform=ref_transform, dst_crs=ref_crs,
            resampling=resampling, src_nodata=src.nodata, dst_nodata=np.nan,
        )
    return dst


def _read_run(var, run):
    workspace, suffix = RUNS[run]
    path = f"{workspace}/{var}_ph_{suffix}.tif"
    with rasterio.open(path) as src:
        arr = src.read(1).astype("float64")
        if src.nodata is not None:
            arr[np.isclose(arr, src.nodata)] = np.nan
    return arr, path


def compare(var, out_png):
    old, old_path = _read_run(var, "09k")
    new, new_path = _read_run(var, "09l")

    baseline = _resample_to_ref(BASELINE[var], new_path, Resampling.average)
    lulc_all = _resample_to_ref(BASELINE_LULC_PATH, new_path, Resampling.mode)

    with rasterio.open(os.path.join(MASK_DIR, f"{var.lower()}_valid_mask.tif")) as mask_src:
        comparison_mask = mask_src.read(1) == 1

    valid = (np.isfinite(old) & np.isfinite(new) & np.isfinite(baseline)
             & np.isfinite(lulc_all) & comparison_mask)
    x, y_old, y_new = baseline[valid], old[valid], new[valid]
    lulc = lulc_all[valid].astype(int)

    rows = []

    def add_row(name, m):
        rows.append({
            "var": var, "class": name, "n_pixels": int(m.sum()),
            "baseline_mean": x[m].mean(), "09k_mean": y_old[m].mean(), "09l_mean": y_new[m].mean(),
            "09k_ratio": y_old[m].mean() / x[m].mean(), "09l_ratio": y_new[m].mean() / x[m].mean(),
            "09k_r": np.corrcoef(x[m], y_old[m])[0, 1], "09l_r": np.corrcoef(x[m], y_new[m])[0, 1],
        })

    add_row("AOI (all classes)", np.ones_like(lulc, dtype=bool))
    for code, name in LULC_CLASSES.items():
        m = lulc == code
        if m.sum() >= 100:
            add_row(name, m)

    df = pd.DataFrame(rows)
    print(f"\n== {var}: WWF-SIPA baseline vs 09k (combined forest/grassland Kc) vs 09l (+ NDVI per-pixel Kc) ==")
    print(df.drop(columns="var").round(3).to_string(index=False))
    max_old_new = np.nanmax(np.abs(y_new - y_old))
    print(f"{var}: max |09l - 09k| = {max_old_new:.4f}")

    rng = np.random.default_rng(42)
    n = x.size
    idx = rng.choice(n, N_PLOT_SAMPLE, replace=False) if n > N_PLOT_SAMPLE else np.arange(n)

    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(7, 7), dpi=130)
    for code, name in LULC_CLASSES.items():
        m = lulc[idx] == code
        if m.sum() == 0:
            continue
        ax.scatter(x[idx][m], y_new[idx][m], s=2, alpha=0.25, color=LULC_COLORS[code], label=name, linewidths=0)
    lims = [0, max(x[idx].max(), y_new[idx].max()) * 1.02]
    ax.plot(lims, lims, "k--", linewidth=1, label="1:1 line")
    ax.set_xlim(lims)
    ax.set_ylim(lims)
    ax.set_xlabel(f"WWF-SIPA baseline {var} (mm/yr)")
    ax.set_ylabel(f"NCP CN + per-pixel Kc {var} (mm/yr)")
    corr = df.loc[df["class"] == "AOI (all classes)", "09l_r"].iloc[0]
    ax.set_title(f"Philippines {var}: WWF-SIPA baseline vs. NCP CN + per-pixel Kc\n(n={n:,} pixels, r={corr:.2f})")
    legend = ax.legend(markerscale=6, fontsize=7, loc="upper left", framealpha=0.9)
    handles = getattr(legend, "legend_handles", None) or legend.legendHandles
    for handle in handles:
        handle.set_alpha(1)
    fig.tight_layout()
    fig.savefig(out_png)
    print(f"Written to {out_png}")
    return df


def main():
    dfs = [
        compare("QF", "docs/reports/swy/swy_ph_ndvi_perpixel_scatter_qf.png"),
        compare("B", "docs/reports/swy/swy_ph_ndvi_perpixel_scatter_b.png"),
    ]
    pd.concat(dfs).to_csv(OUT_CSV, index=False)
    print(f"\nWritten to {OUT_CSV}")


if __name__ == "__main__":
    main()
