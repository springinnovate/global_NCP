"""Pixel-wise scatter comparison: WWF-SIPA baseline QF/B vs. NCP Kc/CN run QF/B.

Terminology, fixed 2026-09-15 (was "her"/"our"): **WWF-SIPA baseline** = the original run using
WWF-SIPA's own Kc/CN parametrization. **NCP Kc/CN run** = this project's isolated-variable run —
same WWF-SIPA inputs (LULC, climate stack), this project's own Kc/CN substituted in.

Why this is more informative than the side-by-side maps: it can distinguish between two very
different explanations for the ~3x QF gap found 2026-09-14 (WWF-SIPA mean 732.8mm/yr vs. NCP
257.3mm/yr). If the gap is a roughly uniform multiplicative shift across all pixels, that points
at the rain-events placeholder (flat 18 events/month everywhere, no spatial variation at all —
exactly the kind of input swap that would produce a spatially uniform effect). If the gap instead
clusters by land-cover class (some classes track closely, others diverge sharply), that points at
the CN/Kc crosswalk instead — those values differ by land-cover class, not uniformly. Colors each
point by the WWF-SIPA LULC class to make that distinction visible directly, rather than inferring
it from the aggregate numbers alone.

Method: WWF-SIPA QF/B (native 30m) are resampled DOWN to the NCP run's ~90m grid using average
resampling — physically the right direction (aggregating fine pixels into a coarse one), not
upsampling the NCP layer. The WWF-SIPA LULC (12-class) is resampled the same way using mode
(categorical — average would invent fractional non-existent classes). Points are randomly
subsampled for a readable plot; full-data correlation/regression is still computed on every valid
paired pixel, not just the plotted subset.

Outputs PNGs to docs/reports/swy/swy_ph_comparison_scatter_qf.png and _b.png, printed stats for the
writeup in swy_methods.qmd.
"""
import os

import numpy as np
import rasterio
from rasterio.warp import Resampling, reproject

NCP_WORKSPACE = "data/swy/philippines/workspace_becky_inputs_90m_snapped"
NCP_SUFFIX = "90m_snapped"
BASELINE_WORKSPACE = (
    "data/swy/philippines/rich_shared/workspace_swy_wwf_PH_baseline_historical_climate"
)
BASELINE_LULC_PATH = "data/swy/philippines/INPUTS_SP/ph_baseline_lulc_md5_7f29da.tif"
# Explicit "valid in both datasets" masks, built by 13_build_valid_comparison_mask.py once this
# run's grid is confirmed nested inside WWF-SIPA's own -- replaces the old, undocumented, plain
# isfinite()-only validity check (found 2026-09-18 to be silently dropping ~25% of NCP's own B
# domain along every coastline, with no record that this was happening).
MASK_DIR = "data/swy/philippines/comparison_maps"

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
        ref_transform, ref_crs, ref_shape, ref_nodata = (
            ref.transform, ref.crs, ref.shape, ref.nodata,
        )
    with rasterio.open(src_path) as src:
        dst = np.full(ref_shape, np.nan, dtype="float64")
        reproject(
            source=rasterio.band(src, 1),
            destination=dst,
            src_transform=src.transform,
            src_crs=src.crs,
            dst_transform=ref_transform,
            dst_crs=ref_crs,
            resampling=resampling,
            src_nodata=src.nodata,
            dst_nodata=np.nan,
        )
    return dst


def run_pair(var, baseline_path, ncp_path, out_png):
    with rasterio.open(ncp_path) as src:
        ncp = src.read(1).astype("float64")
        ncp_nodata = src.nodata

    baseline_resampled = _resample_to_ref(baseline_path, ncp_path, Resampling.average)
    baseline_lulc_resampled = _resample_to_ref(BASELINE_LULC_PATH, ncp_path, Resampling.mode)

    with rasterio.open(os.path.join(MASK_DIR, f"{var.lower()}_valid_mask.tif")) as mask_src:
        comparison_mask = mask_src.read(1) == 1

    valid = np.isfinite(ncp) & np.isfinite(baseline_resampled)
    if ncp_nodata is not None:
        valid &= ~np.isclose(ncp, ncp_nodata)
    valid &= np.isfinite(baseline_lulc_resampled)
    valid &= comparison_mask

    x = baseline_resampled[valid]  # WWF-SIPA baseline
    y = ncp[valid]  # NCP Kc/CN run
    lulc = baseline_lulc_resampled[valid].astype(int)

    n = x.size
    corr = float(np.corrcoef(x, y)[0, 1])
    slope, intercept = np.polyfit(x, y, 1)
    print(f"{var}: n={n} corr={corr:.3f} slope={slope:.3f} intercept={intercept:.2f}")
    print(f"{var}: baseline mean={x.mean():.1f} ncp mean={y.mean():.1f} ratio(ncp/baseline)={y.mean()/x.mean():.3f}")

    # per-class breakdown
    print(f"{var} by WWF-SIPA LULC class (baseline_mean, ncp_mean, ratio, n_pixels):")
    for code, name in LULC_CLASSES.items():
        m = lulc == code
        if m.sum() < 100:
            continue
        hx, hy = x[m], y[m]
        print(f"  {name:18s} baseline={hx.mean():8.1f} ncp={hy.mean():8.1f} ratio={hy.mean()/hx.mean():.3f} n={m.sum()}")

    rng = np.random.default_rng(42)
    if n > N_PLOT_SAMPLE:
        idx = rng.choice(n, N_PLOT_SAMPLE, replace=False)
    else:
        idx = np.arange(n)

    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(7, 7), dpi=130)
    for code, name in LULC_CLASSES.items():
        m = lulc[idx] == code
        if m.sum() == 0:
            continue
        ax.scatter(
            x[idx][m], y[idx][m], s=2, alpha=0.25, color=LULC_COLORS[code], label=name,
            linewidths=0,
        )
    lims = [0, max(x[idx].max(), y[idx].max()) * 1.02]
    ax.plot(lims, lims, "k--", linewidth=1, label="1:1 line")
    ax.set_xlim(lims)
    ax.set_ylim(lims)
    ax.set_xlabel(f"WWF-SIPA baseline {var} (mm/yr)")
    ax.set_ylabel(f"NCP Kc/CN run {var} (mm/yr)")
    ax.set_title(f"Philippines {var}: WWF-SIPA baseline vs. NCP Kc/CN run\n(n={n:,} pixels, r={corr:.2f}, colored by WWF-SIPA LULC class)")
    legend = ax.legend(markerscale=6, fontsize=7, loc="upper left", framealpha=0.9)
    # The scatter points use alpha=0.25 so overlapping clusters stay readable, but that alpha
    # carries over into the legend swatches by default, making the key nearly as hard to read as
    # the plot itself. Force full opacity on the legend markers only -- the data points are
    # untouched.
    handles = getattr(legend, "legend_handles", None) or legend.legendHandles
    for handle in handles:
        handle.set_alpha(1)
    fig.tight_layout()
    fig.savefig(out_png)
    print(f"Written to {out_png}")


def main():
    run_pair(
        "QF",
        f"{BASELINE_WORKSPACE}/QF_wwf_PH_baseline_historical_climate.tif",
        f"{NCP_WORKSPACE}/QF_ph_becky_inputs_{NCP_SUFFIX}.tif",
        "docs/reports/swy/swy_ph_comparison_scatter_qf.png",
    )
    # Masking-corrected, not the raw shared file — her B raster's own nodata flag (-9999) misses
    # ~55M pixels that are really ocean/invalid (many exactly 0.0); her QF raster's own nodata flag
    # correctly excludes them on the same grid, so that mask was applied directly to build this file.
    run_pair(
        "B",
        "data/swy/philippines/comparison_maps/B_wwf_PH_baseline_historical_climate_masked.tif",
        f"{NCP_WORKSPACE}/B_ph_becky_inputs_{NCP_SUFFIX}.tif",
        "docs/reports/swy/swy_ph_comparison_scatter_b.png",
    )


if __name__ == "__main__":
    main()
