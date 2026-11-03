#!/usr/bin/env bash
# Assemble the explorer's data folder (the volume mounted at /data) from a repo clone with its data.
# Usage, from the repo root:  scripts/dashboard/deploy/build_data_bundle.sh <output_dir>
set -euo pipefail

out=${1:?usage: build_data_bundle.sh <output_dir>}
dash=data/processed/dashboard
pa=outputs/plots/output_plots_diff

for f in "$dash/grid_dashboard.rds" "$dash/coastal_points.rds" "$dash/landcover_grid.rds" "$dash/tiles/abs_limits.csv" \
         "$pa/country_map_data.csv" "$pa/region_wb_map_data.csv" "$pa/income_grp_map_data.csv" \
         "$pa/biome_map_data.csv"; do
  [[ -f $f ]] || { echo "missing: $f (run from the repo root)" >&2; exit 1; }
done

mkdir -p "$out/path_a"
cp "$dash/grid_dashboard.rds" "$dash/coastal_points.rds" "$dash/landcover_grid.rds" "$out/"
cp "$pa"/{country,region_wb,income_grp,biome}_map_data.csv "$out/path_a/"
rsync -a --delete --exclude '._*' "$dash/tiles/" "$out/tiles/"
# macOS writes AppleDouble "._" companions for files with extended attributes on non-Apple filesystems
# (exFAT, network shares); they are not data
find "$out" -name '._*' -type f -delete

{
  echo "global_NCP change explorer data"
  echo "built: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "commit: $(git describe --always --dirty)"
} > "$out/VERSION"

# checksums of every file, to verify a copy:  cd <dir> && sha256sum -c SHA256SUMS
(cd "$out" && find . -type f ! -name SHA256SUMS ! -name '._*' -print0 | sort -z | xargs -0 shasum -a 256 > SHA256SUMS)
find "$out" -name '._*' -type f -delete
echo "wrote $out: $(find "$out" -type f | wc -l | tr -d ' ') files, $(du -sh "$out" | cut -f1)"
