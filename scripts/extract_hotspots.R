# 5-service hotspot redesign. Originally written for Becky's 2026-07-21 "water/access"
# framing (N_export, Sed_export, C_Risk); updated 2026-09-01 to the retention/protection
# framing (N_retention, Sed_retention, C_Prot_service, settled per Steve's clarification)
# -- this script's own HOTS_CFG_5 had independently drifted to the old export/risk names,
# same failure mode as 3 other files this session, see docs/pipeline_reference.md B7 and
# R/service_config.R. Keeps: Nature_Access, Pollination, N_retention, Sed_retention,
# C_Prot_service. Adds three overlap categories: water (N_retention + Sed_retention, any),
# access (Nature_Access + Pollination + C_Prot_service, any), and combined_cross
# (at least one water hotspot AND at least one access hotspot in the same cell) -- water/
# access membership matches analysis/hotspot_synthesis.qmd's combos exactly, both sourced
# from R/service_config.R.
#
# Reuses extract_hotspots() (R/get_hotspots.R) directly -- the same function
# analysis/hotspot_extraction.qmd calls via run_one_hotset() -- rather than
# running that ~1900-line notebook, which does a lot of unrelated map/table
# work not needed for this delivery. Global scope only for now (matches the
# immediate ask: get Rich the rasters). Subregional (income/region/biome/
# country) reruns can follow the same pattern later if needed.
#
# Writes to a NEW location (data/processed/hotspots_5service/) rather than
# overwriting the current canonical 8-service hotspot gpkgs -- the paper/
# book/presentation still depend on the 8-service definition until this
# redesign is reviewed and rolled out.

library(sf)
library(dplyr)

source("R/paths.R")
source("R/get_hotspots.R")
source("R/service_config.R")

message("Loading cached plt_long (long-format service change data)...")
plt_long <- readRDS(file.path(data_dir(), "processed", "plt_long.rds"))
message(sprintf("plt_long: %d rows, services: %s", nrow(plt_long),
                 paste(sort(unique(plt_long$service)), collapse = ", ")))

message("Building geometry (from 10k_change_calc.gpkg, which has a real grid_fid column -- NOT landgrid_1_clean_enriched_4326.gpkg, which has no fid/grid_fid/id column at all and would silently fall back to a meaningless positional seq_len() id)...")
master_path <- file.path(data_dir(), "processed", "10k_change_calc.gpkg")
stopifnot(file.exists(master_path))
geom_sf <- sf::st_read(master_path, quiet = TRUE)

if ("orig_fid" %in% names(geom_sf) && !"fid" %in% names(geom_sf)) geom_sf <- dplyr::rename(geom_sf, fid = orig_fid)
if ("grid_fid" %in% names(geom_sf) && !"fid" %in% names(geom_sf)) geom_sf <- dplyr::rename(geom_sf, fid = grid_fid)
if ("id" %in% names(geom_sf) && !"fid" %in% names(geom_sf)) geom_sf <- dplyr::rename(geom_sf, fid = id)
stopifnot("fid" %in% names(geom_sf))  # hard fail rather than silently falling back to seq_len()

geom_sf <- geom_sf |> dplyr::select(dplyr::any_of(c("fid", "iso3", "continent", "region_un", "subregion", "nev_name", "region_wb", "income_grp", "WWF_biome")))

if ("continent" %in% names(geom_sf)) {
  geom_sf <- dplyr::filter(geom_sf, !continent %in% c("Antarctica", "Seven seas (Open Ocean)"))
}
if ("WWF_biome" %in% names(geom_sf)) {
  geom_sf <- dplyr::filter(geom_sf, !WWF_biome %in% c("Lakes", "Rock & Ice"))
}
stopifnot("fid" %in% names(geom_sf), !any(duplicated(geom_sf$fid)))
message(sprintf("geom_sf: %d cells after Antarctica/Lakes/Rock&Ice exclusion", nrow(geom_sf)))

HOTS_CFG_5 <- list(
  pct_cutoff     = 0.05,
  threshold_mode = "percent",
  rule_mode      = "vectors",
  loss = hotspot_direction_lists(looking_for = "decline")$loss_services,
  gain = hotspot_direction_lists(looking_for = "decline")$gain_services,
  combos = list(
    water  = c("N_retention", "Sed_retention"),
    access = c("Nature_Access", "Pollination", "C_Prot_service")
  ),
  out_dir = file.path(data_dir(), "processed", "hotspots_5service")
)

svc_in_data <- unique(as.character(plt_long$service))
svc_in_cfg <- unique(c(HOTS_CFG_5$loss, HOTS_CFG_5$gain))
message("Services kept: ", paste(svc_in_cfg, collapse = ", "))
missing <- setdiff(svc_in_cfg, svc_in_data)
if (length(missing) > 0) stop("CRITICAL: services in HOTS_CFG_5 not found in plt_long: ", paste(missing, collapse=", "))

# IMPORTANT: restrict plt_long to only the 5 kept services before doing anything
# else. extract_hotspots() ranks per-service independently, so leaving the other
# 5 services (the 3 dropped + 2 unrelated ones present in this cached plt_long)
# in the data does NOT corrupt which cells get flagged as hotspots -- but it does
# silently inflate any "n_total" row-count computed from plt_long directly (rows
# are per fid x service, so counting all 10 services' rows overstates the
# meaningful denominator by ~2x). Filtering here makes every downstream count
# correct and the script faster.
n_before_svc_filter <- nrow(plt_long)
plt_long <- plt_long[plt_long$service %in% svc_in_cfg, , drop = FALSE]
message(sprintf("plt_long filtered to 5 kept services: %d -> %d rows", n_before_svc_filter, nrow(plt_long)))

run_one_5service <- function(value_col, metric_stub) {
  df <- plt_long[!is.na(plt_long[[value_col]]), , drop = FALSE]
  # plt_long.rds was cached before the Lakes/Rock & Ice biome exclusion; confirmed
  # (2026-07-28) that every fid absent from geom_sf is exactly one of those excluded
  # cells (30,147 = 10,370 Lakes + 19,777 Rock & Ice, verified against the unfiltered
  # 10k_change_calc.gpkg -- zero truly-unexplained mismatches), so filtering here is
  # safe and matches the pipeline's own established exclusion, not a data gap.
  n_before <- length(unique(df$fid))
  df <- df[df$fid %in% geom_sf$fid, , drop = FALSE]
  n_dropped <- n_before - length(unique(df$fid))
  if (n_dropped > 0) message(sprintf("  (dropped %d fids not in geom_sf -- expected: Lakes/Rock & Ice exclusion)", n_dropped))
  n_total <- nrow(df)
  stopifnot("fid" %in% names(df))

  hs <- extract_hotspots(
    df             = df,
    value_col      = value_col,
    pct_cutoff     = HOTS_CFG_5$pct_cutoff,
    threshold_mode = HOTS_CFG_5$threshold_mode,
    rule_mode      = HOTS_CFG_5$rule_mode,
    loss_services  = HOTS_CFG_5$loss,
    gain_services  = HOTS_CFG_5$gain,
    combos         = HOTS_CFG_5$combos,
    id_cols        = c("fid", intersect(names(df), c("c_fid","iso3","continent","region_un","subregion","nev_name","region_wb","income_grp","WWF_biome"))),
    sf_obj         = geom_sf,
    write_sf_path  = NULL,
    clean_names    = TRUE
  )

  stopifnot(!is.null(hs$hotspots_sf), nrow(hs$hotspots_sf) > 0)

  # Derived cross-category column: the 3rd overlap category Becky asked for --
  # at least one water-service hotspot AND at least one access-service hotspot
  # in the same cell. Not natively produced by the combo mechanism (which only
  # counts membership within one combo, not an AND across two).
  sf_out <- hs$hotspots_sf %>%
    mutate(combined_cross = as.integer(count_water > 0 & count_access > 0))

  n_hot <- nrow(sf_out)
  n_water <- sum(sf_out$count_water > 0)
  n_access <- sum(sf_out$count_access > 0)
  n_cross <- sum(sf_out$combined_cross > 0)
  message(sprintf("[%s] n_hot=%d (%.2f%% of %d) | water=%d | access=%d | combined_cross=%d",
                   metric_stub, n_hot, 100*n_hot/n_total, n_total, n_water, n_access, n_cross))

  folder <- file.path(HOTS_CFG_5$out_dir, metric_stub, "global")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  out_gpkg <- file.path(folder, sprintf("hotspots_global_5service_%s.gpkg", metric_stub))

  sf_to_write <- sf_out
  if ("fid" %in% names(sf_to_write)) sf_to_write <- dplyr::rename(sf_to_write, grid_fid = fid)
  sf::st_write(sf_to_write, out_gpkg, quiet = TRUE, delete_dsn = TRUE)
  message("Wrote: ", out_gpkg)

  tibble::tibble(
    metric = metric_stub, n_hot = n_hot, n_total = n_total,
    pct_hot = 100*n_hot/n_total, n_water = n_water, n_access = n_access,
    n_combined_cross = n_cross, gpkg = out_gpkg
  )
}

message("\n=== Running pct_chg (primary, canonical metric) ===")
res_pct <- run_one_5service("pct_chg", "pct")

message("\n=== Running abs_chg (secondary metric, for completeness) ===")
res_abs <- run_one_5service("abs_chg", "abs")

index <- dplyr::bind_rows(res_pct, res_abs)
write.csv(index, file.path(HOTS_CFG_5$out_dir, "_hotspots_5service_index.csv"), row.names = FALSE)
message("\nIndex written: ", file.path(HOTS_CFG_5$out_dir, "_hotspots_5service_index.csv"))
print(index)
