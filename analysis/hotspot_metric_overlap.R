# Sensitivity of the SPC hotspot rule to the change metric (@tbl-hotspot-metric-overlap).
# Compares the production per-service hotspot flags defined by SPC (hotspots/pct) and by absolute
# change (hotspots/abs), and reports how far SPC hotspot baselines sit from the typical cell.
# Values and baselines come from 10k_change_calc.gpkg, joined on grid_fid.
# Output: outputs/tables/hotspot_metric_overlap.csv

suppressMessages({library(sf); library(dplyr)})
setwd(here::here())

svc <- tibble::tribble(
  ~service,        ~spc,                    ~abs,                    ~base,
  "N_export",      "n_export_pct_chg",      "n_export_abs_chg",      "n_export_raw_1992",
  "Sed_export",    "sed_export_pct_chg",    "sed_export_abs_chg",    "sed_export_raw_1992",
  "C_Risk",        "c_risk_pct_chg",        "c_risk_abs_chg",        NA,
  "Pollination",   "pollination_pct_chg",   "pollination_abs_chg",   NA,
  "Nature_Access", "nature_access_pct_chg", "nature_access_abs_chg", NA
)

read_flags <- function(m) {
  st_read(sprintf("data/processed/hotspots/%s/global/hotspots_global_%s.gpkg", m, m),
          query = sprintf('SELECT grid_fid, %s FROM "hotspots_global_%s"',
                          paste(svc$service, collapse = ", "), m), quiet = TRUE) |>
    st_drop_geometry()
}
hot_pct <- read_flags("pct"); hot_abs <- read_flags("abs")

cols <- unique(na.omit(c(svc$spc, svc$abs, svc$base)))
chg <- st_read("data/processed/10k_change_calc.gpkg",
               query = sprintf('SELECT grid_fid, %s FROM "10k_change_calc" WHERE continent NOT IN (\'Antarctica\', \'Seven seas (open ocean)\', \'Seven seas (Open Ocean)\') AND WWF_biome NOT IN (\'Lakes\', \'Rock & Ice\')',
                               paste(cols, collapse = ", ")), quiet = TRUE) |>
  st_drop_geometry()

res <- lapply(seq_len(nrow(svc)), function(i) {
  s <- svc[i, ]
  ids_pct <- hot_pct$grid_fid[hot_pct[[s$service]] %in% 1]
  ids_abs <- hot_abs$grid_fid[hot_abs[[s$service]] %in% 1]
  spc <- chg[[s$spc]]; ab <- chg[[s$abs]]
  # baseline: 1992 value where available, else mean level (|S92|+|S20|)/2 = |abs| * 100 / |SPC|
  base <- if (!is.na(s$base)) chg[[s$base]] else ifelse(spc != 0, abs(ab) * 100 / abs(spc), NA)
  valid <- !is.na(base) & !is.na(spc) & spc != 0
  in_hot <- chg$grid_fid %in% ids_pct
  tibble(
    service = s$service,
    n_hot_spc = length(ids_pct),
    pct_also_abs = 100 * mean(ids_pct %in% ids_abs),
    pct_at_bound = 100 * mean(abs(spc[in_hot]) >= 199, na.rm = TRUE),
    median_base_ratio = median(base[in_hot & valid]) / median(base[valid])
  )
}) |> bind_rows()

print(res, width = Inf)
readr::write_csv(res, "outputs/tables/hotspot_metric_overlap.csv")
