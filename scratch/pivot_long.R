# One-time: rebuild plt_long and save to disk for reuse (avoids heavy pivot in QMDs).
library(sf)
library(dplyr)
library(tidyr)
library(stringr)

gpkg <- file.path(Sys.getenv("GLOBAL_NCP_DATA"), "processed", "10k_change_calc.gpkg")
stopifnot(file.exists(gpkg))

service_lookup <- c(
  sed_export    = "Sed_export",
  n_export      = "N_export",
  n_retention   = "N_retention",
  nature_access = "Nature_Access",
  pollination   = "Pollination",
  usle          = "USLE",
  n_ret_ratio   = "N_Ret_Ratio",
  sed_ret_ratio = "Sed_Ret_Ratio",
  Rt_ratio      = "C_Risk_Red_Ratio",
  Rt            = "C_Risk",
  Rt_service    = "C_Prot_service",
  Rt_nohab      = "Rt_nohab"
)

sf_f <- sf::st_read(gpkg, layer = "10k_change_calc", quiet = TRUE)
if (!"fid" %in% names(sf_f)) sf_f$fid <- seq_len(nrow(sf_f))
if (!"c_fid" %in% names(sf_f)) {
  if ("c_fid.x" %in% names(sf_f)) sf_f <- dplyr::rename(sf_f, c_fid = c_fid.x)
  else if ("c_fid.y" %in% names(sf_f)) sf_f <- dplyr::rename(sf_f, c_fid = c_fid.y)
  else if ("id" %in% names(sf_f))      sf_f <- dplyr::rename(sf_f, c_fid = id)
}
sf_f <- dplyr::select(sf_f, -dplyr::any_of(c("c_fid.x","c_fid.y")))

plt <- sf::st_drop_geometry(sf_f)
chg_cols <- grep("_(abs|pct)_chg$", names(plt), value = TRUE)
socio_vars <- setdiff(names(plt), c("fid","c_fid", chg_cols))

plt_long <- plt |>
  tidyr::pivot_longer(all_of(chg_cols), names_to = c("service","chg_type"),
                      names_pattern = "^(.*)_(abs|pct)_chg$", values_to = "chg_value") |>
  dplyr::mutate(service = stringr::str_remove(service, "_mean$")) |>
  tidyr::pivot_wider(names_from = chg_type, values_from = chg_value) |>
  dplyr::rename(abs_chg = abs, pct_chg = pct) |>
  dplyr::select(fid, c_fid, service, abs_chg, pct_chg, dplyr::any_of(socio_vars)) |>
  dplyr::filter(!is.na(c_fid), !is.infinite(abs_chg), !is.infinite(pct_chg)) |>
  dplyr::filter(!is.na(abs_chg) | !is.na(pct_chg)) |>
  dplyr::mutate(service = dplyr::recode(service, !!!service_lookup, .default = service))

saveRDS(plt_long, "outputs/tables/plt_long.rds")
# Optional: feather/parquet for Python
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_feather(plt_long, "outputs/tables/plt_long.feather")
}
message("Finished writing outputs/tables/plt_long.rds", if (requireNamespace("arrow", quietly = TRUE)) " and plt_long.feather" else "")
