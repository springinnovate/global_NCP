#!/usr/bin/env Rscript
# Standalone script to rebuild signed barplots with a dashed global reference line.
# Avoids giant pivot_longer by looping over services directly.

library(sf)
library(dplyr)
library(ggplot2)

message("Starting signed bar generation (no long pivot)...")

gpkg <- file.path(Sys.getenv("GLOBAL_NCP_DATA"), "processed", "10k_change_calc.gpkg")
stopifnot(file.exists(gpkg))

groupings <- c("income_grp","region_wb","continent","region_un","WWF_biome")
metrics   <- c("pct","abs")
cut_q     <- 0.999
svc_order <- c("C_Risk","N_export","Sed_export",
               "C_Risk_Red_Ratio","N_Ret_Ratio","Sed_Ret_Ratio",
               "Pollination","Nature_Access")
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

sf_f <- st_read(gpkg, layer = "10k_change_calc", quiet = TRUE)
if (!"fid" %in% names(sf_f)) sf_f$fid <- seq_len(nrow(sf_f))
if (!"c_fid" %in% names(sf_f)) {
  if ("c_fid.x" %in% names(sf_f)) sf_f <- rename(sf_f, c_fid = c_fid.x)
  else if ("c_fid.y" %in% names(sf_f)) sf_f <- rename(sf_f, c_fid = c_fid.y)
  else if ("id" %in% names(sf_f))      sf_f <- rename(sf_f, c_fid = id)
}
sf_f <- select(sf_f, -any_of(c("c_fid.x","c_fid.y")))

# service names inferred from *_chg columns
chg_cols <- grep("_(abs|pct)_chg$", names(sf_f), value = TRUE)
services_raw <- unique(sub("_(abs|pct)_chg$", "", chg_cols))
services_clean <- stringr::str_remove(services_raw, "_mean$")
svc_map <- tibble(raw = services_clean,
                  canonical = dplyr::recode(services_clean, !!!service_lookup, .default = services_clean))
# keep only canonical services in the specified order
canonical_services <- svc_order[svc_order %in% svc_map$canonical]
svc_map <- dplyr::filter(svc_map, canonical %in% canonical_services)
if (!length(canonical_services)) {
  stop("No services from svc_order found in change columns; cannot plot signed bars.")
}

# keep only grouping columns that exist
groupings <- intersect(groupings, names(sf_f))
if (!length(groupings)) {
  stop("No grouping columns found in data; cannot plot signed bars.")
}

plot_one_group <- function(df, group_col, metric, svc_map, canonical_services, cut_q, out_dir) {
  stopifnot(group_col %in% names(df))
  vals <- list(); glob_refs <- list()
  v_suffix <- if (metric == "pct") "pct_chg" else "abs_chg"

  for (svc in canonical_services) {
    raw <- svc_map$raw[match(svc, svc_map$canonical)]
    col <- paste0(raw, "_", v_suffix)
    if (!col %in% names(df)) next
    v <- df[[col]]
    # trim per service
    cap <- quantile(abs(v), cut_q, na.rm = TRUE)
    v_trim <- pmax(pmin(v, cap), -cap)
    # group mean
    grp_mean <- tapply(v_trim, df[[group_col]], function(x) mean(x, na.rm = TRUE))
    grp_df <- tibble(service = svc, !!group_col := names(grp_mean), val = as.numeric(grp_mean))
    grp_df <- filter(grp_df, .data[[group_col]] != "Global")
    vals[[length(vals) + 1]] <- grp_df
    glob_refs[[length(glob_refs) + 1]] <- tibble(service = svc, ref = mean(v_trim, na.rm = TRUE))
  }

  df_trim <- bind_rows(vals)
  if (!nrow(df_trim)) {
    message("No data for ", group_col, " (", metric, "); skipping.")
    return(invisible(NULL))
  }

  glob_ref <- bind_rows(glob_refs)
  df_trim$service <- factor(df_trim$service, levels = canonical_services)
  glob_ref$service <- factor(glob_ref$service, levels = canonical_services)
  df_trim[[group_col]] <- factor(df_trim[[group_col]], levels = sort(unique(df_trim[[group_col]])))

  p <- ggplot(df_trim, aes(x = .data[[group_col]], y = val, fill = .data[[group_col]])) +
    geom_col(show.legend = FALSE) +
    geom_hline(yintercept = 0, color = "#d9d9d9") +
    geom_hline(data = glob_ref, aes(yintercept = ref),
               linetype = "dashed", color = "black", linewidth = 0.8) +
    facet_wrap(~ service, ncol = 3, scales = "free_y") +
    labs(x = group_col,
         y = if (metric == "pct") "Mean % change (trimmed, signed)" else "Mean absolute change (trimmed, signed)",
         title = paste0("Signed mean change by ", group_col, " (global dashed ref)")) +
    theme_minimal(base_size = 12) +
    theme(strip.background = element_rect(fill = "#f3f4f6", color = NA),
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

  dir.create(file.path(out_dir, paste0("signed_", metric), tolower(group_col)), recursive = TRUE, showWarnings = FALSE)
  final_path <- file.path(out_dir, paste0("signed_", metric), tolower(group_col),
                          paste0("bars_signedline_", tolower(group_col), "_", metric, ".png"))
  ggsave(final_path, p, width = 12, height = 8, dpi = 300, bg = "white")
  message("Saved: ", final_path)
}

for (gc in groupings) {
  for (m in metrics) {
    plot_one_group(sf_f, group_col = gc, metric = m,
                   svc_map = svc_map,
                   canonical_services = canonical_services,
                   cut_q = cut_q, out_dir = "outputs/plots")
  }
}

message("Done.")
