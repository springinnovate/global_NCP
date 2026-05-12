suppressPackageStartupMessages({
library(dplyr);
  library(tidyr);
  library(ggplot2); 
  library(viridisLite)
})
if (!requireNamespace("hexbin", quietly = TRUE)) install.packages("hexbin")

socio_labels <- c(
  "GHS_BUILT_S_E2020_mean" = "Built Area",
  "fields_mehrabi_2017_mean" = "Field Size",
  "hdi_raster_predictions_2020_mean" = "HDI",
  "rast_adm1_gini_disp_2020_mean" = "Income Inequality",
  "rast_gdpTot_1990_2020_30arcsec_2020_sum" = "GDP (Total)",
  "GHS_POP_E2020_GLOBE_sum" = "Population (GHS)",
  "GlobPOP_Count_30arc_2020_sum" = "Population (Global)"
)

plt_long_socio <- plt_long %>%
  tidyr::pivot_longer(cols = all_of(socio_vars),
                      names_to = "socio_var", values_to = "socio_val") %>%
  dplyr::mutate(
    pct_chg   = as.numeric(pct_chg),
    socio_val = as.numeric(socio_val)
  ) %>%
  dplyr::filter(is.finite(pct_chg), is.finite(socio_val)) %>%
  dplyr::group_by(service) %>%
  dplyr::mutate(
    pct_low  = quantile(pct_chg, 0.01, na.rm = TRUE),
    pct_high = quantile(pct_chg, 0.99, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(pct_chg >= pct_low, pct_chg <= pct_high)

out_dir <- file.path("outputs", "plots", "hex")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  gsub("_+", "_", x)
}

services <- sort(unique(plt_long_socio$service))

# Open a multi-page PDF once (Cairo, white background)
cairo_pdf(file.path(out_dir, "hex_all_services.pdf"), width = 10, height = 6, onefile = TRUE, bg = "white")
on.exit(dev.off(), add = TRUE)

for (svc in services) {
  plot_data <- plt_long_socio %>%
    dplyr::filter(service == svc) %>%
    dplyr::mutate(socio_label = socio_labels[as.character(socio_var)]) %>%
    dplyr::group_by(socio_label) %>%
    dplyr::filter(
      dplyr::n() > 1,
      dplyr::n_distinct(pct_chg)   > 1,
      dplyr::n_distinct(socio_val) > 1
    ) %>%
    dplyr::ungroup()

  if (nrow(plot_data) == 0) next

  p <- ggplot(plot_data, aes(x = pct_chg, y = socio_val)) +
    geom_hex(bins = 60, na.rm = TRUE) +
    scale_fill_viridis_c(option = "D", direction = 1) +
    facet_wrap(~ socio_label, scales = "free_y", nrow = 2) +
    labs(
      title = paste("Ecosystem Service:", svc),
      x = "Ecosystem Service % Change",
      y = "Socioeconomic Variable Value",
      fill = "Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5),
      axis.text  = element_text(size = 9)
    )

  # write a page into the PDF
  print(p)

  # also save a PNG with white background
  png_file <- file.path(out_dir, paste0("hex_", safe_name(svc), ".png"))
  ggsave(filename = png_file, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
}

message("Saved PNGs and multi-page PDF to: ", out_dir)
