# Hotspot relative-prevalence bars and within-hotspot SPC boxplots, by biome, region and income
# group, in the same style as the change figures (make_combined_diffs.R): group names on the axis
# (no numbered key), full service names, WWF orange instead of red, compact sizes.
# Same data and rules as hotspot_synthesis.qmd (relative intensity, from hotspot_area_stats.csv)
# and make_hotspot_boxplots.R (top/bottom 5% per service within cells that have the grouping);
# only the presentation changes.
#
# Usage: Rscript scripts/mapping/make_hotspot_summary_figures.R [out_root]
#   default out_root: outputs/plots  (writes intensity/ and boxplots_unified/<grouping>/)

suppressMessages({library(dplyr); library(readr); library(ggplot2); library(here)})
setwd(here())

args <- commandArgs(trailingOnly = TRUE)
out_root <- if (length(args)) args[1] else "outputs/plots"

orange <- "#F07D00"
services <- c(N_export = "Nitrogen export", Sed_export = "Sediment export", C_Risk = "Coastal risk",
              Pollination = "Pollination", Nature_Access = "Nature access")
loss_services <- c("Pollination", "Nature_Access")   # hotspot = steepest decline; others steepest increase
income_order <- c("High income: OECD", "Upper middle income", "Lower middle income", "Low income")

groupings <- list(
  WWF_biome  = list(file = "wwf_biome",  title = "Biome"),
  region_wb  = list(file = "region_wb",  title = "World Bank region"),
  income_grp = list(file = "income_grp", title = "Income group")
)

tidy_group <- function(x, g) {
  x <- if (g == "income_grp") sub("^[0-9]\\. ", "", x) else x
  lev <- if (g == "income_grp") rev(income_order) else sort(unique(x), decreasing = TRUE)
  factor(x, levels = lev)
}

base_theme <- theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 10),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        axis.text = element_text(size = 8))

fig_height <- function(n_groups) 1.0 + 0.22 * n_groups

# --- relative prevalence ------------------------------------------------------------------------
# Land-based services by biome, region and income group; coastal risk in its own figure by region and
# income group only (its denominator is coastal cells, and a coastal cell's biome is just the land
# biome behind that stretch of coast, which says little about coastal risk).
land_services <- setdiff(names(services), "C_Risk")
stats <- read_csv("data/processed/tables/hotspot_area_stats.csv", show_col_types = FALSE)
dir.create(file.path(out_root, "intensity"), recursive = TRUE, showWarnings = FALSE)
for (g in names(groupings)) {
  d <- stats |> filter(grouping_var == g, service %in% land_services) |>
    mutate(relative_intensity = coalesce(relative_intensity, 0),
           group = tidy_group(group, g),
           service = factor(services[service], levels = services),
           over = relative_intensity > 1)
  p <- ggplot(d, aes(relative_intensity, group, fill = over)) +
    geom_col(width = 0.75) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey30") +
    facet_wrap(~ service, nrow = 1, scales = "free_x") +
    scale_fill_manual(values = c(`TRUE` = orange, `FALSE` = "grey75"), guide = "none") +
    scale_x_continuous(n.breaks = 4) +
    labs(x = "Relative prevalence (share of hotspots / share of land area)", y = NULL) +
    base_theme
  out <- file.path(out_root, "intensity", paste0("hotspot_relative_intensity_", groupings[[g]]$file, ".png"))
  ggsave(out, p, width = 11, height = fig_height(nlevels(d$group)), dpi = 300, bg = "white")
  message("wrote ", out)
}

coastal <- stats |> filter(service == "C_Risk", grouping_var %in% c("region_wb", "income_grp")) |>
  mutate(panel = factor(ifelse(grouping_var == "region_wb", "World Bank region", "Income group"),
                        levels = c("World Bank region", "Income group")),
         group = ifelse(grouping_var == "income_grp", sub("^[0-9]\\. ", "", group), group),
         group = factor(group, levels = rev(c(sort(unique(group[grouping_var == "region_wb"])), income_order))),
         over = relative_intensity > 1)
p <- ggplot(coastal, aes(relative_intensity, group, fill = over)) +
  geom_col(width = 0.75) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey30") +
  facet_wrap(~ panel, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = c(`TRUE` = orange, `FALSE` = "grey75"), guide = "none") +
  scale_x_continuous(breaks = seq(0, 2, 0.5)) +
  labs(x = "Relative prevalence of coastal risk hotspots (share of hotspots / share of coastal cells)", y = NULL) +
  base_theme
out <- file.path(out_root, "intensity", "hotspot_relative_intensity_coastal_risk.png")
ggsave(out, p, width = 8, height = 2.6, dpi = 300, bg = "white")
message("wrote ", out)

# --- SPC within hotspot cells -------------------------------------------------------------------
plt_long <- readRDS("data/processed/plt_long.rds") |>
  filter(service %in% land_services) |>
  select(service, pct_chg, all_of(names(groupings)))
for (g in names(groupings)) {
  hot <- plt_long |>
    filter(!is.na(.data[[g]]), !(g == "WWF_biome" & .data[[g]] %in% c("Lakes", "Rock & Ice"))) |>
    group_by(service) |>
    mutate(loss = service %in% loss_services,
           cut = if_else(loss, quantile(pct_chg, 0.05, na.rm = TRUE), quantile(pct_chg, 0.95, na.rm = TRUE)),
           hot = if_else(loss, pct_chg <= cut, pct_chg >= cut)) |>
    ungroup() |> filter(hot)
  if (g == "income_grp") hot <- filter(hot, .data[[g]] != "2. High income: nonOECD")
  s <- hot |> group_by(service, group = .data[[g]]) |>
    summarise(middle = median(pct_chg), lower = quantile(pct_chg, .25), upper = quantile(pct_chg, .75),
              iqr = IQR(pct_chg), ymin = max(min(pct_chg), lower - 1.5 * iqr),
              ymax = min(max(pct_chg), upper + 1.5 * iqr), .groups = "drop") |>
    group_by(service) |> mutate(intensity = scales::rescale(abs(middle))) |> ungroup() |>
    mutate(group = tidy_group(group, g), service = factor(services[service], levels = services))
  p <- ggplot(s, aes(y = group, xmin = ymin, xlower = lower, xmiddle = middle, xupper = upper, xmax = ymax)) +
    geom_boxplot(aes(fill = intensity), stat = "identity", orientation = "y", colour = "grey25",
                 linewidth = 0.3, width = 0.7) +
    facet_wrap(~ service, nrow = 1, scales = "free_x") +
    scale_x_continuous(n.breaks = 4) +
    scale_fill_gradient(low = "#FDEEDC", high = "#B85500", guide = "none") +
    labs(x = "SPC within hotspot cells (%)", y = NULL) +
    base_theme
  dir.create(file.path(out_root, "boxplots_unified", g), recursive = TRUE, showWarnings = FALSE)
  out <- file.path(out_root, "boxplots_unified", g, "boxplots_pct.png")
  ggsave(out, p, width = 11, height = fig_height(nlevels(s$group)), dpi = 300, bg = "white")
  message("wrote ", out)
}

# --- coastal risk: SPC within its hotspot cells, by region and income group ----------------------
# Production hotspot flags (same set as the prevalence table), SPC from the 10 km grid.
suppressMessages(library(sf))
c_hot <- st_read("data/processed/hotspots/pct/global/hotspots_global_pct.gpkg",
                 query = 'SELECT grid_fid FROM "hotspots_global_pct" WHERE C_Risk = 1', quiet = TRUE) |>
  st_drop_geometry()
c_spc <- st_read("data/processed/10k_change_calc.gpkg",
                 query = 'SELECT grid_fid, region_wb, income_grp, c_risk_pct_chg FROM "10k_change_calc" WHERE c_risk_pct_chg IS NOT NULL',
                 quiet = TRUE) |>
  st_drop_geometry() |> filter(grid_fid %in% c_hot$grid_fid)
cs <- bind_rows(
  c_spc |> filter(!is.na(region_wb)) |> transmute(panel = "World Bank region", group = region_wb, v = c_risk_pct_chg),
  c_spc |> filter(!is.na(income_grp), income_grp != "2. High income: nonOECD") |>
    transmute(panel = "Income group", group = sub("^[0-9]\\. ", "", income_grp), v = c_risk_pct_chg)) |>
  group_by(panel, group) |>
  summarise(n = n(), middle = median(v), lower = quantile(v, .25), upper = quantile(v, .75), iqr = IQR(v),
            ymin = max(min(v), lower - 1.5 * iqr), ymax = min(max(v), upper + 1.5 * iqr), .groups = "drop") |>
  group_by(panel) |> mutate(intensity = scales::rescale(abs(middle))) |> ungroup() |>
  mutate(panel = factor(panel, levels = c("World Bank region", "Income group")),
         group = factor(group, levels = rev(c(sort(unique(group[panel == "World Bank region"])), income_order))))
p <- ggplot(cs, aes(y = group, xmin = ymin, xlower = lower, xmiddle = middle, xupper = upper, xmax = ymax)) +
  geom_boxplot(aes(fill = intensity), stat = "identity", orientation = "y", colour = "grey25",
               linewidth = 0.3, width = 0.7) +
  facet_wrap(~ panel, nrow = 1, scales = "free_y") +
  scale_fill_gradient(low = "#FDEEDC", high = "#B85500", guide = "none") +
  labs(x = "SPC of coastal risk within its hotspot cells (%)", y = NULL) +
  base_theme
dir.create(file.path(out_root, "boxplots_unified", "coastal_risk"), recursive = TRUE, showWarnings = FALSE)
out <- file.path(out_root, "boxplots_unified", "coastal_risk", "boxplots_pct.png")
ggsave(out, p, width = 8, height = 2.6, dpi = 300, bg = "white")
message("wrote ", out)
print(cs |> select(panel, group, n, middle))
