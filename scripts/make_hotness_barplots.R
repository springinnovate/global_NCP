library(sf)
library(ggplot2)
library(dplyr)
library(here)

out_dir <- here("outputs", "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

generate_hotness_bars <- function(gpkg_path) {
  if(!file.exists(gpkg_path)) {
    message("WARNING: File not found: ", gpkg_path)
    return(NULL)
  }
  
  message("Processing hotness bars...")
  hotspots <- st_read(gpkg_path, quiet = TRUE) %>% st_drop_geometry()
  
  exclude_regions <- c("Seven seas (Open Ocean)", "Seven seas (open ocean)", "Antarctica", "Lakes", "Rock & Ice")
  valid_groupings <- c("income_grp", "region_wb", "WWF_biome")
  
  for (g_var in valid_groupings) {
    if (g_var %in% names(hotspots)) {
      d_summ <- hotspots %>%
        filter(!is.na(.data[[g_var]])) %>%
        filter(!.data[[g_var]] %in% exclude_regions) %>%
        filter(as.numeric(hotspot_count) > 0) %>%
        mutate(
          hotspots_capped = pmin(as.numeric(hotspot_count), 4),
          cat = factor(hotspots_capped, levels = 1:4, labels = c("1", "2", "3", "4+"))
        ) %>%
        count(group = .data[[g_var]], cat) %>%
        group_by(group) %>%
        mutate(pct = n / sum(n)) %>%
        ungroup()

      top_groups <- d_summ %>% filter(cat %in% c("3", "4+")) %>% group_by(group) %>% summarise(pct = sum(pct)) %>% arrange(desc(pct)) %>% slice_head(n = 15) %>% pull(group)
      if (length(unique(d_summ$group)) > 20 && length(top_groups) < 15) {
          top_groups <- c(top_groups, head(setdiff(unique(d_summ$group), top_groups), 15 - length(top_groups)))
      }
      d_summ <- d_summ %>% filter(group %in% top_groups) %>% mutate(group = factor(group, levels = rev(sort(unique(as.character(group))))))

      p <- ggplot(d_summ, aes(x = group, y = pct, fill = cat)) + geom_col(position = "fill") +
        scale_fill_manual(name = "Overlapping\nHotspots", values = c("1" = "#FFD54F", "2" = "#FB8C00", "3" = "#E53935", "4+" = "#800026")) +
        coord_flip() + labs(title = paste0("Multi-service Hotspot Distribution by ", g_var), subtitle = "Share of hotspot gridcells by number of overlapping services", x = NULL, y = "Proportion of hotspot gridcells") + theme_minimal()
      
      out_file <- file.path(out_dir, paste0("hotness_dist_", tolower(g_var), ".png"))
      ggsave(out_file, p, width = 10, height = 6, bg = "white")
      message("Saved: ", out_file)
    }
  }
}
generate_hotness_bars(file.path("home", "jeronimo", "data", "global_ncp", "processed", "hotspots", "pct", "global", "hotspots_global_pct.gpkg"))