# ==============================================================================
# Audit: Where do hotspot cells actually concentrate, geographically?
# ==============================================================================
#
# Built 2026-08-20 after a real near-miss: the Sandra Valenzuela deck claimed
# Colombia's change hotspots concentrate in "el eje cafetero, los Andes
# centrales y el piedemonte de la Orinoquía" -- a claim nobody had actually
# verified against the data. A user's own on-the-ground GIS read caught it as
# probably wrong. This script is the fix: never eyeball a hotspot map and
# assert where it's "obviously" concentrated -- check it.
#
# What this does, in order (each step exists because an earlier step in this
# session's real investigation turned out to be insufficient on its own):
#   1. Raw cell counts by admin-1 (department/state) -- the first, cheapest
#      check. NOT sufficient alone: large admin units generically contain
#      more cells, so raw count conflates "concentrated" with "big."
#   2. Same counts NORMALIZED by each unit's own total cell count (a rate,
#      not a count) -- corrects the size-artifact problem. If a unit still
#      ranks high on the rate, the raw-count finding wasn't just size.
#   3. Point-in-polygon join (st_intersects), not nearest-feature --
#      nearest-feature assigns border-adjacent points to the "closest"
#      polygon even when a properly-drawn boundary would place them
#      elsewhere. Re-run both ways if results look surprising; they should
#      match (they did here) or you have a data problem worth chasing.
#   4. K-means clustering on the highest-concentration cells, to check
#      whether a real geographic pattern (e.g. "two dominant clusters") is
#      being obscured by administrative labels. In this session, "Meta" as
#      a department looked like a distinct third concentration zone, but
#      clustering showed most of its hotspot cells actually belong to the
#      same broad southern cluster as neighboring Caquetá -- the department
#      boundary was hiding the real spatial pattern, not revealing it.
#   5. If a specific study-area/project polygon exists (not just an admin
#      boundary), check cells against THAT directly. Administrative-unit
#      membership is not the same as membership in a specific study area --
#      a department can be huge and mostly outside the area anyone cares
#      about. This is what caught the final, much smaller, correct number
#      in the case that motivated this script.
#
# Usage: edit the CONFIG block below for a new country/region/study-area,
# or source individual sections interactively. Requires `rnaturalearthhires`
# for admin-1 boundaries (install.packages("rnaturalearthhires",
# repos = "https://ropensci.r-universe.dev") if missing).
# ==============================================================================

library(sf)
library(dplyr)

# ------------------------------------------------------------------------------
# CONFIG -- edit per use
# ------------------------------------------------------------------------------

country_name   <- "Colombia"
hotspot_gpkg   <- here::here("data", "processed", "hotspots", "pct", "nev_name",
                              sprintf("hotspots_nev_name_%s_pct.gpkg", country_name))
headline_svcs  <- c("Pollination", "Sed_export", "N_export", "Nature_Access", "C_Risk")
grid_gpkg      <- here::here("data", "processed", "10k_change_calc.gpkg")
nev_name_field <- "nev_name"  # column in grid_gpkg matching country_name

# Optional: a specific study-area polygon to check membership against,
# separate from admin boundaries (leave NULL to skip step 5).
study_area_path <- "C:/projects/LC_orinoquia/vectors/msk_pm_crs.geojson"
study_area_label <- "Piedmont/Altillanura (LC_orinoquia dissertation study area)"

high_concentration_threshold <- 3  # of length(headline_svcs); cells >= this = "high concentration"

# ------------------------------------------------------------------------------
# 1-2. Load hotspot cells + admin boundaries, raw count AND normalized rate
# ------------------------------------------------------------------------------

hs <- st_read(hotspot_gpkg, quiet = TRUE) %>%
  mutate(hotspot_count = rowSums(across(all_of(headline_svcs)), na.rm = TRUE)) %>%
  filter(hotspot_count > 0) %>%
  st_transform(4326) %>% st_make_valid()

deps <- rnaturalearth::ne_states(country = country_name, returnclass = "sf") %>%
  st_transform(4326) %>% st_make_valid()

grid_full <- st_read(grid_gpkg, quiet = TRUE)
grid_country <- grid_full %>% filter(.data[[nev_name_field]] == country_name) %>%
  st_transform(4326) %>% st_make_valid()

# ------------------------------------------------------------------------------
# 3. Point-in-polygon join (both hotspot cells and full grid, for the rate)
# ------------------------------------------------------------------------------

cent_hs <- st_centroid(st_geometry(hs))
pip_hs <- st_join(st_sf(id = 1:nrow(hs), geometry = cent_hs, crs = 4326),
                   deps[, "name"], join = st_intersects, left = TRUE) %>%
  st_drop_geometry() %>% group_by(id) %>% slice(1) %>% ungroup()
hs$dept <- pip_hs$name[match(1:nrow(hs), pip_hs$id)]
hs$lon <- st_coordinates(cent_hs)[, 1]
hs$lat <- st_coordinates(cent_hs)[, 2]

cent_grid <- st_centroid(st_geometry(grid_country))
pip_grid <- st_join(st_sf(id = 1:nrow(grid_country), geometry = cent_grid, crs = 4326),
                     deps[, "name"], join = st_intersects, left = TRUE) %>%
  st_drop_geometry() %>% group_by(id) %>% slice(1) %>% ungroup()
grid_country$dept <- pip_grid$name[match(1:nrow(grid_country), pip_grid$id)]

cat(sprintf("Total %s hotspot cells (>=1 of %d services): %d\n",
            country_name, length(headline_svcs), nrow(hs)))
cat(sprintf("Cells with no department match (coastline/boundary gaps): %d\n\n",
            sum(is.na(hs$dept))))

by_dept <- grid_country %>% st_drop_geometry() %>% count(dept, name = "total_cells") %>%
  left_join(hs %>% st_drop_geometry() %>% count(dept, name = "hotspot_cells"), by = "dept") %>%
  mutate(hotspot_cells = coalesce(hotspot_cells, 0L),
         pct_of_dept_area = round(100 * hotspot_cells / total_cells, 2)) %>%
  filter(total_cells >= 20)  # drop tiny slivers where the rate is noisy

cat("=== By raw hotspot cell count (can conflate 'concentrated' with 'big') ===\n")
print(as.data.frame(head(arrange(by_dept, desc(hotspot_cells)), 10)))

cat("\n=== By RATE (hotspot cells / department's own total cells) -- corrects for size ===\n")
print(as.data.frame(head(arrange(by_dept, desc(pct_of_dept_area)), 10)))

# ------------------------------------------------------------------------------
# 4. K-means on high-concentration cells -- how many real clusters?
# ------------------------------------------------------------------------------

hs_high <- hs %>% st_drop_geometry() %>% filter(hotspot_count >= high_concentration_threshold)
cat(sprintf("\n=== K-means clustering on %d high-concentration cells (>=%d/%d services) ===\n",
            nrow(hs_high), high_concentration_threshold, length(headline_svcs)))
for (k in 2:3) {
  km <- kmeans(hs_high[, c("lon", "lat")], centers = k, nstart = 10)
  cat(sprintf("\n-- k = %d --\n", k))
  print(km$centers)
  print(table(km$cluster))
}

# ------------------------------------------------------------------------------
# 5. Optional: check against a specific study-area polygon, not just admin units
# ------------------------------------------------------------------------------

if (!is.null(study_area_path) && file.exists(study_area_path)) {
  sa <- st_read(study_area_path, quiet = TRUE) %>% st_transform(4326) %>%
    st_make_valid() %>% st_union()

  inside_hs <- st_intersects(cent_hs, sa, sparse = FALSE)[, 1]
  inside_grid <- st_intersects(cent_grid, sa, sparse = FALSE)[, 1]

  cat(sprintf("\n=== Against study-area polygon: %s ===\n", study_area_label))
  cat(sprintf("Hotspot cells inside polygon: %d (%.2f%% of national total)\n",
              sum(inside_hs), 100 * sum(inside_hs) / nrow(hs)))
  cat(sprintf("Total grid cells inside polygon: %d\n", sum(inside_grid)))
  cat(sprintf("Hotspot rate inside polygon: %.1f%% (vs %.1f%% nationally)\n",
              100 * sum(inside_hs) / sum(inside_grid),
              100 * nrow(hs) / nrow(grid_country)))
} else if (!is.null(study_area_path)) {
  cat(sprintf("\n[SKIPPED] Study-area polygon not found at: %s\n", study_area_path))
}
