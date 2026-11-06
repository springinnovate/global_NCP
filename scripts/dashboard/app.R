# Ecosystem service change explorer: maps of change 1992-2020 and the hotspot analysis for any
# country, region, income group or biome (2026-10-08). The change maps show change on the native pixels
# (300 m; 2 km for nature access) as pre-rendered tiles (scripts/dashboard/make_change_tiles.py, colours
# as paper Figure 1); hotspots work on the 10 km grid (Path B), prepared by prepare_dashboard_data.R.
# Land cover change 1992-2020 on the same grid comes from prepare_landcover_data.R (tab hidden without it).
#
# Run from the repo root:  Rscript -e "shiny::runApp('scripts/dashboard', launch.browser = TRUE)"
#
# Settings for a server (see scripts/dashboard/deploy/README.md); unset, the app reads the repo layout:
#   EXPLORER_DATA_DIR        folder with grid_dashboard.rds, coastal_points.rds, landcover_grid.rds, tiles/
#                            and path_a/*_map_data.csv
#   EXPLORER_TILE_URL        where browsers fetch the change-map tiles (default: served by the app at "tiles")
#   EXPLORER_ALLOW_DOWNLOAD  "false" hides the cell download button

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(leaflet); library(dplyr); library(ggplot2)
  library(patchwork)
})

deploy_dir <- Sys.getenv("EXPLORER_DATA_DIR")
data_file <- if (nzchar(deploy_dir)) file.path(deploy_dir, "grid_dashboard.rds") else
  file.path(here::here(), "data", "processed", "dashboard", "grid_dashboard.rds")
if (!file.exists(data_file)) stop(if (nzchar(deploy_dir)) paste("grid_dashboard.rds not found in", deploy_dir)
                                  else "Run scripts/dashboard/prepare_dashboard_data.R first")
grid <- readRDS(data_file)
tiles_dir <- file.path(dirname(data_file), "tiles")
if (dir.exists(tiles_dir)) addResourcePath("tiles", tiles_dir)
tile_url <- sub("/$", "", Sys.getenv("EXPLORER_TILE_URL", "tiles"))
allow_download <- !identical(tolower(Sys.getenv("EXPLORER_ALLOW_DOWNLOAD", "true")), "false")

# Change per unit from the 300 m / 2 km pixels (Path A), the same tables as the paper's change figures
pa_dir <- if (nzchar(deploy_dir)) file.path(deploy_dir, "path_a") else
  file.path(here::here(), "outputs", "plots", "output_plots_diff")
PA_FILE <- c(nev_name = "country", region_wb = "region_wb", income_grp = "income_grp", WWF_biome = "biome")
PA_SERV <- c(N_export = "n_export", Sed_export = "sed_export", C_Risk = "c_risk", Pollination = "pollination",
             Nature_Access = "nature_access")
path_a <- lapply(PA_FILE, function(f) {
  m <- read.csv(file.path(pa_dir, paste0(f, "_map_data.csv")), encoding = "UTF-8")
  names(m)[1] <- "unit"
  m$unit <- sub("^[0-9][.] ", "", m$unit)
  m <- m[m$service %in% names(PA_SERV), ]
  m$service <- unname(PA_SERV[m$service])
  m
})

SERV <- c(n_export = "Nitrogen export", sed_export = "Sediment export", c_risk = "Coastal risk",
          pollination = "Pollination", nature_access = "Nature access")
ADVERSE_UP <- c("n_export", "sed_export", "c_risk")    # an increase is adverse
UNIT_TYPES <- c("Country" = "nev_name", "World Bank region" = "region_wb", "Income group" = "income_grp",
                "Biome" = "WWF_biome", "Whole world" = "all")
SUBUNIT_OF <- c(nev_name = "WWF_biome", region_wb = "nev_name", income_grp = "nev_name",
                WWF_biome = "nev_name", all = "region_wb")
SUBUNIT_LABEL <- c(nev_name = "Country", region_wb = "World Bank region", income_grp = "Income group",
                   WWF_biome = "Biome")
DIRECTION_LABEL <- c(decline = "Decline", improvement = "Improvement")

# flags for one service: cells in the tail of the reference distribution, in the chosen direction
tail_flags <- function(v, ref, service, thr, kind) {
  up_is_tail <- (service %in% ADVERSE_UP) == (kind == "decline")
  cut <- quantile(ref, if (up_is_tail) 1 - thr else thr, na.rm = TRUE, names = FALSE)
  list(flag = !is.na(v) & (if (up_is_tail) v >= cut else v <= cut), cut = cut)
}

hotspots <- function(sel, services, thr, kinds, metric, reference) {
  out <- list(cells = sel, stats = list())
  for (k in kinds) {
    cnt <- integer(nrow(sel))
    for (s in services) {
      col <- paste0(s, "_", metric, "_chg")
      ref <- if (reference == "local") sel[[col]] else grid[[col]]
      tf <- tail_flags(sel[[col]], ref, s, thr, k)
      out$cells[[paste0(k, "_", s)]] <- tf$flag
      cnt <- cnt + tf$flag
      out$stats[[paste(k, s)]] <- data.frame(direction = k, service = s, cut = tf$cut)
    }
    out$cells[[paste0("n_", k)]] <- cnt
  }
  out
}

# ---- change maps: native-pixel tiles, symbology of paper Figure 1 ------------------------------------
# deepest tile level per service (about the native resolution); Leaflet enlarges tiles beyond it
TILE_MAXZOOM <- c(n_export = 9, sed_export = 9, pollination = 9, nature_access = 7)
MAP_SERV <- c(names(TILE_MAXZOOM), "c_risk")
# coastal risk: points along the coast (not tiles), those that changed by 1% or more, cached on first run
coastal_file <- file.path(dirname(data_file), "coastal_points.rds")
if (!file.exists(coastal_file) || !"d" %in% names(readRDS(coastal_file))) {
  pts <- sf::st_read(file.path(here::here(), "data", "coastal_risk_tnc_esa1992_2020_20251224_013825.gpkg"),
                     query = paste('SELECT Rt_1992, Rt_2020, geom FROM "coastal_risk_tnc_esa1992_2020_20251224_013825"',
                                   'WHERE Rt_1992 != Rt_2020'), quiet = TRUE)
  xy <- sf::st_coordinates(sf::st_transform(pts, 4326))
  spc <- (pts$Rt_2020 - pts$Rt_1992) / ((abs(pts$Rt_2020) + abs(pts$Rt_1992)) / 2) * 100
  keep <- abs(spc) >= 1
  saveRDS(data.frame(lon = xy[keep, 1], lat = xy[keep, 2], spc = spc[keep],
                     d = (pts$Rt_2020 - pts$Rt_1992)[keep]), coastal_file)
}
coastal <- readRDS(coastal_file)
COASTAL_LIM <- 14
COASTAL_PAL <- colorNumeric(c("#009191", "#ffffff", "#F07D00"), domain = c(-COASTAL_LIM, COASTAL_LIM), na.color = "#F07D00")
coastal$spc <- pmax(pmin(coastal$spc, COASTAL_LIM), -COASTAL_LIM)
COASTAL_ABS_LIM <- max(abs(quantile(coastal$d, c(0.01, 0.99), names = FALSE)))
COASTAL_ABS_PAL <- colorNumeric(c("#009191", "#ffffff", "#F07D00"), domain = c(-COASTAL_ABS_LIM, COASTAL_ABS_LIM),
                                na.color = "#F07D00")
coastal$d_clamped <- pmax(pmin(coastal$d, COASTAL_ABS_LIM), -COASTAL_ABS_LIM)
# ---- land cover change 1992-2020 on the 10 km grid (prepare_landcover_data.R); the tab is optional ----
lc_file <- file.path(dirname(data_file), "landcover_grid.rds")
lc <- if (file.exists(lc_file)) readRDS(lc_file) else NULL
if (!is.null(lc)) stopifnot(nrow(lc) == nrow(grid))
LC_CLASS <- c(natural = "Natural land", forest = "Forest", grassland = "Grassland and shrubland",
              cropland = "Cropland", urban = "Urban")
LC_VARS <- list(
  "Natural land" = c("Natural land: loss" = "natural_loss", "Natural land: gain" = "natural_gain",
                     "Natural land: net change" = "natural_net"),
  "Forest" = c("Forest: loss" = "forest_loss", "Forest: gain" = "forest_gain", "Forest: net change" = "forest_net"),
  "Grassland and shrubland" = c("Grassland and shrubland: loss" = "grassland_loss",
                                "Grassland and shrubland: gain" = "grassland_gain",
                                "Grassland and shrubland: net change" = "grassland_net"),
  "Expansion" = c("Cropland expansion" = "cropland_gain", "Urban expansion" = "urban_gain"))
LC_VAR_IDS <- unlist(LC_VARS, use.names = FALSE)
LC_VAR_LABEL <- setNames(unlist(lapply(LC_VARS, names), use.names = FALSE), LC_VAR_IDS)
# % of each cell for one variable ("<class>_<loss|gain|net>"); stored in tenths of a percent
lc_values <- function(rows, var) {
  k <- sub("_[a-z]+$", "", var)
  l <- lc[[paste0(k, "_l")]][rows]; g <- lc[[paste0(k, "_g")]][rows]
  switch(sub("^[a-z]+_", "", var), loss = l, gain = g, net = g - l) / 10
}
LC_INFO <- paste(
  "Land cover: ESA CCI (1992) and C3S (2020) maps at 300 m, grouped into 9 classes (flooded forest and",
  "mangroves count as forest). Each 10 km cell compares the two years pixel by pixel.\n\n",
  "- **Loss**: share of the cell that was the class in 1992 and is something else in 2020.\n",
  "- **Gain**: share that became the class.\n",
  "- **Net change**: gain minus loss. A small net change can hide a large turnover.\n\n",
  "**Natural land** is every class except cropland and urban: its loss is conversion to cropland or urban",
  "land, its gain the reverse (for example abandoned fields). **Forest** and **grassland and shrubland**",
  "are tracked on their own, because conversion between natural classes does not show in natural land.",
  "**Cropland and urban expansion**: the share that became cropland or urban from any other class.\n\n",
  "The charts average the selection's cells; the cells have equal area, so the averages are shares of",
  "its area. Transition metrics of Pontius and colleagues (R package diffeR), computed in",
  "`analysis/LC_change.qmd` and `analysis/LC_change_granular.qmd`.")
# land cover maps (make_landcover_tiles.py): 9 classes as in analysis/LC_change.qmd, and the main ESA classes
LC9_LEGEND <- c(Cropland = "#f6e05e", Forest = "#2d6a2e", `Grassland and shrubland` = "#d8b365",
                `Sparse vegetation` = "#c9b99a", Wetland = "#4fb3a9", Urban = "#c0392b", Bare = "#efe3c4",
                `Snow and ice` = "#bed7eb")
ESA_LEGEND <- c(`Cropland, rainfed` = "#ffff64", `Cropland, irrigated` = "#aaf0f0",
                `Mosaic cropland / natural vegetation` = "#dcf064", `Mosaic natural vegetation / cropland` = "#c8c864",
                `Tree cover, broadleaved, evergreen` = "#006400", `Tree cover, broadleaved, deciduous` = "#00a000",
                `Tree cover, needleleaved, evergreen` = "#003c00", `Tree cover, needleleaved, deciduous` = "#285000",
                `Tree cover, mixed leaf type` = "#788200", `Mosaic tree and shrub / herbaceous` = "#8ca000",
                `Mosaic herbaceous / tree and shrub` = "#be9600", Shrubland = "#966400", Grassland = "#ffb432",
                `Lichens and mosses` = "#ffdcd2", `Sparse vegetation` = "#ffebaf",
                `Tree cover, flooded, fresh or brackish` = "#00785a", `Tree cover, flooded, saline` = "#009678",
                `Shrub or herbaceous cover, flooded` = "#00dc82", Urban = "#c31400", Bare = "#fff5d7",
                `Permanent snow and ice` = "#ffffff")
LC_TILE_MAXZOOM <- 9
LC_COLOURS <- c(Forest = "#2d6a2e", `Grassland and shrubland` = "#d8b365", Cropland = "#f6e05e",
                Urban = "#c0392b", Other = "#bdbdbd")

# absolute change: limits and units per service, written by make_change_tiles.py
abs_limits <- function() {
  f <- file.path(tiles_dir, "abs_limits.csv")
  if (file.exists(f)) read.csv(f, stringsAsFactors = FALSE) else data.frame(service = character(), limit = numeric(), unit = character())
}
# one legend for all services: teal = favourable, orange = adverse, square-root stretch over 0-200%
# (tick positions = (1 +/- sqrt(v / 200)) / 2)
change_legend <- function() {
  ticks <- c(-200, -50, -10, 0, 10, 50, 200)
  pos <- (1 + sign(ticks) * sqrt(abs(ticks) / 200)) / 2 * 100
  HTML(paste0(
    '<div style="width:340px;font-size:12px;margin:4px auto 10px;">',
    '<div style="display:flex;justify-content:space-between;font-weight:bold;">',
    '<span style="color:#009191;">Favourable</span><span style="color:#F07D00;">Adverse</span></div>',
    '<div style="height:12px;border:1px solid #888;background:linear-gradient(to right,#009191,#ffffff,#F07D00);"></div>',
    '<div style="position:relative;height:16px;">',
    paste0(sprintf('<span style="position:absolute;left:%.1f%%;transform:translateX(-50%%);">%d</span>', pos, abs(ticks)), collapse = ""),
    '</div><div style="text-align:center;">Change 1992-2020 (SPC, %), native pixels</div></div>'))
}
# keep the maps of the "All services" view on the same view: move one and the others follow
SYNC_JS <- "function(el, x) {
  var m = this;
  window.esSync = (window.esSync || []).filter(function(o) { return document.body.contains(o.getContainer()); });
  window.esSync.push(m);
  // a re-rendered map is destroyed; drop it so the others stop moving it
  m.on('unload', function() { window.esSync = window.esSync.filter(function(o) { return o !== m; }); });
  m.on('move', function() {
    if (m._esLock) return;
    window.esSync.forEach(function(o) {
      if (o === m || !o._loaded || !document.body.contains(o.getContainer())) return;
      o._esLock = true; o.setView(m.getCenter(), m.getZoom(), {animate: false}); o._esLock = false;
    });
  });
}"

fmt_n <- function(x) format(round(x), big.mark = ",")
fmt_pop <- function(x) ifelse(x >= 1e6, sprintf("%.1f M", x / 1e6), sprintf("%s", fmt_n(x)))

ui <- page_sidebar(
  title = "Ecosystem service change explorer (1992-2020)",
  tags$head(tags$style(".lc-legend { font-size: 10.5px; line-height: 13px; } .lc-legend i { width: 12px; height: 12px; }")),
  sidebar = sidebar(
    width = 320,
    selectInput("unit_type", "Unit", UNIT_TYPES, selected = "nev_name"),
    selectizeInput("unit", NULL, choices = NULL),
    checkboxGroupInput("services", "Services", choices = setNames(names(SERV), SERV), selected = names(SERV)),
    radioButtons("metric", "Change metric", c("Relative (SPC)" = "pct", "Absolute" = "abs"), inline = TRUE),
    tags$h6("Hotspots"),
    sliderInput("thr", "Hotspot threshold (% of cells)", min = 1, max = 20, value = 5, step = 1),
    radioButtons("direction", "Direction",
                 c("Decline" = "decline", "Improvement" = "improvement", "Both" = "both")),
    radioButtons("reference", "Threshold computed on",
                 c("The selection (local hotspots)" = "local", "The whole world (global hotspots)" = "global")),
    if (allow_download) downloadButton("download", "Download cells (CSV)")
  ),
  navset_card_tab(
    id = "tab",
    nav_panel("Change maps",
              uiOutput("chg_legend"),
              do.call(navset_pill, c(
                list(id = "chg_tab",
                     nav_panel("All services",
                               do.call(layout_columns, c(list(col_widths = rep(6, length(MAP_SERV))),
                                 lapply(MAP_SERV, function(s) card(card_header(SERV[[s]]),
                                        leafletOutput(paste0("cm_all_", s), height = 330))))))),
                lapply(MAP_SERV, function(s) nav_panel(SERV[[s]], leafletOutput(paste0("cm_", s), height = 620))),
                list(nav_panel("Change by group", textOutput("bars_note"), plotOutput("chg_bars", height = "auto"))))),
              textOutput("change_note"),
              tableOutput("chg_table")),
    nav_panel("Hotspot map", leafletOutput("map", height = 620), textOutput("map_note")),
    nav_panel("Summary", uiOutput("headline"), tableOutput("summary")),
    nav_panel("By sub-unit", textOutput("sub_note"), tableOutput("subunits")),
    nav_panel("Change in hotspots", plotOutput("box", height = 420),
              div(style = "font-size:13px;max-width:900px;", markdown(paste(
                "**How to read this chart.** Each box shows how much a service changed in its hotspot cells",
                "(1992-2020): the line is the median, the box the middle half of the cells, the whiskers the",
                "rest. Orange = decline, teal = improvement. Each service has its own scale, so read each row",
                "on its own.")))),
    nav_panel("Land cover change",
              if (is.null(lc)) p("Land cover data not found: run scripts/dashboard/prepare_landcover_data.R") else tagList(
                div(style = "display:flex;align-items:flex-end;gap:16px;",
                    selectInput("lc_var", "Land cover change, 1992-2020 (% of each 10 km cell)", LC_VARS,
                                selected = "natural_loss", width = "420px"),
                    radioButtons("lc_layer", "Map", c("Change (10 km cells)" = "change", "Land cover 1992" = "1992",
                                                      "Land cover 2020" = "2020"), inline = TRUE),
                    conditionalPanel("input.lc_layer != 'change'",
                                     radioButtons("lc_scheme", "Legend", c("9 classes" = "9class", "ESA classes" = "esa"),
                                                  inline = TRUE)),
                    div(style = "margin-bottom:1rem;", actionLink("lc_info", "What do the variables mean?"))),
                layout_columns(col_widths = c(8, 4),
                               div(leafletOutput("lc_map", height = 680), textOutput("lc_note")),
                               div(plotOutput("lc_comp", height = 330), plotOutput("lc_flows", height = 350))))),
    nav_panel("About", markdown(paste(
      "Each service's hotspots are the cells in the most extreme tail of its change between 1992 and",
      "2020, in the chosen direction: **decline** is an increase in nitrogen export, sediment export or",
      "coastal risk, or a decrease in pollination or nature access; **improvement** is the opposite.",
      "The threshold is computed either within the selection (*local* hotspots: the country's own",
      "extremes) or over the whole world (*global* hotspots that fall inside the selection). Comparing",
      "the two shows how much the answer depends on the reference.\n\n",
      "Data: the evaluated 10 km equal-area cells (1,372,621) of `10k_change_calc.gpkg`; population is",
      "GHS-POP 2020 living in the cells (local residents). Relative prevalence = share of hotspot cells",
      "in a sub-unit / its share of the selection's cells.\n\n",
      "Change maps: symmetric percentage change (SPC) of each service on its native pixels (300 m; 2 km",
      "for nature access), with the symbology of the paper's Figure 1: one scale for all services,",
      "teal = favourable and orange = adverse for that service, no change transparent. Zoomed out, each",
      "screen pixel shows the mean change of the native pixels it covers. In *All services* the four",
      "maps move together. Coastal risk is shown as the coastal points where it changed by 1% or more, on its own scale. The table below",
      "the maps is the change over all native pixels of the selected unit, as in the paper's",
      "change-by-unit figures.\n\n",
      "Land cover change: loss, gain and net change of natural land, forest, and grassland and shrubland,",
      "and cropland and urban expansion, as % of each 10 km cell between the ESA CCI 1992 and C3S 2020",
      "maps (definitions under *What do the variables mean?* in that tab).\n\n",
      "Not included yet: connected beneficiaries (downstream and travel-time reach). They exist only for",
      "the global 5% decline hotspots and need a new beneficiary run for any other scenario.")))
  )
)

server <- function(input, output, session) {
  # inputs arrive from the browser and can be forged: accept only the values the interface offers
  one_of <- function(x, set) length(x) == 1 && !is.na(x) && x %in% set
  valid <- reactive(
    one_of(input$unit_type, UNIT_TYPES) && length(input$services) > 0 && all(input$services %in% names(SERV)) &&
      one_of(input$metric, c("pct", "abs")) && one_of(input$direction, c("decline", "improvement", "both")) &&
      one_of(input$reference, c("local", "global")) && is.numeric(input$thr) && length(input$thr) == 1 &&
      input$thr >= 1 && input$thr <= 20 &&
      (input$unit_type == "all" || one_of(input$unit, grid[[input$unit_type]])))

  observeEvent(input$unit_type, {
    req(one_of(input$unit_type, UNIT_TYPES))
    if (input$unit_type == "all") {
      updateSelectizeInput(session, "unit", choices = "World", selected = "World")
    } else {
      ch <- sort(unique(na.omit(grid[[input$unit_type]])))
      sel <- if (input$unit_type == "nev_name" && "Colombia" %in% ch) "Colombia" else ch[1]
      updateSelectizeInput(session, "unit", choices = ch, selected = sel, server = TRUE)
    }
  })

  sel_rows <- reactive({
    req(input$unit, valid())
    if (input$unit_type == "all") seq_len(nrow(grid)) else which(grid[[input$unit_type]] == input$unit)
  })
  sel <- reactive(grid[sel_rows(), ])
  kinds <- reactive(if (input$direction == "both") c("decline", "improvement") else input$direction)
  res <- reactive({
    req(valid(), nrow(sel()) > 0)
    hotspots(sel(), input$services, input$thr / 100, kinds(), input$metric, input$reference)
  })

  map_note <- reactiveVal("")
  output$map_note <- renderText(map_note())
  output$map <- renderLeaflet({
    # basemaps that need no API key
    leaflet() |>
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") |>
      addLayersControl(baseGroups = c("Light", "Satellite", "OpenStreetMap"),
                       options = layersControlOptions(collapsed = FALSE))
  })
  # draw only once the map widget exists; commands sent earlier are lost
  map_ready <- reactiveVal(FALSE)
  observeEvent(input$map_zoom, map_ready(TRUE), once = TRUE)
  # zoom to the selection only when the unit changes, so changing services, threshold or direction
  # keeps the user's own zoom
  fitted_unit <- reactiveVal("")
  observe({
    req(map_ready(), input$tab == "Hotspot map")
    key <- paste(input$unit_type, input$unit)
    if (key == isolate(fitted_unit())) return()
    # the bulk of the selection (1st-99th percentile of cell centres), so outlying islands
    # (e.g. the Aleutians beyond 180 degrees) do not zoom the map out to the whole world
    # (unnamed: named numbers reach leaflet as JSON objects and the zoom is silently ignored)
    cx <- (sel()$xmin + sel()$xmax) / 2; cy <- (sel()$ymin + sel()$ymax) / 2
    q <- function(v, p) quantile(v, p, names = FALSE)
    leafletProxy("map") |> fitBounds(q(cx, 0.01), q(cy, 0.01), q(cx, 0.99), q(cy, 0.99))
    fitted_unit(key)
  })
  observe({
    req(map_ready(), input$tab == "Hotspot map")
    r <- res(); cells <- r$cells
    # one value per cell (a scalar 0 here made ifelse() below collapse to the first cell)
    zero <- integer(nrow(cells))
    dec <- if ("n_decline" %in% names(cells)) cells$n_decline else zero
    imp <- if ("n_improvement" %in% names(cells)) cells$n_improvement else zero
    cells$cat <- ifelse(dec > 0 & imp > 0, "both", ifelse(dec > 0, "decline", ifelse(imp > 0, "improvement", NA)))
    cells$n <- pmax(dec, imp)
    # hover label: which services, with their change in the chosen metric
    unit_txt <- if (input$metric == "pct") "%+.0f%%" else "%+.3g"
    lab <- character(nrow(cells))
    for (k in kinds()) for (s in input$services) {
      f <- cells[[paste0(k, "_", s)]]
      v <- cells[[paste0(s, "_", input$metric, "_chg")]]
      add <- ifelse(f, paste0(DIRECTION_LABEL[[k]], ": ", SERV[[s]], " (", sprintf(unit_txt, v), ")"), "")
      lab <- ifelse(add == "", lab, ifelse(lab == "", add, paste0(lab, "<br>", add)))
    }
    cells$label <- lab
    # cells whose box wraps the 180 degree meridian (31 worldwide) would draw across the globe
    draw <- cells[!is.na(cells$cat) & (cells$xmax - cells$xmin) < 5, ]
    note <- ""
    if (nrow(draw) > 40000) {   # very large selections: draw a 0.5 degree summary instead of every cell
      draw <- draw |> mutate(bx = floor((xmin + xmax) / 2 / 0.5) * 0.5, by = floor((ymin + ymax) / 2 / 0.5) * 0.5) |>
        group_by(bx, by) |> summarise(cat = names(which.max(table(cat))), n = max(n),
                                      label = paste0(n(), " hotspot cells in this 0.5 degree square"), .groups = "drop") |>
        mutate(xmin = bx, xmax = bx + 0.5, ymin = by, ymax = by + 0.5)
      note <- "Large selection: hotspot cells summarised on a 0.5 degree grid for drawing."
    }
    k <- max(1, length(input$services))
    pal_dec <- colorNumeric(c("#fd8d3c", "#d94801", "#7f2704"), domain = c(1, k))
    pal_imp <- colorNumeric(c("#41ab5d", "#238b45", "#00441b"), domain = c(1, k))
    nn <- pmin(pmax(draw$n, 1), k)
    col <- ifelse(draw$cat == "decline", pal_dec(nn), ifelse(draw$cat == "improvement", pal_imp(nn), "#6a51a3"))
    m <- leafletProxy("map") |> clearShapes() |> clearControls()
    if (nrow(draw)) {
      m <- m |> addRectangles(lng1 = draw$xmin, lat1 = draw$ymin, lng2 = draw$xmax, lat2 = draw$ymax,
                              stroke = FALSE, fillColor = col, fillOpacity = 0.85,
                              label = lapply(draw$label, htmltools::HTML))
    }
    legend_labels <- c("Decline", "Improvement", "Both")[c("decline" %in% kinds(), "improvement" %in% kinds(), length(kinds()) == 2)]
    legend_cols <- c("#d94801", "#238b45", "#6a51a3")[c("decline" %in% kinds(), "improvement" %in% kinds(), length(kinds()) == 2)]
    m |> addLegend("bottomright", colors = legend_cols, labels = legend_labels,
                   title = "Hotspot (darker = more services)")
    map_note(note)
  })

  output$headline <- renderUI({
    s <- sel(); r <- res()$cells
    any_k <- lapply(kinds(), function(k) r[[paste0("n_", k)]] > 0)
    parts <- mapply(function(k, f) sprintf("%s hotspots (at least one service): <b>%s cells</b> (%.1f%%), <b>%s people</b> live in them.",
                                           DIRECTION_LABEL[k], fmt_n(sum(f)), 100 * mean(f), fmt_pop(sum(s$pop[f], na.rm = TRUE))),
                    kinds(), any_k)
    HTML(paste0("<p><b>", htmltools::htmlEscape(input$unit), "</b>: ", fmt_n(nrow(s)), " cells (", fmt_n(nrow(s) * 100), " km&sup2;), ",
                fmt_pop(sum(s$pop, na.rm = TRUE)), " people.</p><p>", paste(parts, collapse = "<br>"), "</p>"))
  })

  output$summary <- renderTable({
    r <- res(); cells <- r$cells
    st <- bind_rows(r$stats)
    rows <- lapply(seq_len(nrow(st)), function(i) {
      k <- st$direction[i]; s <- st$service[i]; f <- cells[[paste0(k, "_", s)]]
      col <- paste0(s, "_", input$metric, "_chg"); valid <- !is.na(cells[[col]])
      data.frame(Direction = unname(DIRECTION_LABEL[k]), Service = unname(SERV[s]),
                 `Cells with data` = fmt_n(sum(valid)),
                 Threshold = sprintf(if (input$metric == "pct") "%+.1f %%" else "%+.4g", st$cut[i]),
                 `Hotspot cells` = fmt_n(sum(f)),
                 `% of cells with data` = sprintf("%.1f", 100 * sum(f) / max(1, sum(valid))),
                 `People in hotspot cells` = fmt_pop(sum(cells$pop[f], na.rm = TRUE)),
                 `Median change in hotspots` = if (sum(f)) sprintf(if (input$metric == "pct") "%+.0f %%" else "%+.4g",
                                                                   median(cells[[col]][f], na.rm = TRUE)) else "-",
                 check.names = FALSE)
    })
    bind_rows(rows)
  }, striped = TRUE, spacing = "s")

  output$sub_note <- renderText({
    sprintf("Relative prevalence of %s hotspots (at least one selected service) by %s within the selection: share of hotspot cells / share of cells. Top 25.",
            tolower(DIRECTION_LABEL[kinds()[1]]), tolower(SUBUNIT_LABEL[SUBUNIT_OF[[input$unit_type]]]))
  })
  output$subunits <- renderTable({
    cells <- res()$cells; g <- SUBUNIT_OF[[input$unit_type]]; k <- kinds()[1]
    cells$hot <- cells[[paste0("n_", k)]] > 0
    cells |> filter(!is.na(.data[[g]])) |>
      group_by(sub = .data[[g]]) |>
      summarise(cells = n(), hot = sum(hot), .groups = "drop") |>
      mutate(share_cells = cells / sum(cells), share_hot = if (sum(hot)) hot / sum(hot) else 0,
             prevalence = ifelse(share_cells > 0, share_hot / share_cells, NA)) |>
      arrange(desc(prevalence)) |> head(25) |>
      transmute(!!SUBUNIT_LABEL[[g]] := sub, Cells = fmt_n(cells), `Hotspot cells` = fmt_n(hot),
                `% of hotspots` = sprintf("%.1f", 100 * share_hot), `% of cells` = sprintf("%.1f", 100 * share_cells),
                `Relative prevalence` = sprintf("%.2f", prevalence))
  }, striped = TRUE, spacing = "s")

  output$box <- renderPlot({
    cells <- res()$cells
    d <- bind_rows(lapply(kinds(), function(k) bind_rows(lapply(input$services, function(s) {
      f <- cells[[paste0(k, "_", s)]]
      data.frame(direction = unname(DIRECTION_LABEL[k]), service = unname(SERV[s]), v = cells[[paste0(s, "_", input$metric, "_chg")]][f])
    }))))
    req(nrow(d) > 0)
    ggplot(d, aes(v, service, fill = direction)) +
      geom_boxplot(outlier.shape = NA, width = 0.6, position = position_dodge(width = 0.7)) +
      scale_fill_manual(values = c(Decline = "#F07D00", Improvement = "#009191"), name = NULL) +
      facet_wrap(~ service, scales = "free", ncol = 1) +
      labs(x = if (input$metric == "pct") "Change within hotspot cells (SPC, %)" else "Absolute change within hotspot cells",
           y = NULL) +
      theme_minimal(base_size = 13) + theme(legend.position = "top", strip.text = element_blank())
  })

  # ---- change maps: native-pixel tiles ------------------------------------------------------------
  # view of the selection: the bulk of its cells (1st-99th percentile of centres), as on the hotspot map
  chg_bounds <- reactive({
    if (input$unit_type == "all") return(c(-170, -58, 180, 80))
    s <- sel(); q <- function(v, p) quantile(v, p, names = FALSE)
    cx <- (s$xmin + s$xmax) / 2; cy <- (s$ymin + s$ymax) / 2
    c(q(cx, 0.01), q(cy, 0.01), q(cx, 0.99), q(cy, 0.99))
  })
  output$chg_legend <- renderUI({
    if (input$metric == "pct") change_legend()
    else HTML(paste('<p style="text-align:center;font-size:13px;margin:4px 0 10px;">Absolute change, 2020 minus 1992,',
                    'per hectare in each service\'s own units: each map has its own scale (legend on the map).',
                    'Teal = favourable, orange = adverse.</p>'))
  })
  change_map <- function(s, sync = FALSE) {
    b <- chg_bounds()
    metric <- input$metric
    m <- leaflet(options = leafletOptions(worldCopyJump = TRUE, preferCanvas = TRUE)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")
    if (s == "c_risk") {
      # coastal points that changed by 1% or more, on their own scale (changes are within about +-14%)
      if (metric == "pct") {
        pal <- COASTAL_PAL; val <- coastal$spc; lim <- COASTAL_LIM; ttl <- "Coastal risk (SPC, %)<br>own scale; increase = adverse"
      } else {
        pal <- COASTAL_ABS_PAL; val <- coastal$d_clamped; lim <- COASTAL_ABS_LIM; ttl <- "Coastal risk index change<br>increase = adverse"
      }
      m <- m |>
        addCircleMarkers(lng = coastal$lon, lat = coastal$lat, radius = 2.5, stroke = FALSE,
                         fillColor = pal(val), fillOpacity = 0.9, group = "Change") |>
        addLegend("bottomright", pal = pal, values = c(-lim, lim), opacity = 1, title = ttl)
    } else {
      folder <- if (metric == "pct") s else paste0(s, "_abs")
      m <- m |> addTiles(urlTemplate = paste0(tile_url, "/", folder, "/{z}/{x}/{y}.png"), group = "Change",
                         options = tileOptions(maxNativeZoom = TILE_MAXZOOM[[s]], maxZoom = 13, noWrap = TRUE))
      if (metric == "abs") {
        al <- abs_limits(); r <- al[al$service == s, ]
        if (nrow(r)) {
          # colours run teal -> orange from favourable to adverse; for services where a decrease is adverse
          # the value axis is reversed, so the legend shows the values as they are drawn
          up <- s %in% ADVERSE_UP
          pal <- colorNumeric(if (up) c("#009191", "#ffffff", "#F07D00") else c("#F07D00", "#ffffff", "#009191"),
                              domain = c(-r$limit, r$limit))
          m <- m |> addLegend("bottomright", pal = pal, values = c(-r$limit, r$limit), opacity = 1,
                              title = paste0(SERV[[s]], "<br>", r$unit, " (2020 - 1992)"))
        }
      }
    }
    m <- m |>
      addLayersControl(baseGroups = c("Light", "Satellite"), overlayGroups = "Change",
                       options = layersControlOptions(collapsed = TRUE)) |>
      addScaleBar(position = "bottomleft") |>
      fitBounds(b[1], b[2], b[3], b[4])
    if (sync) m <- htmlwidgets::onRender(m, SYNC_JS)
    m
  }
  for (s in MAP_SERV) local({
    sv <- s
    output[[paste0("cm_all_", sv)]] <- renderLeaflet(change_map(sv, sync = TRUE))
    output[[paste0("cm_", sv)]] <- renderLeaflet(change_map(sv))
  })
  # ---- change by group: the selection among its peers, from the native-pixel group totals (Path A) ----
  peers <- reactive({
    ut <- input$unit_type
    if (ut == "all") {
      list(m = path_a$region_wb, units = unique(path_a$region_wb$unit), hl = NA, what = "World Bank regions")
    } else if (ut == "nev_name") {
      reg <- grid$region_wb[match(input$unit, grid$nev_name)]
      list(m = path_a$nev_name, units = unique(grid$nev_name[grid$region_wb %in% reg]), hl = input$unit,
           what = paste("countries of", reg))
    } else {
      list(m = path_a[[ut]], units = unique(path_a[[ut]]$unit), hl = input$unit,
           what = tolower(names(UNIT_TYPES)[UNIT_TYPES == ut]))
    }
  })
  bars_data <- reactive({
    p <- peers(); val <- if (input$metric == "pct") "sym_pct_change" else "mean_val"
    p$m |>
      filter(unit %in% p$units, service %in% input$services, is.finite(.data[[val]])) |>
      mutate(value = .data[[val]],
             direction = ifelse(value == 0, "No change",
                                ifelse((value > 0) == (service %in% ADVERSE_UP), "Adverse", "Favourable")),
             selected = !is.na(p$hl) & unit == p$hl,
             service = factor(unname(SERV[service]), levels = unname(SERV)))
  })
  output$bars_note <- renderText({
    p <- peers()
    paste0("Change 1992-2020 over all native pixels of each unit (", if (input$metric == "pct") "SPC, %" else
             "absolute change per hectare, own units per service", "), ", p$what,
           if (!is.na(p$hl)) paste0("; ", p$hl, " outlined") else "", ". Same totals as the paper's change-by-group figures.")
  })
  output$chg_bars <- renderPlot({
    d <- bars_data(); req(nrow(d) > 0)
    ord <- d |> group_by(unit) |> summarise(o = first(unit)) |> pull(unit) |> sort(decreasing = TRUE)
    d$unit <- factor(d$unit, levels = ord)
    ggplot(d, aes(value, unit)) +
      geom_col(aes(fill = direction), width = 0.75) +
      geom_col(data = d[d$selected, ], fill = NA, colour = "black", linewidth = 0.7, width = 0.75) +
      geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
      facet_wrap(~ service, nrow = 1, scales = "free_x") +
      scale_fill_manual(values = c(Adverse = "#F07D00", Favourable = "#009191", `No change` = "grey70"), name = NULL) +
      labs(x = if (input$metric == "pct") "Symmetric percentage change (%), 1992 to 2020" else "Absolute change per hectare, 2020 minus 1992",
           y = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", strip.text = element_text(face = "bold"), panel.grid.major.y = element_blank())
  }, height = function() 140 + 22 * max(4, length(peers()$units)))

  output$change_note <- renderText({
    if (!dir.exists(tiles_dir) && tile_url == "tiles") "Change-map tiles not found: run scripts/dashboard/make_change_tiles.py all"
    else paste("Coastal risk is computed at points along the coast; the map shows the points where it changed",
               "by 1% or more, on its own colour scale (its changes are within about +-14%).")
  })

  # change over the native pixels (Path A tables), with the unit's rank among its peers
  chg <- reactive({
    req(input$unit_type != "all")
    m <- path_a[[input$unit_type]] |> mutate(adverse = ifelse(service %in% ADVERSE_UP, 1, -1))
    # rank: 1 = largest change in the adverse direction among all units of this type
    m |> group_by(service) |> mutate(rank = rank(-adverse * sym_pct_change, ties.method = "min"), n_units = n()) |>
      ungroup() |> filter(unit == input$unit)
  })
  output$chg_table <- renderTable({
    d <- chg(); req(nrow(d) > 0)
    d |> arrange(match(service, names(SERV))) |>
      transmute(Service = unname(SERV[service]), `Relative change (SPC)` = sprintf("%+.1f %%", sym_pct_change),
                `Absolute change per unit area` = sprintf("%+.3g", mean_val),
                `Rank (1 = largest adverse change)` = sprintf("%d of %d", rank, n_units))
  }, striped = TRUE, spacing = "s", caption.placement = "top",
  caption = "Change over all native pixels of the unit (300 m; 2 km for nature access), as in the paper's change-by-unit figures")

  # ---- land cover change on the 10 km grid ----------------------------------------------------------
  lc_rows <- reactive({
    req(!is.null(lc), valid())
    r <- sel_rows(); r[!is.na(lc$natural_p[r])]
  })
  lc_draw <- reactive({
    req(one_of(input$lc_var, LC_VAR_IDS))
    r <- lc_rows(); s <- grid[r, c("xmin", "xmax", "ymin", "ymax")]
    s$v <- lc_values(r, input$lc_var)
    s <- s[abs(s$v) >= 0.05 & (s$xmax - s$xmin) < 5, ]   # no change: transparent; skip cells across 180 degrees
    note <- ""
    if (nrow(s) > 40000) {     # large selections: mean of the cells in 0.5 or 1 degree squares, for drawing
      step <- if (nrow(s) > 300000) 1 else 0.5
      s <- s |> mutate(bx = floor((xmin + xmax) / 2 / step) * step, by = floor((ymin + ymax) / 2 / step) * step) |>
        group_by(bx, by) |> summarise(v = mean(v), .groups = "drop") |>
        mutate(xmin = bx, xmax = bx + step, ymin = by, ymax = by + step)
      note <- sprintf("Large selection: changed cells averaged over %s degree squares for drawing.", step)
    }
    list(s = s, note = note)
  })
  output$lc_map <- renderLeaflet({
    req(one_of(input$lc_layer, c("change", "1992", "2020")))
    if (input$lc_layer != "change") {
      req(one_of(input$lc_scheme, c("9class", "esa")))
      b <- chg_bounds(); leg <- if (input$lc_scheme == "esa") ESA_LEGEND else LC9_LEGEND
      return(leaflet(options = leafletOptions(worldCopyJump = TRUE)) |>
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
        addTiles(urlTemplate = paste0(tile_url, "/lc_", input$lc_scheme, "_", input$lc_layer, "/{z}/{x}/{y}.png"),
                 group = "Land cover", options = tileOptions(maxNativeZoom = LC_TILE_MAXZOOM, maxZoom = 13, noWrap = TRUE)) |>
        addLegend("bottomright", colors = unname(leg), labels = names(leg), opacity = 1,
                  title = paste("Land cover", input$lc_layer), className = "info legend lc-legend") |>
        addLayersControl(baseGroups = c("Light", "Satellite"), overlayGroups = "Land cover",
                         options = layersControlOptions(collapsed = TRUE)) |>
        addScaleBar(position = "bottomleft") |>
        fitBounds(b[1], b[2], b[3], b[4]))
    }
    d <- lc_draw(); s <- d$s; var <- input$lc_var; b <- chg_bounds()
    kind <- sub("^[a-z]+_", "", var)
    # colour scale capped at the 99th percentile of the changed cells, so a few cells do not wash it out
    lim <- max(1, quantile(abs(s$v), 0.99, names = FALSE, na.rm = TRUE))
    pal <- switch(kind,
      net = colorNumeric(c("#F07D00", "#ffffff", "#009191"), domain = c(-lim, lim)),
      loss = colorNumeric(c("#fee6ce", "#F07D00", "#7f2704"), domain = c(0, lim)),
      gain = if (var %in% c("cropland_gain", "urban_gain")) colorNumeric(c("#efedf5", "#807dba", "#3f007d"), domain = c(0, lim))
             else colorNumeric(c("#d5f0ee", "#009191", "#00403f"), domain = c(0, lim)))
    vv <- if (kind == "net") pmax(pmin(s$v, lim), -lim) else pmin(s$v, lim)
    m <- leaflet(options = leafletOptions(worldCopyJump = TRUE, preferCanvas = TRUE)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")
    if (nrow(s)) {
      m <- m |> addRectangles(lng1 = s$xmin, lat1 = s$ymin, lng2 = s$xmax, lat2 = s$ymax, stroke = FALSE,
                              fillColor = pal(vv), fillOpacity = 0.85, group = "Land cover change",
                              label = sprintf("%+.1f%% of the cell", s$v))
    }
    m |> addLegend("bottomright", pal = pal, values = if (kind == "net") c(-lim, lim) else c(0, lim), opacity = 1,
                   title = paste0(LC_VAR_LABEL[[var]], "<br>% of cell", if (kind == "net") "<br>orange = loss, teal = gain" else "")) |>
      addLayersControl(baseGroups = c("Light", "Satellite"), overlayGroups = "Land cover change",
                       options = layersControlOptions(collapsed = TRUE)) |>
      addScaleBar(position = "bottomleft") |>
      fitBounds(b[1], b[2], b[3], b[4])
  })
  observeEvent(input$lc_info, showModal(modalDialog(markdown(LC_INFO), title = "Land cover change: the variables",
                                                    easyClose = TRUE, size = "l", footer = modalButton("Close"))))
  output$lc_note <- renderText({
    if (!identical(input$lc_layer, "change"))
      return(paste("ESA CCI (1992) and C3S (2020) land cover, 300 m; zoomed out, each screen pixel shows the most",
                   "common class of the area it covers. Water is not drawn. The charts and the variable menu refer",
                   "to the change view."))
    r <- lc_rows()
    paste0(fmt_n(length(r)), " of ", fmt_n(length(sel_rows())), " cells in the selection have land cover values. ",
           "Cells without change are not drawn. ", lc_draw()$note)
  })
  # composition and gross flows: means over the selection's cells (equal-area cells: shares of its area)
  lc_means <- reactive({
    r <- lc_rows(); req(length(r) > 0)
    m <- function(col) mean(lc[[col]][r]) / 10
    data.frame(key = names(LC_CLASS), class = unname(LC_CLASS),
               p = sapply(names(LC_CLASS), function(k) m(paste0(k, "_p"))),
               l = sapply(names(LC_CLASS), function(k) m(paste0(k, "_l"))),
               g = sapply(names(LC_CLASS), function(k) m(paste0(k, "_g"))))
  })
  output$lc_comp <- renderPlot({
    d <- lc_means() |> filter(key != "natural")
    comp <- bind_rows(data.frame(year = "1992", class = d$class, share = d$p + d$l),
                      data.frame(year = "2020", class = d$class, share = d$p + d$g))
    comp <- bind_rows(comp, comp |> group_by(year) |> summarise(share = 100 - sum(share)) |> mutate(class = "Other"))
    comp$class <- factor(comp$class, levels = rev(names(LC_COLOURS)))
    ggplot(comp, aes(share, year, fill = class)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = ifelse(share >= 4, sprintf("%.0f%%", share), "")), position = position_stack(vjust = 0.5), size = 3.2) +
      scale_fill_manual(values = LC_COLOURS, breaks = names(LC_COLOURS), name = NULL) +
      labs(x = "Share of the selection's area (%)", y = NULL, title = "Land cover, 1992 and 2020",
           subtitle = "Other = sparse, wetland, bare, water, snow and ice") +
      guides(fill = guide_legend(nrow = 2)) +
      theme_minimal(base_size = 11) + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
  })
  output$lc_flows <- renderPlot({
    d <- lc_means()
    fl <- bind_rows(data.frame(class = d$class, what = "Loss", v = d$l), data.frame(class = d$class, what = "Gain", v = d$g))
    fl$class <- factor(fl$class, levels = rev(unname(LC_CLASS)))
    net <- data.frame(class = factor(d$class, levels = levels(fl$class)), v = d$g - d$l)
    ggplot(fl, aes(v, class, fill = what)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.65) +
      geom_point(data = net, aes(v, class), inherit.aes = FALSE, shape = 23, size = 3, fill = "black") +
      geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
      scale_fill_manual(values = c(Loss = "#F07D00", Gain = "#009191"), name = NULL) +
      labs(x = "Share of the selection's area (%)", y = NULL, title = "Gross loss and gain, 1992-2020",
           subtitle = "Diamond = net change (gain minus loss)") +
      theme_minimal(base_size = 11) + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
  })

  if (allow_download) output$download <- downloadHandler(
    filename = function() sprintf("hotspots_%s_%s_%dpct.csv", gsub("[^A-Za-z0-9]+", "_", input$unit),
                                  input$direction, input$thr),
    content = function(file) {
      cells <- res()$cells |> mutate(lon = (xmin + xmax) / 2, lat = (ymin + ymax) / 2) |>
        select(-xmin, -xmax, -ymin, -ymax)
      write.csv(cells, file, row.names = FALSE)
    })
}

shinyApp(ui, server)
