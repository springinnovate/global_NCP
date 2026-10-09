# Ecosystem service change explorer: maps of change 1992-2020 and the hotspot analysis for any
# country, region, income group or biome (2026-10-08). Works on the 10 km grid (Path B); the data
# are prepared once by scripts/dashboard/prepare_dashboard_data.R.
#
# Run from the repo root:  Rscript -e "shiny::runApp('scripts/dashboard', launch.browser = TRUE)"

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(leaflet); library(dplyr); library(ggplot2)
  library(patchwork)
})

data_file <- file.path(here::here(), "data", "processed", "dashboard", "grid_dashboard.rds")
if (!file.exists(data_file)) stop("Run scripts/dashboard/prepare_dashboard_data.R first")
grid <- readRDS(data_file)
layers <- readRDS(file.path(dirname(data_file), "change_map_layers.rds"))

# Change per unit from the 300 m / 2 km pixels (Path A), the same tables as the paper's change figures
pa_dir <- file.path(here::here(), "outputs", "plots", "output_plots_diff")
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

# ---- change maps: the symbology of the paper's change figures (scripts/mapping/make_global_change_5panel.R)
ABS_UNITS <- c(n_export = "kg N/ha/yr", sed_export = "t/ha/yr", c_risk = "Rt (InVEST)",
               pollination = "people-fed equiv./ha", nature_access = "access index")
# symmetric colour range: the larger of |1st| and |99th| percentile
sym_limits <- function(v) { lim <- max(abs(quantile(v, c(0.01, 0.99), na.rm = TRUE))); c(-lim, lim) }
# the paper's range: over all evaluated cells, per service and metric
PAPER_LIMITS <- sapply(as.vector(outer(names(SERV), c("pct", "abs"), paste, sep = "_")),
                       function(sm) sym_limits(grid[[paste0(sm, "_chg")]]), simplify = FALSE)

change_panel <- function(px, s, metric, limits) {
  v <- grid[[paste0(s, "_", metric, "_chg")]][px$row]
  ok <- !is.na(v); px <- px[ok, ]; v <- v[ok]
  px$value <- pmax(pmin(v, limits[2]), limits[1])
  # near-zero change fades to the background, as in the paper
  px$alpha <- pmin(abs(px$value) / max(0.08 * limits[2], 1e-9), 1)
  hi <- if (s %in% ADVERSE_UP) "#F07D00" else "#009191"
  lo <- if (s %in% ADVERSE_UP) "#009191" else "#F07D00"
  ggplot() +
    geom_sf(data = layers$base, fill = "gray95", color = "gray80", linewidth = 0.1) +
    geom_raster(data = px, aes(x, y, fill = value, alpha = alpha)) +
    scale_alpha_identity(guide = "none") +
    scale_fill_gradient2(low = lo, mid = "white", high = hi, midpoint = 0, limits = limits,
                         name = if (metric == "pct") "% change" else ABS_UNITS[[s]], na.value = NA) +
    labs(title = SERV[[s]]) +
    theme_void() +
    # the paper's theme, with type sized for the screen rather than a 200 dpi figure
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15, color = "#004D1E"),
          legend.position = "bottom", legend.key.width = unit(1.4, "cm"), legend.key.height = unit(0.3, "cm"),
          legend.title = element_text(size = 11, vjust = 0.8), legend.text = element_text(size = 10),
          plot.margin = margin(6, 8, 6, 8))
}

fmt_n <- function(x) format(round(x), big.mark = ",")
fmt_pop <- function(x) ifelse(x >= 1e6, sprintf("%.1f M", x / 1e6), sprintf("%s", fmt_n(x)))

ui <- page_sidebar(
  title = "Ecosystem service change explorer (1992-2020, 10 km grid)",
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
    downloadButton("download", "Download cells (CSV)")
  ),
  navset_card_tab(
    id = "tab",
    nav_panel("Change maps",
              radioButtons("scale", NULL, inline = TRUE,
                           c("Colour range as in the paper (all cells worldwide)" = "paper",
                             "Colour range stretched to the selection" = "local")),
              uiOutput("change_maps_ui"), textOutput("change_note"),
              tableOutput("chg_table")),
    nav_panel("Hotspot map", leafletOutput("map", height = 620), textOutput("map_note")),
    nav_panel("Summary", uiOutput("headline"), tableOutput("summary")),
    nav_panel("By sub-unit", textOutput("sub_note"), tableOutput("subunits")),
    nav_panel("Change in hotspots", plotOutput("box", height = 420)),
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
      "Change maps: change of each service per 10 km cell, with the symbology of the paper's change",
      "maps (Equal Earth projection; orange = adverse direction, teal = favourable; values beyond the",
      "colour range are shown at its end; near-zero change fades out). The table below them is the",
      "change over all native pixels (300 m; 2 km for nature access) of the selected unit, the same",
      "tables as the paper's change-by-unit figures.\n\n",
      "Not included yet: connected beneficiaries (downstream and travel-time reach). They exist only for",
      "the global 5% decline hotspots and need a new beneficiary run for any other scenario.")))
  )
)

server <- function(input, output, session) {
  observeEvent(input$unit_type, {
    if (input$unit_type == "all") {
      updateSelectizeInput(session, "unit", choices = "World", selected = "World")
    } else {
      ch <- sort(unique(na.omit(grid[[input$unit_type]])))
      sel <- if (input$unit_type == "nev_name" && "Colombia" %in% ch) "Colombia" else ch[1]
      updateSelectizeInput(session, "unit", choices = ch, selected = sel, server = TRUE)
    }
  })

  sel_rows <- reactive({
    req(input$unit)
    if (input$unit_type == "all") seq_len(nrow(grid)) else which(grid[[input$unit_type]] == input$unit)
  })
  sel <- reactive(grid[sel_rows(), ])
  kinds <- reactive(if (input$direction == "both") c("decline", "improvement") else input$direction)
  res <- reactive({
    req(length(input$services) > 0, nrow(sel()) > 0)
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
    HTML(paste0("<p><b>", input$unit, "</b>: ", fmt_n(nrow(s)), " cells (", fmt_n(nrow(s) * 100), " km&sup2;), ",
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

  # ---- change maps (10 km grid, Equal Earth) -----------------------------------------------------
  map_services <- reactive({
    # coastal risk is a one-cell coastal fringe, invisible at world scale (left out of the paper's maps too)
    if (input$unit_type == "all") setdiff(input$services, "c_risk") else input$services
  })
  map_pix <- reactive({
    world <- input$unit_type == "all"
    pix <- if (world) layers$pix$world else layers$pix$local
    if (world) return(pix)
    keep <- logical(nrow(grid)); keep[sel_rows()] <- TRUE
    pix[keep[pix$row], ]
  })
  map_extent <- reactive({
    px <- map_pix()
    # the bulk of the selection, as on the hotspot map, plus a margin
    q <- function(v) { r <- quantile(v, c(0.005, 0.995), names = FALSE); r + c(-1, 1) * max(0.05 * diff(r), 30000) }
    if (input$unit_type == "all") list(x = range(px$x), y = range(px$y)) else list(x = q(px$x), y = q(px$y))
  })
  map_layout <- reactive({
    n <- length(map_services()); e <- map_extent()
    aspect <- min(max(diff(e$y) / diff(e$x), 0.45), 1.4)
    # wide extents (the world, Russia) two per row; tall or square ones three per row
    ncol <- min(n, if (aspect < 0.8) 2 else 3)
    list(ncol = ncol, nrow = ceiling(n / ncol), aspect = aspect)
  })
  output$change_maps <- renderPlot({
    req(length(map_services()) > 0)
    px <- map_pix(); e <- map_extent(); rows <- sel_rows()
    panels <- lapply(map_services(), function(s) {
      col <- paste0(s, "_", input$metric, "_chg")
      lim <- if (input$scale == "paper") PAPER_LIMITS[[paste0(s, "_", input$metric)]] else sym_limits(grid[[col]][rows])
      if (!all(is.finite(lim)) || lim[2] == 0) lim <- c(-1, 1)
      change_panel(px, s, input$metric, lim) +
        coord_sf(crs = "EPSG:8857", xlim = e$x, ylim = e$y, expand = FALSE, datum = NA)
    })
    # pixels of a sparse selection leave gaps between columns, which ggplot reports as an uneven raster;
    # the drawing is still correct (it uses the smallest spacing, the true resolution)
    suppressWarnings(print(wrap_plots(panels, ncol = map_layout()$ncol)))
  }) |> bindCache(input$unit_type, input$unit, input$services, input$metric, input$scale)
  # the plot's height follows the panel layout and the selection's shape; the width is the page's.
  output$change_maps_ui <- renderUI({
    lay <- map_layout()
    # the tab card is a flex container that shrinks its children to the space left on screen, which
    # squeezed the maps to a thumbnail; flex-shrink: 0 keeps the set height (the card scrolls instead)
    div(style = "flex-shrink: 0;",
        plotOutput("change_maps", height = sprintf("%dpx", round(lay$nrow * (1000 / lay$ncol * lay$aspect + 80))),
                   fill = FALSE))
  })
  output$change_note <- renderText({
    if (input$unit_type == "all" && "c_risk" %in% input$services)
      "Coastal risk is left out at world scale: it exists only on a one-cell coastal fringe (choose a country or region to see it)."
    else ""
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

  output$download <- downloadHandler(
    filename = function() sprintf("hotspots_%s_%s_%dpct.csv", gsub("[^A-Za-z0-9]+", "_", input$unit),
                                  input$direction, input$thr),
    content = function(file) {
      cells <- res()$cells |> mutate(lon = (xmin + xmax) / 2, lat = (ymin + ymax) / 2) |>
        select(-xmin, -xmax, -ymin, -ymax)
      write.csv(cells, file, row.names = FALSE)
    })
}

shinyApp(ui, server)
