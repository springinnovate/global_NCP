#' Coalesced left join that preserves (or drops) sf geometry
#'
#' @description
#' A convenience wrapper around [dplyr::left_join()] for joining an `sf`
#' object (`x`) with an `sf` or data frame (`y`) while keeping exactly **one**
#' copy of overlapping **non-key** columns. Overlaps are resolved via
#' `prefer = c("coalesce","x","y","error")`. Geometry from `x` is preserved
#' (and replicated for duplicated rows) when `output = "sf"`.
#'
#' @param x An `sf` object (recommended) or a tibble/data.frame. If
#'   `output = "sf"`, `x` **must** be an `sf`.
#' @param y An `sf` object or a tibble/data.frame to join into `x`.
#'   If `y` is `sf`, its geometry is dropped before joining.
#' @param by Join keys passed to [dplyr::left_join()] (`character` vector or
#'   named vector). Keys present in both inputs are kept once (dplyr’s default).
#' @param prefer How to resolve overlapping **non-key** columns that exist in
#'   both `x` and `y`:
#'   - `"coalesce"`: take `x` where not `NA`, otherwise `y` (default).
#'   - `"x"`: keep `x` values (ignore `y`).
#'   - `"y"`: take `y` where not `NA`, otherwise `x`.
#'   - `"error"`: error if any non-missing conflicts remain.
#' @param suffix Length-2 character vector of suffixes passed to
#'   [dplyr::left_join()]. The second element (e.g., `".y"`) is used to find
#'   and drop the duplicate columns after resolving conflicts.
#' @param require_unique_y Logical; if `TRUE`, error if `y` has multiple rows
#'   per key in `by`. If `FALSE` (default), the join is allowed to duplicate
#'   rows of `x`.
#' @param output `"sf"` (default) to return an `sf` with `x`'s geometry
#'   preserved, or `"tibble"` to return a plain tibble with no geometry.
#'
#' @return An object of class specified by `output`:
#'   - `"sf"`: an `sf` with `x`'s CRS and geometry, possibly with duplicated
#'     rows if `y` is one-to-many on the keys.
#'   - `"tibble"`: a tibble/data.frame (no geometry column).
#'
#' @details
#' - Overlapping **non-key** columns are detected after the join using the
#'   `suffix[2]` naming (e.g., `col.y`). They’re resolved per `prefer` and
#'   the suffixed copies are dropped.
#' - Type compatibility: `coalesce()` requires compatible types. Cast
#'   beforehand if needed (e.g., `mutate(across(col, as.numeric))`).
#' - If you need a **spatial** join (e.g., intersects/within), use
#'   [sf::st_join()] and apply a similar coalescing pattern afterward.
#'
#' @examples
#' \dontrun{
#' library(sf); library(dplyr)
#' pts <- st_as_sf(data.frame(id = 1:3, x = c(0,1,2), y = c(0,1,1)),
#'                 coords = c("x","y"), crs = 4326)
#' attrs <- tibble::tibble(id = c(1,2,2,3),
#'                         name = c("a","b","b2","c"),
#'                         score = c(NA, 10, 12, 20))
#'
#' # Keep geometry; resolve overlaps by coalescing (x then y)
#' res_sf <- coalesced_left_join_sf(pts, attrs, by = "id", prefer = "coalesce", output = "sf")
#'
#' # Return a plain tibble (no geometry)
#' res_tbl <- coalesced_left_join_sf(pts, attrs, by = "id", prefer = "y", output = "tibble")
#' }
#'
#' @seealso [dplyr::left_join()], [sf::st_join()], [sf::st_drop_geometry()]
#' @export
coalesced_left_join_sf <- function(
    x, y, by,
    prefer = c("coalesce","x","y","error"),
    suffix = c("", ".y"),
    require_unique_y = FALSE,
    output = c("sf","tibble")
) {
  output <- match.arg(output)
  prefer <- match.arg(prefer)
  
  is_x_sf <- inherits(x, "sf")
  if (output == "sf" && !is_x_sf) {
    stop("When output = 'sf', `x` must be an sf object.")
  }
  
  # Drop geometry from y if present; we always keep x's geometry (when requested)
  y_attr <- if (inherits(y, "sf")) sf::st_drop_geometry(y) else y
  
  # Optionally enforce uniqueness of y on the keys
  if (isTRUE(require_unique_y)) {
    dupy <- y_attr |>
      dplyr::count(dplyr::across(all_of(by)), name = "n") |>
      dplyr::filter(.data$n > 1)
    if (nrow(dupy) > 0) {
      stop("`y` has duplicate rows for join keys. ",
           "Summarise/deduplicate `y` first or set require_unique_y = FALSE.")
    }
  }
  
  # Remember geometry + CRS (if x is sf) and attach a stable row id
  if (is_x_sf) {
    geom_x <- sf::st_geometry(x)
    crs_x  <- sf::st_crs(x)
    x_attr <- sf::st_drop_geometry(x)
  } else {
    geom_x <- NULL; crs_x <- NULL
    x_attr <- x
  }
  
  tmp_id <- ".__rowid__."
  while (tmp_id %in% names(x_attr)) tmp_id <- paste0(tmp_id, "_", sample(letters, 1))
  x_attr[[tmp_id]] <- seq_len(nrow(x_attr))
  
  # Plain attribute join
  out <- dplyr::left_join(x_attr, y_attr, by = by, suffix = suffix)
  
  # Resolve overlapping NON-KEY columns (keep keys as dplyr handles)
  dup_cols <- intersect(
    setdiff(names(x_attr), c(by, tmp_id)),
    setdiff(names(y_attr), by)
  )
  
  for (nm in dup_cols) {
    ynm <- paste0(nm, suffix[2])
    if (!ynm %in% names(out)) next
    
    if (prefer == "coalesce") {
      out[[nm]] <- dplyr::coalesce(out[[nm]], out[[ynm]])
    } else if (prefer == "y") {
      out[[nm]] <- dplyr::coalesce(out[[ynm]], out[[nm]])
    } else if (prefer == "error") {
      a <- out[[nm]]; b <- out[[ynm]]
      conflicted <- !(is.na(a) & is.na(b)) &
        (xor(is.na(a), is.na(b)) | (!is.na(a) & !is.na(b) & a != b))
      if (any(conflicted, na.rm = TRUE)) {
        stop("Conflict in overlapping column '", nm,
             "'. Choose prefer = 'x', 'y', or 'coalesce'.")
      }
      # prefer == "x": keep a as-is
    }
    out[[ynm]] <- NULL
  }
  
  # Finalize output class
  if (output == "tibble") {
    out[[tmp_id]] <- NULL
    return(tibble::as_tibble(out))
  }
  
  # output == "sf": reattach geometry from x, respecting any row duplication
  out$geometry <- geom_x[out[[tmp_id]]]
  out[[tmp_id]] <- NULL
  sf::st_as_sf(out, sf_column_name = "geometry", crs = crs_x)
}

