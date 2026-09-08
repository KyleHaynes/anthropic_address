## Spatial helpers: shapefile import, fast lookup, plotting

#' Read a shapefile and print available columns
#'
#' This reads a polygon shapefile using the `sf` package and verbosely prints
#' the non-geometry column names and their classes. Default path is
#' `C:/temp/sa2/SA2_2021_AUST_GDA2020.shp`.
#'
#' @param path Path to a shapefile (.shp).
#' @param quiet If `FALSE` (default) prints messages from `sf::st_read`.
#' @param ... Passed to `sf::st_read`.
#' @return An `sf` object (invisible).
#' @export
read_shapefile <- function(path = "C:/temp/sa2/SA2_2021_AUST_GDA2020.shp", quiet = FALSE, ...) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required; please install it.")
  if (!file.exists(path)) stop(sprintf("Shapefile not found: %s", path))
  sf_obj <- sf::st_read(path, quiet = quiet, ...)
  attrs <- sf::st_drop_geometry(sf_obj)
  cols <- names(attrs)
  classes <- vapply(attrs, function(x) paste(class(x), collapse = "/"), character(1))
  message("Shapefile: ", path)
  message("CRS: ", as.character(sf::st_crs(sf_obj)))
  message("Available non-geometry columns:")
  for (i in seq_along(cols)) message(sprintf(" - %s : %s", cols[i], classes[i]))
  invisible(sf_obj)
}

#' Subset an `sf` object by a column value
#'
#' @param sf_obj An `sf` polygon object.
#' @param var Character name of the column to filter on.
#' @param values Value or vector of values to keep (uses \code{\%in\%}).
#' @param invert If `TRUE`, keep rows not matching `values`.
#' @return Subsetted `sf` object.
#' @export
subset_shapefile <- function(sf_obj, var, values, invert = FALSE) {
  if (missing(var) || !is.character(var)) stop("`var` must be a character column name")
  if (!var %in% names(sf::st_drop_geometry(sf_obj))) stop(sprintf("Column '%s' not found in shapefile", var))
  sel <- sf_obj[[var]] %in% values
  if (invert) sel <- !sel
  sf_obj[which(sel), , drop = FALSE]
}

#' Fast point-in-polygon lookup using `sf` + `data.table`
#'
#' Map a table of latitude/longitude points to attributes from a polygon
#' shapefile. Designed for speed: points are processed in chunks and the
#' spatial index on the polygons is used via `sf::st_intersects`.
#'
#' @param points_dt A `data.table` (or coercible) with longitude and latitude columns.
#' @param shapes An `sf` polygon object (e.g. as returned by `read_shapefile`).
#' @param lat Name of latitude column in `points_dt` (default `"latitude"`).
#' @param lon Name of longitude column in `points_dt` (default `"longitude"`).
#' @param return_cols Character vector of columns from `shapes` to return (default: all non-geometry columns).
#' @param chunk_size Integer number of points to process per chunk (tune for memory).
#' @param multiple If `"first"` (default) return first matching polygon per point; if `"all"` return all matches.
#' @param verbose Print progress messages if `TRUE`.
#' @return A `data.table` combining the input point columns with the requested polygon attributes (one row per input point or per match if `multiple = "all"`).
#' @export
spatial_lookup <- function(points_dt, shapes, lat = "latitude", lon = "longitude",
                           return_cols = NULL, chunk_size = 100000L,
                           multiple = c("first", "all"), verbose = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required; please install it.")
  multiple <- match.arg(multiple)
  chunk_size <- .as_positive_integer(chunk_size, "chunk_size")
  points_dt <- data.table::as.data.table(points_dt)
  if (length(lat) != 1L || length(lon) != 1L ||
      !is.character(lat) || !is.character(lon) ||
      is.na(lat) || is.na(lon) || lat == lon ||
      !all(c(lat, lon) %in% names(points_dt)))
    stop("Latitude/longitude columns not found or not distinct in points_dt", call. = FALSE)
  if (!is.numeric(points_dt[[lat]]) || !is.numeric(points_dt[[lon]]))
    stop("Latitude/longitude columns must be numeric", call. = FALSE)
  if (!inherits(shapes, "sf") || is.na(sf::st_crs(shapes)))
    stop("'shapes' must be an sf object with a known CRS", call. = FALSE)

  shapes_dt <- data.table::as.data.table(sf::st_drop_geometry(shapes))
  if (is.null(return_cols)) return_cols <- names(shapes_dt)
  if (!is.character(return_cols) || anyNA(return_cols) ||
      anyDuplicated(return_cols) || !all(return_cols %in% names(shapes_dt)))
    stop("'return_cols' must name distinct columns in shapes", call. = FALSE)
  if (any(return_cols %in% names(points_dt)))
    stop("Requested shape columns already exist in points_dt: ",
         paste(intersect(return_cols, names(points_dt)), collapse = ", "), call. = FALSE)
  if (length(return_cols) == 0L && multiple == "first")
    return(data.table::copy(points_dt))
  n <- nrow(points_dt)
  if (n == 0L)
    return(cbind(data.table::copy(points_dt), shapes_dt[0L, return_cols, with = FALSE]))

  chunk_starts <- seq.int(1L, n, by = chunk_size)
  out_list <- vector("list", length(chunk_starts))
  shapes_crs <- sf::st_crs(shapes)

  for (i in seq_along(chunk_starts)) {
    start <- chunk_starts[i]
    end <- min(n, as.double(start) + chunk_size - 1L)
    chunk <- points_dt[start:end]
    valid <- which(is.finite(chunk[[lon]]) & is.finite(chunk[[lat]]) &
                     abs(chunk[[lon]]) <= 180 & abs(chunk[[lat]]) <= 90)
    intersections <- vector("list", nrow(chunk))
    if (length(valid) > 0L) {
      coordinates <- data.frame(lng = chunk[[lon]][valid], lat = chunk[[lat]][valid])
      pts_sf <- sf::st_as_sf(coordinates, coords = c("lng", "lat"), crs = 4326)
      if (!identical(sf::st_crs(pts_sf), shapes_crs))
        pts_sf <- sf::st_transform(pts_sf, shapes_crs)
      intersections[valid] <- sf::st_intersects(pts_sf, shapes, sparse = TRUE)
    }

    # Index the original attribute columns so NA rows retain character, Date,
    # factor and integer types. Chunks already follow input order, so no
    # temporary point-ID column or final sort is necessary.
    if (multiple == "first") {
      shape_rows <- vapply(intersections,
        function(hits) if (length(hits)) hits[1L] else NA_integer_, integer(1L))
      point_rows <- seq_len(nrow(chunk))
    } else {
      intersections[lengths(intersections) == 0L] <- list(NA_integer_)
      point_rows <- rep(seq_len(nrow(chunk)), lengths(intersections))
      shape_rows <- unlist(intersections, use.names = FALSE)
    }
    out_list[[i]] <- if (length(return_cols)) {
      cbind(chunk[point_rows], shapes_dt[shape_rows, return_cols, with = FALSE])
    } else {
      chunk[point_rows]
    }
    if (isTRUE(verbose)) message(sprintf("Processed points %d..%d", start, end))
  }
  data.table::rbindlist(out_list, use.names = TRUE)
}

#' Plot polygon boundaries and a heatmap of latitude/longitude points
#'
#' Draws polygon boundaries (optionally simplified) and overlays a 2D density
#' heatmap of the provided points. Uses `ggplot2` + `sf` for fast, static
#' plotting. For very large point sets, consider pre-aggregating or using
#' a smaller `chunk_size` when performing lookups.
#'
#' @param shapes An `sf` polygon object.
#' @param points_dt Optional data.frame / data.table of points with latitude/longitude.
#' @param lat Name of latitude column in `points_dt`.
#' @param lon Name of longitude column in `points_dt`.
#' @param simplify_tolerance If provided (numeric), geometries are simplified with this tolerance.
#' @param bins Number of grid cells for density estimation (higher = finer).
#' @param alpha Alpha for the density raster.
#' @param palette Color palette function (defaults to `viridisLite::viridis`).
#' @param verbose If `TRUE`, report geometry simplification.
#' @param use_leaflet If `TRUE`, render an interactive `leaflet` map using `leaflet.extras::addHeatmap`.
#' @param heatmap_options A named list of options passed to the leaflet heatmap (e.g. `radius`, `blur`, `max`, `minOpacity`).
#' @return A `ggplot` object (when `use_leaflet = FALSE`) or a `leaflet` map object (when `use_leaflet = TRUE`).
#' @export
plot_boundaries_heatmap <- function(shapes, points_dt = NULL, lat = "latitude", lon = "longitude",
                                    simplify_tolerance = NULL, bins = 150, alpha = 0.6,
                                    palette = viridisLite::viridis, verbose = TRUE,
                                    use_leaflet = FALSE, heatmap_options = list()) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required; please install it.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required; please install it.")

  shp <- shapes
  if (!is.null(simplify_tolerance)) {
    if (isTRUE(verbose)) message(sprintf("Simplifying geometries with tolerance %s", simplify_tolerance))
    shp <- sf::st_simplify(shp, dTolerance = simplify_tolerance)
  }

  shp_plot <- sf::st_transform(shp, 4326)

  if (isTRUE(use_leaflet)) {
    if (!requireNamespace("leaflet", quietly = TRUE)) stop("Package 'leaflet' is required; please install it.")
    if (!requireNamespace("leaflet.extras", quietly = TRUE)) stop("Package 'leaflet.extras' is required; please install it.")

    # prepare points
    if (is.null(points_dt)) {
      m <- leaflet::addTiles(leaflet::leaflet())
      m <- leaflet::addPolygons(m, data = shp_plot, fill = FALSE, color = "black", weight = 1)
      return(m)
    }

    pts_df <- data.table::as.data.table(points_dt)
    if (!(lat %in% names(pts_df) && lon %in% names(pts_df))) stop("Latitude/longitude columns not found in points_dt")
    pts_df2 <- data.table::copy(pts_df)
    # drop NA coords
    pts_df2 <- pts_df2[!is.na(get(lat)) & !is.na(get(lon))]
    if (nrow(pts_df2) == 0L) {
      m <- leaflet::addTiles(leaflet::leaflet())
      m <- leaflet::addPolygons(m, data = shp_plot, fill = FALSE, color = "black", weight = 1)
      return(m)
    }

    # normalize column names to lat/lng for leaflet formula interface
    setnames(pts_df2, old = c(lon, lat), new = c("lng", "lat"))

    # heatmap options defaults
    hm_def <- list(radius = 15, blur = 20, max = 1, minOpacity = 0.5)
    hm <- utils::modifyList(hm_def, heatmap_options)

    m <- leaflet::leaflet(data = pts_df2)
    m <- leaflet::addProviderTiles(m, leaflet::providers$CartoDB.Positron)
    m <- leaflet::addPolygons(m, data = shp_plot, fill = FALSE, color = "black", weight = 1)

    # use addHeatmap from leaflet.extras
    m <- leaflet.extras::addHeatmap(m, lng = ~lng, lat = ~lat,
                                         blur = hm$blur, max = hm$max,
                                         radius = hm$radius, minOpacity = hm$minOpacity)
    return(m)
  }

  # ggplot fallback
  p <- ggplot2::ggplot() + ggplot2::geom_sf(data = shp_plot, fill = NA, colour = "black", size = 0.25)

  if (!is.null(points_dt)) {
    pts_df <- data.table::as.data.table(points_dt)
    if (!(lat %in% names(pts_df) && lon %in% names(pts_df))) stop("Latitude/longitude columns not found in points_dt")
    p <- p + ggplot2::stat_density_2d(data = pts_df, ggplot2::aes_string(x = lon, y = lat, fill = "..density.."), geom = "raster", contour = FALSE, n = bins, alpha = alpha) +
      ggplot2::scale_fill_gradientn(colours = palette(256)) +
      ggplot2::geom_point(data = pts_df, ggplot2::aes_string(x = lon, y = lat), colour = "red", alpha = 0.4, size = 0.5)
  }

  p
}
