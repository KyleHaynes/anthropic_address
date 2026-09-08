lookup_shapes <- function() {
  polygon <- function(xmin, xmax) sf::st_polygon(list(matrix(
    c(xmin, 0, xmax, 0, xmax, 2, xmin, 2, xmin, 0), ncol = 2L, byrow = TRUE)))
  sf::st_sf(
    area = c("west", "east"), code = c(7L, 8L),
    date = as.Date(c("2026-01-01", "2026-02-01")),
    category = factor(c("A", "B")),
    geometry = sf::st_sfc(polygon(0, 2), polygon(1, 3), crs = 4326)
  )
}

test_that("spatial first matches preserve attribute types and caller columns", {
  shapes <- lookup_shapes()
  points <- data.table::data.table(
    .point_id = letters[1:5], longitude = c(NA, 0.5, 1.5, 4, Inf), latitude = 1
  )
  original <- data.table::copy(points)
  out <- spatial_lookup(points, shapes, chunk_size = 1L, verbose = FALSE)
  expect_identical(points, original)
  expect_identical(out$.point_id, points$.point_id)
  expect_identical(out$area, c(NA_character_, "west", "west", NA_character_, NA_character_))
  expect_identical(out$code, c(NA_integer_, 7L, 7L, NA_integer_, NA_integer_))
  expect_identical(out$date, shapes$date[c(NA_integer_, 1L, 1L, NA_integer_, NA_integer_)])
  expect_identical(out$category, shapes$category[c(NA_integer_, 1L, 1L, NA_integer_, NA_integer_)])
})

test_that("spatial all matches retain missing and unmatched points in order", {
  points <- data.table::data.table(id = 1:3, longitude = c(1.5, NA, 4), latitude = 1)
  out <- spatial_lookup(points, lookup_shapes(), multiple = "all", verbose = FALSE)
  expect_identical(out$id, c(1L, 1L, 2L, 3L))
  expect_identical(out$area, c("west", "east", NA_character_, NA_character_))
  expect_identical(out$code, c(7L, 8L, NA_integer_, NA_integer_))
  points_only <- spatial_lookup(points, lookup_shapes(), multiple = "all",
                               return_cols = character(), verbose = FALSE)
  expect_identical(points_only$id, out$id)
  expect_named(points_only, names(points))
})

test_that("empty spatial lookups preserve their schema and validate chunk sizes", {
  points <- data.table::data.table(id = integer(), longitude = numeric(), latitude = numeric())
  shapes <- lookup_shapes()
  out <- spatial_lookup(points, shapes, verbose = FALSE)
  expect_named(out, c(names(points), "area", "code", "date", "category"))
  expect_identical(out$code, integer())
  expect_identical(out$date, as.Date(character()))
  expect_error(spatial_lookup(points, shapes, chunk_size = 0), "chunk_size")
  expect_error(spatial_lookup(points, shapes, chunk_size = 1.5), "chunk_size")
})

test_that("the interactive boundaries plot works without an attached pipe package", {
  out <- plot_boundaries_heatmap(lookup_shapes(), use_leaflet = TRUE, verbose = FALSE)
  expect_s3_class(out, "leaflet")
})
