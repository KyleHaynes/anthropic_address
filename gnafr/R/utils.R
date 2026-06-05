# Cached lookup tables — loaded once per session
.gnafr_env <- new.env(parent = emptyenv())

.get_street_type_map <- function() {
  if (is.null(.gnafr_env$st_map)) {
    path <- system.file("extdata", "street_types.csv", package = "gnafr")
    dt <- fread(path)
    m <- dt$canonical
    names(m) <- dt$abbrev
    .gnafr_env$st_map <- m
  }
  .gnafr_env$st_map
}

.get_flat_type_map <- function() {
  if (is.null(.gnafr_env$ft_map)) {
    path <- system.file("extdata", "flat_types.csv", package = "gnafr")
    dt <- fread(path)
    m <- dt$canonical
    names(m) <- dt$abbrev
    .gnafr_env$ft_map <- m
  }
  .gnafr_env$ft_map
}

.build_street_type_regex <- function(st_map) {
  abbrevs <- names(st_map)
  # Longest first so regex engine doesn't short-circuit on a prefix
  abbrevs <- abbrevs[order(-nchar(abbrevs))]
  paste0("\\b(", paste(abbrevs, collapse = "|"), ")\\b")
}

#' Normalize a raw address string for parsing
#' @noRd
.normalize_addr <- function(x) {
  x <- toupper(trimws(x))
  x <- gsub(",", " ", x, fixed = TRUE)
  x <- gsub("\\.", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Normalize a street name for scoring (remove leading/trailing whitespace,
#' collapse internal spaces)
#' @noRd
.normalize_str <- function(x) {
  trimws(gsub("\\s+", " ", x))
}
