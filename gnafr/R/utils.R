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

.street_type_case_sql <- function(col_expr) {
  m <- .get_street_type_map()
  when_clauses <- paste(
    sprintf("WHEN '%s' THEN '%s'", names(m), m),
    collapse = " "
  )
  sprintf("CASE %s %s ELSE %s END", col_expr, when_clauses, col_expr)
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

# Convert an alias_types argument to a SQL WHERE fragment.
# NULL  → NULL (caller skips the filter entirely)
# NA    → g.alias_type IS NULL
# "foo" → g.alias_type IN ('foo')
# c(NA, "foo") → (g.alias_type IS NULL OR g.alias_type IN ('foo'))
# character(0) → 1 = 0  (match nothing — caller should guard against this)
.alias_type_sql <- function(alias_types, alias = "g") {
  if (is.null(alias_types)) return(NULL)

  has_na <- any(is.na(alias_types))
  non_na <- alias_types[!is.na(alias_types)]

  parts <- character(0L)
  if (has_na)
    parts <- c(parts, sprintf("%s.alias_type IS NULL", alias))
  if (length(non_na) > 0L) {
    quoted <- paste0("'", gsub("'", "''", non_na), "'", collapse = ", ")
    parts  <- c(parts, sprintf("%s.alias_type IN (%s)", alias, quoted))
  }

  if (length(parts) == 0L) return("1 = 0")
  if (length(parts) == 1L) return(parts)
  sprintf("(%s)", paste(parts, collapse = " OR "))
}
