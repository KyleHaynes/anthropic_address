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

# Fallback operator used across the package. Deliberately broader than the
# usual null-coalesce: length-0 vectors and empty strings also fall through.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (is.character(x) && !nzchar(x))) y else x
}

#' Normalize a raw address string for parsing
#' @noRd
.normalize_addr <- function(x) {
  x <- stringi::stri_trans_toupper(stringi::stri_trim_both(x))
  x <- stringi::stri_replace_all_fixed(x, ",", " ")
  x <- stringi::stri_replace_all_fixed(x, ".", " ")
  x <- stringi::stri_replace_all_regex(x, "\\s+", " ")
  x <- .fix_glued_number_letters(x)
  stringi::stri_trim_both(x)
}

# A number directly followed by 2+ letters with no space (e.g. "25ST JAMES
# CR") is virtually always a missing space rather than an intentional token —
# the only legitimate no-space numeric suffix in AU addresses is a single
# trailing letter (e.g. "190A"). The one ambiguous case is an ordinal numeral
# ("1ST", "3RD", "12TH" used as a street name, e.g. "5 1ST AVE"): we leave
# those glued whenever the letters are the grammatically correct ordinal
# suffix for that number, and only insert a space otherwise.
.fix_glued_number_letters <- function(x) {
  glue_re <- "(\\d+)([A-Z]{2,})"
  needs <- stringi::stri_detect_regex(x, glue_re)
  if (!any(needs, na.rm = TRUE)) return(x)
  idx <- which(needs)
  x[idx] <- vapply(x[idx], .fix_one_glued_number, character(1L), USE.NAMES = FALSE)
  x
}

.fix_one_glued_number <- function(s) {
  m <- gregexpr("(\\d+)([A-Z]{2,})", s, perl = TRUE)[[1L]]
  if (m[1L] < 0L) return(s)
  caps <- attr(m, "capture.start")
  lens <- attr(m, "capture.length")
  # Walk matches right-to-left so earlier insertions don't shift later positions.
  for (i in rev(seq_len(length(m)))) {
    l_start <- caps[i, 2L]
    l_len   <- lens[i, 2L]
    digits  <- substr(s, caps[i, 1L], caps[i, 1L] + lens[i, 1L] - 1L)
    letters <- substr(s, l_start, l_start + l_len - 1L)
    if (!.is_ordinal_suffix(digits, letters)) {
      s <- paste0(substr(s, 1L, l_start - 1L), " ", substr(s, l_start, nchar(s)))
    }
  }
  s
}

.is_ordinal_suffix <- function(digits, letters) {
  n <- suppressWarnings(as.integer(digits))
  if (is.na(n)) return(FALSE)
  last_two <- n %% 100L
  last_one <- n %% 10L
  expected <- if (last_two %in% c(11L, 12L, 13L)) "TH"
              else if (last_one == 1L) "ST"
              else if (last_one == 2L) "ND"
              else if (last_one == 3L) "RD"
              else "TH"
  identical(letters, expected)
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
