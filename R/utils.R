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

.get_level_type_map <- function() {
  if (is.null(.gnafr_env$level_map)) {
    path <- system.file("extdata", "level_types.csv", package = "gnafr")
    dt <- fread(path)
    m <- dt$canonical
    names(m) <- dt$abbrev
    .gnafr_env$level_map <- m
  }
  .gnafr_env$level_map
}

# Parser lookup maps and their derived regular expressions are immutable package
# data. Building them once per session matters for short, repeated parser calls
# and also keeps every parsing path on the same vocabulary.
.get_parser_resources <- function() {
  if (is.null(.gnafr_env$parser_resources)) {
    st_map <- .get_street_type_map()
    ft_map <- .get_flat_type_map()
    level_map <- .get_level_type_map()
    longest_first <- function(x) x[order(-nchar(x), x)]
    # G-NAF type tokens contain letters, spaces, and hyphens only; none of
    # those need escaping in the alternations below.
    regex_escape <- function(x) x
    st_keys <- longest_first(names(st_map))
    ft_keys <- longest_first(names(ft_map))
    level_keys <- longest_first(names(level_map))
    ft_alt <- paste(regex_escape(ft_keys), collapse = "|")
    level_alt <- paste(regex_escape(level_keys), collapse = "|")
    .gnafr_env$parser_resources <- list(
      st_map = st_map,
      st_regex = paste0("\\b(", paste(regex_escape(st_keys), collapse = "|"), ")\\b"),
      ft_map = ft_map,
      ft_alt = ft_alt,
      ft_re = paste0("^(", ft_alt, ")\\s+(\\d+[A-Z]?)\\s+"),
      level_map = level_map,
      level_alt = level_alt
    )
  }
  .gnafr_env$parser_resources
}

.as_positive_integer <- function(x, arg) {
  if (length(x) != 1L || !(is.numeric(x) || is.character(x))) {
    stop("'", arg, "' must be a single positive integer", call. = FALSE)
  }
  value <- suppressWarnings(as.numeric(x))
  if (!is.finite(value) || value < 1 || value > .Machine$integer.max ||
      value != trunc(value)) {
    stop("'", arg, "' must be a single positive integer", call. = FALSE)
  }
  as.integer(value)
}

#' Normalize a raw address string for parsing
#' @noRd
.normalize_addr <- function(x) {
  x <- .normalize_addr_keep_commas(x)
  x <- fast.string::fgsub(",", " ", x, fixed = TRUE)
  x <- fast.string::fgsub("\\s+", " ", x)
  fast.string::ftrimws(x)
}

# Preserve comma structure until the parser has identified the street/locality
# boundary. Other callers still receive the historical comma-free form through
# .normalize_addr().
.normalize_addr_keep_commas <- function(x) {
  x <- stringi::stri_trans_toupper(fast.string::ftrimws(x))
  x <- fast.string::fgsub(".", " ", x, fixed = TRUE)
  x <- fast.string::fgsub("\\s*,\\s*", ",", x)
  x <- fast.string::fgsub("\\s+", " ", x)
  x <- .fix_glued_number_letters(x)
  fast.string::ftrimws(x)
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
  needs <- fast.string::fgrepl(glue_re, x, perl = TRUE)
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
  fast.string::ftrimws(fast.string::fgsub("\\s+", " ", x))
}

# Convert an alias_types argument to a SQL WHERE fragment.
# NULL  → NULL (caller skips the filter entirely)
# NA    → g.alias_type IS NULL
# "foo" → g.alias_type IN ('foo')
# c(NA, "foo") → (g.alias_type IS NULL OR g.alias_type IN ('foo'))
# character(0) → 1 = 0  (match nothing — caller should guard against this)
.alias_type_sql <- function(alias_types, alias = "g") {
  if (is.null(alias_types)) return(NULL)

  if (identical(alias_types, "__GNAFR_EXACT_ALIASES__")) {
    return(sprintf(
      "%s.alias_type IS NOT NULL AND lower(%s.alias_type) <> 'street_only' AND NOT starts_with(upper(%s.alias_type), 'LOCALITY:')",
      alias, alias, alias
    ))
  }
  if (identical(alias_types, "__GNAFR_LOCALITY_ALIASES__")) {
    return(sprintf(
      "starts_with(upper(COALESCE(%s.alias_type, '')), 'LOCALITY:')",
      alias
    ))
  }

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
