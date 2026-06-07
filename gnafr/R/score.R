# Weights must sum to 100
# .WEIGHTS <- list(
#   postcode     = 25L,
#   suburb       = 20L,
#   street_name  = 25L,
#   street_type  = 10L,
#   number       = 12L,
#   flat         = 8L
# )

.WEIGHTS <- list(postcode = 20L, suburb = 15L, street_name = 40L, street_type = 10L, number = 10L, flat = 5L)


.default_match_weights <- function() {
  as.list(.WEIGHTS)
}

.validate_match_weights <- function(weights) {
  required_names <- names(.WEIGHTS)

  if (!is.list(weights) || is.null(names(weights))) {
    stop("'weights' must be a named list")
  }
  if (!setequal(names(weights), required_names)) {
    stop(
      "'weights' must be a named list with exactly these entries: ",
      paste(required_names, collapse = ", ")
    )
  }

  weights <- weights[required_names]
  weight_values <- unlist(weights, use.names = TRUE)
  if (!is.numeric(weight_values) || anyNA(weight_values)) {
    stop("'weights' values must all be numeric and non-missing")
  }
  if (any(weight_values < 0)) {
    stop("'weights' values must be non-negative")
  }
  if (!isTRUE(all.equal(sum(weight_values), 100, tolerance = 1e-8))) {
    stop("'weights' must sum to 100")
  }

  lapply(weights, as.numeric)
}

#' Score candidate pairs
#'
#' Operates on a data.table that has been produced by joining the parsed inputs
#' with GNAF candidates.  Adds score columns in-place and returns the table.
#'
#' Expected columns from the parsed side (prefixed \code{in_}):
#'   in_postcode, in_locality, in_street_name, in_street_type,
#'   in_number_first, in_flat_number
#'
#' Expected columns from the GNAF side (no prefix):
#'   postcode, locality_name, street_name, street_type,
#'   number_first, number_last, flat_number
#'

# Generates DuckDB SQL CASE expressions for each score component.
# i / g are the table aliases for inputs and gnaf candidates respectively.
.score_sql_exprs <- function(weights, i = "i", g = "g") {
  w_pc  <- as.integer(round(weights$postcode))
  w_sub <- weights$suburb
  w_sn  <- weights$street_name
  w_st  <- weights$street_type
  w_num <- weights$number
  w_fl  <- weights$flat

  list(
    score_postcode = sprintf(
      "CASE WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND %s.in_postcode = %s.postcode THEN %d WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND ABS(CAST(%s.in_postcode AS INTEGER) - CAST(%s.postcode AS INTEGER)) = 1 THEN CAST(ROUND(%d * 0.7) AS INTEGER) WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND ABS(CAST(%s.in_postcode AS INTEGER) - CAST(%s.postcode AS INTEGER)) = 2 THEN CAST(ROUND(%d * 0.4) AS INTEGER) ELSE 0 END",
      i, g, i, g, w_pc,
      i, g, i, g, w_pc,
      i, g, i, g, w_pc
    ),
    score_suburb = sprintf(
      "CASE WHEN %s.in_locality IS NOT NULL AND %s.locality_name IS NOT NULL THEN CAST(ROUND(%g * jaro_winkler_similarity(%s.in_locality, %s.locality_name)) AS INTEGER) ELSE 0 END",
      i, g, w_sub, i, g
    ),
    score_street_name = sprintf(
      "CASE WHEN %s.in_street_name IS NOT NULL AND %s.street_name IS NOT NULL THEN CAST(ROUND(%g * jaro_winkler_similarity(%s.in_street_name, %s.street_name)) AS INTEGER) ELSE 0 END",
      i, g, w_sn, i, g
    ),
    score_street_type = sprintf(
      "CASE WHEN (%s.in_street_type IS NULL AND %s.street_type IS NULL) OR %s.in_street_type = %s.street_type THEN %d WHEN (%s.in_street_type IS NULL) != (%s.street_type IS NULL) THEN %d ELSE %d END",
      i, g, i, g, as.integer(round(w_st)),
      i, g, as.integer(round(w_st * 0.5)),
      as.integer(round(w_st * 0.4))
    ),
    score_number = sprintf(paste0(
      "CASE",
      " WHEN %s.in_number_first IS NULL THEN 0",
      " WHEN %s.in_number_suffix IS NOT NULL",
      "      AND (%s.in_number_first = %s.number_first OR %s.number_first IS NULL)",
      "      AND starts_with(%s.address_label, CAST(%s.in_number_first AS VARCHAR) || %s.in_number_suffix || ' ')",
      "      THEN %d",
      " WHEN %s.in_number_suffix IS NOT NULL AND %s.in_number_first = %s.number_first THEN 0",
      " WHEN %s.in_number_suffix IS NULL AND %s.in_number_first = %s.number_first THEN %d",
      " WHEN %s.in_number_suffix IS NULL AND %s.number_last IS NOT NULL",
      "      AND %s.number_first <= %s.in_number_first",
      "      AND %s.in_number_first <= %s.number_last THEN %d",
      " ELSE 0 END"
    ),
      i,             # in_number_first IS NULL
      i,             # in_number_suffix IS NOT NULL
      i, g, g,       # (in_number_first = number_first OR number_first IS NULL)
      g, i, i,       # starts_with(address_label, cast(in_number_first) || in_number_suffix || ' ')
      as.integer(round(w_num)),
      i, i, g,       # suffix present, number matches, starts_with fails → 0
      i, i, g,       # no suffix, exact
      as.integer(round(w_num)),
      i, g, g, i, i, g,  # no suffix, range
      as.integer(round(w_num * 0.7))
    ),
    score_flat = sprintf(
      "CASE WHEN (TRIM(COALESCE(%s.in_flat_number, '')) = '' AND TRIM(COALESCE(%s.flat_number, '')) = '') OR (TRIM(COALESCE(%s.in_flat_number, '')) != '' AND TRIM(COALESCE(%s.in_flat_number, '')) = TRIM(COALESCE(%s.flat_number, ''))) THEN %d ELSE 0 END",
      i, g, i, i, g, as.integer(round(w_fl))
    )
  )
}

#' @param pairs data.table of candidate pairs (modified in-place).
#' @param weights Named list of scoring weights.
#' @return The same data.table with added columns \code{score_*} and
#'   \code{total_score}.
#' @noRd
.score_pairs <- function(pairs, weights = .WEIGHTS) {

  # --- Postcode (20 pts) ---------------------------------------------------
  pairs[, score_postcode := {
    both_present <- !is.na(in_postcode) & !is.na(postcode)
    diff <- abs(as.integer(in_postcode) - as.integer(postcode))
    fifelse(!both_present,  0L,
    fifelse(diff == 0L,     as.integer(round(weights$postcode)),
    fifelse(diff == 1L,     as.integer(round(weights$postcode * 0.7)),
    fifelse(diff == 2L,     as.integer(round(weights$postcode * 0.4)), 0L))))
  }]

  # --- Suburb / locality (20 pts) ------------------------------------------
  # Jaro-Winkler similarity; NA on either side → 0
  jw_suburb <- rep(0, nrow(pairs))
  ok <- !is.na(pairs$in_locality) & !is.na(pairs$locality_name)
  if (any(ok)) {
    jw_suburb[ok] <- 1 - stringdist::stringdist(
      pairs$in_locality[ok], pairs$locality_name[ok],
      method = "jw", p = 0.1
    )
  }
  pairs[, score_suburb := as.integer(round(weights$suburb * jw_suburb))]

  # --- Street name (25 pts) ------------------------------------------------
  jw_street <- rep(0, nrow(pairs))
  ok <- !is.na(pairs$in_street_name) & !is.na(pairs$street_name)
  if (any(ok)) {
    jw_street[ok] <- 1 - stringdist::stringdist(
      pairs$in_street_name[ok], pairs$street_name[ok],
      method = "jw", p = 0.1
    )
  }
  pairs[, score_street_name := as.integer(round(weights$street_name * jw_street))]

  # --- Street type (10 pts) ------------------------------------------------
  # Partial credit (40%) when both sides supply a type but they differ — wrong
  # street type is a very common user error and shouldn't fully cancel out a
  # strong street-name match.
  pairs[, score_street_type := {
    both_na <- is.na(in_street_type) & is.na(street_type)
    one_na  <- xor(is.na(in_street_type), is.na(street_type))
    matched <- !is.na(in_street_type) & !is.na(street_type) & in_street_type == street_type
    fifelse(both_na | matched, as.integer(round(weights$street_type)),
    fifelse(one_na,            as.integer(round(weights$street_type * 0.5)),
                               as.integer(round(weights$street_type * 0.4))))
  }]

  # --- Street number (12 pts) ----------------------------------------------
  pairs[, score_number := {
    exact    <- !is.na(in_number_first) & in_number_first == number_first
    in_range <- !is.na(in_number_first) & !is.na(number_last) &
                in_number_first >= number_first & in_number_first <= number_last
    fifelse(is.na(in_number_first), 0L,
    fifelse(exact, as.integer(round(weights$number)),
    fifelse(in_range, as.integer(round(weights$number * 0.7)), 0L)))
  }]

  # --- Flat / unit (8 pts) -------------------------------------------------
  pairs[, score_flat := {
    in_f  <- trimws(fifelse(is.na(in_flat_number),  "", in_flat_number))
    gnaf_f <- trimws(fifelse(is.na(flat_number), "", flat_number))
    both_absent <- in_f == "" & gnaf_f == ""
    matched     <- in_f != "" & gnaf_f != "" & in_f == gnaf_f
    fifelse(both_absent | matched, as.integer(round(weights$flat)), 0L)
  }]

  # --- Total ---------------------------------------------------------------
  pairs[, total_score := score_postcode + score_suburb + score_street_name +
                         score_street_type + score_number + score_flat]

  pairs
}
