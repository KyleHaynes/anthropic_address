# Weights must sum to 100.
.WEIGHTS <- list(postcode = 20L, suburb = 15L, street_name = 40L, street_type = 10L, number = 10L, flat = 5L)


.default_match_weights <- function() {
  as.list(.WEIGHTS)
}

.validate_match_weights <- function(weights) {
  required_names <- names(.WEIGHTS)

  if (!is.list(weights) || is.null(names(weights))) {
    stop("'weights' must be a named list")
  }
  if (anyDuplicated(names(weights)) || !setequal(names(weights), required_names)) {
    stop(
      "'weights' must be a named list with exactly these entries: ",
      paste(required_names, collapse = ", ")
    )
  }

  weights <- weights[required_names]
  if (!all(vapply(weights, function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x)
  }, logical(1L)))) {
    stop("'weights' values must each be one finite number", call. = FALSE)
  }
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

#' @noRd
# Generates DuckDB SQL CASE expressions for each score component.
# i / g are the table aliases for inputs and gnaf candidates respectively.
.score_sql_exprs <- function(weights, i = "i", g = "g",
                             suburb_similarity = NULL,
                             street_similarity = NULL) {
  w_pc  <- as.integer(round(weights$postcode))
  w_sub <- weights$suburb
  w_sn  <- weights$street_name
  w_st  <- weights$street_type
  w_num <- weights$number
  w_fl  <- weights$flat
  if (is.null(suburb_similarity)) {
    suburb_similarity <- sprintf(
      "jaro_winkler_similarity(%s.in_locality, %s.locality_name)", i, g
    )
  }
  if (is.null(street_similarity)) {
    street_similarity <- sprintf(
      "jaro_winkler_similarity(%s.in_street_name, %s.street_name)", i, g
    )
  }

  # Match R rounding, including ties to even and fractional postcode weights.
  list(
    score_postcode = sprintf(
      "CASE WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND %s.in_postcode = %s.postcode THEN %d WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND ABS(CAST(%s.in_postcode AS INTEGER) - CAST(%s.postcode AS INTEGER)) = 1 THEN %d WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND ABS(CAST(%s.in_postcode AS INTEGER) - CAST(%s.postcode AS INTEGER)) = 2 THEN %d WHEN %s.in_postcode IS NOT NULL AND %s.postcode IS NOT NULL AND ABS(CAST(%s.in_postcode AS INTEGER) - CAST(%s.postcode AS INTEGER)) = 3 THEN %d ELSE 0 END",
      i, g, i, g, w_pc,
      i, g, i, g, as.integer(round(weights$postcode * 0.7)),
      i, g, i, g, as.integer(round(weights$postcode * 0.4)),
      i, g, i, g, as.integer(round(weights$postcode * 0.2))
    ),
    score_suburb = sprintf(
      "CASE WHEN %s.in_locality IS NOT NULL AND %s.locality_name IS NOT NULL THEN CAST(ROUND_EVEN(%g * %s, 0) AS INTEGER) ELSE 0 END",
      i, g, w_sub, suburb_similarity
    ),
    score_street_name = sprintf(
      "CASE WHEN %s.in_street_name IS NOT NULL AND %s.street_name IS NOT NULL THEN CAST(ROUND_EVEN(%g * %s, 0) AS INTEGER) ELSE 0 END",
      i, g, w_sn, street_similarity
    ),
    score_street_type = sprintf(
      "CASE WHEN (%s.in_street_type IS NULL AND %s.street_type IS NULL) OR %s.in_street_type = %s.street_type THEN %d WHEN (%s.in_street_type IS NULL) != (%s.street_type IS NULL) THEN %d ELSE %d END",
      i, g, i, g, as.integer(round(w_st)),
      i, g, as.integer(round(w_st * 0.5)),
      as.integer(round(w_st * 0.4))
    ),
    score_number = sprintf(paste0(
      "CASE",
      " WHEN TRIM(COALESCE(%s.in_lot_number, '')) != ''",
      "      AND TRIM(COALESCE(%s.in_lot_number, '')) = TRIM(COALESCE(%s.lot_number, '')) THEN %d",
      " WHEN TRIM(COALESCE(%s.in_lot_number, '')) != '' THEN 0",
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
      i, i, g, as.integer(round(w_num)),
      i,
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
    score_flat = sprintf(paste0(
      "CASE",
      " WHEN TRIM(COALESCE(%s.in_flat_number, '')) = ''",
      "  AND TRIM(COALESCE(%s.flat_number, '')) = ''",
      "  AND TRIM(COALESCE(%s.in_level_number, '')) = ''",
      "  AND TRIM(COALESCE(%s.level_number, '')) = '' THEN %d",
      " WHEN TRIM(COALESCE(%s.in_flat_number, '')) = TRIM(COALESCE(%s.flat_number, ''))",
      "  AND TRIM(COALESCE(%s.in_level_number, '')) = TRIM(COALESCE(%s.level_number, ''))",
      "  AND NOT (",
      "    (TRIM(COALESCE(%s.in_flat_type, '')) != '' AND TRIM(COALESCE(%s.flat_type, '')) != ''",
      "      AND TRIM(%s.in_flat_type) != TRIM(%s.flat_type))",
      "    OR (TRIM(COALESCE(%s.in_level_type, '')) != '' AND TRIM(COALESCE(%s.level_type, '')) != ''",
      "      AND TRIM(%s.in_level_type) != TRIM(%s.level_type))",
      "  ) THEN %d",
      " WHEN TRIM(COALESCE(%s.in_flat_number, '')) = TRIM(COALESCE(%s.flat_number, ''))",
      "  AND TRIM(COALESCE(%s.in_level_number, '')) = TRIM(COALESCE(%s.level_number, '')) THEN %d",
      " ELSE 0 END"
    ),
      i, g, i, g, as.integer(round(w_fl)),
      i, g, i, g,
      i, g, i, g,
      i, g, i, g,
      as.integer(round(w_fl)),
      i, g, i, g, as.integer(round(w_fl * 0.5))
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
    fifelse(diff == 2L,     as.integer(round(weights$postcode * 0.4)),
    fifelse(diff == 3L,     as.integer(round(weights$postcode * 0.2)), 0L)))))
  }]

  # --- Suburb / locality (15 pts) ------------------------------------------
  # Jaro-Winkler similarity; NA on either side → 0
  jw_suburb <- rep(0, nrow(pairs))
  ok <- !is.na(pairs$in_locality) & !is.na(pairs$locality_name)
  if (any(ok)) {
    jw_suburb[ok] <- fast.string::jaro_winkler(
      pairs$in_locality[ok], pairs$locality_name[ok], p = 0.1
    )
  }
  pairs[, score_suburb := as.integer(round(weights$suburb * jw_suburb))]

  # --- Street name (40 pts) ------------------------------------------------
  jw_street <- rep(0, nrow(pairs))
  ok <- !is.na(pairs$in_street_name) & !is.na(pairs$street_name)
  if (any(ok)) {
    jw_street[ok] <- fast.string::jaro_winkler(
      pairs$in_street_name[ok], pairs$street_name[ok], p = 0.1
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

  # --- Street number or lot (10 pts) ---------------------------------------
  # Mirrors .score_sql_exprs score_number, including the number-suffix rule: a
  # parsed suffix (e.g. "190A") only earns credit when the candidate's
  # address_label starts with "<number><suffix> "; otherwise it scores 0.
  # Callers without those columns (or with all-NA suffixes) get the plain
  # exact/range behaviour.
  in_sfx <- if ("in_number_suffix" %in% names(pairs)) pairs$in_number_suffix
            else rep(NA_character_, nrow(pairs))
  lbl    <- if ("address_label" %in% names(pairs)) pairs$address_label
            else rep(NA_character_, nrow(pairs))
  sfx_lbl_ok <- !is.na(lbl) & !is.na(pairs$in_number_first) & !is.na(in_sfx) &
    startsWith(lbl, paste0(pairs$in_number_first, in_sfx, " "))

  in_lot <- if ("in_lot_number" %in% names(pairs)) pairs$in_lot_number
            else rep(NA_character_, nrow(pairs))
  gnaf_lot <- if ("lot_number" %in% names(pairs)) pairs$lot_number
              else rep(NA_character_, nrow(pairs))
  pairs[, score_number := {
    has_lot <- !is.na(in_lot) & nzchar(trimws(in_lot))
    lot_match <- has_lot & !is.na(gnaf_lot) & trimws(in_lot) == trimws(gnaf_lot)
    has_sfx  <- !is.na(in_sfx)
    exact    <- !is.na(in_number_first) & !is.na(number_first) &
                in_number_first == number_first
    in_range <- !is.na(in_number_first) & !is.na(number_first) &
                !is.na(number_last) &
                in_number_first >= number_first & in_number_first <= number_last
    fifelse(has_lot, fifelse(lot_match, as.integer(round(weights$number)), 0L),
    fifelse(is.na(in_number_first), 0L,
    fifelse(has_sfx & (exact | is.na(number_first)) & sfx_lbl_ok,
            as.integer(round(weights$number)),
    fifelse(has_sfx, 0L,
    fifelse(exact, as.integer(round(weights$number)),
    fifelse(in_range, as.integer(round(weights$number * 0.7)), 0L))))))
  }]

  # --- Composite flat / level component (5 pts) ----------------------------
  value_or_empty <- function(column) {
    trimws(fifelse(is.na(column), "", as.character(column)))
  }
  in_level_number <- if ("in_level_number" %in% names(pairs))
    pairs$in_level_number else rep(NA_character_, nrow(pairs))
  level_number <- if ("level_number" %in% names(pairs))
    pairs$level_number else rep(NA_character_, nrow(pairs))
  in_flat_type <- if ("in_flat_type" %in% names(pairs))
    pairs$in_flat_type else rep(NA_character_, nrow(pairs))
  flat_type <- if ("flat_type" %in% names(pairs))
    pairs$flat_type else rep(NA_character_, nrow(pairs))
  in_level_type <- if ("in_level_type" %in% names(pairs))
    pairs$in_level_type else rep(NA_character_, nrow(pairs))
  level_type <- if ("level_type" %in% names(pairs))
    pairs$level_type else rep(NA_character_, nrow(pairs))

  pairs[, score_flat := {
    in_f <- value_or_empty(in_flat_number)
    gnaf_f <- value_or_empty(flat_number)
    in_l <- value_or_empty(in_level_number)
    gnaf_l <- value_or_empty(level_number)
    all_absent <- in_f == "" & gnaf_f == "" & in_l == "" & gnaf_l == ""
    ids_match <- in_f == gnaf_f & in_l == gnaf_l
    in_ft <- value_or_empty(in_flat_type)
    gnaf_ft <- value_or_empty(flat_type)
    in_lt <- value_or_empty(in_level_type)
    gnaf_lt <- value_or_empty(level_type)
    type_conflict <- (in_ft != "" & gnaf_ft != "" & in_ft != gnaf_ft) |
      (in_lt != "" & gnaf_lt != "" & in_lt != gnaf_lt)
    fifelse(all_absent | (ids_match & !type_conflict),
      as.integer(round(weights$flat)),
      fifelse(ids_match, as.integer(round(weights$flat * 0.5)), 0L)
    )
  }]

  # --- Total ---------------------------------------------------------------
  pairs[, total_score := score_postcode + score_suburb + score_street_name +
                         score_street_type + score_number + score_flat]

  pairs
}
