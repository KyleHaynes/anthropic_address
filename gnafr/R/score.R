# Weights must sum to 100
.WEIGHTS <- list(
  postcode     = 25L,
  suburb       = 20L,
  street_name  = 25L,
  street_type  = 10L,
  number       = 12L,
  flat         = 8L
)

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
#' @param pairs data.table of candidate pairs (modified in-place).
#' @return The same data.table with added columns \code{score_*} and
#'   \code{total_score}.
#' @noRd
.score_pairs <- function(pairs) {

  # --- Postcode (25 pts) ---------------------------------------------------
  pairs[, score_postcode := fifelse(
    !is.na(in_postcode) & !is.na(postcode) & in_postcode == postcode,
    .WEIGHTS$postcode, 0L
  )]

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
  pairs[, score_suburb := as.integer(round(.WEIGHTS$suburb * jw_suburb))]

  # --- Street name (25 pts) ------------------------------------------------
  jw_street <- rep(0, nrow(pairs))
  ok <- !is.na(pairs$in_street_name) & !is.na(pairs$street_name)
  if (any(ok)) {
    jw_street[ok] <- 1 - stringdist::stringdist(
      pairs$in_street_name[ok], pairs$street_name[ok],
      method = "jw", p = 0.1
    )
  }
  pairs[, score_street_name := as.integer(round(.WEIGHTS$street_name * jw_street))]

  # --- Street type (10 pts) ------------------------------------------------
  pairs[, score_street_type := {
    both_na <- is.na(in_street_type) & is.na(street_type)
    one_na  <- xor(is.na(in_street_type), is.na(street_type))
    matched <- !is.na(in_street_type) & !is.na(street_type) & in_street_type == street_type
    fifelse(both_na, .WEIGHTS$street_type,
    fifelse(matched, .WEIGHTS$street_type,
    fifelse(one_na,  as.integer(.WEIGHTS$street_type * 0.5), 0L)))
  }]

  # --- Street number (12 pts) ----------------------------------------------
  pairs[, score_number := {
    exact    <- !is.na(in_number_first) & in_number_first == number_first
    in_range <- !is.na(in_number_first) & !is.na(number_last) &
                in_number_first >= number_first & in_number_first <= number_last
    fifelse(is.na(in_number_first), 0L,
    fifelse(exact, .WEIGHTS$number,
    fifelse(in_range, as.integer(.WEIGHTS$number * 0.7), 0L)))
  }]

  # --- Flat / unit (8 pts) -------------------------------------------------
  pairs[, score_flat := {
    in_f  <- trimws(fifelse(is.na(in_flat_number),  "", in_flat_number))
    gnaf_f <- trimws(fifelse(is.na(flat_number), "", flat_number))
    both_absent <- in_f == "" & gnaf_f == ""
    matched     <- in_f != "" & gnaf_f != "" & in_f == gnaf_f
    fifelse(both_absent | matched, .WEIGHTS$flat, 0L)
  }]

  # --- Total ---------------------------------------------------------------
  pairs[, total_score := score_postcode + score_suburb + score_street_name +
                         score_street_type + score_number + score_flat]

  pairs
}
