#' gnafr: Australian Address Matching Using GNAF
#'
#' Fast, fuzzy Australian address matching against the Geocoded National
#' Address File (GNAF). Supports bulk lookup (100k+), a confidence scoring
#' algorithm, DuckDB-backed storage, and custom address additions.
#'
#' @docType package
#' @name gnafr-package
#' @import data.table
#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_success cli_alert_warning cli_h1 cli_li cli_text col_blue col_cyan col_green col_magenta col_yellow
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbExistsTable dbGetQuery dbWriteTable
#' @importFrom duckdb duckdb duckdb_register duckdb_unregister
#' @importFrom stringdist stringdist
"_PACKAGE"

utils::globalVariables(c(
  ".", "address_detail_pid",
  "address_label", "alt_postcode", "best_score", "candidate_count",
  "comparison", "complete", "date_created", "flat_number", "has_diff", "i.input_id",
  "i.principal_address_label", "i.principal_longitude", "i.principal_latitude",
  "i.principal_locality_name", "i.principal_postcode",
  "in_building_name", "in_flat_number", "in_flat_type", "in_locality",
  "in_number_first", "in_number_last", "in_postcode", "in_state",
  "in_street_name", "in_street_type", "input_id", "input_raw",
  "input_standardised", "jaccard_score", "jarowinkler_score", "lbl_key",
  "left", "match_input_id", "match_rank", "match_status", "matched",
  "number_first", "number_last", "perturbations", "postcode",
  "retained_count", "right", "score_flat", "score_number",
  "score_postcode", "score_street_name", "score_street_type",
  "score_suburb", "simulated_address", "street_type", "text_similarity",
  "total_score"
))
