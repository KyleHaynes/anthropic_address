#' gnafr: Australian Address Matching Using GNAF
#'
#' Fast, fuzzy Australian address matching against the Geocoded National
#' Address File (GNAF). Supports bulk lookup (100k+), a confidence scoring
#' algorithm, DuckDB-backed storage, and custom address additions.
#'
#' @docType package
#' @name gnafr-package
#' @import data.table
#' @importFrom cli cli_alert_danger cli_alert_info cli_alert_success cli_alert_warning cli_h1 cli_li cli_text col_blue col_cyan col_green col_magenta col_yellow plural
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbExistsTable dbGetQuery dbWriteTable
#' @importFrom duckdb duckdb duckdb_register duckdb_unregister
#' @importFrom stringdist stringdist
"_PACKAGE"
