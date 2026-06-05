#' Connect to a gnafr DuckDB database
#'
#' @param path Path to the DuckDB file. Pass ":memory:" for an in-memory DB.
#' @param read_only Open in read-only mode.
#' @return A DBI connection object.
#' @export
gnaf_connect <- function(path, read_only = FALSE) {
  DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)
}

#' Disconnect from a gnafr database
#'
#' @param con DBI connection returned by \code{gnaf_connect}.
#' @export
gnaf_disconnect <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
}

#' Initialise the gnafr schema
#'
#' Creates the \code{gnaf_addresses} and \code{custom_addresses} tables and
#' their indexes. Safe to call on an existing database — uses
#' \code{CREATE TABLE IF NOT EXISTS}.
#'
#' @param con DBI connection.
#' @export
gnaf_init <- function(con) {
  col_ddl <- "
    address_detail_pid VARCHAR PRIMARY KEY,
    address_label      VARCHAR,
    address_site_name  VARCHAR,
    building_name      VARCHAR,
    flat_type          VARCHAR,
    flat_number        VARCHAR,
    level_type         VARCHAR,
    level_number       VARCHAR,
    number_first       INTEGER,
    number_last        INTEGER,
    lot_number         VARCHAR,
    street_name        VARCHAR,
    street_type        VARCHAR,
    street_suffix      VARCHAR,
    locality_name      VARCHAR,
    state              VARCHAR,
    postcode           INTEGER,
    longitude          DOUBLE,
    latitude           DOUBLE,
    source             VARCHAR,
    alias_type         VARCHAR
  "

  DBI::dbExecute(con, sprintf("CREATE TABLE IF NOT EXISTS gnaf_addresses (%s)", col_ddl))
  DBI::dbExecute(con, sprintf("CREATE TABLE IF NOT EXISTS custom_addresses (%s)", col_ddl))

  # Migration: add alias_type to tables created before this column existed
  for (tbl in c("gnaf_addresses", "custom_addresses")) {
    tryCatch(
      DBI::dbExecute(con, sprintf(
        "ALTER TABLE %s ADD COLUMN IF NOT EXISTS alias_type VARCHAR", tbl
      )),
      error = function(e) NULL
    )
  }

  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_gnaf_pc    ON gnaf_addresses(postcode)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_gnaf_pcnum ON gnaf_addresses(postcode, number_first)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_cust_pc    ON custom_addresses(postcode)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_cust_pcnum ON custom_addresses(postcode, number_first)")

  invisible(con)
}

#' Report row counts for gnafr tables
#'
#' @param con DBI connection.
#' @return A data.table with table name and row count.
#' @export
gnaf_status <- function(con) {
  tbls <- c("gnaf_addresses", "custom_addresses")
  rbindlist(lapply(tbls, function(t) {
    if (DBI::dbExistsTable(con, t)) {
      n <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", t))$n
    } else {
      n <- NA_integer_
    }
    data.table(table = t, rows = n)
  }))
}
