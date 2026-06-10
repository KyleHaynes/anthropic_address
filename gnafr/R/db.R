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

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gnaf_locality_index (
      locality_name VARCHAR,
      postcode      INTEGER,
      state         VARCHAR,
      UNIQUE (locality_name, postcode, state)
    )
  ")

  # Migration: rebuild locality index if address data exists but the index is empty
  # (databases initialised before this feature was added)
  n_addr <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM gnaf_addresses")$n
  n_idx  <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM gnaf_locality_index")$n
  if (n_addr > 0L && n_idx == 0L) gnaf_rebuild_locality_index(con)

  # Match cache
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gnaf_match_cache (
      input_standardised VARCHAR PRIMARY KEY,
      address_detail_pid VARCHAR NOT NULL,
      total_score        INTEGER NOT NULL,
      score_postcode     INTEGER,
      score_suburb       INTEGER,
      score_street_name  INTEGER,
      score_street_type  INTEGER,
      score_number       INTEGER,
      score_flat         INTEGER,
      cached_at          TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Address label indexes — used by the exact-label first-pass in gnaf_match()
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_gnaf_label ON gnaf_addresses(address_label)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_cust_label ON custom_addresses(address_label)")

  invisible(con)
}

#' Rebuild the locality search index
#'
#' Rebuilds \code{gnaf_locality_index} from the current contents of
#' \code{gnaf_addresses} and \code{custom_addresses}.  The index is a compact
#' table of distinct \code{(locality_name, postcode, state)} tuples (~3 000 rows
#' for QLD) used by \code{gnaf_match}'s locality-fallback path to run
#' Jaro-Winkler suburb searches without scanning the full address table.
#'
#' The index is rebuilt automatically by \code{gnaf_load}, \code{gnaf_load_psv},
#' and \code{gnaf_add}.  Call this manually after bulk deletions or after
#' migrating a database created before this feature existed.
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @return Invisibly, the number of unique locality rows now in the index.
#' @export
gnaf_rebuild_locality_index <- function(con) {
  DBI::dbExecute(con, "DELETE FROM gnaf_locality_index")
  DBI::dbExecute(con, "
    INSERT INTO gnaf_locality_index
    SELECT DISTINCT locality_name, postcode, state
    FROM gnaf_addresses
    WHERE locality_name IS NOT NULL
    ON CONFLICT DO NOTHING
  ")
  if (DBI::dbExistsTable(con, "custom_addresses")) {
    n_cust <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM custom_addresses")$n
    if (n_cust > 0L)
      DBI::dbExecute(con, "
        INSERT INTO gnaf_locality_index
        SELECT DISTINCT locality_name, postcode, state
        FROM custom_addresses
        WHERE locality_name IS NOT NULL
        ON CONFLICT DO NOTHING
      ")
  }
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM gnaf_locality_index")$n
  invisible(n)
}

#' Build street-level aliases in the GNAF database
#'
#' Extracts every unique combination of \code{(street_name, street_type,
#' street_suffix, locality_name, state, postcode)} from the GNAF core data,
#' constructs a number-free address label for each, and inserts the results
#' back into \code{gnaf_addresses} with \code{alias_type = "street_only"}.
#'
#' Street aliases allow \code{gnaf_match} to return a match for inputs that
#' carry no street number, or whose number is absent from GNAF.  Numbered
#' inputs are never matched against street-only records (the pre-filter in
#' each match path requires \code{number_first} to be NULL on the input side).
#'
#' The function is idempotent: PIDs are derived from an MD5 of the key fields,
#' so re-running without \code{overwrite = TRUE} silently skips existing rows.
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @param overwrite If \code{TRUE}, removes all existing \code{street_only}
#'   aliases before rebuilding.  Default \code{FALSE}.
#' @return Invisibly, the number of street-only aliases now in the database.
#' @export
gnaf_build_street_aliases <- function(con, overwrite = FALSE) {
  if (isTRUE(overwrite)) {
    DBI::dbExecute(con,
      "DELETE FROM gnaf_addresses WHERE alias_type = 'street_only'"
    )
    message("Removed existing street_only aliases.")
  }

  DBI::dbExecute(con, "
    INSERT INTO gnaf_addresses (
      address_detail_pid,
      address_label,
      address_site_name, building_name,
      flat_type, flat_number,
      level_type, level_number,
      number_first, number_last,
      lot_number,
      street_name, street_type, street_suffix,
      locality_name, state, postcode,
      longitude, latitude,
      source, alias_type
    )
    SELECT
      'SO_' || md5(
        COALESCE(street_name,  '')  || '|' ||
        COALESCE(street_type,  '')  || '|' ||
        COALESCE(street_suffix,'')  || '|' ||
        COALESCE(locality_name,'')  || '|' ||
        COALESCE(state,        '')  || '|' ||
        COALESCE(CAST(postcode AS VARCHAR), '')
      )                                                                AS address_detail_pid,
      CONCAT_WS(' ', street_name, street_type, street_suffix) || ', ' ||
      CONCAT_WS(' ', locality_name, state, CAST(postcode AS VARCHAR)) AS address_label,
      NULL, NULL,
      NULL, NULL,
      NULL, NULL,
      NULL, NULL,
      NULL,
      street_name, street_type, street_suffix,
      locality_name, state, postcode,
      NULL, NULL,
      'gnaf', 'street_only'
    FROM (
      SELECT DISTINCT
        street_name, street_type, street_suffix,
        locality_name, state, postcode
      FROM gnaf_addresses
      WHERE source       = 'gnaf'
        AND alias_type  IS NULL
        AND street_name  IS NOT NULL
        AND locality_name IS NOT NULL
        AND postcode     IS NOT NULL
        AND state        IS NOT NULL
    ) u
    ON CONFLICT DO NOTHING
  ")

  n <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM gnaf_addresses WHERE alias_type = 'street_only'"
  )$n
  message(sprintf("Street-only aliases in database: %s", format(n, big.mark = ",")))
  invisible(n)
}

#' Canonicalize street types in an existing gnafr database
#'
#' Updates the \code{street_type} column in \code{gnaf_addresses} and
#' \code{custom_addresses} so that abbreviated forms (e.g. "RD", "AV") are
#' replaced with their canonical equivalents ("ROAD", "AVENUE").
#'
#' Call this once on databases loaded before this fix was applied.  Newly
#' loaded databases are canonicalized automatically at insert time.
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @return Invisibly, the total number of rows updated across both tables.
#' @export
gnaf_canonicalize_street_types <- function(con) {
  case_sql <- .street_type_case_sql("street_type")
  n <- 0L
  for (tbl in c("gnaf_addresses", "custom_addresses")) {
    if (DBI::dbExistsTable(con, tbl)) {
      result <- DBI::dbExecute(con, sprintf(
        "UPDATE %s SET street_type = (%s) WHERE street_type IS NOT NULL",
        tbl, case_sql
      ))
      n <- n + result
    }
  }
  message(sprintf("Updated %s rows.", format(n, big.mark = ",")))
  invisible(n)
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

#' Sample rows from gnafr database tables
#'
#' Returns a random sample of rows from each user table in the connected DuckDB
#' database. If the database has a single table, a single `data.table` is
#' returned. If it has multiple tables, the result is a named list of
#' `data.table`s keyed by table name.
#'
#' @param con DBI connection.
#' @param n Number of rows to sample per table.
#' @return A `data.table` for a single table database, or a named list of
#'   `data.table`s when multiple tables are present.
#' @export
sample_gnaf <- function(con, n = 10L) {
  n <- as.integer(n)
  if (is.na(n) || length(n) != 1L || n < 1L) {
    stop("'n' must be a single positive integer")
  }

  table_info <- setDT(DBI::dbGetQuery(
    con,
    paste(
      "SELECT table_name",
      "FROM information_schema.tables",
      "WHERE table_schema = 'main' AND table_type = 'BASE TABLE'",
      "ORDER BY table_name"
    )
  ))

  if (nrow(table_info) == 0L) {
    stop("No user tables found in the connected database")
  }

  sampled_tables <- lapply(table_info$table_name, function(table_name) {
    table_sql <- as.character(DBI::dbQuoteIdentifier(con, table_name))
    setDT(DBI::dbGetQuery(
      con,
      sprintf("SELECT * FROM %s ORDER BY random() LIMIT %d", table_sql, n)
    ))
  })
  names(sampled_tables) <- table_info$table_name

  if (length(sampled_tables) == 1L) {
    return(sampled_tables[[1L]])
  }

  sampled_tables
}
