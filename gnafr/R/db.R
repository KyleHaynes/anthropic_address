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
