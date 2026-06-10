#' Load GNAF CSV data into the database
#'
#' Uses DuckDB's native CSV reader for maximum speed. The GNAF CSV must contain
#' at minimum the columns produced by the standard GNAF Core download.
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @param path Character vector of one or more paths to GNAF CSV files.
#' @param overwrite If \code{TRUE}, deletes existing GNAF rows before loading.
#' @return Invisibly, the total number of GNAF rows now in the database.
#' @export
gnaf_load <- function(con, path, overwrite = FALSE) {
  if (!is.character(path) || length(path) == 0L)
    stop("'path' must be a non-empty character vector")

  missing <- path[!file.exists(path)]
  if (length(missing) > 0L)
    stop("File(s) not found:\n  ", paste(missing, collapse = "\n  "))

  if (overwrite) {
    DBI::dbExecute(con, "DELETE FROM gnaf_addresses WHERE source = 'gnaf'")
    message("Cleared existing GNAF rows.")
  }

  st_case_sql <- .street_type_case_sql("STREET_TYPE")
  for (p in path) {
    # Normalise to forward slashes (DuckDB accepts them on Windows)
    p_fwd <- gsub("\\\\", "/", p)
    message("Loading: ", p)

    DBI::dbExecute(con, sprintf("
      INSERT INTO gnaf_addresses
      SELECT
        ADDRESS_DETAIL_PID                     AS address_detail_pid,
        ADDRESS_LABEL                          AS address_label,
        ADDRESS_SITE_NAME                      AS address_site_name,
        BUILDING_NAME                          AS building_name,
        FLAT_TYPE                              AS flat_type,
        CAST(FLAT_NUMBER   AS VARCHAR)         AS flat_number,
        LEVEL_TYPE                             AS level_type,
        CAST(LEVEL_NUMBER  AS VARCHAR)         AS level_number,
        TRY_CAST(NUMBER_FIRST AS INTEGER)      AS number_first,
        TRY_CAST(NUMBER_LAST  AS INTEGER)      AS number_last,
        LOT_NUMBER                             AS lot_number,
        STREET_NAME                            AS street_name,
        (%s)                                   AS street_type,
        STREET_SUFFIX                          AS street_suffix,
        LOCALITY_NAME                          AS locality_name,
        STATE                                  AS state,
        TRY_CAST(POSTCODE  AS INTEGER)         AS postcode,
        TRY_CAST(LONGITUDE AS DOUBLE)          AS longitude,
        TRY_CAST(LATITUDE  AS DOUBLE)          AS latitude,
        'gnaf'                                 AS source,
        NULL::VARCHAR                          AS alias_type
      FROM read_csv('%s', header = true, ignore_errors = true)
      ON CONFLICT DO NOTHING
    ", st_case_sql, p_fwd))

    message("Done: ", p)
  }

  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM gnaf_addresses")$n
  message("Total GNAF addresses in database: ", format(n, big.mark = ","))
  message("Rebuilding locality index ...")
  gnaf_rebuild_locality_index(con)
  invisible(n)
}
