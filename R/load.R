#' Load GNAF CSV data into the database
#'
#' Uses DuckDB's native CSV reader for maximum speed. The GNAF CSV must contain
#' at minimum the columns produced by the standard GNAF Core download. All
#' GNAF Core columns are captured, including \code{DATE_CREATED},
#' \code{LEGAL_PARCEL_ID}, \code{MB_CODE}, \code{ALIAS_PRINCIPAL},
#' \code{PRINCIPAL_PID} (alias-to-principal address mapping), and
#' \code{PRIMARY_SECONDARY} / \code{PRIMARY_PID} (main-dwelling vs.
#' sub-dwelling mapping), stored as \code{alias_principal}, \code{principal_pid},
#' \code{primary_secondary} and \code{primary_pid}.
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
      INSERT INTO gnaf_addresses (
        address_detail_pid, address_label, address_site_name, building_name,
        flat_type, flat_number, level_type, level_number,
        number_first, number_last, lot_number,
        street_name, street_type, street_suffix,
        locality_name, state, postcode,
        longitude, latitude, source, alias_type,
        date_created, legal_parcel_id, mb_code,
        alias_principal, principal_pid, primary_secondary, primary_pid,
        geocode_type
      )
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
        NULL::VARCHAR                          AS alias_type,
        CAST(TRY_STRPTIME(CAST(DATE_CREATED AS VARCHAR), '%%d-%%m-%%Y') AS DATE) AS date_created,
        LEGAL_PARCEL_ID                        AS legal_parcel_id,
        CAST(MB_CODE AS VARCHAR)               AS mb_code,
        ALIAS_PRINCIPAL                        AS alias_principal,
        PRINCIPAL_PID                          AS principal_pid,
        PRIMARY_SECONDARY                      AS primary_secondary,
        PRIMARY_PID                            AS primary_pid,
        GEOCODE_TYPE                           AS geocode_type
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
