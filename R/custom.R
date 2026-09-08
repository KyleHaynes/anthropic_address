#' Add custom addresses to the matching database
#'
#' Custom addresses live in the \code{custom_addresses} table and are
#' transparently included in all \code{gnaf_match} calls.
#'
#' @import data.table
#' @param con DBI connection from \code{gnaf_connect}.
#' @param addresses A \code{data.table} (or data.frame) with one row per
#'   address.  Required columns: \code{number_first}, \code{street_name},
#'   \code{street_type}, \code{locality_name}, \code{state}, \code{postcode}.
#'   Optional: \code{address_detail_pid} (auto-generated if absent),
#'   \code{address_label}, \code{building_name}, \code{flat_type},
#'   \code{flat_number}, \code{number_last}, \code{street_suffix},
#'   \code{longitude}, \code{latitude}, \code{date_created},
#'   \code{legal_parcel_id}, \code{mb_code}, \code{alias_principal},
#'   \code{principal_pid}, \code{primary_secondary}, \code{primary_pid},
#'   \code{geocode_type}.
#' @param upsert If \code{FALSE} (default) duplicate PIDs are silently skipped.
#'   If \code{TRUE}, existing rows with the same PID are replaced.
#' @return Invisibly, the number of rows inserted or updated.
#' @export
gnaf_add <- function(con, addresses, upsert = FALSE) {
  if (!is.data.frame(addresses))
    stop("'addresses' must be a data.frame or data.table", call. = FALSE)
  if (!is.logical(upsert) || length(upsert) != 1L || is.na(upsert))
    stop("'upsert' must be TRUE or FALSE", call. = FALSE)
  dt <- as.data.table(copy(addresses))

  required <- c("number_first", "street_name", "street_type",
                 "locality_name", "state", "postcode")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0L)
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  if (nrow(dt) == 0L) return(invisible(0L))

  DBI::dbBegin(con)
  committed <- FALSE
  on.exit(if (!committed) DBI::dbRollback(con), add = TRUE)

  # Row counts can reuse a surviving PID after deletions. Allocate above the
  # largest existing numeric suffix, including explicitly supplied CUSTOM IDs.
  if (!"address_detail_pid" %in% names(dt)) {
    last_id <- DBI::dbGetQuery(con, "
      SELECT COALESCE(MAX(TRY_CAST(substr(address_detail_pid, 8) AS BIGINT)), 0) AS id
      FROM custom_addresses
      WHERE regexp_full_match(address_detail_pid, 'CUSTOM_[0-9]+')
    ")$id
    dt[, address_detail_pid := paste0("CUSTOM_", sprintf("%.0f", as.numeric(last_id) + .I))]
  }
  if (!is.character(dt$address_detail_pid) || anyNA(dt$address_detail_pid) ||
      any(!nzchar(trimws(dt$address_detail_pid))))
    stop("'address_detail_pid' must contain non-missing, non-empty strings", call. = FALSE)

  # Fill optional columns with NA if absent
  opt_cols <- c("address_label", "address_site_name", "building_name",
                "flat_type", "flat_number", "level_type", "level_number",
                "number_last", "lot_number", "street_suffix",
                "longitude", "latitude", "alias_type",
                "date_created", "legal_parcel_id", "mb_code",
                "alias_principal", "principal_pid",
                "primary_secondary", "primary_pid", "geocode_type")
  for (col in opt_cols) {
    if (!col %in% names(dt)) dt[, (col) := NA]
  }

  # Uppercase text fields to match GNAF convention
  chr_cols <- c("address_label", "address_site_name", "building_name",
                "flat_type", "flat_number", "level_type", "level_number",
                "lot_number",
                "street_name", "street_type", "street_suffix", "locality_name",
                "state", "legal_parcel_id", "mb_code", "alias_principal",
                "principal_pid", "primary_secondary", "primary_pid", "geocode_type")
  for (col in chr_cols) {
    if (col %in% names(dt))
      set(dt, j = col, value = toupper(trimws(dt[[col]])))
  }

  st_map <- .get_street_type_map()
  canonical <- unname(st_map[dt$street_type])
  known <- which(!is.na(canonical))
  set(dt, i = known, j = "street_type", value = canonical[known])

  dt[, source := "custom"]
  dt[, number_first := as.integer(number_first)]
  if ("number_last" %in% names(dt)) dt[, number_last := as.integer(number_last)]
  dt[, postcode := as.integer(postcode)]
  dt[, date_created := as.Date(date_created)]

  # Use DuckDB's virtual-table registration for fast, type-safe bulk insert
  duckdb::duckdb_register(con, "__gnafr_insert__", dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_insert__"), silent = TRUE), add = TRUE)

  conflict_clause <- if (upsert) {
    "ON CONFLICT (address_detail_pid) DO UPDATE SET
       address_label      = EXCLUDED.address_label,
       address_site_name  = EXCLUDED.address_site_name,
       building_name      = EXCLUDED.building_name,
       flat_type          = EXCLUDED.flat_type,
       flat_number        = EXCLUDED.flat_number,
       level_type         = EXCLUDED.level_type,
       level_number       = EXCLUDED.level_number,
       number_first       = EXCLUDED.number_first,
       number_last        = EXCLUDED.number_last,
       lot_number         = EXCLUDED.lot_number,
       street_name        = EXCLUDED.street_name,
       street_type        = EXCLUDED.street_type,
       street_suffix      = EXCLUDED.street_suffix,
       locality_name      = EXCLUDED.locality_name,
       state              = EXCLUDED.state,
       postcode           = EXCLUDED.postcode,
       longitude          = EXCLUDED.longitude,
       latitude           = EXCLUDED.latitude,
       alias_type         = EXCLUDED.alias_type,
       date_created       = EXCLUDED.date_created,
       legal_parcel_id    = EXCLUDED.legal_parcel_id,
       mb_code            = EXCLUDED.mb_code,
       alias_principal    = EXCLUDED.alias_principal,
       principal_pid      = EXCLUDED.principal_pid,
       primary_secondary  = EXCLUDED.primary_secondary,
       primary_pid        = EXCLUDED.primary_pid,
       geocode_type       = EXCLUDED.geocode_type"
  } else {
    "ON CONFLICT DO NOTHING"
  }

  n_changed <- DBI::dbExecute(con, sprintf(
    "INSERT INTO custom_addresses (
       address_detail_pid, address_label, address_site_name,
       building_name, flat_type, flat_number, level_type, level_number,
       number_first, number_last, lot_number, street_name, street_type,
       street_suffix, locality_name, state, postcode,
       longitude, latitude, source, alias_type,
       date_created, legal_parcel_id, mb_code,
       alias_principal, principal_pid, primary_secondary, primary_pid,
       geocode_type
     )
     SELECT address_detail_pid, address_label, address_site_name,
            building_name, flat_type, flat_number, level_type, level_number,
            number_first, number_last, lot_number, street_name, street_type,
            street_suffix, locality_name, state, postcode,
            longitude, latitude, source, alias_type,
            date_created, legal_parcel_id, mb_code,
            alias_principal, principal_pid, primary_secondary, primary_pid,
            geocode_type
     FROM __gnafr_insert__
     %s",
    conflict_clause
  ))

  if (n_changed > 0L && DBI::dbExistsTable(con, "gnaf_locality_index")) {
    if (upsert) {
      # Updates can remove the last address in a locality.
      gnaf_rebuild_locality_index(con)
    } else {
      DBI::dbExecute(con, "
        INSERT INTO gnaf_locality_index
        SELECT DISTINCT c.locality_name, c.postcode, c.state
        FROM custom_addresses c
        JOIN __gnafr_insert__ i USING (address_detail_pid)
        WHERE c.locality_name IS NOT NULL
        ON CONFLICT DO NOTHING
      ")
    }
  }

  n_after <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM custom_addresses")$n
  if (n_changed > 0L) .invalidate_match_cache(con)
  DBI::dbCommit(con)
  committed <- TRUE
  message(sprintf("%s %d custom address(es). Total custom: %d.",
                  if (upsert) "Inserted or updated" else "Inserted", n_changed, n_after))
  invisible(n_changed)
}

#' Remove custom addresses by PID
#'
#' @param con DBI connection.
#' @param pids Character vector of \code{address_detail_pid} values to remove.
#' @return Invisibly, the number of rows deleted.
#' @export
gnaf_remove_custom <- function(con, pids) {
  if (!is.character(pids) || anyNA(pids) || any(!nzchar(trimws(pids))))
    stop("'pids' must be a character vector of non-missing, non-empty strings", call. = FALSE)
  if (length(pids) == 0L) return(invisible(0L))

  ids <- data.table(address_detail_pid = unique(pids))
  duckdb::duckdb_register(con, "__gnafr_delete__", ids, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_delete__"), silent = TRUE), add = TRUE)
  DBI::dbBegin(con)
  committed <- FALSE
  on.exit(if (!committed) DBI::dbRollback(con), add = TRUE)
  n <- DBI::dbExecute(con, "
    DELETE FROM custom_addresses
    WHERE address_detail_pid IN (SELECT address_detail_pid FROM __gnafr_delete__)
  ")
  if (n > 0L) {
    gnaf_rebuild_locality_index(con)
    .invalidate_match_cache(con)
  }
  DBI::dbCommit(con)
  committed <- TRUE
  invisible(n)
}
