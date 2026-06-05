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
#'   \code{longitude}, \code{latitude}.
#' @param upsert If \code{FALSE} (default) duplicate PIDs are silently skipped.
#'   If \code{TRUE}, existing rows with the same PID are replaced.
#' @return Invisibly, the number of rows inserted or updated.
#' @export
gnaf_add <- function(con, addresses, upsert = FALSE) {
  dt <- as.data.table(addresses)

  required <- c("number_first", "street_name", "street_type",
                 "locality_name", "state", "postcode")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0L)
    stop("Missing required columns: ", paste(missing, collapse = ", "))

  # Auto-generate PIDs
  if (!"address_detail_pid" %in% names(dt)) {
    existing_n <- DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM custom_addresses")$n
    dt[, address_detail_pid := paste0("CUSTOM_", existing_n + .I)]
  }

  # Fill optional columns with NA if absent
  opt_cols <- c("address_label", "address_site_name", "building_name",
                "flat_type", "flat_number", "level_type", "level_number",
                "number_last", "lot_number", "street_suffix",
                "longitude", "latitude", "alias_type")
  for (col in opt_cols) {
    if (!col %in% names(dt)) dt[, (col) := NA]
  }

  # Uppercase text fields to match GNAF convention
  chr_cols <- c("address_label", "building_name", "flat_type", "flat_number",
                "street_name", "street_type", "street_suffix", "locality_name",
                "state")
  for (col in chr_cols) {
    if (col %in% names(dt))
      set(dt, j = col, value = toupper(trimws(dt[[col]])))
  }

  dt[, source := "custom"]
  dt[, number_first := as.integer(number_first)]
  if ("number_last" %in% names(dt)) dt[, number_last := as.integer(number_last)]
  dt[, postcode := as.integer(postcode)]

  # Use DuckDB's virtual-table registration for fast, type-safe bulk insert
  duckdb::duckdb_register(con, "__gnafr_insert__", dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_insert__"), silent = TRUE))

  n_before <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM custom_addresses")$n

  conflict_clause <- if (upsert) {
    "ON CONFLICT (address_detail_pid) DO UPDATE SET
       address_label = EXCLUDED.address_label,
       building_name = EXCLUDED.building_name,
       flat_type     = EXCLUDED.flat_type,
       flat_number   = EXCLUDED.flat_number,
       number_first  = EXCLUDED.number_first,
       number_last   = EXCLUDED.number_last,
       street_name   = EXCLUDED.street_name,
       street_type   = EXCLUDED.street_type,
       street_suffix = EXCLUDED.street_suffix,
       locality_name = EXCLUDED.locality_name,
       state         = EXCLUDED.state,
       postcode      = EXCLUDED.postcode,
       longitude     = EXCLUDED.longitude,
       latitude      = EXCLUDED.latitude"
  } else {
    "ON CONFLICT DO NOTHING"
  }

  DBI::dbExecute(con, sprintf(
    "INSERT INTO custom_addresses
     SELECT address_detail_pid, address_label, address_site_name,
            building_name, flat_type, flat_number, level_type, level_number,
            number_first, number_last, lot_number, street_name, street_type,
            street_suffix, locality_name, state, postcode,
            longitude, latitude, source, alias_type
     FROM __gnafr_insert__
     %s",
    conflict_clause
  ))

  n_after <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM custom_addresses")$n
  n_changed <- n_after - n_before
  message(sprintf("Inserted %d custom address(es). Total custom: %d.",
                  n_changed, n_after))
  invisible(n_changed)
}

#' Remove custom addresses by PID
#'
#' @param con DBI connection.
#' @param pids Character vector of \code{address_detail_pid} values to remove.
#' @return Invisibly, the number of rows deleted.
#' @export
gnaf_remove_custom <- function(con, pids) {
  pid_csv <- paste0("'", gsub("'", "''", pids), "'", collapse = ",")
  n <- DBI::dbExecute(con, sprintf(
    "DELETE FROM custom_addresses WHERE address_detail_pid IN (%s)", pid_csv
  ))
  invisible(n)
}
