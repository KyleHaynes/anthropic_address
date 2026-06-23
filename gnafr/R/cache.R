# ---------------------------------------------------------------------------
# Match cache: fast lookup for previously-matched high-confidence addresses
# ---------------------------------------------------------------------------

#' Show the current state of the match cache
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @return A \code{data.table} with row count and oldest/newest cache timestamps.
#' @export
gnaf_cache_status <- function(con) {
  if (!DBI::dbExistsTable(con, "gnaf_match_cache"))
    return(data.table(rows = 0L, oldest_cached = NA, newest_cached = NA))
  setDT(DBI::dbGetQuery(con, "
    SELECT COUNT(*)   AS rows,
           MIN(cached_at) AS oldest_cached,
           MAX(cached_at) AS newest_cached
    FROM gnaf_match_cache
  "))
}

#' Clear all entries from the match cache
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @param ask If \code{TRUE} (default), prompts for confirmation before
#'   deleting.  Pass \code{FALSE} for non-interactive / scripted use.
#' @return Invisibly, the number of rows removed.
#' @export
gnaf_cache_clear <- function(con, ask = TRUE) {
  if (!DBI::dbExistsTable(con, "gnaf_match_cache")) return(invisible(0L))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM gnaf_match_cache")$n
  if (n == 0L) {
    message("Match cache is already empty.")
    return(invisible(0L))
  }
  if (isTRUE(ask)) {
    if (!interactive())
      stop("Cannot prompt in a non-interactive session. Pass ask = FALSE to proceed.")
    ans <- readline(sprintf(
      "Remove all %s cached address(es)? [y/N] ", format(n, big.mark = ",")
    ))
    if (!tolower(trimws(ans)) %in% c("y", "yes")) {
      message("Aborted.")
      return(invisible(0L))
    }
  }
  DBI::dbExecute(con, "DELETE FROM gnaf_match_cache")
  message(sprintf("Removed %s cached address(es).", format(n, big.mark = ",")))
  invisible(n)
}

#' Roll back cache entries added after a given timestamp
#'
#' Removes cache entries whose \code{cached_at} is on or after \code{after}.
#' Useful when a batch of addresses was matched poorly and you want to force
#' re-matching without clearing the entire cache.
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @param after A \code{POSIXct}, \code{Date}, or ISO-8601 character string
#'   (e.g. \code{"2024-06-01 12:00:00"}).  Entries cached at or after this
#'   time are removed.
#' @param ask If \code{TRUE} (default), prompts for confirmation before
#'   deleting.
#' @return Invisibly, the number of rows removed.
#' @export
gnaf_cache_rollback <- function(con, after, ask = TRUE) {
  if (!DBI::dbExistsTable(con, "gnaf_match_cache"))
    stop("gnaf_match_cache table not found; run gnaf_init() first.")

  after_ts <- tryCatch(
    as.POSIXct(after, tz = "UTC"),
    error = function(e) stop("'after' must be coercible to POSIXct: ",
                             conditionMessage(e))
  )
  after_str <- format(after_ts, "%Y-%m-%d %H:%M:%S")

  n <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) AS n FROM gnaf_match_cache WHERE cached_at >= '%s'",
    after_str
  ))$n

  if (n == 0L) {
    message("No cache entries found at or after ", after_str, ".")
    return(invisible(0L))
  }
  if (isTRUE(ask)) {
    if (!interactive())
      stop("Cannot prompt in a non-interactive session. Pass ask = FALSE to proceed.")
    ans <- readline(sprintf(
      "Remove %s cached address(es) added on or after %s? [y/N] ",
      format(n, big.mark = ","), after_str
    ))
    if (!tolower(trimws(ans)) %in% c("y", "yes")) {
      message("Aborted.")
      return(invisible(0L))
    }
  }
  DBI::dbExecute(con, sprintf(
    "DELETE FROM gnaf_match_cache WHERE cached_at >= '%s'", after_str
  ))
  message(sprintf("Removed %s cached address(es).", format(n, big.mark = ",")))
  invisible(n)
}

#' Summarise cache entries over time
#'
#' Returns cache counts grouped by day or hour so you can inspect cache growth
#' and identify when entries were added.
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @param by Time grain for the summary. One of \code{"day"} or \code{"hour"}.
#' @return A \code{data.table} with one row per time bucket.
#' @export
gnaf_cache_history <- function(con, by = c("day", "hour")) {
  if (!DBI::dbExistsTable(con, "gnaf_match_cache")) {
    return(data.table(bucket = as.POSIXct(character()), rows = integer()))
  }

  by <- match.arg(by)
  bucket_expr <- if (identical(by, "hour")) {
    "date_trunc('hour', cached_at)"
  } else {
    "CAST(cached_at AS DATE)"
  }

  setDT(DBI::dbGetQuery(con, sprintf(
    "SELECT %s AS bucket,
            COUNT(*) AS rows,
            MIN(total_score) AS min_score,
            AVG(total_score) AS avg_score,
            MAX(total_score) AS max_score
     FROM gnaf_match_cache
     GROUP BY 1
     ORDER BY 1",
    bucket_expr
  )))
}

#' Sample rows from the match cache
#'
#' Returns a random sample of cache rows, optionally filtered to a single date
#' or a datetime window. Joined address columns are included so you can inspect
#' the matched record without doing a second lookup.
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @param n Number of rows to sample.
#' @param cached_on Optional single \code{Date} or \code{YYYY-MM-DD} string.
#'   When supplied, only cache rows from that calendar date are sampled.
#' @param from Optional inclusive lower bound for \code{cached_at}. Accepts a
#'   \code{POSIXct}, \code{Date}, or ISO-8601 character string.
#' @param to Optional inclusive upper bound for \code{cached_at}. Accepts a
#'   \code{POSIXct}, \code{Date}, or ISO-8601 character string.
#' @param include_custom If \code{TRUE} (default), joined address details may
#'   come from either \code{gnaf_addresses} or \code{custom_addresses}.
#' @return A \code{data.table} containing sampled cache rows and matched
#'   address columns.
#' @export
gnaf_cache_sample <- function(con, n = 10L, cached_on = NULL,
                              from = NULL, to = NULL,
                              include_custom = TRUE) {
  if (!DBI::dbExistsTable(con, "gnaf_match_cache")) {
    return(data.table())
  }

  n <- as.integer(n)
  if (is.na(n) || length(n) != 1L || n < 1L) {
    stop("'n' must be a single positive integer")
  }
  if (!is.null(cached_on) && (!is.null(from) || !is.null(to))) {
    stop("Use either 'cached_on' or 'from'/'to', not both")
  }

  filter_sql <- .cache_filter_sql(cached_on = cached_on, from = from, to = to)
  addr_src <- .cache_address_source_sql(con, include_custom = include_custom)

  setDT(DBI::dbGetQuery(con, sprintf(
    "SELECT c.input_standardised,
            c.address_detail_pid,
            c.total_score,
            c.score_postcode,
            c.score_suburb,
            c.score_street_name,
            c.score_street_type,
            c.score_number,
            c.score_flat,
            c.cached_at,
            g.address_label,
            g.building_name,
            g.flat_type,
            g.flat_number,
            g.number_first,
            g.number_last,
            g.street_name,
            g.street_type,
            g.street_suffix,
            g.locality_name,
            g.state,
            g.postcode,
            g.longitude,
            g.latitude,
            g.source,
            g.alias_type,
            g.alias_principal,
            g.principal_pid,
            g.primary_secondary,
            g.primary_pid,
            g.geocode_type,
            g.date_created,
            g.legal_parcel_id,
            g.mb_code
     FROM gnaf_match_cache c
     JOIN %s g ON g.address_detail_pid = c.address_detail_pid
     %s
     ORDER BY random()
     LIMIT %d",
    addr_src,
    filter_sql,
    n
  )))
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.cache_address_source_sql <- function(con, include_custom) {
  addr_sel <- "address_detail_pid, address_label, building_name,
               flat_type, flat_number, number_first, number_last,
               street_name, street_type, street_suffix, locality_name,
               state, postcode, longitude, latitude, source, alias_type,
               alias_principal, principal_pid, primary_secondary, primary_pid,
               geocode_type, date_created, legal_parcel_id, mb_code"

  if (include_custom && DBI::dbExistsTable(con, "custom_addresses")) {
    sprintf("(SELECT %s FROM gnaf_addresses UNION ALL SELECT %s FROM custom_addresses)",
            addr_sel, addr_sel)
  } else {
    sprintf("(SELECT %s FROM gnaf_addresses)", addr_sel)
  }
}

.cache_filter_sql <- function(cached_on = NULL, from = NULL, to = NULL) {
  clauses <- character()

  if (!is.null(cached_on)) {
    cached_on_date <- tryCatch(
      as.Date(cached_on),
      error = function(e) stop("'cached_on' must be coercible to Date: ", conditionMessage(e))
    )
    if (is.na(cached_on_date)) stop("'cached_on' must be coercible to Date")
    clauses <- c(clauses, sprintf("CAST(c.cached_at AS DATE) = '%s'", format(cached_on_date, "%Y-%m-%d")))
  }

  if (!is.null(from)) {
    from_str <- .cache_timestamp_string(from, arg = "from")
    clauses <- c(clauses, sprintf("c.cached_at >= '%s'", from_str))
  }

  if (!is.null(to)) {
    to_str <- .cache_timestamp_string(to, arg = "to")
    clauses <- c(clauses, sprintf("c.cached_at <= '%s'", to_str))
  }

  if (length(clauses) == 0L) {
    return("")
  }

  paste("WHERE", paste(clauses, collapse = " AND "))
}

.cache_timestamp_string <- function(x, arg) {
  ts <- tryCatch(
    as.POSIXct(x, tz = "UTC"),
    error = function(e) stop("'", arg, "' must be coercible to POSIXct: ", conditionMessage(e))
  )
  if (is.na(ts)) stop("'", arg, "' must be coercible to POSIXct")
  format(ts, "%Y-%m-%d %H:%M:%S")
}

.cache_lookup <- function(con, standardised_vec, include_custom, alias_types = NULL) {
  standardised_vec <- standardised_vec[!is.na(standardised_vec) & nzchar(standardised_vec)]
  if (length(standardised_vec) == 0L) return(data.table())

  lkp <- data.table(input_standardised = standardised_vec)
  duckdb::duckdb_register(con, "__gnafr_cache_lkp__", lkp, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_cache_lkp__"), silent = TRUE))

  addr_src    <- .cache_address_source_sql(con, include_custom = include_custom)
  alias_sql   <- .alias_type_sql(alias_types)
  alias_where <- if (!is.null(alias_sql)) sprintf("\n    WHERE %s", alias_sql) else ""

  setDT(DBI::dbGetQuery(con, sprintf("
    SELECT c.input_standardised,
           c.total_score,
           c.score_postcode, c.score_suburb, c.score_street_name,
           c.score_street_type, c.score_number, c.score_flat,
           g.*
    FROM __gnafr_cache_lkp__ l
    JOIN gnaf_match_cache c ON c.input_standardised = l.input_standardised
    JOIN %s g ON g.address_detail_pid = c.address_detail_pid%s
  ", addr_src, alias_where)))
}

.cache_store <- function(con, result_dt, threshold) {
  if (!DBI::dbExistsTable(con, "gnaf_match_cache")) return(invisible(NULL))
  if (is.null(result_dt) || nrow(result_dt) == 0L) return(invisible(NULL))

  score_cols <- c("score_postcode", "score_suburb", "score_street_name",
                  "score_street_type", "score_number", "score_flat")

  need <- c("match_rank", "total_score", "input_standardised",
            "address_detail_pid", score_cols)
  if (!all(need %in% names(result_dt))) return(invisible(NULL))

  to_cache <- result_dt[
    match_rank == 1L &
      !is.na(total_score) & total_score >= threshold &
      !is.na(input_standardised) & !is.na(address_detail_pid),
    c("input_standardised", "address_detail_pid", "total_score", score_cols),
    with = FALSE
  ]
  if (nrow(to_cache) == 0L) return(invisible(NULL))

  duckdb::duckdb_register(con, "__gnafr_cache_ins__", to_cache, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_cache_ins__"), silent = TRUE))

  DBI::dbExecute(con, "
    INSERT INTO gnaf_match_cache
      (input_standardised, address_detail_pid, total_score,
       score_postcode, score_suburb, score_street_name,
       score_street_type, score_number, score_flat)
    SELECT input_standardised, address_detail_pid, total_score,
           score_postcode, score_suburb, score_street_name,
           score_street_type, score_number, score_flat
    FROM __gnafr_cache_ins__
    ON CONFLICT DO NOTHING
  ")
  invisible(NULL)
}
