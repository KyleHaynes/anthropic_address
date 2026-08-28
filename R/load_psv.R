#' Load G-NAF PSV data into the database
#'
#' Reads directly from the raw G-NAF pipe-separated (PSV) files distributed by
#' the Geoscape G-NAF product (not GNAF Core).  All joining and transformation
#' happens inside DuckDB — data is never pulled into R memory — so RAM usage is
#' bounded by DuckDB's internal buffers regardless of how many rows are loaded.
#'
#' Three categories of address records are loaded:
#' \enumerate{
#'   \item \strong{Standard addresses} — every active record from
#'     \code{ADDRESS_DETAIL} joined to its geocode, street, and locality, plus
#'     \code{PRIMARY_SECONDARY} (main dwelling vs. sub-dwelling linkage),
#'     \code{ADDRESS_SITE} (site name), and mesh block (via
#'     \code{ADDRESS_MESH_BLOCK_2021} / \code{MB_2021}). Records with
#'     \code{ALIAS_PRINCIPAL = 'A'} are included and flagged (e.g.
#'     \code{alias_type = "ADDRESS:RA"}), with \code{principal_pid} pointing
#'     back at the real (principal) \code{address_detail_pid}.
#'   \item \strong{Locality alias addresses} — for each address in a locality
#'     that has a \code{LOCALITY_ALIAS} entry, a duplicate record is created
#'     carrying the alias locality name and its postcode (when different).
#'     These allow matching when a person writes a recognised alternative
#'     suburb name or postcode. Tagged \code{"LOCALITY:SYN"} or
#'     \code{"LOCALITY:SR"}.
#'   \item \strong{Street alias addresses} — for each address on a street that
#'     has a \code{STREET_LOCALITY_ALIAS} entry, a duplicate record is created
#'     with the alias street name / type.  Tagged \code{"STREET:SYN"} or
#'     \code{"STREET:ALT"}.
#' }
#'
#' The \code{alias_type} column in \code{gnaf_match} results is \code{NA} for
#' standard principal records and a short code (e.g. \code{"LOCALITY:SYN"})
#' for alias variants. Every alias row (official \code{ADDRESS_ALIAS} records
#' and the locality/street synonyms derived here) carries \code{principal_pid}
#' pointing back at the real \code{address_detail_pid} it was derived from, so
#' an alias match can always be resolved to its canonical address — see
#' \code{resolve_principal} on \code{\link{gnaf_match}}.
#'
#' @param con DBI connection from \code{gnaf_connect}.  The database must have
#'   been initialised with \code{gnaf_init()}.
#' @param gnaf_dir Path to the G-NAF \strong{Standard} directory that contains
#'   the \code{<STATE>_*_psv.psv} files (e.g. \file{G-NAF MAY 2026/Standard}).
#' @param state One or more G-NAF state file prefixes to load, e.g.
#'   \code{"QLD"} or \code{c("QLD", "NSW")}. Defaults to \code{"QLD"}. States
#'   are loaded one at a time. Use \code{\link{gnaf_build_db}} to load every
#'   state present in \code{gnaf_dir} without listing them by hand.
#' @param overwrite If \code{TRUE}, deletes existing \code{source = 'gnaf'}
#'   rows for the state(s) being loaded before loading — other states already
#'   in the database are left untouched. Defaults to \code{FALSE}.
#' @param load_aliases If \code{TRUE} (default), loads locality and street
#'   alias address variants in addition to the standard records.
#' @return Invisibly, the total number of GNAF rows in the database after
#'   loading (across all states, not just the one(s) just loaded).
#' @export
gnaf_load_psv <- function(con, gnaf_dir, state = "QLD", overwrite = FALSE,
                          load_aliases = TRUE) {

  gnaf_dir <- normalizePath(gnaf_dir, mustWork = TRUE)
  state <- toupper(state)
  if (length(state) == 0L || !all(grepl("^[A-Z]{2,3}$", state)))
    stop("'state' must be one or more 2-3 letter G-NAF state codes, ",
         "e.g. \"QLD\" or c(\"QLD\", \"NSW\")")

  if (length(state) > 1L) {
    total <- NULL
    for (s in state) {
      total <- gnaf_load_psv(con, gnaf_dir, state = s, overwrite = overwrite,
                             load_aliases = load_aliases)
    }
    return(invisible(total))
  }

  file_specs <- c(
    detail            = "ADDRESS_DETAIL",
    geocode           = "ADDRESS_DEFAULT_GEOCODE",
    street            = "STREET_LOCALITY",
    locality          = "LOCALITY",
    addr_alias        = "ADDRESS_ALIAS",
    primary_secondary = "PRIMARY_SECONDARY",
    address_site      = "ADDRESS_SITE",
    mesh_block        = "ADDRESS_MESH_BLOCK_2021",
    mb                = "MB_2021"
  )
  alias_file_specs <- c(
    loc_alias = "LOCALITY_ALIAS",
    str_alias = "STREET_LOCALITY_ALIAS"
  )

  required_files <- stats::setNames(
    sprintf("%s_%s_psv.psv", state, file_specs), names(file_specs)
  )
  alias_files <- stats::setNames(
    sprintf("%s_%s_psv.psv", state, alias_file_specs), names(alias_file_specs)
  )

  needed <- if (load_aliases) c(required_files, alias_files) else required_files
  missing <- needed[!file.exists(file.path(gnaf_dir, needed))]
  if (length(missing) > 0L)
    stop("Missing G-NAF files for state '", state, "' in '", gnaf_dir, "':\n  ",
         paste(missing, collapse = "\n  "))

  # Ensure alias_type column exists for databases created before this feature
  for (tbl in c("gnaf_addresses", "custom_addresses")) {
    tryCatch(
      DBI::dbExecute(con, sprintf(
        "ALTER TABLE %s ADD COLUMN IF NOT EXISTS alias_type VARCHAR", tbl
      )),
      error = function(e) NULL
    )
  }

  if (overwrite) {
    DBI::dbExecute(con, sprintf(
      "DELETE FROM gnaf_addresses WHERE source = 'gnaf' AND state = '%s'", state
    ))
    message(sprintf("Cleared existing GNAF rows for state '%s'.", state))
  }

  # Build forward-slash paths (DuckDB accepts them on Windows)
  fps <- lapply(
    stats::setNames(c(required_files, alias_files),
                    c(names(required_files), names(alias_files))),
    function(f) gsub("\\\\", "/", file.path(gnaf_dir, f))
  )

  message(sprintf("Loading GNAF state '%s' ...", state))

  message("Loading standard GNAF addresses ...")
  n1 <- .psv_insert_standard(con, fps, state)
  message(sprintf("  Inserted %s address records.", format(n1, big.mark = ",")))

  if (load_aliases) {
    message("Loading locality alias records ...")
    n2 <- .psv_insert_locality_aliases(con, fps, state)
    message(sprintf("  Inserted %s locality alias records.",
                    format(n2, big.mark = ",")))

    message("Loading street alias records ...")
    n3 <- .psv_insert_street_aliases(con, fps, state)
    message(sprintf("  Inserted %s street alias records.",
                    format(n3, big.mark = ",")))
  }

  total <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM gnaf_addresses")$n
  message(sprintf("Total GNAF addresses in database: %s",
                  format(total, big.mark = ",")))

  message("Rebuilding locality index ...")
  gnaf_rebuild_locality_index(con)
  DBI::dbExecute(con, "ANALYZE gnaf_addresses")
  DBI::dbExecute(con, "ANALYZE gnaf_locality_index")
  .invalidate_match_cache(con)

  invisible(total)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Returns a DuckDB read_csv(...) expression for a given PSV path.
.psv_csv <- function(path) {
  sprintf("read_csv('%s', delim='|', header=true, ignore_errors=true)", path)
}

# Builds the address label SQL expression.
# street_name / street_type / street_suffix / locality_name / postcode are
# SQL expressions; callers substitute alias-table columns for alias variants.
.psv_label_sql <- function(state,
                           street_name   = "sl.STREET_NAME",
                           street_type   = "sl.STREET_TYPE_CODE",
                           street_suffix = "sl.STREET_SUFFIX_CODE",
                           locality_name = "l.LOCALITY_NAME",
                           postcode      = "d.POSTCODE") {
  canon_st <- .street_type_case_sql(street_type)
  sprintf(
    "TRIM(
       COALESCE(d.BUILDING_NAME || ' ', '') ||
       CASE WHEN d.LOT_NUMBER IS NOT NULL AND d.NUMBER_FIRST IS NULL THEN
         'LOT ' || COALESCE(d.LOT_NUMBER_PREFIX, '') ||
         CAST(d.LOT_NUMBER AS VARCHAR) ||
         COALESCE(d.LOT_NUMBER_SUFFIX, '') || ' '
       ELSE '' END ||
       CASE WHEN d.FLAT_NUMBER IS NOT NULL THEN
         COALESCE(d.FLAT_TYPE_CODE || ' ', '') ||
         COALESCE(d.FLAT_NUMBER_PREFIX, '') ||
         CAST(TRY_CAST(d.FLAT_NUMBER AS INTEGER) AS VARCHAR) ||
         COALESCE(d.FLAT_NUMBER_SUFFIX, '') || ' '
       ELSE '' END ||
       CASE WHEN d.LEVEL_NUMBER IS NOT NULL THEN
         COALESCE(d.LEVEL_TYPE_CODE || ' ', 'LEVEL ') ||
         COALESCE(d.LEVEL_NUMBER_PREFIX, '') ||
         CAST(TRY_CAST(d.LEVEL_NUMBER AS INTEGER) AS VARCHAR) ||
         COALESCE(d.LEVEL_NUMBER_SUFFIX, '') || ' '
       ELSE '' END ||
       CASE WHEN d.NUMBER_FIRST IS NOT NULL THEN
         COALESCE(d.NUMBER_FIRST_PREFIX, '') ||
         CAST(TRY_CAST(d.NUMBER_FIRST AS INTEGER) AS VARCHAR) ||
         COALESCE(d.NUMBER_FIRST_SUFFIX, '') ||
         CASE WHEN d.NUMBER_LAST IS NOT NULL THEN
           '-' || COALESCE(d.NUMBER_LAST_PREFIX, '') ||
           CAST(TRY_CAST(d.NUMBER_LAST AS INTEGER) AS VARCHAR) ||
           COALESCE(d.NUMBER_LAST_SUFFIX, '')
         ELSE '' END || ' '
       ELSE '' END ||
       (%s) ||
       COALESCE(' ' || NULLIF((%s), ''), '') ||
       COALESCE(' ' || NULLIF((%s), ''), '') ||
       ', ' || (%s) || ' %s ' || (%s)
     )",
    street_name, canon_st, street_suffix, locality_name, state, postcode
  )
}

# SQL for the address-component columns derived from ADDRESS_DETAIL plus its
# PRIMARY_SECONDARY / ADDRESS_SITE / mesh-block / geocode-type joins. Used
# identically in all three INSERT statements — the aliases `ps`, `asite`,
# `mbq` and `g` referenced here must be joined into every query that uses it
# (see the FROM clauses in .psv_insert_standard / _locality_aliases /
# _street_aliases).
.psv_detail_cols_sql <- function() {
  "asite.ADDRESS_SITE_NAME                                   AS address_site_name,
   d.BUILDING_NAME,
   d.FLAT_TYPE_CODE,
   CASE WHEN d.FLAT_NUMBER IS NOT NULL THEN
     COALESCE(d.FLAT_NUMBER_PREFIX, '') ||
     CAST(TRY_CAST(d.FLAT_NUMBER AS INTEGER) AS VARCHAR) ||
     COALESCE(d.FLAT_NUMBER_SUFFIX, '')
   END                                                      AS flat_number,
   d.LEVEL_TYPE_CODE,
   CASE WHEN d.LEVEL_NUMBER IS NOT NULL THEN
     CAST(TRY_CAST(d.LEVEL_NUMBER AS INTEGER) AS VARCHAR)
   END                                                      AS level_number,
   TRY_CAST(d.NUMBER_FIRST AS INTEGER)                     AS number_first,
   TRY_CAST(d.NUMBER_LAST  AS INTEGER)                     AS number_last,
   d.LOT_NUMBER,
   TRY_CAST(d.DATE_CREATED AS DATE)                        AS date_created,
   d.LEGAL_PARCEL_ID                                       AS legal_parcel_id,
   mbq.MB_2021_CODE                                        AS mb_code,
   d.PRIMARY_SECONDARY                                     AS primary_secondary,
   ps.PRIMARY_PID                                          AS primary_pid,
   g.GEOCODE_TYPE_CODE                                     AS geocode_type"
}

# Subquery that returns one geocode row per address (any geocode type).
# ADDRESS_DEFAULT_GEOCODE is one active row per address by design; ANY_VALUE
# is a no-op tie-break in the normal case and a safe fallback if duplicates
# ever slip through.
.psv_geocode_cte <- function(geocode_path) {
  sprintf(
    "(SELECT ADDRESS_DETAIL_PID,
             ANY_VALUE(LONGITUDE) AS LONGITUDE,
             ANY_VALUE(LATITUDE)  AS LATITUDE,
             ANY_VALUE(GEOCODE_TYPE_CODE) AS GEOCODE_TYPE_CODE
      FROM %s
      WHERE DATE_RETIRED IS NULL OR DATE_RETIRED = ''
      GROUP BY ADDRESS_DETAIL_PID)",
    .psv_csv(geocode_path)
  )
}

# Subquery mapping a secondary (sub-dwelling) ADDRESS_DETAIL_PID to its
# primary (main dwelling) ADDRESS_DETAIL_PID.
.psv_primary_secondary_cte <- function(ps_path) {
  sprintf(
    "(SELECT SECONDARY_PID, ANY_VALUE(PRIMARY_PID) AS PRIMARY_PID
      FROM %s
      WHERE DATE_RETIRED IS NULL OR DATE_RETIRED = ''
      GROUP BY SECONDARY_PID)",
    .psv_csv(ps_path)
  )
}

# Subquery mapping ADDRESS_SITE_PID to its ADDRESS_SITE_NAME.
.psv_address_site_cte <- function(site_path) {
  sprintf(
    "(SELECT ADDRESS_SITE_PID, ANY_VALUE(ADDRESS_SITE_NAME) AS ADDRESS_SITE_NAME
      FROM %s
      WHERE DATE_RETIRED IS NULL OR DATE_RETIRED = ''
      GROUP BY ADDRESS_SITE_PID)",
    .psv_csv(site_path)
  )
}

# Subquery mapping ADDRESS_DETAIL_PID to its 2021 mesh block code, via the
# ADDRESS_MESH_BLOCK_2021 link table joined through to MB_2021.
.psv_mesh_block_cte <- function(mesh_block_path, mb_path) {
  sprintf(
    "(SELECT amb.ADDRESS_DETAIL_PID AS ADDRESS_DETAIL_PID,
             ANY_VALUE(mb.MB_2021_CODE) AS MB_2021_CODE
      FROM %s amb
      JOIN %s mb ON mb.MB_2021_PID = amb.MB_2021_PID
      WHERE amb.DATE_RETIRED IS NULL OR amb.DATE_RETIRED = ''
      GROUP BY amb.ADDRESS_DETAIL_PID)",
    .psv_csv(mesh_block_path), .psv_csv(mb_path)
  )
}

# Common trailing FROM/JOIN fragment shared by all three insert queries: pulls
# in the geocode, primary/secondary, address-site and mesh-block subqueries
# used by .psv_detail_cols_sql(). `d` must already be in scope as the alias
# for ADDRESS_DETAIL.
.psv_common_joins_sql <- function(fps) {
  sprintf("
    LEFT JOIN %s g
      ON  g.ADDRESS_DETAIL_PID = d.ADDRESS_DETAIL_PID
    LEFT JOIN %s ps
      ON  ps.SECONDARY_PID = d.ADDRESS_DETAIL_PID
    LEFT JOIN %s asite
      ON  asite.ADDRESS_SITE_PID = d.ADDRESS_SITE_PID
    LEFT JOIN %s mbq
      ON  mbq.ADDRESS_DETAIL_PID = d.ADDRESS_DETAIL_PID",
    .psv_geocode_cte(fps$geocode),
    .psv_primary_secondary_cte(fps$primary_secondary),
    .psv_address_site_cte(fps$address_site),
    .psv_mesh_block_cte(fps$mesh_block, fps$mb)
  )
}

# Shared INSERT column list for all three insert queries — must match the
# SELECT order produced by .psv_detail_cols_sql() plus the per-query columns
# each function appends around it.
.PSV_INSERT_COLS <- "
      address_detail_pid, address_label,
      address_site_name, building_name, flat_type, flat_number,
      level_type, level_number,
      number_first, number_last, lot_number, date_created, legal_parcel_id,
      mb_code, primary_secondary, primary_pid, geocode_type,
      street_name, street_type, street_suffix,
      locality_name, state, postcode,
      longitude, latitude, source, alias_type,
      alias_principal, principal_pid
"

# ---------------------------------------------------------------------------
# Step 1: load ADDRESS_DETAIL (principal + alias records)
# ---------------------------------------------------------------------------
.psv_insert_standard <- function(con, fps, state) {
  st_case_sql <- .street_type_case_sql("sl.STREET_TYPE_CODE")
  DBI::dbExecute(con, sprintf("
    INSERT INTO gnaf_addresses (%s)
    SELECT
      d.ADDRESS_DETAIL_PID,
      %s                                                    AS address_label,
      %s,
      sl.STREET_NAME,
      (%s),
      sl.STREET_SUFFIX_CODE,
      l.LOCALITY_NAME,
      '%s',
      TRY_CAST(d.POSTCODE AS INTEGER),
      g.LONGITUDE,
      g.LATITUDE,
      'gnaf',
      CASE WHEN d.ALIAS_PRINCIPAL = 'A' THEN
        'ADDRESS:' || COALESCE(aa.ALIAS_TYPE_CODE, 'ALIAS')
      END,
      CASE WHEN d.ALIAS_PRINCIPAL = 'A' THEN 'ALIAS' ELSE 'PRINCIPAL' END,
      aa.PRINCIPAL_PID
    FROM %s d
    JOIN %s sl
      ON  d.STREET_LOCALITY_PID = sl.STREET_LOCALITY_PID
      AND (sl.DATE_RETIRED IS NULL OR sl.DATE_RETIRED = '')
    JOIN %s l
      ON  d.LOCALITY_PID = l.LOCALITY_PID
      AND (l.DATE_RETIRED IS NULL OR l.DATE_RETIRED = '')
    LEFT JOIN (
      SELECT ALIAS_PID,
             MAX(ALIAS_TYPE_CODE)      AS ALIAS_TYPE_CODE,
             ANY_VALUE(PRINCIPAL_PID)  AS PRINCIPAL_PID
      FROM %s
      WHERE DATE_RETIRED IS NULL OR DATE_RETIRED = ''
      GROUP BY ALIAS_PID
    ) aa ON aa.ALIAS_PID = d.ADDRESS_DETAIL_PID
    %s
    WHERE (d.DATE_RETIRED IS NULL OR d.DATE_RETIRED = '')
    ON CONFLICT DO NOTHING",
    .PSV_INSERT_COLS,
    .psv_label_sql(state),
    .psv_detail_cols_sql(),
    st_case_sql,
    state,
    .psv_csv(fps$detail),
    .psv_csv(fps$street),
    .psv_csv(fps$locality),
    .psv_csv(fps$addr_alias),
    .psv_common_joins_sql(fps)
  ))
}

# ---------------------------------------------------------------------------
# Step 2: locality alias records
# One copy per address per active locality alias.
# Derived PID: <original_pid>_LA<alias_pid>. principal_pid always points back
# at the real ADDRESS_DETAIL_PID this variant was derived from.
# ---------------------------------------------------------------------------
.psv_insert_locality_aliases <- function(con, fps, state) {
  st_case_sql <- .street_type_case_sql("sl.STREET_TYPE_CODE")
  DBI::dbExecute(con, sprintf("
    INSERT INTO gnaf_addresses (%s)
    SELECT
      d.ADDRESS_DETAIL_PID || '_LA' || la.LOCALITY_ALIAS_PID,
      %s                                                    AS address_label,
      %s,
      sl.STREET_NAME,
      (%s),
      sl.STREET_SUFFIX_CODE,
      la.NAME                                               AS locality_name,
      '%s',
      COALESCE(TRY_CAST(la.POSTCODE AS INTEGER), TRY_CAST(d.POSTCODE AS INTEGER)) AS postcode,
      g.LONGITUDE,
      g.LATITUDE,
      'gnaf',
      'LOCALITY:' || la.ALIAS_TYPE_CODE,
      'ALIAS',
      d.ADDRESS_DETAIL_PID
    FROM %s d
    JOIN %s sl
      ON  d.STREET_LOCALITY_PID = sl.STREET_LOCALITY_PID
      AND (sl.DATE_RETIRED IS NULL OR sl.DATE_RETIRED = '')
    JOIN %s l
      ON  d.LOCALITY_PID = l.LOCALITY_PID
      AND (l.DATE_RETIRED IS NULL OR l.DATE_RETIRED = '')
    JOIN %s la
      ON  la.LOCALITY_PID = d.LOCALITY_PID
      AND (la.DATE_RETIRED IS NULL OR la.DATE_RETIRED = '')
    %s
    WHERE (d.DATE_RETIRED IS NULL OR d.DATE_RETIRED = '')
      AND d.ALIAS_PRINCIPAL = 'P'
    ON CONFLICT DO NOTHING",
    .PSV_INSERT_COLS,
    .psv_label_sql(state, locality_name = "la.NAME",
                   postcode = "COALESCE(CAST(la.POSTCODE AS VARCHAR), CAST(d.POSTCODE AS VARCHAR))"),
    .psv_detail_cols_sql(),
    st_case_sql,
    state,
    .psv_csv(fps$detail),
    .psv_csv(fps$street),
    .psv_csv(fps$locality),
    .psv_csv(fps$loc_alias),
    .psv_common_joins_sql(fps)
  ))
}

# ---------------------------------------------------------------------------
# Step 3: street alias records
# One copy per address per active street locality alias.
# Derived PID: <original_pid>_SA<alias_pid>. principal_pid always points back
# at the real ADDRESS_DETAIL_PID this variant was derived from.
# ---------------------------------------------------------------------------
.psv_insert_street_aliases <- function(con, fps, state) {
  st_case_sql <- .street_type_case_sql("sla.STREET_TYPE_CODE")
  DBI::dbExecute(con, sprintf("
    INSERT INTO gnaf_addresses (%s)
    SELECT
      d.ADDRESS_DETAIL_PID || '_SA' || sla.STREET_LOCALITY_ALIAS_PID,
      %s                                                    AS address_label,
      %s,
      sla.STREET_NAME,
      (%s),
      sla.STREET_SUFFIX_CODE,
      l.LOCALITY_NAME,
      '%s',
      TRY_CAST(d.POSTCODE AS INTEGER),
      g.LONGITUDE,
      g.LATITUDE,
      'gnaf',
      'STREET:' || sla.ALIAS_TYPE_CODE,
      'ALIAS',
      d.ADDRESS_DETAIL_PID
    FROM %s d
    JOIN %s sl
      ON  d.STREET_LOCALITY_PID = sl.STREET_LOCALITY_PID
      AND (sl.DATE_RETIRED IS NULL OR sl.DATE_RETIRED = '')
    JOIN %s sla
      ON  sla.STREET_LOCALITY_PID = sl.STREET_LOCALITY_PID
      AND (sla.DATE_RETIRED IS NULL OR sla.DATE_RETIRED = '')
    JOIN %s l
      ON  d.LOCALITY_PID = l.LOCALITY_PID
      AND (l.DATE_RETIRED IS NULL OR l.DATE_RETIRED = '')
    %s
    WHERE (d.DATE_RETIRED IS NULL OR d.DATE_RETIRED = '')
      AND d.ALIAS_PRINCIPAL = 'P'
    ON CONFLICT DO NOTHING",
    .PSV_INSERT_COLS,
    .psv_label_sql(state, street_name   = "sla.STREET_NAME",
                   street_type   = "sla.STREET_TYPE_CODE",
                   street_suffix = "sla.STREET_SUFFIX_CODE"),
    .psv_detail_cols_sql(),
    st_case_sql,
    state,
    .psv_csv(fps$detail),
    .psv_csv(fps$street),
    .psv_csv(fps$str_alias),
    .psv_csv(fps$locality),
    .psv_common_joins_sql(fps)
  ))
}
