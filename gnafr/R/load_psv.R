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
#'     \code{ADDRESS_DETAIL} joined to its geocode, street, and locality.
#'     Records with \code{ALIAS_PRINCIPAL = 'A'} are included and flagged
#'     (e.g. \code{alias_type = "ADDRESS:RA"}).
#'   \item \strong{Locality alias addresses} — for each address in a locality
#'     that has a \code{LOCALITY_ALIAS} entry, a duplicate record is created
#'     carrying the alias locality name and its postcode (when different).
#'     These allow matching when a person writes a recognised alternative
#'     suburb name.  Tagged \code{"LOCALITY:SYN"} or \code{"LOCALITY:SR"}.
#'   \item \strong{Street alias addresses} — for each address on a street that
#'     has a \code{STREET_LOCALITY_ALIAS} entry, a duplicate record is created
#'     with the alias street name / type.  Tagged \code{"STREET:SYN"} or
#'     \code{"STREET:ALT"}.
#' }
#'
#' The \code{alias_type} column in \code{gnaf_match} results is \code{NA} for
#' standard principal records and a short code (e.g. \code{"LOCALITY:SYN"})
#' for alias variants.
#'
#' @param con DBI connection from \code{gnaf_connect}.  The database must have
#'   been initialised with \code{gnaf_init()}.
#' @param gnaf_dir Path to the G-NAF \strong{Standard} directory that contains
#'   the \code{QLD_*_psv.psv} files
#'   (e.g. \file{G-NAF MAY 2026/Standard}).
#' @param overwrite If \code{TRUE}, deletes all existing
#'   \code{source = 'gnaf'} rows before loading.  Defaults to \code{FALSE}.
#' @param load_aliases If \code{TRUE} (default), loads locality and street
#'   alias address variants in addition to the standard records.
#' @return Invisibly, the total number of GNAF rows in the database after
#'   loading.
#' @export
gnaf_load_psv <- function(con, gnaf_dir, overwrite = FALSE,
                          load_aliases = TRUE) {

  gnaf_dir <- normalizePath(gnaf_dir, mustWork = TRUE)

  required_files <- c(
    "QLD_ADDRESS_DETAIL_psv.psv",
    "QLD_ADDRESS_DEFAULT_GEOCODE_psv.psv",
    "QLD_STREET_LOCALITY_psv.psv",
    "QLD_LOCALITY_psv.psv",
    "QLD_ADDRESS_ALIAS_psv.psv"
  )
  alias_files <- c(
    "QLD_LOCALITY_ALIAS_psv.psv",
    "QLD_STREET_LOCALITY_ALIAS_psv.psv"
  )

  needed <- if (load_aliases) c(required_files, alias_files) else required_files
  missing <- needed[!file.exists(file.path(gnaf_dir, needed))]
  if (length(missing) > 0L)
    stop("Missing G-NAF files in '", gnaf_dir, "':\n  ",
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
    DBI::dbExecute(con, "DELETE FROM gnaf_addresses WHERE source = 'gnaf'")
    message("Cleared existing GNAF rows.")
  }

  # Build forward-slash paths (DuckDB accepts them on Windows)
  fps <- lapply(
    setNames(c(required_files, alias_files),
             c("detail", "geocode", "street", "locality", "addr_alias",
               "loc_alias", "str_alias")),
    function(f) gsub("\\\\", "/", file.path(gnaf_dir, f))
  )

  # ---------------------------------------------------------------------------
  # Step 1: standard + address-alias records
  # ---------------------------------------------------------------------------
  message("Loading standard GNAF addresses ...")
  n1 <- .psv_insert_standard(con, fps)
  message(sprintf("  Inserted %s address records.", format(n1, big.mark = ",")))

  # ---------------------------------------------------------------------------
  # Steps 2 & 3: locality and street alias variants
  # ---------------------------------------------------------------------------
  if (load_aliases) {
    message("Loading locality alias records ...")
    n2 <- .psv_insert_locality_aliases(con, fps)
    message(sprintf("  Inserted %s locality alias records.",
                    format(n2, big.mark = ",")))

    message("Loading street alias records ...")
    n3 <- .psv_insert_street_aliases(con, fps)
    message(sprintf("  Inserted %s street alias records.",
                    format(n3, big.mark = ",")))
  }

  total <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM gnaf_addresses")$n
  message(sprintf("Total GNAF addresses in database: %s",
                  format(total, big.mark = ",")))
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
.psv_label_sql <- function(street_name   = "sl.STREET_NAME",
                           street_type   = "sl.STREET_TYPE_CODE",
                           street_suffix = "sl.STREET_SUFFIX_CODE",
                           locality_name = "l.LOCALITY_NAME",
                           postcode      = "d.POSTCODE") {
  sprintf(
    "TRIM(
       COALESCE(d.BUILDING_NAME || ' ', '') ||
       CASE WHEN d.FLAT_NUMBER IS NOT NULL THEN
         COALESCE(d.FLAT_TYPE_CODE || ' ', '') ||
         COALESCE(d.FLAT_NUMBER_PREFIX, '') ||
         CAST(TRY_CAST(d.FLAT_NUMBER AS INTEGER) AS VARCHAR) ||
         COALESCE(d.FLAT_NUMBER_SUFFIX, '') || ' '
       ELSE '' END ||
       COALESCE(d.NUMBER_FIRST_PREFIX, '') ||
       CAST(TRY_CAST(d.NUMBER_FIRST AS INTEGER) AS VARCHAR) ||
       COALESCE(d.NUMBER_FIRST_SUFFIX, '') ||
       CASE WHEN d.NUMBER_LAST IS NOT NULL THEN
         '-' || COALESCE(d.NUMBER_LAST_PREFIX, '') ||
         CAST(TRY_CAST(d.NUMBER_LAST AS INTEGER) AS VARCHAR) ||
         COALESCE(d.NUMBER_LAST_SUFFIX, '')
       ELSE '' END ||
       ' ' || (%s) ||
       COALESCE(' ' || NULLIF((%s), ''), '') ||
       COALESCE(' ' || NULLIF((%s), ''), '') ||
       ', ' || (%s) || ' QLD ' || (%s)
     )",
    street_name, street_type, street_suffix, locality_name, postcode
  )
}

# SQL for the address-component columns derived from ADDRESS_DETAIL.
# Used identically in all three INSERT statements.
.psv_detail_cols_sql <- function() {
  "NULL::VARCHAR                                             AS address_site_name,
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
   d.LOT_NUMBER"
}

# Subquery that returns one geocode row per address (any geocode type).
.psv_geocode_cte <- function(geocode_path) {
  sprintf(
    "(SELECT ADDRESS_DETAIL_PID,
             ANY_VALUE(LONGITUDE) AS LONGITUDE,
             ANY_VALUE(LATITUDE)  AS LATITUDE
      FROM %s
      WHERE DATE_RETIRED IS NULL OR DATE_RETIRED = ''
      GROUP BY ADDRESS_DETAIL_PID)",
    .psv_csv(geocode_path)
  )
}

# ---------------------------------------------------------------------------
# Step 1: load ADDRESS_DETAIL (principal + alias records)
# ---------------------------------------------------------------------------
.psv_insert_standard <- function(con, fps) {
  DBI::dbExecute(con, sprintf("
    INSERT INTO gnaf_addresses (
      address_detail_pid, address_label, address_site_name, building_name,
      flat_type, flat_number, level_type, level_number,
      number_first, number_last, lot_number,
      street_name, street_type, street_suffix,
      locality_name, state, postcode,
      longitude, latitude, source, alias_type
    )
    SELECT
      d.ADDRESS_DETAIL_PID,
      %s                                                    AS address_label,
      %s,
      sl.STREET_NAME,
      sl.STREET_TYPE_CODE,
      sl.STREET_SUFFIX_CODE,
      l.LOCALITY_NAME,
      'QLD',
      TRY_CAST(d.POSTCODE AS INTEGER),
      g.LONGITUDE,
      g.LATITUDE,
      'gnaf',
      CASE WHEN d.ALIAS_PRINCIPAL = 'A' THEN
        'ADDRESS:' || COALESCE(aa.ALIAS_TYPE_CODE, 'ALIAS')
      END
    FROM %s d
    JOIN %s sl
      ON  d.STREET_LOCALITY_PID = sl.STREET_LOCALITY_PID
      AND (sl.DATE_RETIRED IS NULL OR sl.DATE_RETIRED = '')
    JOIN %s l
      ON  d.LOCALITY_PID = l.LOCALITY_PID
      AND (l.DATE_RETIRED IS NULL OR l.DATE_RETIRED = '')
    LEFT JOIN %s g
      ON  g.ADDRESS_DETAIL_PID = d.ADDRESS_DETAIL_PID
    LEFT JOIN (
      SELECT ALIAS_PID, MAX(ALIAS_TYPE_CODE) AS ALIAS_TYPE_CODE
      FROM %s
      WHERE DATE_RETIRED IS NULL OR DATE_RETIRED = ''
      GROUP BY ALIAS_PID
    ) aa ON aa.ALIAS_PID = d.ADDRESS_DETAIL_PID
    WHERE (d.DATE_RETIRED IS NULL OR d.DATE_RETIRED = '')
    ON CONFLICT DO NOTHING",
    .psv_label_sql(),
    .psv_detail_cols_sql(),
    .psv_csv(fps$detail),
    .psv_csv(fps$street),
    .psv_csv(fps$locality),
    .psv_geocode_cte(fps$geocode),
    .psv_csv(fps$addr_alias)
  ))
}

# ---------------------------------------------------------------------------
# Step 2: locality alias records
# One copy per address per active locality alias.
# Derived PID: <original_pid>_LA<alias_pid>
# ---------------------------------------------------------------------------
.psv_insert_locality_aliases <- function(con, fps) {
  DBI::dbExecute(con, sprintf("
    INSERT INTO gnaf_addresses (
      address_detail_pid, address_label, address_site_name, building_name,
      flat_type, flat_number, level_type, level_number,
      number_first, number_last, lot_number,
      street_name, street_type, street_suffix,
      locality_name, state, postcode,
      longitude, latitude, source, alias_type
    )
    SELECT
      d.ADDRESS_DETAIL_PID || '_LA' || la.LOCALITY_ALIAS_PID,
      %s                                                    AS address_label,
      %s,
      sl.STREET_NAME,
      sl.STREET_TYPE_CODE,
      sl.STREET_SUFFIX_CODE,
      la.NAME                                               AS locality_name,
      'QLD',
      COALESCE(TRY_CAST(la.POSTCODE AS INTEGER), TRY_CAST(d.POSTCODE AS INTEGER)) AS postcode,
      g.LONGITUDE,
      g.LATITUDE,
      'gnaf',
      'LOCALITY:' || la.ALIAS_TYPE_CODE
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
    LEFT JOIN %s g
      ON  g.ADDRESS_DETAIL_PID = d.ADDRESS_DETAIL_PID
    WHERE (d.DATE_RETIRED IS NULL OR d.DATE_RETIRED = '')
      AND d.ALIAS_PRINCIPAL = 'P'
    ON CONFLICT DO NOTHING",
    .psv_label_sql(locality_name = "la.NAME",
                   postcode      = "COALESCE(CAST(la.POSTCODE AS VARCHAR), CAST(d.POSTCODE AS VARCHAR))"),
    .psv_detail_cols_sql(),
    .psv_csv(fps$detail),
    .psv_csv(fps$street),
    .psv_csv(fps$locality),
    .psv_csv(fps$loc_alias),
    .psv_geocode_cte(fps$geocode)
  ))
}

# ---------------------------------------------------------------------------
# Step 3: street alias records
# One copy per address per active street locality alias.
# Derived PID: <original_pid>_SA<alias_pid>
# ---------------------------------------------------------------------------
.psv_insert_street_aliases <- function(con, fps) {
  DBI::dbExecute(con, sprintf("
    INSERT INTO gnaf_addresses (
      address_detail_pid, address_label, address_site_name, building_name,
      flat_type, flat_number, level_type, level_number,
      number_first, number_last, lot_number,
      street_name, street_type, street_suffix,
      locality_name, state, postcode,
      longitude, latitude, source, alias_type
    )
    SELECT
      d.ADDRESS_DETAIL_PID || '_SA' || sla.STREET_LOCALITY_ALIAS_PID,
      %s                                                    AS address_label,
      %s,
      sla.STREET_NAME,
      sla.STREET_TYPE_CODE,
      sla.STREET_SUFFIX_CODE,
      l.LOCALITY_NAME,
      'QLD',
      TRY_CAST(d.POSTCODE AS INTEGER),
      g.LONGITUDE,
      g.LATITUDE,
      'gnaf',
      'STREET:' || sla.ALIAS_TYPE_CODE
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
    LEFT JOIN %s g
      ON  g.ADDRESS_DETAIL_PID = d.ADDRESS_DETAIL_PID
    WHERE (d.DATE_RETIRED IS NULL OR d.DATE_RETIRED = '')
      AND d.ALIAS_PRINCIPAL = 'P'
    ON CONFLICT DO NOTHING",
    .psv_label_sql(street_name   = "sla.STREET_NAME",
                   street_type   = "sla.STREET_TYPE_CODE",
                   street_suffix = "sla.STREET_SUFFIX_CODE"),
    .psv_detail_cols_sql(),
    .psv_csv(fps$detail),
    .psv_csv(fps$street),
    .psv_csv(fps$str_alias),
    .psv_csv(fps$locality),
    .psv_geocode_cte(fps$geocode)
  ))
}
