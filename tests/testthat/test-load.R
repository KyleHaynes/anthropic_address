core_csv_row <- function() {
  fields <- c(
    "ADDRESS_DETAIL_PID", "ADDRESS_LABEL", "ADDRESS_SITE_NAME", "BUILDING_NAME",
    "FLAT_TYPE", "FLAT_NUMBER", "LEVEL_TYPE", "LEVEL_NUMBER", "NUMBER_FIRST",
    "NUMBER_LAST", "LOT_NUMBER", "STREET_NAME", "STREET_TYPE", "STREET_SUFFIX",
    "LOCALITY_NAME", "STATE", "POSTCODE", "LONGITUDE", "LATITUDE", "DATE_CREATED",
    "LEGAL_PARCEL_ID", "MB_CODE", "ALIAS_PRINCIPAL", "PRINCIPAL_PID",
    "PRIMARY_SECONDARY", "PRIMARY_PID", "GEOCODE_TYPE"
  )
  row <- data.table::as.data.table(stats::setNames(as.list(rep(NA_character_, length(fields))), fields))
  row[, `:=`(ADDRESS_DETAIL_PID = "NEW", ADDRESS_LABEL = "UNIT 01 10 SMITH ROAD, BRISBANE QLD 4000",
             NUMBER_FIRST = "10", STREET_NAME = "SMITH", STREET_TYPE = "RD",
             LOCALITY_NAME = "BRISBANE", STATE = "QLD", POSTCODE = "4000",
             FLAT_TYPE = "UNIT", FLAT_NUMBER = "01", DATE_CREATED = "2017-07-27")]
  row
}

test_that("CSV imports preserve string identifiers and ISO dates in quoted paths", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  path <- tempfile(pattern = "gnaf's-", fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  data.table::fwrite(core_csv_row(), path)
  expect_equal(suppressMessages(gnaf_load(con, path)), 1)
  row <- DBI::dbGetQuery(con, "SELECT flat_number, street_type, date_created FROM gnaf_addresses")
  expect_equal(row$flat_number, "01")
  expect_equal(row$street_type, "ROAD")
  expect_equal(row$date_created, as.Date("2017-07-27"))
})

test_that("failed multi-file CSV replacement retains addresses, index and cache", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_addresses
    (address_detail_pid, source, locality_name, state, postcode)
    VALUES ('OLD', 'gnaf', 'BRISBANE', 'QLD', 4000)")
  gnaf_rebuild_locality_index(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_match_cache
    (input_standardised, address_detail_pid, total_score) VALUES ('OLD', 'OLD', 100)")
  paths <- c(tempfile(fileext = ".csv"), tempfile(fileext = ".csv"))
  on.exit(unlink(paths), add = TRUE)
  data.table::fwrite(core_csv_row(), paths[1L])
  data.table::fwrite(data.table::data.table(wrong_column = "bad"), paths[2L])
  expect_error(suppressMessages(gnaf_load(con, paths, overwrite = TRUE)))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM gnaf_addresses")$address_detail_pid, "OLD")
  expect_equal(DBI::dbGetQuery(con,
    "SELECT locality_name FROM gnaf_locality_index")$locality_name, "BRISBANE")
  expect_equal(gnaf_cache_status(con)$rows, 1)
})

test_that("failed PSV replacement retains the previous state", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_addresses
    (address_detail_pid, source, state) VALUES ('OLD', 'gnaf', 'QLD')")
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  specs <- c("ADDRESS_DETAIL", "ADDRESS_DEFAULT_GEOCODE", "STREET_LOCALITY",
             "LOCALITY", "ADDRESS_ALIAS", "PRIMARY_SECONDARY", "ADDRESS_SITE",
             "ADDRESS_MESH_BLOCK_2021", "MB_2021")
  for (spec in specs) writeLines(c("wrong_column", "bad"),
    file.path(path, paste0("QLD_", spec, "_psv.psv")))
  expect_error(suppressMessages(gnaf_load_psv(con, path, overwrite = TRUE, load_aliases = FALSE)))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM gnaf_addresses")$address_detail_pid, "OLD")
})
