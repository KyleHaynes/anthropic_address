custom_row <- function() {
  data.table::data.table(
    number_first = 10L, street_name = "SMITH", street_type = "RD",
    locality_name = "BRISBANE", state = "QLD", postcode = 4000L
  )
}

test_that("generated custom PIDs remain unique after deletions", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  rows <- custom_row()[rep(1L, 3L)]
  suppressMessages(gnaf_add(con, rows))
  gnaf_remove_custom(con, "CUSTOM_2")
  expect_equal(suppressMessages(gnaf_add(con, custom_row())), 1)
  expect_setequal(DBI::dbGetQuery(con,
    "SELECT address_detail_pid FROM custom_addresses")$address_detail_pid,
    c("CUSTOM_1", "CUSTOM_3", "CUSTOM_4"))
})

test_that("custom upserts count updates, canonicalise types and refresh localities", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  row <- custom_row()
  row[, address_detail_pid := "CUSTOM_A"]
  original <- data.table::copy(row)
  expect_equal(suppressMessages(gnaf_add(con, row)), 1)
  expect_identical(row, original)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT street_type FROM custom_addresses")$street_type, "ROAD")

  row[, `:=`(locality_name = "SYDNEY", postcode = 2000L, state = "NSW",
             alias_type = "STREET:SYN")]
  expect_equal(suppressMessages(gnaf_add(con, row, upsert = TRUE)), 1)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT alias_type FROM custom_addresses")$alias_type, "STREET:SYN")
  expect_equal(DBI::dbGetQuery(con,
    "SELECT locality_name FROM gnaf_locality_index")$locality_name, "SYDNEY")

  row[, locality_name := "IGNORED"]
  expect_equal(suppressMessages(gnaf_add(con, row)), 0)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT locality_name FROM gnaf_locality_index")$locality_name, "SYDNEY")
})

test_that("empty custom operations are no-ops and identifiers are validated", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  expect_equal(gnaf_add(con, custom_row()[0L]), 0)
  expect_equal(gnaf_remove_custom(con, character()), 0)
  expect_error(gnaf_remove_custom(con, NA_character_), "pids")
  row <- custom_row()
  row[, address_detail_pid := NA_character_]
  expect_error(gnaf_add(con, row), "address_detail_pid")
  row[, address_detail_pid := "O'BRIEN"]
  suppressMessages(gnaf_add(con, row))
  expect_equal(gnaf_remove_custom(con, "O'BRIEN"), 1)
})

test_that("a failed custom index update rolls back the address insert", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  DBI::dbExecute(con, "DROP TABLE gnaf_locality_index")
  DBI::dbExecute(con, "CREATE TABLE gnaf_locality_index (broken INTEGER)")
  expect_error(suppressMessages(gnaf_add(con, custom_row())))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM custom_addresses")$n, 0)
})
