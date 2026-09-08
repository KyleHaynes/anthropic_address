test_that("cache time filters compare instants in UTC and reject invalid windows", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  DBI::dbExecute(con, "INSERT INTO gnaf_match_cache
    (input_standardised, address_detail_pid, total_score, cached_at)
    VALUES ('old', 'A', 100, '2026-01-01 01:00:00'),
           ('new', 'B', 100, '2026-01-01 03:00:00')")
  after <- as.POSIXct("2026-01-01 12:00:00", tz = "Australia/Brisbane")
  expect_equal(suppressMessages(gnaf_cache_rollback(con, after, ask = FALSE)), 1)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT input_standardised FROM gnaf_match_cache")$input_standardised, "old")
  expect_error(gnaf_cache_sample(con, cached_on = c("2026-01-01", "2026-01-02")),
               "'cached_on' must be a single value", fixed = TRUE)
  expect_error(gnaf_cache_sample(con, from = "2026-02-01", to = "2026-01-01"),
               "'from' must be on or before 'to'", fixed = TRUE)
})

test_that("cache read hits do not rewrite their creation timestamp", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  row <- data.table::data.table(number_first = 10L, street_name = "SMITH",
    street_type = "ROAD", locality_name = "BRISBANE", state = "QLD", postcode = 4000L)
  suppressMessages(gnaf_add(con, row))
  input <- rep("10 Smith Rd, Brisbane QLD 4000", 3L)
  first <- gnaf_match(input, con, verbose = FALSE)
  DBI::dbExecute(con, "UPDATE gnaf_match_cache SET cached_at = '2026-01-01 00:00:00'")
  second <- gnaf_match(input, con, verbose = FALSE)
  expect_equal(second$address_detail_pid, first$address_detail_pid)
  expect_equal(gnaf_cache_status(con)$rows, 1)
  expect_equal(as.character(gnaf_cache_status(con)$newest_cached), "2026-01-01")
})
