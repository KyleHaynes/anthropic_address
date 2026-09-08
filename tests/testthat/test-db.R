test_that("initialising a legacy schema restores address fields and versions its cache", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)
  for (index in c("idx_gnaf_pc", "idx_cust_pc", "idx_gnaf_label", "idx_cust_label"))
    DBI::dbExecute(con, paste("DROP INDEX", index))
  for (table in c("gnaf_addresses", "custom_addresses")) {
    for (column in c("address_site_name", "level_type", "level_number", "lot_number"))
      DBI::dbExecute(con, sprintf("ALTER TABLE %s DROP COLUMN %s", table, column))
  }
  DBI::dbExecute(con, "ALTER TABLE gnaf_match_cache DROP COLUMN algorithm_version")
  DBI::dbExecute(con, "INSERT INTO gnaf_match_cache
    (input_standardised, address_detail_pid, total_score) VALUES ('old', 'A', 100)")
  gnaf_init(con)
  for (table in c("gnaf_addresses", "custom_addresses"))
    expect_true(all(c("address_site_name", "level_type", "level_number", "lot_number") %in%
      DBI::dbListFields(con, table)))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT algorithm_version FROM gnaf_match_cache")$algorithm_version, 1L)
  expect_no_error(gnaf_init(con))
})
