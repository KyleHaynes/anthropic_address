test_that("sampling functions reject vector-valued counts clearly", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)

  expect_error(sample_gnaf(con, n = c(1L, 2L)),
               "'n' must be a single positive integer", fixed = TRUE)
  expect_error(gnaf_cache_sample(con, n = c(1L, 2L)),
               "'n' must be a single positive integer", fixed = TRUE)
})

test_that("address perturbation validates scalar counts", {
  addresses <- data.frame(address_label = "1 TEST STREET")

  expect_error(address_perturb_sample(addresses, n = c(1L, 2L)),
               "'n' must be a single positive integer", fixed = TRUE)
  expect_error(
    address_perturb_sample(addresses, max_changes = c(1L, 2L)),
    "'max_changes' must be a single positive integer",
    fixed = TRUE
  )
})

test_that("cache timestamps reject missing and vector-valued inputs clearly", {
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  gnaf_init(con)

  expect_error(
    gnaf_cache_rollback(con, after = NA_character_, ask = FALSE),
    "'after' must be coercible to POSIXct",
    fixed = TRUE
  )
  expect_error(
    gnaf_cache_rollback(
      con,
      after = c("2024-01-01", "2024-01-02"),
      ask = FALSE
    ),
    "'after' must be a single value coercible to POSIXct",
    fixed = TRUE
  )
})
