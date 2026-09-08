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

test_that("counts reject fractional values instead of silently truncating them", {
  expect_error(gnaf_match("x", NULL, max_results = 1.5), "positive integer")
  expect_error(gnaf_match("x", NULL, max_results = TRUE), "positive integer")
  expect_error(address_perturb_sample(data.frame(address_label = "10 SMITH ROAD"),
                                      max_changes = 1.5), "positive integer")
})

test_that("matching validates thresholds and flags before opening queries", {
  expect_error(gnaf_match("x", NULL, fallback_threshold = NA_real_), "fallback_threshold")
  expect_error(gnaf_match("x", NULL, cache_threshold = c(0, 95)), "cache_threshold")
  expect_error(gnaf_match("x", NULL, include_custom = NA), "include_custom")
  expect_error(gnaf_match("x", NULL, cache = c(TRUE, FALSE)), "cache")
})

test_that("each scoring weight must be one finite number with a unique name", {
  weights <- gnafr:::.default_match_weights()
  weights$postcode <- c(10, 10)
  expect_error(gnafr:::.validate_match_weights(weights), "one finite number")
  weights$postcode <- NULL
  weights$postcode <- list(20)
  expect_error(gnafr:::.validate_match_weights(weights), "one finite number")
  weights <- c(gnafr:::.default_match_weights(), postcode = list(20))
  expect_error(gnafr:::.validate_match_weights(weights), "exactly these entries")
})
