library(data.table)

# ---- Input validation -------------------------------------------------------

test_that("gnaf_match rejects non-character addresses", {
  expect_error(
    gnaf_match(NULL, addresses = 123L),
    "'addresses' must be a non-empty character vector"
  )
})

test_that("gnaf_match rejects empty character vector", {
  expect_error(
    gnaf_match(NULL, addresses = character(0)),
    "'addresses' must be a non-empty character vector"
  )
})

# ---- .standardise_input() ---------------------------------------------------

make_parsed <- function(in_number_first = 25L,
                        in_number_last  = NA_integer_,
                        in_flat_type    = NA_character_,
                        in_flat_number  = NA_character_,
                        in_building_name = NA_character_,
                        in_street_name  = "SAINT JAMES",
                        in_street_type  = "COURT",
                        in_street_suffix = NA_character_,
                        in_locality     = "TAMBORINE MOUNTAIN",
                        in_state        = "QLD",
                        in_postcode     = 4272L) {
  data.table(
    in_number_first  = in_number_first,
    in_number_last   = in_number_last,
    in_flat_type     = in_flat_type,
    in_flat_number   = in_flat_number,
    in_building_name = in_building_name,
    in_street_name   = in_street_name,
    in_street_type   = in_street_type,
    in_street_suffix = in_street_suffix,
    in_locality      = in_locality,
    in_state         = in_state,
    in_postcode      = in_postcode
  )
}

test_that("standardise_input produces expected string for full address", {
  p <- make_parsed()
  out <- gnafr:::.standardise_input(p)
  expect_equal(out, "25 SAINT JAMES COURT, TAMBORINE MOUNTAIN QLD 4272")
})

test_that("standardise_input includes flat type and number", {
  p <- make_parsed(in_flat_type = "UNIT", in_flat_number = "3")
  out <- gnafr:::.standardise_input(p)
  expect_true(grepl("^UNIT 3", out))
})

test_that("standardise_input handles number range", {
  p <- make_parsed(in_number_first = 110L, in_number_last = 120L)
  out <- gnafr:::.standardise_input(p)
  expect_true(grepl("110-120", out))
})

test_that("standardise_input returns NA for all-NA input", {
  p <- make_parsed(
    in_number_first = NA_integer_,
    in_street_name  = NA_character_,
    in_street_type  = NA_character_,
    in_locality     = NA_character_,
    in_state        = NA_character_,
    in_postcode     = NA_integer_
  )
  out <- gnafr:::.standardise_input(p)
  expect_true(is.na(out))
})

test_that("standardise_input is vectorised over multiple rows", {
  p <- rbindlist(list(
    make_parsed(in_number_first = 25L, in_street_name = "SAINT JAMES",
                in_postcode = 4272L),
    make_parsed(in_number_first = 10L, in_street_name = "SMITH",
                in_street_type = "AVENUE", in_locality = "BRISBANE",
                in_postcode = 4000L)
  ))
  out <- gnafr:::.standardise_input(p)
  expect_equal(length(out), 2L)
  expect_true(grepl("SAINT JAMES", out[1]))
  expect_true(grepl("SMITH",       out[2]))
})

# ---- .collapse_address_parts() ----------------------------------------------

test_that("collapse_address_parts joins non-NA parts with spaces", {
  out <- gnafr:::.collapse_address_parts("10", "SMITH", "STREET")
  expect_equal(out, "10 SMITH STREET")
})

test_that("collapse_address_parts ignores NA parts", {
  out <- gnafr:::.collapse_address_parts(NA_character_, "SMITH", "STREET")
  expect_equal(out, "SMITH STREET")
})

test_that("collapse_address_parts returns NA when all parts are NA", {
  out <- gnafr:::.collapse_address_parts(NA_character_, NA_character_)
  expect_true(is.na(out))
})

test_that("match_address is working as expected (expected results and reproducible)", {
  con <- gnaf_connect("C:/temp/gnaf.duckdb")
  test <- gnaf_match(c(
        "25 ST JAMES CR EAGLE HEIGHTS QLD 4271",
        "25 ST JAMES CR EAGLE HEIGHTS 4271 QLD",
        "110-120 MUSGRADE RD RED HILL QLD 4060",
        "112 MUSGRAVE RD RED HILLS QLD 4059",
        "112 MUSGRAVE RD PADDINGTON 4059",
        "PARLAND 6019/6 PARKLAND BVD BRISBANE QLD 4001",
        "PARLAND 6019 6 PARKLAND BVD BRISBANE QLD 4001",
        "U6019 6 PARKLAND BVD BRISBANE QLD 4001"
    ), con = con, max_results = 1)

    test_vec <- test$address_label == c(
        "25 SAINT JAMES COURT, EAGLE HEIGHTS QLD 4272",
        "25 SAINT JAMES COURT, EAGLE HEIGHTS QLD 4272",
        "110 MUSGRAVE RD, RED HILL QLD 4059",
        "110-120 MUSGRAVE RD, RED HILL QLD 4059",
        "110-120 MUSGRAVE ROAD, PADDINGTON QLD 4059",
        "UNIT 6019 6 PARKLAND BOULEVARD, BRISBANE QLD 4000",
        "UNIT 6019 6 PARKLAND BOULEVARD, BRISBANE QLD 4000",
        "UNIT 6019 6 PARKLAND BOULEVARD, BRISBANE QLD 4000"
    )
  expect_true(all(test_vec))
})
