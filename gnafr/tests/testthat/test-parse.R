library(data.table)

# ---- Basic well-formed addresses -------------------------------------------

test_that("standard address parses all fields", {
  r <- address_parse("25 SAINT JAMES CT, TAMBORINE MOUNTAIN QLD 4272")
  expect_equal(r$in_number_first, 25L)
  expect_equal(r$in_street_name,  "SAINT JAMES")
  expect_equal(r$in_street_type,  "COURT")
  expect_equal(r$in_locality,     "TAMBORINE MOUNTAIN")
  expect_equal(r$in_state,        "QLD")
  expect_equal(r$in_postcode,     4272L)
})

# ---- Common user errors: wrong / abbreviated street type -------------------

test_that("Rd instead of Ct still parses street name correctly", {
  r <- address_parse("25 St James Rd, Tamborine Mountain QLD 4272")
  expect_equal(r$in_street_name, "ST JAMES")
  expect_equal(r$in_street_type, "ROAD")
  expect_equal(r$in_number_first, 25L)
})

test_that("Street instead of Drive parses correctly", {
  r <- address_parse("12 Kings Street, Sydney NSW 2000")
  expect_equal(r$in_street_name, "KINGS")
  expect_equal(r$in_street_type, "STREET")
})

test_that("abbreviated Ave parses to AVENUE", {
  r <- address_parse("10 Smith Ave, Brisbane QLD 4000")
  expect_equal(r$in_street_type, "AVENUE")
  expect_equal(r$in_street_name, "SMITH")
})

test_that("abbreviated Cres parses to CRESCENT", {
  r <- address_parse("7 Rose Cres, Perth WA 6000")
  expect_equal(r$in_street_type, "CRESCENT")
  expect_equal(r$in_street_name, "ROSE")
})

test_that("abbreviated Dr parses to DRIVE", {
  r <- address_parse("3 Oak Dr, Melbourne VIC 3000")
  expect_equal(r$in_street_type, "DRIVE")
  expect_equal(r$in_street_name, "OAK")
})

test_that("abbreviated Tce parses to TERRACE", {
  r <- address_parse("50 Murray Tce, Adelaide SA 5000")
  expect_equal(r$in_street_type, "TERRACE")
  expect_equal(r$in_street_name, "MURRAY")
})

# ---- Missing comma ----------------------------------------------------------

test_that("no comma between street and suburb still parses", {
  r <- address_parse("25 Saint James Ct Tamborine Mountain QLD 4272")
  expect_equal(r$in_number_first, 25L)
  expect_equal(r$in_postcode,     4272L)
  expect_equal(r$in_state,        "QLD")
})

# ---- Mixed case input -------------------------------------------------------

test_that("lowercase input is normalised and parsed", {
  r <- address_parse("25 saint james ct, tamborine mountain qld 4272")
  expect_equal(r$in_number_first, 25L)
  expect_equal(r$in_street_type,  "COURT")
  expect_equal(r$in_state,        "QLD")
  expect_equal(r$in_postcode,     4272L)
})

test_that("title-case input parses correctly", {
  r <- address_parse("25 Saint James Ct, Tamborine Mountain Qld 4272")
  expect_equal(r$in_street_type, "COURT")
  expect_equal(r$in_state,       "QLD")
})

# ---- Unit / flat notation --------------------------------------------------

test_that("slash notation: 3/25 Saint James Ct", {
  r <- address_parse("3/25 Saint James Ct, Tamborine Mountain QLD 4272")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "3")
  expect_equal(r$in_number_first, 25L)
  expect_equal(r$in_street_name,  "SAINT JAMES")
})

test_that("UNIT prefix notation parses flat and number", {
  r <- address_parse("UNIT 3 25 Smith St, Sydney NSW 2000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "3")
  expect_equal(r$in_number_first, 25L)
})

test_that("APT prefix notation parses flat", {
  r <- address_parse("APT 4 10 Main Rd, Melbourne VIC 3000")
  expect_equal(r$in_flat_number, "4")
  expect_equal(r$in_number_first, 10L)
})

test_that("attached U prefix: U3 25 Smith St", {
  r <- address_parse("U3 25 Smith St, Sydney NSW 2000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "3")
  expect_equal(r$in_number_first, 25L)
})

# ---- Street number ranges --------------------------------------------------

test_that("range number: 110-120 Musgrave Rd parses first and last", {
  r <- address_parse("110-120 Musgrave Rd, Red Hill QLD 4059")
  expect_equal(r$in_number_first, 110L)
  expect_equal(r$in_number_last,  120L)
  expect_equal(r$in_street_name,  "MUSGRAVE")
})

# ---- Multi-word street names -----------------------------------------------

test_that("multi-word street name like Saint James is preserved", {
  r <- address_parse("25 Saint James Ct, Tamborine Mountain QLD 4272")
  expect_equal(r$in_street_name, "SAINT JAMES")
})

test_that("multi-word locality is preserved", {
  r <- address_parse("25 Saint James Ct, Tamborine Mountain QLD 4272")
  expect_equal(r$in_locality, "TAMBORINE MOUNTAIN")
})

# ---- Missing postcode / state ----------------------------------------------

test_that("address without postcode has NA in_postcode", {
  r <- address_parse("25 Saint James Ct, Tamborine Mountain QLD")
  expect_true(is.na(r$in_postcode))
  expect_equal(r$in_state, "QLD")
})

test_that("address without state has NA in_state", {
  r <- address_parse("25 Saint James Ct, Tamborine Mountain 4272")
  expect_true(is.na(r$in_state))
  expect_equal(r$in_postcode, 4272L)
})

# ---- Periods in address (e.g. "St." abbreviation) -------------------------

test_that("period after street type abbreviation is stripped", {
  r <- address_parse("10 Oak St. Sydney NSW 2000")
  expect_equal(r$in_street_type, "STREET")
  expect_equal(r$in_number_first, 10L)
})

# ---- Vectorised input ------------------------------------------------------

test_that("multiple addresses returned as one row each", {
  addrs <- c(
    "25 Saint James Ct, Tamborine Mountain QLD 4272",
    "10 Smith Ave, Brisbane QLD 4000"
  )
  r <- address_parse(addrs)
  expect_equal(nrow(r), 2L)
  expect_equal(r$input_id, c(1L, 2L))
  expect_equal(r$in_number_first, c(25L, 10L))
  expect_equal(r$in_street_type,  c("COURT", "AVENUE"))
})

test_that("empty string gives all-NA row", {
  r <- address_parse("")
  expect_equal(nrow(r), 1L)
  expect_true(is.na(r$in_postcode))
  expect_true(is.na(r$in_street_name))
})

# ---- Alpha-suffixed street numbers (e.g. 190A, 10B) -----------------------

test_that("number with trailing alpha extracts integer and suffix", {
  r <- address_parse("190A MUSGRAVE RD RED HILL QLD 4059")
  expect_equal(r$in_number_first,  190L)
  expect_equal(r$in_number_suffix, "A")
  expect_equal(r$in_street_name,   "MUSGRAVE")
  expect_equal(r$in_street_type,   "ROAD")
  expect_equal(r$in_locality,      "RED HILL")
  expect_equal(r$in_postcode,      4059L)
  expect_equal(r$in_state,         "QLD")
})

test_that("lowercase alpha suffix is normalised before extraction", {
  r <- address_parse("190a MUSGRAVE RD RED HILL QLD 4059")
  expect_equal(r$in_number_first,  190L)
  expect_equal(r$in_number_suffix, "A")
  expect_equal(r$in_street_name,   "MUSGRAVE")
})

test_that("plain number has NA suffix", {
  r <- address_parse("190 MUSGRAVE RD RED HILL QLD 4059")
  expect_equal(r$in_number_first, 190L)
  expect_true(is.na(r$in_number_suffix))
})

test_that("flat + alpha-suffixed street number parses all fields", {
  r <- address_parse("UNIT 3 190A MUSGRAVE RD RED HILL QLD 4059")
  expect_equal(r$in_flat_type,     "UNIT")
  expect_equal(r$in_flat_number,   "3")
  expect_equal(r$in_number_first,  190L)
  expect_equal(r$in_number_suffix, "A")
  expect_equal(r$in_street_name,   "MUSGRAVE")
})
