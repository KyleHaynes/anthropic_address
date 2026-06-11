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
  expect_equal(r$in_street_name, "SAINT JAMES")
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

# ---- Missing street type ----------------------------------------------------

test_that("missing street type still extracts number and guesses street/locality split", {
  r <- address_parse("190 MUSGRAVE RED HILL QLD 4059")
  expect_equal(r$in_number_first, 190L)
  expect_true(is.na(r$in_street_type))
  expect_equal(r$in_street_name, "MUSGRAVE")
  expect_equal(r$in_locality,    "RED HILL")
  expect_equal(r$in_state,       "QLD")
  expect_equal(r$in_postcode,    4059L)
})

test_that("missing street type with single-word remainder is treated as street name", {
  r <- address_parse("190 MUSGRAVE QLD 4059")
  expect_equal(r$in_number_first, 190L)
  expect_true(is.na(r$in_street_type))
  expect_equal(r$in_street_name, "MUSGRAVE")
  expect_true(is.na(r$in_locality))
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

# ---- Unit/street-number ambiguity & noisy prefixes -------------------------
# All of the following describe the same address — "Unit 6019, 6 Parkland
# Boulevard" — written with varying noise, marker placement and ordering.
# They should all parse to identical flat/number/street fields.

test_that("implied-pair convention picks rightmost NUM NUM STREETNAME over a noisy leading flat marker", {
  r <- address_parse("U10 BLAH 6019 6 parkland bvd brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
  expect_equal(r$in_street_type,  "BOULEVARD")
})

test_that("explicit UNIT marker mid-string after the street number is recognised", {
  r <- address_parse("6 UNIT 6019 parkland bvd brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
})

test_that("explicit UNIT marker after a noisy leading flat marker resolves to the trailing pair", {
  r <- address_parse("U10 BLAH UNIT 6019 6 parkland bvd brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
})

test_that("explicit UNIT marker preceded by a noisy leading number+street phrase resolves to the trailing pair", {
  r <- address_parse("5 BLIND ROAD UNIT 6019 6 parkland bvd brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
})

test_that("attached U-prefix flat number with following street number parses correctly", {
  r <- address_parse("U6019 6 parkland bvd brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
})

# ---- Comma hint: word before the (last) comma is structurally the type -----
# Most real-world addresses look like "1 SMITH ST, BRISBANE QLD 4000". When
# present, the comma-adjacent word disambiguates the street type globally —
# both for coincidental abbreviation collisions inside multi-word street
# names, and for misspelt types that the generic fuzzy scan would otherwise
# attribute to a street-name word that merely resembles a type.

test_that("misspelt street type before a comma resolves over a coincidental name collision", {
  r <- address_parse("UNIT 6019 6 parkland bvdz, brisbane city QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "6019")
  expect_equal(r$in_number_first, 6L)
  expect_equal(r$in_street_name,  "PARKLAND")
  expect_equal(r$in_street_type,  "BOULEVARD")
  expect_equal(r$in_locality,     "BRISBANE CITY")
})

test_that("comma hint resolves a misspelt type that collides with an abbreviation inside the street name", {
  r <- address_parse("25 St James Rode, Tamborine Mountain QLD 4272")
  expect_equal(r$in_street_name, "SAINT JAMES")
  expect_equal(r$in_street_type, "ROAD")
  expect_equal(r$in_locality,    "TAMBORINE MOUNTAIN")
})

test_that("comma hint resolves a near-miss abbreviation to its canonical type", {
  r <- address_parse("1 Smith STX, Brisbane QLD 4000")
  expect_equal(r$in_street_name, "SMITH")
  expect_equal(r$in_street_type, "STREET")
  expect_equal(r$in_locality,    "BRISBANE")
})

test_that("comma hint is ignored when the comma-adjacent word doesn't look like a street type", {
  r <- address_parse("3/25 Saint James Ct, Tamborine Mountain QLD 4272")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "3")
  expect_equal(r$in_street_name,  "SAINT JAMES")
  expect_equal(r$in_street_type,  "COURT")
})

test_that("comma hint uses the last comma so a leading unit comma isn't mistaken for the type", {
  r <- address_parse("UNIT 5, 10 Smith St, Brisbane QLD 4000")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "5")
  expect_equal(r$in_number_first, 10L)
  expect_equal(r$in_street_name,  "SMITH")
  expect_equal(r$in_street_type,  "STREET")
  expect_equal(r$in_locality,     "BRISBANE")
})

# ---- Attached single-letter flat prefix (F8, A6, D2 etc.) ------------------
# User-supplied addresses often use an informal shorthand: a single capital
# letter immediately followed by the flat number, with no space — e.g. "F8"
# (Flat 8), "A6" (Apartment 6). The street number follows as a separate token.

test_that("F-prefix flat parses flat_type=FLAT, flat_number, and correct street number", {
  r <- address_parse("F8 536 BEACONSFIELD TERRACE, BRIGHTON QLD 4017")
  expect_equal(r$in_flat_type,    "FLAT")
  expect_equal(r$in_flat_number,  "8")
  expect_equal(r$in_number_first, 536L)
  expect_equal(r$in_street_name,  "BEACONSFIELD")
  expect_equal(r$in_street_type,  "TERRACE")
  expect_equal(r$in_locality,     "BRIGHTON")
})

test_that("A-prefix flat parses flat_type=APARTMENT and correct street number", {
  r <- address_parse("A6 536 BEACONSFIELD TERRACE, BRIGHTON QLD 4017")
  expect_equal(r$in_flat_type,    "APARTMENT")
  expect_equal(r$in_flat_number,  "6")
  expect_equal(r$in_number_first, 536L)
  expect_equal(r$in_street_name,  "BEACONSFIELD")
})

test_that("unknown-letter prefix defaults to UNIT flat_type", {
  r <- address_parse("D2 536 BEACONSFIELD TERRACE, BRIGHTON QLD 4017")
  expect_equal(r$in_flat_type,    "UNIT")
  expect_equal(r$in_flat_number,  "2")
  expect_equal(r$in_number_first, 536L)
  expect_equal(r$in_street_name,  "BEACONSFIELD")
})

test_that("letter+digit flat designator after street number is extracted from street name", {
  # "36 A1 TAVISTOCK ST" — "A1" is Apartment 1, not part of street name
  r <- address_parse("36 A1 TAVISTOCK ST, TORQUAY QLD 4655")
  expect_equal(r$in_flat_type,    "APARTMENT")
  expect_equal(r$in_flat_number,  "1")
  expect_equal(r$in_number_first, 36L)
  expect_equal(r$in_street_name,  "TAVISTOCK")
  expect_equal(r$in_street_type,  "STREET")
})

# ---- Abbreviation normalization (normalize = TRUE) -------------------------

test_that("MT and MNT expand to MOUNT in street name and locality", {
  r <- address_parse("5 MT GRAVATT RD, MT GRAVATT QLD 4122")
  expect_equal(r$in_street_name, "MOUNT GRAVATT")
  expect_equal(r$in_locality,    "MOUNT GRAVATT")

  r2 <- address_parse("5 MNT VIEW DR, MNT ISA QLD 4825")
  expect_equal(r2$in_street_name, "MOUNT VIEW")
  expect_equal(r2$in_locality,    "MOUNT ISA")
})

test_that("ST expands to SAINT in street name and locality after street type is stripped", {
  r <- address_parse("10 ST JAMES CT, ST LUCIA QLD 4067")
  expect_equal(r$in_street_name, "SAINT JAMES")
  expect_equal(r$in_street_type, "COURT")
  expect_equal(r$in_locality,    "SAINT LUCIA")
})

test_that("NTH and STH expand to NORTH and SOUTH", {
  r <- address_parse("1 NTH QUAY ST, STH BRISBANE QLD 4101")
  expect_equal(r$in_street_name, "NORTH QUAY")
  expect_equal(r$in_locality,    "SOUTH BRISBANE")
})

test_that("leading single-letter compass expands only when field-initial", {
  r <- address_parse("1 N SHORE DR, W END QLD 4101")
  expect_equal(r$in_street_name, "NORTH SHORE")
  expect_equal(r$in_locality,    "WEST END")
})

test_that("CK expands to CREEK", {
  r <- address_parse("12 MOUNTAIN CK RD, MOUNTAIN CREEK QLD 4557")
  expect_equal(r$in_street_name, "MOUNTAIN CREEK")
  expect_equal(r$in_locality,    "MOUNTAIN CREEK")
})

test_that("ordinals in street name expand to words", {
  r <- address_parse("5 1ST AVE, BROADBEACH QLD 4218")
  expect_equal(r$in_street_name, "FIRST")
  expect_equal(r$in_street_type, "AVENUE")

  r2 <- address_parse("5 3RD AVE, BROADBEACH QLD 4218")
  expect_equal(r2$in_street_name, "THIRD")

  r3 <- address_parse("5 12TH ST, SOUTH BRISBANE QLD 4101")
  expect_equal(r3$in_street_name, "TWELFTH")
  expect_equal(r3$in_street_type, "STREET")
})

test_that("normalize = FALSE leaves abbreviations unchanged", {
  r <- address_parse("5 MT GRAVATT RD, MT GRAVATT QLD 4122", normalize = FALSE)
  expect_equal(r$in_street_name, "MT GRAVATT")
  expect_equal(r$in_locality,    "MT GRAVATT")

  r2 <- address_parse("10 ST JAMES CT, ST LUCIA QLD 4067", normalize = FALSE)
  expect_equal(r2$in_street_name, "ST JAMES")
  expect_equal(r2$in_locality,    "ST LUCIA")
})
