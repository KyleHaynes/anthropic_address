library(data.table)

# Helper: build a minimal pairs data.table for .score_pairs()
make_pair <- function(in_street_type, street_type,
                      in_street_name = "MAIN",    street_name = "MAIN",
                      in_postcode = 4000L,        postcode = 4000L,
                      in_locality = "BRISBANE",   locality_name = "BRISBANE",
                      in_number_first = 10L,      number_first = 10L,
                      number_last = NA_integer_,
                      in_flat_number = NA_character_, flat_number = NA_character_,
                      in_flat_type = NA_character_) {
  data.table(
    in_postcode = in_postcode, postcode = postcode,
    in_locality = in_locality, locality_name = locality_name,
    in_street_name = in_street_name, street_name = street_name,
    in_street_type = in_street_type, street_type = street_type,
    in_number_first = in_number_first, number_first = number_first,
    number_last = number_last,
    in_flat_number = in_flat_number, flat_number = flat_number,
    in_flat_type = in_flat_type,
    in_street_suffix = NA_character_, street_suffix = NA_character_
  )
}

# ---- Street type scoring ----------------------------------------------------

test_that("matching street type scores full weight", {
  p <- make_pair("ROAD", "ROAD")
  out <- gnafr:::.score_pairs(p)
  expect_equal(out$score_street_type, 10L)
})

test_that("both-absent street type scores full weight", {
  p <- make_pair(NA_character_, NA_character_)
  out <- gnafr:::.score_pairs(p)
  expect_equal(out$score_street_type, 10L)
})

test_that("one-side absent scores 50 pct", {
  p <- make_pair("ROAD", NA_character_)
  out <- gnafr:::.score_pairs(p)
  expect_equal(out$score_street_type, 5L)

  p2 <- make_pair(NA_character_, "ROAD")
  out2 <- gnafr:::.score_pairs(p2)
  expect_equal(out2$score_street_type, 5L)
})

test_that("mismatched street type scores 40 pct (not 0)", {
  p <- make_pair("ROAD", "COURT")
  out <- gnafr:::.score_pairs(p)
  expect_equal(out$score_street_type, 4L)  # 40% of 10
})

# ---- Wrong type doesn't override a strong name match -----------------------

test_that("correct address ranks #1 even when input has wrong street type", {
  # Scenario: user types "25 St James Rd" — in_street_type = ROAD
  # Correct match:  SAINT JAMES COURT  (type mismatch but strong name match)
  # Incorrect match: LAHEY ROAD         (type matches but weak name match)

  correct <- make_pair(
    in_street_name = "ST JAMES", street_name = "SAINT JAMES",
    in_street_type = "ROAD",     street_type = "COURT",
    in_postcode = 4272L, postcode = 4272L,
    in_locality = "TAMBORINE MOUNTAIN", locality_name = "TAMBORINE MOUNTAIN",
    in_number_first = 25L, number_first = 25L
  )
  incorrect <- make_pair(
    in_street_name = "ST JAMES", street_name = "LAHEY",
    in_street_type = "ROAD",     street_type = "ROAD",
    in_postcode = 4272L, postcode = 4272L,
    in_locality = "TAMBORINE MOUNTAIN", locality_name = "TAMBORINE MOUNTAIN",
    in_number_first = 25L, number_first = 25L
  )

  pairs <- rbindlist(list(correct, incorrect))
  pairs[, input_id := c(1L, 1L)]
  out <- gnafr:::.score_pairs(pairs)

  expect_gt(out[street_name == "SAINT JAMES", total_score],
            out[street_name == "LAHEY",       total_score])
})

test_that("wrong Rd vs Ct: correct Court address beats unrelated Road", {
  correct <- make_pair(
    in_street_name = "MAPLE",  street_name = "MAPLE",
    in_street_type = "ROAD",   street_type = "COURT",
    in_postcode = 3000L, postcode = 3000L,
    in_locality = "MELBOURNE", locality_name = "MELBOURNE",
    in_number_first = 5L, number_first = 5L
  )
  wrong <- make_pair(
    in_street_name = "MAPLE",  street_name = "OAK",
    in_street_type = "ROAD",   street_type = "ROAD",
    in_postcode = 3000L, postcode = 3000L,
    in_locality = "MELBOURNE", locality_name = "MELBOURNE",
    in_number_first = 5L, number_first = 5L
  )
  pairs <- rbindlist(list(correct, wrong))
  out <- gnafr:::.score_pairs(pairs)

  expect_gt(out[street_name == "MAPLE", total_score],
            out[street_name == "OAK",   total_score])
})

test_that("wrong St vs Dr: correct Drive address beats unrelated Street", {
  correct <- make_pair(
    in_street_name = "KINGS",  street_name = "KINGS",
    in_street_type = "STREET", street_type = "DRIVE",
    in_postcode = 2000L, postcode = 2000L,
    in_locality = "SYDNEY",    locality_name = "SYDNEY",
    in_number_first = 12L, number_first = 12L
  )
  wrong <- make_pair(
    in_street_name = "KINGS",  street_name = "BURNS",
    in_street_type = "STREET", street_type = "STREET",
    in_postcode = 2000L, postcode = 2000L,
    in_locality = "SYDNEY",    locality_name = "SYDNEY",
    in_number_first = 12L, number_first = 12L
  )
  pairs <- rbindlist(list(correct, wrong))
  out <- gnafr:::.score_pairs(pairs)

  expect_gt(out[street_name == "KINGS" & street_type == "DRIVE", total_score],
            out[street_name == "BURNS", total_score])
})

# ---- Other scoring dimensions -----------------------------------------------

test_that("exact number match scores full weight", {
  p <- make_pair("ROAD", "ROAD", in_number_first = 42L, number_first = 42L)
  out <- gnafr:::.score_pairs(p)
  expect_equal(out$score_number, 12L)
})

test_that("number in range scores 70 pct", {
  p <- make_pair("ROAD", "ROAD",
                 in_number_first = 15L, number_first = 10L, number_last = 20L)
  out <- gnafr:::.score_pairs(p)
  expect_equal(out$score_number, 8L)  # round(12 * 0.7) = 8
})

test_that("flat number match scores full weight; mismatch scores 0", {
  p_match <- make_pair("ROAD", "ROAD",
                       in_flat_number = "3", flat_number = "3")
  out_match <- gnafr:::.score_pairs(p_match)
  expect_equal(out_match$score_flat, 8L)

  p_miss <- make_pair("ROAD", "ROAD",
                      in_flat_number = "3", flat_number = "7")
  out_miss <- gnafr:::.score_pairs(p_miss)
  expect_equal(out_miss$score_flat, 0L)
})

test_that("postcode mismatch scores 0 for postcode component", {
  p <- make_pair("ROAD", "ROAD", in_postcode = 4000L, postcode = 4001L)
  out <- gnafr:::.score_pairs(p)
  expect_equal(out$score_postcode, 0L)
})

test_that("total_score is sum of component scores", {
  p <- make_pair("COURT", "COURT")
  out <- gnafr:::.score_pairs(p)
  expect_equal(out$total_score,
               out$score_postcode + out$score_suburb + out$score_street_name +
               out$score_street_type + out$score_number + out$score_flat)
})
