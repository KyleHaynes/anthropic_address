library(data.table)

# Helper: build a minimal pairs data.table for .score_pairs()
make_pair <- function(in_street_type, street_type,
                      in_street_name = "MAIN",    street_name = "MAIN",
                      in_postcode = 4000L,        postcode = 4000L,
                      in_locality = "BRISBANE",   locality_name = "BRISBANE",
                      in_number_first = 10L,      number_first = 10L,
                      number_last = NA_integer_,
                      in_flat_number = NA_character_, flat_number = NA_character_,
                      in_flat_type = NA_character_, flat_type = NA_character_,
                      in_level_number = NA_character_, level_number = NA_character_,
                      in_level_type = NA_character_, level_type = NA_character_,
                      in_lot_number = NA_character_, lot_number = NA_character_) {
  data.table(
    in_postcode = in_postcode, postcode = postcode,
    in_locality = in_locality, locality_name = locality_name,
    in_street_name = in_street_name, street_name = street_name,
    in_street_type = in_street_type, street_type = street_type,
    in_number_first = in_number_first, number_first = number_first,
    number_last = number_last,
    in_flat_number = in_flat_number, flat_number = flat_number,
    in_flat_type = in_flat_type, flat_type = flat_type,
    in_level_number = in_level_number, level_number = level_number,
    in_level_type = in_level_type, level_type = level_type,
    in_lot_number = in_lot_number, lot_number = lot_number,
    in_number_suffix = NA_character_, address_label = NA_character_,
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
  expect_equal(out$score_number, 10L)
})

test_that("number in range scores 70 pct", {
  p <- make_pair("ROAD", "ROAD",
                 in_number_first = 15L, number_first = 10L, number_last = 20L)
  out <- gnafr:::.score_pairs(p)
  expect_equal(out$score_number, 7L)  # round(10 * 0.7) = 7
})

test_that("flat number match scores full weight; mismatch scores 0", {
  p_match <- make_pair("ROAD", "ROAD",
                       in_flat_number = "3", flat_number = "3")
  out_match <- gnafr:::.score_pairs(p_match)
  expect_equal(out_match$score_flat, 5L)

  p_miss <- make_pair("ROAD", "ROAD",
                      in_flat_number = "3", flat_number = "7")
  out_miss <- gnafr:::.score_pairs(p_miss)
  expect_equal(out_miss$score_flat, 0L)
})

test_that("flat and level identifiers form one backward-compatible score", {
  full <- make_pair(
    "ROAD", "ROAD", in_flat_number = "2", flat_number = "2",
    in_flat_type = "UNIT", flat_type = "UNIT",
    in_level_number = "3", level_number = "3",
    in_level_type = "LEVEL", level_type = "LEVEL"
  )
  conflict <- copy(full)
  conflict[, level_type := "FLOOR"]
  missing <- copy(full)
  missing[, level_number := NA_character_]

  expect_equal(gnafr:::.score_pairs(full)$score_flat, 5L)
  expect_equal(gnafr:::.score_pairs(conflict)$score_flat, 2L)
  expect_equal(gnafr:::.score_pairs(missing)$score_flat, 0L)
})

test_that("explicit lot number replaces street number scoring", {
  p <- make_pair(
    "ROAD", "ROAD", in_number_first = 99L, number_first = 10L,
    in_lot_number = "7", lot_number = "7"
  )
  expect_equal(gnafr:::.score_pairs(p)$score_number, 10L)
  p[, lot_number := "8"]
  expect_equal(gnafr:::.score_pairs(p)$score_number, 0L)
})

test_that("R and DuckDB score expressions remain component-identical", {
  pairs <- rbindlist(list(
    make_pair("ROAD", "ROAD", in_flat_number = "2", flat_number = "2",
              in_level_number = "3", level_number = "3"),
    make_pair("ROAD", "COURT", in_lot_number = "7", lot_number = "7",
              in_number_first = 99L, number_first = 10L),
    make_pair("ROAD", "ROAD", in_number_first = 15L,
              number_first = 10L, number_last = 20L)
  ))
  r_scored <- gnafr:::.score_pairs(copy(pairs))
  pairs[, input_id := .I]
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  duckdb::duckdb_register(con, "score_pairs", pairs)
  on.exit(duckdb::duckdb_unregister(con, "score_pairs"), add = TRUE)
  expressions <- gnafr:::.score_sql_exprs(gnafr:::.default_match_weights(),
                                           i = "p", g = "p")
  select_sql <- paste(
    sprintf("%s AS %s", expressions, names(expressions)), collapse = ", "
  )
  sql_scored <- as.data.table(DBI::dbGetQuery(con, sprintf(
    "SELECT input_id, %s FROM score_pairs p ORDER BY input_id", select_sql
  )))
  score_cols <- names(expressions)
  expect_equal(sql_scored[, ..score_cols], r_scored[, ..score_cols])
})

test_that("postcode +-1/+-2/+-3 score partial credit; >+-3 scores 0", {
  p1 <- make_pair("ROAD", "ROAD", in_postcode = 4000L, postcode = 4001L)
  expect_equal(gnafr:::.score_pairs(p1)$score_postcode, 14L)  # round(20 * 0.7)

  p2 <- make_pair("ROAD", "ROAD", in_postcode = 4000L, postcode = 4002L)
  expect_equal(gnafr:::.score_pairs(p2)$score_postcode, 8L)   # round(20 * 0.4)

  p3 <- make_pair("ROAD", "ROAD", in_postcode = 4000L, postcode = 4003L)
  expect_equal(gnafr:::.score_pairs(p3)$score_postcode, 4L)   # round(20 * 0.2)

  p4 <- make_pair("ROAD", "ROAD", in_postcode = 4000L, postcode = 4005L)
  expect_equal(gnafr:::.score_pairs(p4)$score_postcode, 0L)
})

test_that("total_score is sum of component scores", {
  p <- make_pair("COURT", "COURT")
  out <- gnafr:::.score_pairs(p)
  expect_equal(out$total_score,
               out$score_postcode + out$score_suburb + out$score_street_name +
               out$score_street_type + out$score_number + out$score_flat)
})

test_that("custom weights use the same rounding in R and DuckDB", {
  pairs <- rbindlist(list(
    make_pair("ROAD", "ROAD", in_postcode = 4000L, postcode = 4001L),
    make_pair("ROAD", "ROAD")
  ))
  con <- gnaf_connect(":memory:")
  on.exit(gnaf_disconnect(con), add = TRUE)
  duckdb::duckdb_register(con, "rounding_pairs", pairs)
  on.exit(duckdb::duckdb_unregister(con, "rounding_pairs"), add = TRUE)
  for (weights in list(
    list(postcode = 15, suburb = 20, street_name = 40, street_type = 10, number = 10, flat = 5),
    list(postcode = 21.5, suburb = 14.5, street_name = 39, street_type = 10, number = 10, flat = 5)
  )) {
    expressions <- gnafr:::.score_sql_exprs(weights, i = "p", g = "p")
    sql <- paste(sprintf("%s AS %s", expressions, names(expressions)), collapse = ", ")
    actual <- as.data.table(DBI::dbGetQuery(con, paste("SELECT", sql, "FROM rounding_pairs p")))
    expected <- gnafr:::.score_pairs(copy(pairs), weights)
    expect_equal(actual, expected[, names(expressions), with = FALSE])
  }
})
