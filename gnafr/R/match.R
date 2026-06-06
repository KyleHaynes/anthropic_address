#' Match a vector of address strings against the GNAF database
#'
#' Uses a three-path strategy:
#' \enumerate{
#'   \item \strong{Postcode path} — primary, blocks on the parsed postcode.
#'   \item \strong{State path} — for inputs with no parseable postcode.
#'   \item \strong{Locality fallback} — for inputs whose best postcode result is
#'         weak (score below \code{fallback_threshold}). Uses DuckDB's built-in
#'         \code{jaro_winkler_similarity} to find the correct postcode from the
#'         parsed suburb name, then re-scores. Handles wrong or missing postcodes.
#' }
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @param addresses Character vector of address strings.
#' @param max_results Maximum number of matches to return per input.
#' @param min_score Minimum total score (0–100) to include in results.
#' @param include_custom Include custom addresses in matching.
#' @param locality_fallback If \code{TRUE} (default), re-searches by locality
#'   name for inputs whose best score is below \code{fallback_threshold}.
#' @param fallback_threshold Total score below which locality fallback fires.
#'   Default 80: a correct match with a matching postcode will typically score
#'   85+, so 80 catches wrong-postcode and genuinely poor matches.
#' @param weights Named list of score weights. Defaults to postcode = 25,
#'   suburb = 20, street_name = 25, street_type = 10, number = 12, flat = 8.
#'   Weights must sum to 100.
#' @param cache If \code{TRUE} (default), checks \code{gnaf_match_cache} for
#'   previously matched addresses and stores new high-confidence results.
#' @param cache_threshold Minimum score for a new result to be cached.
#'   Default 95.
#' @param verbose If \code{TRUE}, prints colored progress, timings, and match
#'   summary information using the \pkg{cli} package.
#' @return A \code{data.table} ordered by \code{input_id} then descending
#'   \code{total_score}. Includes a standardised input string for every row and
#'   retains unmatched inputs with missing match columns.
#' @export
gnaf_match <- function(addresses, con, max_results = 3L, min_score = 60L,
                       include_custom = TRUE,
                       locality_fallback = TRUE,
                       fallback_threshold = 80L,
                       weights = .default_match_weights(),
                       cache = TRUE,
                       cache_threshold = 95L,
                       verbose = TRUE) {

  if (!is.character(addresses) || length(addresses) == 0L)
    stop("'addresses' must be a non-empty character vector")

  weights <- .validate_match_weights(weights)

  total_timer <- proc.time()[["elapsed"]]
  address_count <- length(addresses)
  address_word <- if (address_count == 1L) "address" else "addresses"
  .cli_match_step(
    verbose,
    sprintf(
      "Parsing %s %s.",
      cli::col_blue(format(address_count, big.mark = ",")),
      address_word
    )
  )
  parse_timer <- proc.time()[["elapsed"]]
  parsed <- address_parse(addresses)
  parse_elapsed <- proc.time()[["elapsed"]] - parse_timer

  .cli_match_step(verbose, "Standardising parsed input addresses.")
  standardise_timer <- proc.time()[["elapsed"]]
  parsed[, input_standardised := .standardise_input(parsed)]
  standardise_elapsed <- proc.time()[["elapsed"]] - standardise_timer
  .cli_match_detail(verbose, sprintf(
    "Input standardisation completed in %s.",
    cli::col_cyan(sprintf("%.2fs", standardise_elapsed))
  ))

  verbose_stats <- list(
    parse_elapsed = parse_elapsed,
    standardise_elapsed = standardise_elapsed,
    exact_inputs = 0L,
    exact_elapsed = 0,
    cache_inputs = 0L,
    cache_elapsed = 0,
    slow_inputs = 0L,
    slow_elapsed = 0,
    wrangle_elapsed = 0
  )

  results    <- list()
  diagnostics <- list()
  skip_ids   <- integer(0L)

  # ------------------------------------------------------------------
  # Fast path 1: exact address_label match
  # Fires when input_raw (uppercased) equals a GNAF address_label exactly.
  # Ideal for re-processing previously matched/standardised output.
  # ------------------------------------------------------------------
  exact_timer <- proc.time()[["elapsed"]]
  exact_path <- .exact_label_match(con, parsed, include_custom)
  verbose_stats$exact_elapsed <- proc.time()[["elapsed"]] - exact_timer
  if (!is.null(exact_path) && nrow(exact_path) > 0L) {
    results[["exact"]] <- exact_path
    skip_ids <- unique(exact_path$input_id)
    verbose_stats$exact_inputs <- length(skip_ids)
    .cli_match_detail(verbose, sprintf(
      "%s input(s) matched via exact label lookup in %s.",
      cli::col_green(format(length(skip_ids), big.mark = ",")),
      cli::col_cyan(sprintf("%.2fs", verbose_stats$exact_elapsed))
    ))
  } else {
    .cli_match_detail(verbose, sprintf(
      "%s input(s) matched via exact label lookup in %s.",
      cli::col_green("0"),
      cli::col_cyan(sprintf("%.2fs", verbose_stats$exact_elapsed))
    ))
  }

  # ------------------------------------------------------------------
  # Fast path 2: match cache
  # Previously matched addresses above cache_threshold skip the full pipeline.
  # ------------------------------------------------------------------
  cache_timer <- proc.time()[["elapsed"]]
  if (isTRUE(cache) && DBI::dbExistsTable(con, "gnaf_match_cache")) {
    remaining_stds <- unique(na.omit(
      parsed[!input_id %in% skip_ids, input_standardised]
    ))
    if (length(remaining_stds) > 0L) {
      cache_raw <- .cache_lookup(con, remaining_stds, include_custom)
      if (nrow(cache_raw) > 0L) {
        cache_hits <- cache_raw[
          parsed[!input_id %in% skip_ids, .(input_id, input_standardised)],
          on = "input_standardised", nomatch = 0L
        ]
        if (nrow(cache_hits) > 0L) {
          cache_hits[, match_rank := 1L]
          results[["cache"]] <- cache_hits
          verbose_stats$cache_inputs <- uniqueN(cache_hits$input_id)
          skip_ids <- unique(c(skip_ids, cache_hits$input_id))
        }
      }
    }
  }
  verbose_stats$cache_elapsed <- proc.time()[["elapsed"]] - cache_timer
  .cli_match_detail(verbose, sprintf(
    "%s input(s) served from match cache in %s.",
    cli::col_green(format(verbose_stats$cache_inputs, big.mark = ",")),
    cli::col_cyan(sprintf("%.2fs", verbose_stats$cache_elapsed))
  ))

  has_pc <- parsed[!input_id %in% skip_ids & !is.na(in_postcode)]
  no_pc  <- parsed[!input_id %in% skip_ids & is.na(in_postcode)]
  slow_timer <- proc.time()[["elapsed"]]

  # ------------------------------------------------------------------
  # Path 1: postcode path — scored entirely in DuckDB
  # ------------------------------------------------------------------
  if (nrow(has_pc) > 0L) {
    n_pc <- uniqueN(has_pc$in_postcode)
    .cli_match_step(verbose, sprintf(
      "Scoring %s input(s) across %s unique postcode(s) in DuckDB.",
      cli::col_blue(format(nrow(has_pc), big.mark = ",")),
      cli::col_cyan(format(n_pc, big.mark = ","))
    ))
    pc_path <- .match_postcode_duckdb(con, has_pc, max_results, min_score,
                                       weights, include_custom, verbose)
    results[["postcode"]]     <- pc_path$matches
    diagnostics[["postcode"]] <- pc_path$diagnostics
  }

  # ------------------------------------------------------------------
  # Path 2: no-postcode inputs — state-level fallback in DuckDB
  # ------------------------------------------------------------------
  if (nrow(no_pc) > 0L) {
    no_pc_state <- no_pc[!is.na(in_state)]
    if (nrow(no_pc_state) > 0L) {
      input_word <- if (nrow(no_pc_state) == 1L) "row" else "rows"
      .cli_match_step(verbose, sprintf(
        "Attempting state fallback for %s input %s without a postcode.",
        cli::col_yellow(format(nrow(no_pc_state), big.mark = ",")),
        input_word
      ))
      st_path <- .match_state_duckdb(con, no_pc_state, max_results, min_score,
                                      weights, include_custom, verbose)
      results[["no_postcode"]]     <- st_path$matches
      diagnostics[["no_postcode"]] <- st_path$diagnostics
    } else {
      .cli_match_alert(verbose, "warning",
                       "No postcode or state found for some inputs; skipping.")
    }
  }

  # ------------------------------------------------------------------
  # Path 3: locality fallback for weak / wrong-postcode results
  # ------------------------------------------------------------------
  if (locality_fallback) {
    pc_res <- results[["postcode"]]

    best_by_input <- if (!is.null(pc_res) && nrow(pc_res) > 0L) {
      pc_res[, .(best_score = max(total_score)), by = input_id]
    } else {
      data.table(input_id = integer(0), best_score = integer(0))
    }

    weak_ids      <- best_by_input[best_score < fallback_threshold, input_id]
    matched_ids   <- best_by_input$input_id
    unmatched_ids <- has_pc[!input_id %in% matched_ids, input_id]
    no_pc_loc_ids <- no_pc[!is.na(in_locality), input_id]

    fallback_ids   <- unique(c(weak_ids, unmatched_ids, no_pc_loc_ids))
    fallback_parse <- parsed[input_id %in% fallback_ids & !is.na(in_locality)]

    if (nrow(fallback_parse) > 0L) {
      fallback_word <- if (nrow(fallback_parse) == 1L) "row" else "rows"
      .cli_match_step(verbose, sprintf(
        "Running locality fallback for %s input %s.",
        cli::col_magenta(format(nrow(fallback_parse), big.mark = ",")),
        fallback_word
      ))
      loc_path <- .match_locality_duckdb(con, fallback_parse, max_results,
                                          min_score, weights, include_custom, verbose)
      results[["locality"]]     <- loc_path$matches
      diagnostics[["locality"]] <- loc_path$diagnostics
    }
  }

  verbose_stats$slow_elapsed <- proc.time()[["elapsed"]] - slow_timer

  # ------------------------------------------------------------------
  # Combine paths, deduplicate, re-rank
  # ------------------------------------------------------------------
  .cli_match_step(verbose, "Wrangling final match output.")
  wrangle_timer <- proc.time()[["elapsed"]]
  out <- rbindlist(results, fill = TRUE, use.names = TRUE)
  if (nrow(out) > 0L) {
    # Deduplicate: same GNAF record may appear from multiple paths; keep higher score
    setorder(out, input_id, -total_score)
    out <- unique(out, by = c("input_id", "address_detail_pid"))

    # Re-apply max_results and assign final rank
    setorder(out, input_id, -total_score)
    out <- out[, .SD[seq_len(min(.N, max_results))], by = input_id]
    out[, match_rank := seq_len(.N), by = input_id]
  } else {
    out <- .empty_result()
  }

  common_cols <- setdiff(intersect(names(out), names(parsed)), "input_id")
  if (length(common_cols) > 0L) out[, (common_cols) := NULL]

  out <- merge(parsed, out, by = "input_id", all.x = TRUE, sort = FALSE)
  out[, matched := !is.na(address_detail_pid)]
  out <- .append_match_status(out, parsed, diagnostics)

  cols_first <- c("input_id", "input_raw", "input_standardised", "match_rank",
                  "matched", "match_status", "total_score", "score_postcode", "score_suburb",
                  "score_street_name", "score_street_type", "score_number",
                  "score_flat")
  setcolorder(out, c(cols_first, setdiff(names(out), cols_first)))
  setorder(out, input_id, -matched, match_rank)
  # Store newly matched high-confidence results in the cache.
  # ON CONFLICT DO NOTHING means cache/exact-path hits are silently skipped.
  if (isTRUE(cache) && DBI::dbExistsTable(con, "gnaf_match_cache") && nrow(out) > 0L)
    .cache_store(con, out[matched == TRUE], cache_threshold)

  verbose_stats$wrangle_elapsed <- proc.time()[["elapsed"]] - wrangle_timer
  slow_path_matches <- out[
    matched == TRUE & !input_id %in% unique(c(
      results[["exact"]]$input_id %||% integer(0L),
      results[["cache"]]$input_id %||% integer(0L)
    )),
    uniqueN(input_id)
  ]
  verbose_stats$slow_inputs <- slow_path_matches
  .cli_match_detail(verbose, sprintf(
    "Final output wrangling completed in %s.",
    cli::col_cyan(sprintf("%.2fs", verbose_stats$wrangle_elapsed))
  ))

  .cli_match_summary(verbose, parsed, out, total_timer, verbose_stats)
  out[]
}

# ---------------------------------------------------------------------------
# DuckDB-based path implementations
# All scoring, joining, filtering and top-N ranking happen inside DuckDB.
# Only the final (small) result set is transferred to R.
# ---------------------------------------------------------------------------

# Core query runner shared by all paths.
# inputs_tbl  : name of a duckdb_register'd virtual table of parsed inputs
# gnaf_tbl    : "gnaf_addresses" or "custom_addresses"
# join_clause : SQL ON expression (uses aliases i = inputs, g = gnaf)
# pre_filter  : additional WHERE predicates (coarse, no JW)
#
# Design notes:
#   * No window-function aggregates (COUNT/MAX OVER) before the score filter —
#     those force full materialisation of the join which kills RAM at scale.
#   * Street-name JW pre-filter (>= 0.3) in the WHERE clause cuts the
#     intermediate table size dramatically before scoring the remaining rows.
#   * candidate_count is set to NA; match_status "below_min_score" vs
#     "no_candidate" is not distinguishable, which is an acceptable trade-off.
.run_duckdb_score_query <- function(con, inputs_tbl, gnaf_tbl, join_clause,
                                    pre_filter, weights, max_results, min_score,
                                    verbose = FALSE, label = "") {
  exprs      <- .score_sql_exprs(weights)
  sel_scores <- paste(
    mapply(function(nm, ex) sprintf("    %s AS %s", ex, nm), names(exprs), exprs),
    collapse = ",\n"
  )
  score_total <- paste(names(exprs), collapse = " + ")

  gnaf_cols <- "g.address_detail_pid, g.address_label, g.building_name,
    g.flat_type, g.flat_number, g.number_first, g.number_last,
    g.street_name, g.street_type, g.street_suffix, g.locality_name,
    g.state, g.postcode, g.longitude, g.latitude, g.source, g.alias_type"

  sql <- sprintf("
WITH joined AS (
  SELECT
    %s,
    i.input_id,
%s
  FROM %s g
  JOIN %s i ON %s
  WHERE %s
    AND (i.in_street_name IS NULL
         OR jaro_winkler_similarity(g.street_name, i.in_street_name) >= 0.3)
),
scored AS (
  SELECT *, %s AS total_score
  FROM joined
),
ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (PARTITION BY input_id ORDER BY total_score DESC) AS match_rank
  FROM scored
  WHERE total_score >= %d
)
SELECT * FROM ranked WHERE match_rank <= %d
",
    gnaf_cols, sel_scores,
    gnaf_tbl, inputs_tbl, join_clause, pre_filter,
    score_total,
    min_score, max_results
  )

  t0 <- proc.time()[["elapsed"]]
  dt <- tryCatch(
    setDT(DBI::dbGetQuery(con, sql)),
    error = function(e) {
      .cli_match_alert(verbose, "warning",
        sprintf("%s query failed: %s", label, conditionMessage(e)))
      data.table()
    }
  )
  elapsed <- proc.time()[["elapsed"]] - t0

  if (verbose && nzchar(label)) {
    .cli_match_detail(verbose, sprintf(
      "%s: %s row(s) returned in %s.",
      label,
      cli::col_green(format(nrow(dt), big.mark = ",")),
      cli::col_cyan(sprintf("%.2fs", elapsed))
    ))
  }

  if (nrow(dt) == 0L) return(.empty_path_result())

  diag_dt <- dt[, .(
    candidate_count = NA_integer_,
    retained_count  = .N,
    best_score      = max(total_score)
  ), by = input_id]

  list(matches = dt, diagnostics = diag_dt)
}

# Combine results from gnaf_addresses + custom_addresses, re-rank.
.combine_path_results <- function(r1, r2, max_results) {
  matches <- rbindlist(
    Filter(Negate(is.null), list(r1$matches, r2$matches)),
    fill = TRUE, use.names = TRUE
  )
  if (nrow(matches) > 0L) {
    setorder(matches, input_id, -total_score)
    matches <- matches[, .SD[seq_len(min(.N, max_results))], by = input_id]
    matches[, match_rank := seq_len(.N), by = input_id]
  } else {
    matches <- NULL
  }

  diags <- rbindlist(
    Filter(Negate(is.null), list(r1$diagnostics, r2$diagnostics)),
    fill = TRUE, use.names = TRUE
  )
  if (nrow(diags) > 0L) {
    diags <- diags[, .(
      candidate_count = sum(candidate_count, na.rm = TRUE),
      retained_count  = sum(retained_count,  na.rm = TRUE),
      best_score      = suppressWarnings(max(best_score, na.rm = TRUE))
    ), by = input_id]
    diags[!is.finite(best_score), best_score := NA_real_]
  } else {
    diags <- NULL
  }

  list(matches = matches, diagnostics = diags)
}

.match_postcode_duckdb <- function(con, inputs_dt, max_results, min_score,
                                   weights, include_custom, verbose = FALSE) {
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  duckdb::duckdb_register(con, "__gnafr_pc_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_pc_inputs__"), silent = TRUE))

  join_on  <- "g.postcode = i.in_postcode"
  # Check range bounds explicitly; "OR g.number_last IS NOT NULL" without bounds
  # pulls every range record in the postcode regardless of the input number.
  pre_filt <- paste(
    "i.in_postcode IS NOT NULL AND (",
    "  i.in_number_first IS NULL",
    "  OR g.number_first = i.in_number_first",
    "  OR (g.number_last IS NOT NULL",
    "      AND g.number_first <= i.in_number_first",
    "      AND i.in_number_first <= g.number_last)",
    ")"
  )

  res <- .run_duckdb_score_query(
    con, "__gnafr_pc_inputs__", "gnaf_addresses",
    join_on, pre_filt, weights, max_results, min_score, verbose,
    label = "gnaf_addresses (postcode)"
  )

  if (include_custom && DBI::dbExistsTable(con, "custom_addresses")) {
    r2 <- .run_duckdb_score_query(
      con, "__gnafr_pc_inputs__", "custom_addresses",
      join_on, pre_filt, weights, max_results, min_score, verbose,
      label = "custom_addresses (postcode)"
    )
    res <- .combine_path_results(res, r2, max_results)
  }

  res
}

.match_state_duckdb <- function(con, inputs_dt, max_results, min_score,
                                weights, include_custom, verbose = FALSE) {
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  duckdb::duckdb_register(con, "__gnafr_st_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_st_inputs__"), silent = TRUE))

  join_on  <- "g.state = i.in_state"
  pre_filt <- "i.in_state IS NOT NULL"

  res <- .run_duckdb_score_query(
    con, "__gnafr_st_inputs__", "gnaf_addresses",
    join_on, pre_filt, weights, max_results, min_score, verbose,
    label = "gnaf_addresses (state)"
  )

  if (include_custom && DBI::dbExistsTable(con, "custom_addresses")) {
    r2 <- .run_duckdb_score_query(
      con, "__gnafr_st_inputs__", "custom_addresses",
      join_on, pre_filt, weights, max_results, min_score, verbose,
      label = "custom_addresses (state)"
    )
    res <- .combine_path_results(res, r2, max_results)
  }

  res
}

# Locality fallback: fuzzy-match suburb → discover correct postcodes → score.
# The entire pipeline (locality lookup + join + scoring + ranking) runs in one
# DuckDB query, so no cartesian product ever lands in R memory.
.match_locality_duckdb <- function(con, inputs_dt, max_results, min_score,
                                   weights, include_custom, verbose = FALSE) {
  if (nrow(inputs_dt) == 0L || !any(!is.na(inputs_dt$in_locality)))
    return(.empty_path_result())

  duckdb::duckdb_register(con, "__gnafr_loc_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_loc_inputs__"), silent = TRUE))

  exprs      <- .score_sql_exprs(weights)
  sel_scores <- paste(
    mapply(function(nm, ex) sprintf("    %s AS %s", ex, nm), names(exprs), exprs),
    collapse = ",\n"
  )
  score_total <- paste(names(exprs), collapse = " + ")

  gnaf_cols <- "g.address_detail_pid, g.address_label, g.building_name,
    g.flat_type, g.flat_number, g.number_first, g.number_last,
    g.street_name, g.street_type, g.street_suffix, g.locality_name,
    g.state, g.postcode, g.longitude, g.latitude, g.source, g.alias_type"

  make_sql <- function(gnaf_tbl) sprintf("
WITH unique_locs AS (
  -- Deduplicate localities before the JW scan so the cross-product is
  -- (unique_localities × locality_index) not (all_inputs × locality_index).
  SELECT DISTINCT in_locality, in_state
  FROM __gnafr_loc_inputs__
  WHERE in_locality IS NOT NULL
),
loc_map AS (
  SELECT DISTINCT ul.in_locality, g.postcode
  FROM gnaf_locality_index g
  JOIN unique_locs ul
    ON  jaro_winkler_similarity(g.locality_name, ul.in_locality) >= 0.85
    AND (ul.in_state IS NULL OR g.state = ul.in_state)
),
expanded AS (
  SELECT i.*, loc_map.postcode AS alt_postcode
  FROM __gnafr_loc_inputs__ i
  JOIN loc_map ON loc_map.in_locality = i.in_locality
),
joined AS (
  SELECT
    %s,
    i.input_id,
%s
  FROM %s g
  JOIN expanded i ON g.postcode = i.alt_postcode
  WHERE (i.in_number_first IS NULL
     OR g.number_first = i.in_number_first
     OR (g.number_last IS NOT NULL
         AND g.number_first <= i.in_number_first
         AND i.in_number_first <= g.number_last))
    AND (i.in_street_name IS NULL
         OR jaro_winkler_similarity(g.street_name, i.in_street_name) >= 0.3)
),
scored AS (
  SELECT *, %s AS total_score
  FROM joined
),
ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (PARTITION BY input_id ORDER BY total_score DESC) AS match_rank
  FROM scored
  WHERE total_score >= %d
)
SELECT * FROM ranked WHERE match_rank <= %d
",
    gnaf_cols, sel_scores, gnaf_tbl,
    score_total,
    min_score, max_results
  )

  t0 <- proc.time()[["elapsed"]]
  dt <- tryCatch(
    setDT(DBI::dbGetQuery(con, make_sql("gnaf_addresses"))),
    error = function(e) {
      .cli_match_alert(verbose, "warning",
        sprintf("Locality fallback query failed: %s", conditionMessage(e)))
      data.table()
    }
  )
  elapsed <- proc.time()[["elapsed"]] - t0
  if (verbose) .cli_match_detail(verbose, sprintf(
    "gnaf_addresses (locality): %s row(s) in %s.",
    cli::col_green(format(nrow(dt), big.mark = ",")),
    cli::col_cyan(sprintf("%.2fs", elapsed))
  ))

  if (nrow(dt) == 0L) {
    res <- .empty_path_result()
  } else {
    diag_dt <- dt[, .(candidate_count = NA_integer_, retained_count = .N,
                       best_score = max(total_score)), by = input_id]
    res <- list(matches = dt, diagnostics = diag_dt)
  }

  if (include_custom && DBI::dbExistsTable(con, "custom_addresses")) {
    t0 <- proc.time()[["elapsed"]]
    dt2 <- tryCatch(setDT(DBI::dbGetQuery(con, make_sql("custom_addresses"))),
                    error = function(e) data.table())
    elapsed2 <- proc.time()[["elapsed"]] - t0
    if (verbose) .cli_match_detail(verbose, sprintf(
      "custom_addresses (locality): %s row(s) in %s.",
      cli::col_green(format(nrow(dt2), big.mark = ",")),
      cli::col_cyan(sprintf("%.2fs", elapsed2))
    ))
    if (nrow(dt2) > 0L) {
      diag2 <- dt2[, .(candidate_count = NA_integer_, retained_count = .N,
                        best_score = max(total_score)), by = input_id]
      res <- .combine_path_results(res, list(matches = dt2, diagnostics = diag2), max_results)
    }
  }

  res
}

.empty_result <- function() {
  data.table(
    input_id = integer(), input_raw = character(), input_standardised = character(),
    match_rank = integer(), matched = logical(), match_status = character(),
    total_score = integer(),
    score_postcode = integer(), score_suburb = integer(),
    score_street_name = integer(), score_street_type = integer(),
    score_number = integer(), score_flat = integer(),
    address_detail_pid = character(), address_label = character(),
    building_name = character(),
    flat_type = character(), flat_number = character(),
    number_first = integer(), number_last = integer(),
    street_name = character(), street_type = character(),
    street_suffix = character(),
    locality_name = character(), state = character(),
    postcode = integer(), longitude = numeric(), latitude = numeric(),
    source = character(), alias_type = character(),
    in_postcode = integer(), in_state = character(), in_locality = character(),
    in_street_name = character(), in_street_type = character(),
    in_street_suffix = character(), in_number_first = integer(),
    in_number_last = integer(), in_flat_type = character(),
    in_flat_number = character(), in_building_name = character()
  )
}

.empty_path_result <- function() {
  list(matches = NULL, diagnostics = NULL)
}

.standardise_input <- function(parsed) {
  postcode_chr <- ifelse(is.na(parsed$in_postcode), NA_character_, as.character(parsed$in_postcode))
  number_chr <- ifelse(
    is.na(parsed$in_number_first),
    NA_character_,
    ifelse(
      is.na(parsed$in_number_last),
      as.character(parsed$in_number_first),
      paste0(parsed$in_number_first, "-", parsed$in_number_last)
    )
  )
  flat_chr <- ifelse(
    !is.na(parsed$in_flat_type) & !is.na(parsed$in_flat_number),
    paste(parsed$in_flat_type, parsed$in_flat_number),
    ifelse(!is.na(parsed$in_flat_number), parsed$in_flat_number, NA_character_)
  )

  line_one <- .collapse_address_parts(
    parsed$in_building_name,
    flat_chr,
    number_chr,
    parsed$in_street_name,
    parsed$in_street_type,
    parsed$in_street_suffix
  )
  line_two <- .collapse_address_parts(parsed$in_locality, parsed$in_state, postcode_chr)

  out <- ifelse(
    !is.na(line_one) & !is.na(line_two),
    paste(line_one, line_two, sep = ", "),
    ifelse(!is.na(line_one), line_one, line_two)
  )
  out[nzchar(out) == FALSE] <- NA_character_
  out
}

.collapse_address_parts <- function(...) {
  parts <- list(...)
  parts <- lapply(parts, function(x) ifelse(is.na(x), "", as.character(x)))
  out <- do.call(paste, c(parts, sep = " "))
  out <- trimws(gsub("\\s+", " ", out))
  out[out == ""] <- NA_character_
  out
}

.append_match_status <- function(out, parsed, diagnostics) {
  diagnostic_dt <- rbindlist(diagnostics, fill = TRUE, use.names = TRUE)
  if (nrow(diagnostic_dt) > 0L) {
    diagnostic_dt <- diagnostic_dt[, .(
      candidate_count = sum(candidate_count, na.rm = TRUE),
      retained_count = sum(retained_count, na.rm = TRUE),
      best_score = suppressWarnings(max(best_score, na.rm = TRUE))
    ), by = input_id]
    diagnostic_dt[!is.finite(best_score), best_score := NA_real_]
    out <- diagnostic_dt[out, on = "input_id"]
  } else {
    out[, `:=`(candidate_count = NA_integer_, retained_count = NA_integer_, best_score = NA_real_)]
  }

  out[, match_status := fifelse(
    matched,
    "matched",
    fifelse(
      !is.na(candidate_count) & candidate_count > 0L & (is.na(retained_count) | retained_count == 0L),
      "below_min_score",
      fifelse(
        is.na(in_street_name) | (is.na(in_postcode) & is.na(in_state) & is.na(in_locality)),
        "insufficient_parse",
        "no_candidate"
      )
    )
  )]

  out[, c("candidate_count", "retained_count", "best_score") := NULL]
  out
}

# Exact address_label pass: returns scored candidate pairs for any input whose
# raw text (uppercased) matches a GNAF address_label exactly.
.exact_label_match <- function(con, parsed, include_custom) {
  raw_upper <- unique(toupper(trimws(parsed$input_raw)))
  raw_upper <- raw_upper[nzchar(raw_upper) & !is.na(raw_upper)]
  if (length(raw_upper) == 0L) return(NULL)

  # Register as a virtual table so DuckDB can hash-join instead of scanning
  # with a 50k-item IN() literal (which kills the query planner at scale).
  lkp <- data.table(lbl_key = raw_upper)
  duckdb::duckdb_register(con, "__gnafr_exact_lkp__", lkp, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_exact_lkp__"), silent = TRUE))

  sel <- "g.address_detail_pid, g.address_label, g.building_name,
          g.flat_type, g.flat_number, g.number_first, g.number_last,
          g.street_name, g.street_type, g.street_suffix, g.locality_name,
          g.state, g.postcode, g.longitude, g.latitude, g.source, g.alias_type"

  sql <- sprintf(
    "SELECT %s FROM gnaf_addresses g
     JOIN __gnafr_exact_lkp__ l ON UPPER(TRIM(g.address_label)) = l.lbl_key",
    sel
  )
  cands <- setDT(DBI::dbGetQuery(con, sql))

  if (include_custom && DBI::dbExistsTable(con, "custom_addresses")) {
    sql2 <- sprintf(
      "SELECT %s FROM custom_addresses g
       JOIN __gnafr_exact_lkp__ l ON UPPER(TRIM(g.address_label)) = l.lbl_key",
      sel
    )
    cands <- rbindlist(list(cands, setDT(DBI::dbGetQuery(con, sql2))), fill = TRUE)
  }

  if (nrow(cands) == 0L) return(NULL)

  cands[, lbl_key := toupper(trimws(address_label))]
  pi <- copy(parsed)
  pi[, lbl_key := toupper(trimws(input_raw))]

  joined <- cands[pi, on = "lbl_key", nomatch = 0L, allow.cartesian = TRUE]
  joined[, lbl_key := NULL]
  if (nrow(joined) == 0L) return(NULL)

  joined <- .score_pairs(joined, weights = .default_match_weights())
  joined[, match_rank := 1L]
  joined
}

.cli_match_step <- function(verbose, text) {
  if (isTRUE(verbose)) cli::cli_alert_info(text)
}

.cli_match_detail <- function(verbose, text) {
  if (isTRUE(verbose)) cli::cli_li(text)
}

.cli_match_alert <- function(verbose, level, text) {
  if (!isTRUE(verbose)) return(invisible(NULL))

  switch(
    level,
    warning = cli::cli_alert_warning(text),
    danger = cli::cli_alert_danger(text),
    success = cli::cli_alert_success(text),
    cli::cli_alert_info(text)
  )
}

.cli_match_summary <- function(verbose, parsed, out, total_timer,
                               verbose_stats = NULL) {
  if (!isTRUE(verbose)) return(invisible(NULL))

  total_elapsed <- proc.time()[["elapsed"]] - total_timer
  matched_rows <- out[matched %in% TRUE]
  matched_inputs <- if (nrow(matched_rows) > 0L) uniqueN(matched_rows$input_id) else 0L
  unmatched_inputs <- nrow(parsed) - matched_inputs
  matched_pct <- if (nrow(parsed) > 0L) 100 * matched_inputs / nrow(parsed) else 0
  input_word <- if (nrow(parsed) == 1L) "row" else "rows"
  candidate_word <- if (nrow(matched_rows) == 1L) "row" else "rows"
  avg_best <- if (nrow(matched_rows) > 0L) {
    round(mean(matched_rows[match_rank == 1L, total_score]), 1)
  } else {
    NA_real_
  }

  cli::cli_h1("gnaf_match summary")
  cli::cli_alert_success(
    sprintf(
      "Matched %s of %s input %s (%s).",
      cli::col_green(format(matched_inputs, big.mark = ",")),
      cli::col_blue(format(nrow(parsed), big.mark = ",")),
      input_word,
      cli::col_green(sprintf("%.1f%%", matched_pct))
    )
  )
  cli::cli_li(
    sprintf(
      "Returned %s candidate %s after ranking and filtering.",
      cli::col_cyan(format(nrow(matched_rows), big.mark = ",")),
      candidate_word
    )
  )
  cli::cli_li(
    sprintf(
      "Unmatched inputs above min_score: %s.",
      cli::col_yellow(format(unmatched_inputs, big.mark = ","))
    )
  )
  if (!is.null(verbose_stats)) {
    cli::cli_li(
      sprintf(
        "Exact label matches: %s in %s.",
        cli::col_green(format(verbose_stats$exact_inputs, big.mark = ",")),
        cli::col_cyan(sprintf("%.2fs", verbose_stats$exact_elapsed))
      )
    )
    cli::cli_li(
      sprintf(
        "Cache matches: %s in %s.",
        cli::col_green(format(verbose_stats$cache_inputs, big.mark = ",")),
        cli::col_cyan(sprintf("%.2fs", verbose_stats$cache_elapsed))
      )
    )
    cli::cli_li(
      sprintf(
        "Slow-path matches: %s in %s.",
        cli::col_green(format(verbose_stats$slow_inputs, big.mark = ",")),
        cli::col_cyan(sprintf("%.2fs", verbose_stats$slow_elapsed))
      )
    )
  }
  if (!is.na(avg_best)) {
    cli::cli_li(sprintf("Average top-match score: %s.", cli::col_magenta(sprintf("%.1f", avg_best))))
  }
  cli::cli_text(
    sprintf(
      "Timings: parse %s, standardise %s, slow path %s, wrangle %s, total %s.",
      cli::col_cyan(sprintf("%.2fs", verbose_stats$parse_elapsed %||% 0)),
      cli::col_cyan(sprintf("%.2fs", verbose_stats$standardise_elapsed %||% 0)),
      cli::col_cyan(sprintf("%.2fs", verbose_stats$slow_elapsed %||% 0)),
      cli::col_cyan(sprintf("%.2fs", verbose_stats$wrangle_elapsed %||% 0)),
      cli::col_cyan(sprintf("%.2fs", total_elapsed))
    )
  )
}
