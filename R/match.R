#' Match a vector of address strings against the GNAF database
#'
#' Uses a four-path strategy:
#' \enumerate{
#'   \item \strong{Postcode path} - primary, blocks on the parsed postcode.
#'   \item \strong{State path} - for inputs with no parseable postcode.
#'   \item \strong{Locality fallback} - for inputs whose best postcode result is
#'         weak (score below \code{fallback_threshold}). Uses DuckDB's built-in
#'         \code{jaro_winkler_similarity} to find the correct postcode from the
#'         parsed suburb name, then re-scores. Handles wrong or missing postcodes.
#'   \item \strong{Street-only fallback} - optional; fires for inputs that are
#'         still unmatched after all other paths. Matches against street-level
#'         aliases built by \code{gnaf_build_street_aliases}. Useful when a
#'         specific street number is absent from GNAF but the street itself
#'         exists.
#' }
#'
#' @param con DBI connection from \code{gnaf_connect}.
#' @param addresses Character vector of address strings.
#' @param max_results Maximum number of matches to return per input.
#' @param min_score Minimum total score (0-100) to include in results.
#' @param include_custom Include custom addresses in matching.
#' @param alias_types Character vector of \code{alias_type} values to include
#'   in matching. Use \code{NA} to include core GNAF rows (where
#'   \code{alias_type} is \code{NULL}). Default \code{NULL} matches all rows
#'   regardless of alias type. Example: \code{c(NA, "street_only")} restricts
#'   to core addresses and street-level aliases only.
#' @param locality_fallback If \code{TRUE} (default), re-searches by locality
#'   name for unmatched inputs and results below \code{fallback_threshold}
#'   whose locality component is weak.
#' @param street_only_fallback If \code{TRUE}, inputs still unmatched after all
#'   other paths are re-matched against street-level aliases
#'   (\code{alias_type = "street_only"}) in \code{gnaf_addresses}. Requires
#'   \code{gnaf_build_street_aliases} to have been run first. Default
#'   \code{FALSE}.
#' @param fallback_threshold Total score at or below which a weak-locality
#'   result is eligible for locality fallback. Default 90: a clean correct
#'   match typically scores 95+, whereas a
#'   coincidental match - same postcode and house number, unrelated street -
#'   can still reach into the low-to-mid 80s (e.g. 20 postcode + 10 number +
#'   partial suburb/street-name credit). 90 leaves enough margin to catch those
#'   coincidences and let the locality scan (which discovers the right postcode
#'   from the suburb name regardless of how far off the stated one is) find the
#'   true candidate, without firing for genuinely good matches.
#' @param weights Named list of score weights. Defaults to postcode = 20,
#'   suburb = 15, street_name = 40, street_type = 10, number = 10, flat = 5.
#'   Weights must sum to 100.
#' @param normalize Passed to \code{address_parse}; defaults to \code{TRUE}.
#' @param cache If \code{TRUE} (default), checks \code{gnaf_match_cache} for
#'   previously matched addresses and stores new high-confidence results. The
#'   one-result cache is bypassed when \code{max_results > 1}.
#' @param cache_threshold Minimum score for a new result to be cached.
#'   Default 95.
#' @param verbose If \code{TRUE}, prints colored progress, timings, and match
#'   summary information using the \pkg{cli} package.
#' @return A \code{data.table} ordered by \code{input_id} then descending
#'   \code{total_score}. Includes a standardised input string for every row and
#'   retains unmatched inputs with missing match columns.
#' @export
gnaf_match <- function(addresses, con, max_results = 1L, min_score = 60L,
                       include_custom = TRUE,
                       alias_types = NULL,
                       locality_fallback = TRUE,
                       street_only_fallback = FALSE,
                       fallback_threshold = 90L,
                       weights = .default_match_weights(),
                       normalize = TRUE,
                       cache = TRUE,
                       cache_threshold = 95L,
                       verbose = TRUE) {

  legacy_order <- tryCatch(DBI::dbIsValid(addresses), error = function(e) FALSE) &&
    is.character(con)
  if (legacy_order) {
    warning(
      "`gnaf_match(con, addresses)` is deprecated; use `gnaf_match(addresses, con)`.",
      call. = FALSE
    )
    tmp <- addresses
    addresses <- con
    con <- tmp
  }

  if (!is.character(addresses) || length(addresses) == 0L)
    stop("'addresses' must be a non-empty character vector")

  max_results <- .as_positive_integer(max_results, "max_results")
  if (length(min_score) != 1L || is.na(min_score) || !is.numeric(min_score) ||
      min_score < 0 || min_score > 100) {
    stop("'min_score' must be one number between 0 and 100", call. = FALSE)
  }
  min_score <- as.integer(min_score)

  weights <- .validate_match_weights(weights)
  staged_alias_search <- is.null(alias_types)
  primary_alias_types <- if (staged_alias_search) NA_character_ else alias_types

  # Cached scores were computed with the default weights; serving or storing
  # them under different weights would silently mis-score, so the cache is
  # bypassed for the whole call when non-default weights are supplied.
  cache_schema_current <- DBI::dbExistsTable(con, "gnaf_match_cache") &&
    "algorithm_version" %in% DBI::dbListFields(con, "gnaf_match_cache")
  cache_usable <- isTRUE(cache) && cache_schema_current &&
    max_results == 1L &&
    identical(weights, .validate_match_weights(.default_match_weights()))

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
  parsed <- address_parse(addresses, normalize = normalize)
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
  raw_is_standard <- toupper(trimws(parsed$input_raw)) == parsed$input_standardised
  use_exact_label_path <- nrow(parsed) <= 100L ||
    mean(raw_is_standard, na.rm = TRUE) >= 0.05
  exact_path <- if (isTRUE(use_exact_label_path)) {
    .exact_label_match(
      con, parsed, include_custom, alias_types, weights, min_score
    )
  } else {
    NULL
  }
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
  if (isTRUE(cache) && !cache_usable)
    .cli_match_detail(
      verbose,
      "Match cache bypassed: it requires default weights and max_results = 1."
    )
  if (cache_usable && DBI::dbExistsTable(con, "gnaf_match_cache")) {
    remaining_stds <- unique(stats::na.omit(
      parsed[!input_id %in% skip_ids, input_standardised]
    ))
    if (length(remaining_stds) > 0L) {
      cache_raw <- .cache_lookup(
        con, remaining_stds, include_custom, alias_types, min_score
      )
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

  deduplicated <- .deduplicate_match_inputs(
    parsed[!input_id %in% skip_ids]
  )
  match_inputs <- deduplicated$inputs
  input_fanout <- deduplicated$fanout
  has_pc <- match_inputs[!is.na(in_postcode)]
  no_pc  <- match_inputs[is.na(in_postcode)]
  slow_timer <- proc.time()[["elapsed"]]

  # ------------------------------------------------------------------
  # Path 1: postcode path - scored entirely in DuckDB
  # ------------------------------------------------------------------
  if (nrow(has_pc) > 0L) {
    n_pc <- uniqueN(has_pc$in_postcode)
    .cli_match_step(verbose, sprintf(
      "Scoring %s input(s) across %s unique postcode(s) in DuckDB.",
      cli::col_blue(format(nrow(has_pc), big.mark = ",")),
      cli::col_cyan(format(n_pc, big.mark = ","))
    ))
    # A separate exact-component pass is faster for interactive-sized calls,
    # but becomes a second full table scan for large registered input batches.
    # Large batches use exact-number, range, lot, suffix, and missing-number
    # branches inside one postcode query instead.
    use_separate_exact_path <- nrow(has_pc) <= 100L
    exact_components <- if (use_separate_exact_path) {
      .match_exact_components_duckdb(
        con, has_pc, max_results, min_score, weights, include_custom,
        verbose, primary_alias_types
      )
    } else {
      .empty_path_result()
    }
    strong_ids <- if (!is.null(exact_components$matches) &&
                      nrow(exact_components$matches) > 0L) {
      exact_components$matches[
        , .(best_score = max(total_score)), by = input_id
      ][best_score > fallback_threshold, input_id]
    } else {
      integer()
    }
    fuzzy_inputs <- if (use_separate_exact_path) {
      has_pc[!input_id %in% strong_ids]
    } else {
      has_pc
    }
    fuzzy_components <- .match_postcode_duckdb(
      con, fuzzy_inputs, max_results, min_score, weights, include_custom,
      verbose, primary_alias_types
    )
    pc_path <- .combine_path_results(
      exact_components, fuzzy_components, max_results
    )
    results[["postcode"]]     <- pc_path$matches
    diagnostics[["postcode"]] <- pc_path$diagnostics
  }

  # ------------------------------------------------------------------
  # Path 2: no-postcode inputs - state-level fallback in DuckDB
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
                                      weights, include_custom, verbose,
                                      primary_alias_types)
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
      ordered_pc <- copy(pc_res)
      setorder(ordered_pc, input_id, -total_score, address_detail_pid)
      ordered_pc[, .SD[1L], by = input_id]
    } else {
      data.table(
        input_id = integer(), total_score = integer(),
        score_suburb = integer()
      )
    }

    weak_ids <- best_by_input[
      total_score <= fallback_threshold & score_suburb < 13L,
      input_id
    ]
    matched_ids   <- best_by_input$input_id
    unmatched_ids <- has_pc[!input_id %in% matched_ids, input_id]
    no_pc_loc_ids <- no_pc[!is.na(in_locality), input_id]

    fallback_ids   <- unique(c(weak_ids, unmatched_ids, no_pc_loc_ids))
    fallback_parse <- match_inputs[
      input_id %in% fallback_ids & !is.na(in_locality)
    ]

    if (nrow(fallback_parse) > 0L) {
      fallback_word <- if (nrow(fallback_parse) == 1L) "row" else "rows"
      .cli_match_step(verbose, sprintf(
        "Running locality fallback for %s input %s.",
        cli::col_magenta(format(nrow(fallback_parse), big.mark = ",")),
        fallback_word
      ))
      loc_path <- .match_locality_duckdb(con, fallback_parse, max_results,
                                          min_score, weights, include_custom,
                                          verbose, primary_alias_types)
      results[["locality"]]     <- loc_path$matches
      diagnostics[["locality"]] <- loc_path$diagnostics
    }
  }

  # Default alias search is deliberately staged after the core table. Street
  # and address aliases are searched only for weak street results; the much
  # larger locality-alias set is searched only when locality agreement is weak.
  if (staged_alias_search && nrow(match_inputs) > 0L) {
    empty_best <- data.table(
      input_id = integer(), total_score = integer(),
      score_street_name = integer(), score_suburb = integer()
    )
    current_parts <- Filter(
      function(x) !is.null(x) && nrow(x) > 0L,
      results
    )
    current <- if (length(current_parts) > 0L) {
      rbindlist(current_parts, fill = TRUE, use.names = TRUE)
    } else {
      data.table()
    }
    if (nrow(current) > 0L) {
      current <- current[input_id %in% match_inputs$input_id]
      if (nrow(current) > 0L) {
        setorder(current, input_id, -total_score, address_detail_pid)
        current_best <- current[, .SD[1L], by = input_id]
      } else {
        current_best <- copy(empty_best)
      }
    } else {
      current_best <- copy(empty_best)
    }
    unmatched_alias_ids <- match_inputs[
      !input_id %in% current_best$input_id, input_id
    ]
    alias_weak_threshold <- min(fallback_threshold, min_score + 5L)
    street_alias_ids <- unique(c(
      unmatched_alias_ids,
      current_best[
        total_score <= alias_weak_threshold & score_street_name < 30L,
        input_id
      ]
    ))
    locality_alias_ids <- current_best[
      total_score <= alias_weak_threshold & score_suburb < 13L,
      unique(input_id)
    ]

    if (length(street_alias_ids) > 0L || length(locality_alias_ids) > 0L) {
      exact_alias_types <- "__GNAFR_EXACT_ALIASES__"
      locality_alias_types <- "__GNAFR_LOCALITY_ALIASES__"

      street_inputs <- match_inputs[input_id %in% street_alias_ids]
      if (nrow(street_inputs) > 0L && length(exact_alias_types) > 0L) {
        exact_alias <- .match_exact_components_duckdb(
          con, street_inputs[!is.na(in_postcode)], max_results, min_score,
          weights, include_custom, verbose, exact_alias_types
        )
        exact_alias_strong <- if (!is.null(exact_alias$matches)) {
          exact_alias$matches[
            , .(best_score = max(total_score)), by = input_id
          ][best_score > fallback_threshold, input_id]
        } else {
          integer()
        }
        fuzzy_alias_inputs <- street_inputs[
          !input_id %in% exact_alias_strong & !is.na(in_postcode)
        ]
        fuzzy_alias <- .match_postcode_duckdb(
          con, fuzzy_alias_inputs, max_results, min_score, weights,
          include_custom, verbose, exact_alias_types
        )
        state_alias <- .match_state_duckdb(
          con, street_inputs[is.na(in_postcode) & !is.na(in_state)],
          max_results, min_score, weights, include_custom, verbose, exact_alias_types
        )
        street_alias <- .combine_path_results(
          exact_alias, fuzzy_alias, max_results
        )
        street_alias <- .combine_path_results(
          street_alias, state_alias, max_results
        )
        results[["street_aliases"]] <- street_alias$matches
        diagnostics[["street_aliases"]] <- street_alias$diagnostics
        strong_street_alias_ids <- if (!is.null(street_alias$matches)) {
          street_alias$matches[
            , .(best_score = max(total_score)), by = input_id
          ][best_score > fallback_threshold, input_id]
        } else {
          integer()
        }
      } else {
        strong_street_alias_ids <- integer()
      }

      locality_inputs <- match_inputs[
        input_id %in% locality_alias_ids &
          !input_id %in% strong_street_alias_ids
      ]
      if (nrow(locality_inputs) > 0L && length(locality_alias_types) > 0L) {
        locality_alias <- .match_locality_aliases_duckdb(
          con, locality_inputs, max_results, min_score, weights,
          include_custom, verbose, locality_alias_types
        )
        results[["locality_aliases"]] <- locality_alias$matches
        diagnostics[["locality_aliases"]] <- locality_alias$diagnostics
      }
    }
  }

  # ------------------------------------------------------------------
  # Path 4: street-only fallback for inputs still unmatched after all paths
  # ------------------------------------------------------------------
  if (isTRUE(street_only_fallback)) {
    matched_so_far <- unique(unlist(lapply(
      results,
      function(r) if (!is.null(r) && nrow(r) > 0L) r$input_id else integer(0L)
    )))
    so_parse <- match_inputs[
      !input_id %in% matched_so_far &
      !is.na(in_postcode) &
      !is.na(in_street_name)
    ]
    if (nrow(so_parse) > 0L) {
      .cli_match_step(verbose, sprintf(
        "Running street-only fallback for %s input(s).",
        cli::col_magenta(format(nrow(so_parse), big.mark = ","))
      ))
      so_path <- .match_street_only_duckdb(
        con, so_parse, max_results, min_score, weights, verbose
      )
      results[["street_only"]]     <- so_path$matches
      diagnostics[["street_only"]] <- so_path$diagnostics
    }
  }

  verbose_stats$slow_elapsed <- proc.time()[["elapsed"]] - slow_timer

  # ------------------------------------------------------------------
  # Combine paths, deduplicate, re-rank
  # ------------------------------------------------------------------
  .cli_match_step(verbose, "Wrangling final match output.")
  wrangle_timer <- proc.time()[["elapsed"]]
  slow_names <- setdiff(names(results), c("exact", "cache"))
  for (nm in slow_names) {
    results[[nm]] <- .fanout_match_rows(results[[nm]], input_fanout)
  }
  for (nm in names(diagnostics)) {
    diagnostics[[nm]] <- .fanout_match_rows(diagnostics[[nm]], input_fanout)
  }
  out <- rbindlist(results, fill = TRUE, use.names = TRUE)
  if (nrow(out) > 0L) {
    # Deduplicate: same GNAF record may appear from multiple paths; the order
    # puts the higher-scoring duplicate first so unique() keeps it.
    setorder(out, input_id, -total_score, address_detail_pid)
    out <- unique(out, by = c("input_id", "address_detail_pid"))

    # Re-apply max_results and assign final rank
    out <- out[out[, .I[seq_len(min(.N, max_results))], by = input_id]$V1]
    out[, match_rank := seq_len(.N), by = input_id]
  } else {
    out <- .empty_result()
  }

  common_cols <- setdiff(intersect(names(out), names(parsed)), "input_id")
  if (length(common_cols) > 0L) out[, (common_cols) := NULL]

  out <- merge(parsed, out, by = "input_id", all.x = TRUE, sort = FALSE)
  out[, matched := !is.na(address_detail_pid)]
  out <- .append_match_status(out, parsed, diagnostics)

  cols_first <- c("input_id", "input_raw", "input_standardised", "address_label", "match_rank",
                  "matched", "match_status", "total_score", "score_postcode", "score_suburb",
                  "score_street_name", "score_street_type", "score_number",
                  "score_flat")
  setcolorder(out, c(cols_first, setdiff(names(out), cols_first)))
  setorder(out, input_id, -matched, match_rank)
  # Store newly matched high-confidence results in the cache.
  # ON CONFLICT DO NOTHING means cache/exact-path hits are silently skipped.
  if (cache_usable && is.null(alias_types) &&
      DBI::dbExistsTable(con, "gnaf_match_cache") && nrow(out) > 0L)
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

# Candidate columns selected from the GNAF/custom side in every path query.
.GNAF_SELECT_COLS <- "g.address_detail_pid, g.address_label, g.address_site_name,
    g.building_name, g.flat_type, g.flat_number, g.level_type, g.level_number,
    g.number_first, g.number_last, g.lot_number,
    g.street_name, g.street_type, g.street_suffix, g.locality_name,
    g.state, g.postcode, g.longitude, g.latitude, g.source, g.alias_type,
    g.alias_principal, g.principal_pid, g.primary_secondary, g.primary_pid,
    g.geocode_type, g.date_created, g.legal_parcel_id, g.mb_code"

.table_has_rows <- function(con, table) {
  if (!DBI::dbExistsTable(con, table)) return(FALSE)
  isTRUE(tryCatch(
    DBI::dbGetQuery(
      con,
      sprintf("SELECT EXISTS (SELECT 1 FROM %s LIMIT 1) AS has_rows", table)
    )$has_rows[[1L]],
    error = function(e) FALSE
  ))
}

# Coarse street-number pre-filter shared by the postcode, state and locality
# paths: when the input carries a number, only keep candidates whose number
# matches exactly, covers it as a range, or (suffixed inputs like "190A")
# matches the address_label prefix. Range bounds are checked explicitly;
# "OR g.number_last IS NOT NULL" without bounds would pull every range record
# regardless of the input number.
.number_prefilter_sql <- function() {
  paste(
    "(",
    "  (i.in_lot_number IS NOT NULL AND TRIM(CAST(g.lot_number AS VARCHAR)) = TRIM(i.in_lot_number))",
    "  OR (i.in_lot_number IS NULL AND (",
    "    i.in_number_first IS NULL",
    "    OR g.number_first = i.in_number_first",
    "    OR (g.number_last IS NOT NULL",
    "        AND g.number_first <= i.in_number_first",
    "        AND i.in_number_first <= g.number_last)",
    "    OR (g.number_first IS NULL AND i.in_number_suffix IS NOT NULL",
    "        AND starts_with(g.address_label, CAST(i.in_number_first AS VARCHAR) || i.in_number_suffix || ' '))",
    "  ))",
    ")"
  )
}

# Core query runner shared by all paths.
# inputs_tbl  : name of a duckdb_register'd virtual table of parsed inputs
# gnaf_tbl    : "gnaf_addresses" or "custom_addresses"
# join_clause : SQL ON expression (uses aliases i = inputs, g = gnaf)
# pre_filter  : additional WHERE predicates (coarse, no JW)
#
# Design notes:
#   * No window-function aggregates (COUNT/MAX OVER) before the score filter -
#     those force full materialisation of the join which kills RAM at scale.
#   * Street-name JW pre-filter (>= 0.3) in the WHERE clause cuts the
#     intermediate table size dramatically before scoring the remaining rows.
#   * candidate_count is set to NA; match_status "below_min_score" vs
#     "no_candidate" is not distinguishable, which is an acceptable trade-off.
.run_duckdb_score_query <- function(con, inputs_tbl, gnaf_tbl, join_clause,
                                    pre_filter, weights, max_results, min_score,
                                    verbose = FALSE, label = "",
                                    split_number = FALSE) {
  exprs <- .score_sql_exprs(
    weights, i = "c", g = "c",
    suburb_similarity = "c.suburb_similarity",
    street_similarity = "c.street_similarity"
  )
  sel_scores <- paste(
    mapply(function(nm, ex) sprintf("    %s AS %s", ex, nm), names(exprs), exprs),
    collapse = ",\n"
  )
  score_total <- paste(names(exprs), collapse = " + ")
  raw_projection <- "SELECT
    g.address_detail_pid, g.address_label,
    g.postcode, g.locality_name, g.street_name, g.street_type,
    g.number_first, g.number_last, g.lot_number,
    g.flat_type, g.flat_number, g.level_type, g.level_number,
    i.input_id,
    i.in_postcode, i.in_locality, i.in_street_name, i.in_street_type,
    i.in_number_first, i.in_number_suffix, i.in_lot_number,
    i.in_flat_type, i.in_flat_number, i.in_level_type, i.in_level_number,
    CASE WHEN i.in_locality IS NOT NULL AND i.in_locality = g.locality_name
         THEN 1.0
         WHEN i.in_locality IS NOT NULL AND g.locality_name IS NOT NULL
         THEN jaro_winkler_similarity(i.in_locality, g.locality_name)
         ELSE 0 END AS suburb_similarity,
    CASE WHEN i.in_street_name IS NOT NULL AND i.in_street_name = g.street_name
         THEN 1.0
         WHEN i.in_street_name IS NOT NULL AND g.street_name IS NOT NULL
         THEN jaro_winkler_similarity(i.in_street_name, g.street_name)
         ELSE 0 END AS street_similarity"
  candidate_sql <- if (split_number) {
    branch <- function(predicate) sprintf(
      "%s
       FROM %s g JOIN %s i ON (%s) AND (%s)
       WHERE %s",
      raw_projection, gnaf_tbl, inputs_tbl, join_clause, predicate, pre_filter
    )
    paste(c(
      branch(paste(
        "i.in_lot_number IS NOT NULL",
        "AND TRIM(CAST(g.lot_number AS VARCHAR)) = TRIM(i.in_lot_number)"
      )),
      branch(paste(
        "i.in_lot_number IS NULL AND i.in_number_first IS NOT NULL",
        "AND g.number_first = i.in_number_first"
      )),
      branch(paste(
        "i.in_lot_number IS NULL AND i.in_number_first IS NOT NULL",
        "AND g.number_last IS NOT NULL",
        "AND g.number_first < i.in_number_first",
        "AND i.in_number_first <= g.number_last"
      )),
      branch(paste(
        "i.in_lot_number IS NULL AND i.in_number_first IS NOT NULL",
        "AND i.in_number_suffix IS NOT NULL AND g.number_first IS NULL",
        "AND starts_with(g.address_label, CAST(i.in_number_first AS VARCHAR) || i.in_number_suffix || ' ')"
      )),
      branch(
        "i.in_lot_number IS NULL AND i.in_number_first IS NULL"
      )
    ), collapse = "\nUNION ALL\n")
  } else {
    sprintf(
      "%s
       FROM %s g JOIN %s i ON %s
       WHERE %s",
      raw_projection, gnaf_tbl, inputs_tbl, join_clause, pre_filter
    )
  }

  final_scores <- paste0("r.", names(exprs), collapse = ", ")
  sql <- sprintf("
WITH raw_candidates AS (
%s
),
candidates AS (
  SELECT * FROM raw_candidates
  WHERE in_street_name IS NULL OR street_similarity >= 0.3
),
components AS (
  SELECT address_detail_pid, input_id,
%s
  FROM candidates c
),
scored AS (
  SELECT *, %s AS total_score
  FROM components
),
ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (PARTITION BY input_id ORDER BY total_score DESC, address_detail_pid) AS match_rank
  FROM scored
  WHERE total_score >= %d
)
SELECT %s, r.input_id, %s, r.total_score, r.match_rank
FROM ranked r
JOIN %s g ON g.address_detail_pid = r.address_detail_pid
WHERE r.match_rank <= %d
",
    candidate_sql,
    sel_scores,
    score_total,
    min_score,
    .GNAF_SELECT_COLS, final_scores, gnaf_tbl, max_results
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
    setorder(matches, input_id, -total_score, address_detail_pid)
    matches <- matches[matches[, .I[seq_len(min(.N, max_results))], by = input_id]$V1]
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

.match_exact_components_duckdb <- function(
    con, inputs_dt, max_results, min_score, weights, include_custom,
    verbose = FALSE, alias_types = NULL) {
  inputs_dt <- inputs_dt[!is.na(in_street_name)]
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  duckdb::duckdb_register(
    con, "__gnafr_exact_component_inputs__", inputs_dt, overwrite = TRUE
  )
  on.exit(try(
    duckdb::duckdb_unregister(con, "__gnafr_exact_component_inputs__"),
    silent = TRUE
  ))

  alias_sql <- .alias_type_sql(alias_types)
  split_number <- identical(alias_types, NA_character_)
  pre_filter <- paste(
    if (split_number) "TRUE" else .number_prefilter_sql(),
    if (!is.null(alias_sql)) paste("AND", alias_sql) else ""
  )
  join_on <- paste(
    "g.postcode = i.in_postcode",
    "AND g.street_name = i.in_street_name"
  )
  out <- .run_duckdb_score_query(
    con, "__gnafr_exact_component_inputs__", "gnaf_addresses",
    join_on, pre_filter, weights, max_results, min_score, verbose,
    label = "gnaf_addresses (exact components)", split_number = split_number
  )
  if (include_custom && .table_has_rows(con, "custom_addresses")) {
    custom <- .run_duckdb_score_query(
      con, "__gnafr_exact_component_inputs__", "custom_addresses",
      join_on, pre_filter, weights, max_results, min_score, verbose,
      label = "custom_addresses (exact components)", split_number = split_number
    )
    out <- .combine_path_results(out, custom, max_results)
  }
  out
}

.match_postcode_duckdb <- function(con, inputs_dt, max_results, min_score,
                                   weights, include_custom, verbose = FALSE,
                                   alias_types = NULL) {
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  # Block on an exact postcode match - this is the hot path and must stay a
  # cheap hash join against the full multi-million-row gnaf_addresses table.
  # Near-miss postcodes (off by a digit, postal vs. delivery postcode, etc.)
  # are NOT retrieved here; instead .score_sql_exprs still gives partial
  # credit when in_postcode and postcode happen to both appear (e.g. via the
  # locality-fallback path below, which discovers candidates by suburb name
  # regardless of how far off the stated postcode is - the right tool for
  # "the postcode looks fine but is actually wrong", which broadening this
  # join to +/- N would only handle for small, fixed N at a steep cost: every
  # extra offset multiplies the join's candidate volume (and runtime) because
  # both the input side AND the matching gnaf rows per postcode multiply.
  duckdb::duckdb_register(con, "__gnafr_pc_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_pc_inputs__"), silent = TRUE))

  alias_sql <- .alias_type_sql(alias_types)
  join_on  <- "g.postcode = i.in_postcode"
  split_number <- nrow(inputs_dt) > 100L
  pre_filt <- paste(
    "i.in_postcode IS NOT NULL",
    if (!split_number) paste("AND", .number_prefilter_sql()) else "",
    if (!is.null(alias_sql)) paste("AND", alias_sql) else ""
  )

  res <- .run_duckdb_score_query(
    con, "__gnafr_pc_inputs__", "gnaf_addresses",
    join_on, pre_filt, weights, max_results, min_score, verbose,
    label = "gnaf_addresses (postcode)", split_number = split_number
  )

  if (include_custom && .table_has_rows(con, "custom_addresses")) {
    r2 <- .run_duckdb_score_query(
      con, "__gnafr_pc_inputs__", "custom_addresses",
      join_on, pre_filt, weights, max_results, min_score, verbose,
      label = "custom_addresses (postcode)", split_number = split_number
    )
    res <- .combine_path_results(res, r2, max_results)
  }

  res
}

.match_state_duckdb <- function(con, inputs_dt, max_results, min_score,
                                weights, include_custom, verbose = FALSE,
                                alias_types = NULL) {
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  duckdb::duckdb_register(con, "__gnafr_st_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_st_inputs__"), silent = TRUE))

  alias_sql <- .alias_type_sql(alias_types)
  join_on  <- "g.state = i.in_state"
  # The same number pre-filter as the postcode path: without it this path
  # scores an entire state's rows per input, which dominates its runtime.
  pre_filt <- paste(
    "i.in_state IS NOT NULL",
    if (!is.null(alias_sql)) paste("AND", alias_sql) else ""
  )

  res <- .run_duckdb_score_query(
    con, "__gnafr_st_inputs__", "gnaf_addresses",
    join_on, pre_filt, weights, max_results, min_score, verbose,
    label = "gnaf_addresses (state)", split_number = TRUE
  )

  if (include_custom && .table_has_rows(con, "custom_addresses")) {
    r2 <- .run_duckdb_score_query(
      con, "__gnafr_st_inputs__", "custom_addresses",
      join_on, pre_filt, weights, max_results, min_score, verbose,
      label = "custom_addresses (state)", split_number = TRUE
    )
    res <- .combine_path_results(res, r2, max_results)
  }

  res
}

# Locality fallback: fuzzy-match suburb -> discover correct postcodes -> score.
# The entire pipeline (locality lookup + join + scoring + ranking) runs in one
# DuckDB query, so no cartesian product ever lands in R memory.
.match_locality_aliases_duckdb <- function(
    con, inputs_dt, max_results, min_score, weights, include_custom,
    verbose = FALSE,
    alias_types) {
  inputs_dt <- inputs_dt[!is.na(in_locality)]
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  locality_keys <- unique(inputs_dt[, .(input_id, in_locality, in_state)])
  duckdb::duckdb_register(
    con, "__gnafr_alias_loc_keys__", locality_keys, overwrite = TRUE
  )
  on.exit(try(
    duckdb::duckdb_unregister(con, "__gnafr_alias_loc_keys__"), silent = TRUE
  ))
  loc_map <- setDT(DBI::dbGetQuery(con, "
    WITH similarities AS (
      SELECT i.input_id, l.locality_name AS alias_locality,
             CASE WHEN l.locality_name = i.in_locality THEN 1.0
                  ELSE jaro_winkler_similarity(l.locality_name, i.in_locality)
             END AS similarity
      FROM __gnafr_alias_loc_keys__ i
      JOIN gnaf_locality_index l
        ON i.in_state IS NULL OR l.state = i.in_state
    ), ranked AS (
      SELECT *, ROW_NUMBER() OVER (
        PARTITION BY input_id ORDER BY similarity DESC, alias_locality
      ) AS locality_rank
      FROM similarities
      WHERE similarity >= 0.85
    )
    SELECT DISTINCT input_id, alias_locality
    FROM ranked
    WHERE locality_rank <= 5
  "))
  if (nrow(loc_map) == 0L) return(.empty_path_result())

  expanded <- inputs_dt[loc_map, on = "input_id", nomatch = 0L]
  alias_sql <- .alias_type_sql(alias_types)
  pre_filter <- paste(
    .number_prefilter_sql(),
    if (!is.null(alias_sql)) paste("AND", alias_sql) else ""
  )
  out <- .empty_path_result()

  pc <- expanded[!is.na(in_postcode)]
  if (nrow(pc) > 0L) {
    duckdb::duckdb_register(
      con, "__gnafr_alias_loc_pc__", pc, overwrite = TRUE
    )
    on.exit(try(
      duckdb::duckdb_unregister(con, "__gnafr_alias_loc_pc__"), silent = TRUE
    ), add = TRUE)
    pc_out <- .run_duckdb_score_query(
      con, "__gnafr_alias_loc_pc__", "gnaf_addresses",
      paste(
        "g.postcode = i.in_postcode",
        "AND g.locality_name = i.alias_locality"
      ),
      pre_filter, weights, max_results, min_score, verbose,
      label = "gnaf_addresses (locality aliases)"
    )
    out <- .combine_path_results(out, pc_out, max_results)
    if (include_custom && .table_has_rows(con, "custom_addresses")) {
      pc_custom <- .run_duckdb_score_query(
        con, "__gnafr_alias_loc_pc__", "custom_addresses",
        paste(
          "g.postcode = i.in_postcode",
          "AND g.locality_name = i.alias_locality"
        ),
        pre_filter, weights, max_results, min_score, verbose,
        label = "custom_addresses (locality aliases)"
      )
      out <- .combine_path_results(out, pc_custom, max_results)
    }
  }

  state <- expanded[is.na(in_postcode) & !is.na(in_state)]
  if (nrow(state) > 0L) {
    duckdb::duckdb_register(
      con, "__gnafr_alias_loc_state__", state, overwrite = TRUE
    )
    on.exit(try(
      duckdb::duckdb_unregister(con, "__gnafr_alias_loc_state__"), silent = TRUE
    ), add = TRUE)
    state_out <- .run_duckdb_score_query(
      con, "__gnafr_alias_loc_state__", "gnaf_addresses",
      paste(
        "g.state = i.in_state",
        "AND g.locality_name = i.alias_locality"
      ),
      pre_filter, weights, max_results, min_score, verbose,
      label = "gnaf_addresses (state locality aliases)"
    )
    out <- .combine_path_results(out, state_out, max_results)
    if (include_custom && .table_has_rows(con, "custom_addresses")) {
      state_custom <- .run_duckdb_score_query(
        con, "__gnafr_alias_loc_state__", "custom_addresses",
        paste(
          "g.state = i.in_state",
          "AND g.locality_name = i.alias_locality"
        ),
        pre_filter, weights, max_results, min_score, verbose,
        label = "custom_addresses (state locality aliases)"
      )
      out <- .combine_path_results(out, state_custom, max_results)
    }
  }
  out
}

.match_locality_duckdb_legacy <- function(con, inputs_dt, max_results, min_score,
                                   weights, include_custom, verbose = FALSE,
                                   alias_types = NULL) {
  if (nrow(inputs_dt) == 0L || !any(!is.na(inputs_dt$in_locality)))
    return(.empty_path_result())

  duckdb::duckdb_register(con, "__gnafr_loc_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_loc_inputs__"), silent = TRUE))

  alias_sql    <- .alias_type_sql(alias_types)
  alias_clause <- if (!is.null(alias_sql)) paste0("\n    AND ", alias_sql) else ""

  exprs <- .score_sql_exprs(
    weights, i = "c", g = "c",
    suburb_similarity = "c.suburb_similarity",
    street_similarity = "c.street_similarity"
  )
  sel_scores <- paste(
    mapply(function(nm, ex) sprintf("    %s AS %s", ex, nm), names(exprs), exprs),
    collapse = ",\n"
  )
  score_total <- paste(names(exprs), collapse = " + ")
  final_scores <- paste0("r.", names(exprs), collapse = ", ")

  make_sql <- function(gnaf_tbl) sprintf("
WITH unique_locs AS (
  -- Deduplicate localities before the JW scan so the cross-product is
  -- (unique_localities x locality_index) not (all_inputs x locality_index).
  SELECT DISTINCT in_locality, in_state
  FROM __gnafr_loc_inputs__
  WHERE in_locality IS NOT NULL
),
loc_similarity AS (
  SELECT ul.in_locality, ul.in_state, g.postcode, g.state,
         CASE WHEN g.locality_name = ul.in_locality THEN 1.0
              ELSE jaro_winkler_similarity(g.locality_name, ul.in_locality)
         END AS locality_similarity
  FROM gnaf_locality_index g
  JOIN unique_locs ul
    ON ul.in_state IS NULL OR g.state = ul.in_state
),
loc_candidates AS (
  SELECT *,
         ROW_NUMBER() OVER (
           PARTITION BY ul.in_locality, ul.in_state
           ORDER BY locality_similarity DESC, postcode, state
         ) AS locality_rank
  FROM loc_similarity ul
  WHERE locality_similarity >= 0.85
),
loc_map AS (
  SELECT DISTINCT in_locality, in_state, postcode, state
  FROM loc_candidates
  WHERE locality_rank <= 5
),
expanded AS (
  -- Two ways to discover an alternative postcode worth trying, both gated to
  -- this already-small fallback set (inputs whose postcode-path result was
  -- weak, missing, or absent):
  --   (a) fuzzy-match the parsed locality name against gnaf_locality_index -
  --       finds the right postcode regardless of how far off the stated one is.
  --   (b) try postcodes within +/- 3 of the stated one - catches near-miss
  --       typos / postal-vs-delivery postcodes whose locality didn't fuzzy-match
  --       (e.g. it was itself misspelt, or absent from the input).
  -- Doing this only here - rather than broadening the primary postcode-path
  -- join - keeps the hot path a cheap equi-join; this fallback only ever
  -- touches the minority of inputs that didn't already score well.
  SELECT i.*, loc_map.postcode AS alt_postcode
  FROM __gnafr_loc_inputs__ i
  JOIN loc_map
    ON loc_map.in_locality = i.in_locality
   AND loc_map.in_state IS NOT DISTINCT FROM i.in_state

  UNION

  SELECT i.*, (i.in_postcode + o.pc_offset) AS alt_postcode
  FROM __gnafr_loc_inputs__ i
  CROSS JOIN (VALUES (-3), (-2), (-1), (1), (2), (3)) AS o(pc_offset)
  WHERE i.in_postcode IS NOT NULL
),
raw_candidates AS (
  SELECT
    g.address_detail_pid, g.address_label,
    g.postcode, g.locality_name, g.street_name, g.street_type,
    g.number_first, g.number_last, g.lot_number,
    g.flat_type, g.flat_number, g.level_type, g.level_number,
    i.input_id,
    i.in_postcode, i.in_locality, i.in_street_name, i.in_street_type,
    i.in_number_first, i.in_number_suffix, i.in_lot_number,
    i.in_flat_type, i.in_flat_number, i.in_level_type, i.in_level_number,
    CASE WHEN i.in_locality IS NOT NULL AND i.in_locality = g.locality_name
         THEN 1.0
         WHEN i.in_locality IS NOT NULL AND g.locality_name IS NOT NULL
         THEN jaro_winkler_similarity(i.in_locality, g.locality_name)
         ELSE 0 END AS suburb_similarity,
    CASE WHEN i.in_street_name IS NOT NULL AND i.in_street_name = g.street_name
         THEN 1.0
         WHEN i.in_street_name IS NOT NULL AND g.street_name IS NOT NULL
         THEN jaro_winkler_similarity(i.in_street_name, g.street_name)
         ELSE 0 END AS street_similarity
  FROM %s g
  JOIN expanded i ON g.postcode = i.alt_postcode
  WHERE %s
    %s
),
candidates AS (
  SELECT * FROM raw_candidates
  WHERE in_street_name IS NULL OR street_similarity >= 0.3
),
components AS (
  SELECT address_detail_pid, input_id,
%s
  FROM candidates c
),
scored AS (
  SELECT *, %s AS total_score
  FROM components
),
ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (PARTITION BY input_id ORDER BY total_score DESC, address_detail_pid) AS match_rank
  FROM scored
  WHERE total_score >= %d
)
SELECT %s, r.input_id, %s, r.total_score, r.match_rank
FROM ranked r
JOIN %s g ON g.address_detail_pid = r.address_detail_pid
WHERE r.match_rank <= %d
",
    gnaf_tbl, .number_prefilter_sql(), alias_clause,
    sel_scores,
    score_total,
    min_score,
    .GNAF_SELECT_COLS, final_scores, gnaf_tbl, max_results
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

  if (include_custom && .table_has_rows(con, "custom_addresses")) {
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

.match_locality_duckdb <- function(con, inputs_dt, max_results, min_score,
                                   weights, include_custom, verbose = FALSE,
                                   alias_types = NULL) {
  if (nrow(inputs_dt) == 0L || !any(!is.na(inputs_dt$in_locality)))
    return(.empty_path_result())

  duckdb::duckdb_register(con, "__gnafr_loc_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_loc_inputs__"), silent = TRUE))

  # Resolve exact locality names with an equi-join. Only genuinely noisy
  # locality tokens pay for an all-locality Jaro-Winkler comparison.
  loc_map <- setDT(DBI::dbGetQuery(con, "
    SELECT DISTINCT i.input_id, l.postcode AS alt_postcode
    FROM __gnafr_loc_inputs__ i
    JOIN gnaf_locality_index l
      ON l.locality_name = i.in_locality
     AND (i.in_state IS NULL OR l.state = i.in_state)
    WHERE i.in_locality IS NOT NULL
  "))

  fuzzy_inputs <- unique(inputs_dt[
    !input_id %in% loc_map$input_id & !is.na(in_locality),
    .(input_id, in_locality, in_state)
  ])
  if (nrow(fuzzy_inputs) > 0L) {
    duckdb::duckdb_register(
      con, "__gnafr_fuzzy_loc_inputs__", fuzzy_inputs, overwrite = TRUE
    )
    on.exit(try(
      duckdb::duckdb_unregister(con, "__gnafr_fuzzy_loc_inputs__"),
      silent = TRUE
    ), add = TRUE)
    fuzzy_map <- setDT(DBI::dbGetQuery(con, "
      WITH similarities AS (
        SELECT i.input_id, l.postcode AS alt_postcode,
               jaro_winkler_similarity(l.locality_name, i.in_locality) AS similarity
        FROM __gnafr_fuzzy_loc_inputs__ i
        JOIN gnaf_locality_index l
          ON i.in_state IS NULL OR l.state = i.in_state
      ), ranked AS (
        SELECT *, ROW_NUMBER() OVER (
          PARTITION BY input_id ORDER BY similarity DESC, alt_postcode
        ) AS locality_rank
        FROM similarities
        WHERE similarity >= 0.85
      )
      SELECT DISTINCT input_id, alt_postcode
      FROM ranked
      WHERE locality_rank <= 5
    "))
    loc_map <- unique(rbindlist(list(loc_map, fuzzy_map), use.names = TRUE))
  }

  offsets <- inputs_dt[
    !input_id %in% loc_map$input_id & !is.na(in_postcode),
    .(alt_postcode = in_postcode + c(-3L, -2L, -1L, 1L, 2L, 3L)),
    by = input_id
  ]
  alt_map <- unique(rbindlist(list(loc_map, offsets), use.names = TRUE))
  if (nrow(alt_map) == 0L) return(.empty_path_result())

  expanded <- inputs_dt[alt_map, on = "input_id", nomatch = 0L]
  expanded <- expanded[
    is.na(in_postcode) | is.na(alt_postcode) | alt_postcode != in_postcode
  ]
  if (nrow(expanded) == 0L) return(.empty_path_result())
  duckdb::duckdb_register(
    con, "__gnafr_loc_expanded__", expanded, overwrite = TRUE
  )
  on.exit(try(
    duckdb::duckdb_unregister(con, "__gnafr_loc_expanded__"), silent = TRUE
  ), add = TRUE)

  alias_sql <- .alias_type_sql(alias_types)
  pre_filter <- paste(
    .number_prefilter_sql(),
    if (!is.null(alias_sql)) paste("AND", alias_sql) else ""
  )
  res <- .run_duckdb_score_query(
    con, "__gnafr_loc_expanded__", "gnaf_addresses",
    "g.postcode = i.alt_postcode", pre_filter,
    weights, max_results, min_score, verbose,
    label = "gnaf_addresses (locality)"
  )
  if (include_custom && .table_has_rows(con, "custom_addresses")) {
    custom <- .run_duckdb_score_query(
      con, "__gnafr_loc_expanded__", "custom_addresses",
      "g.postcode = i.alt_postcode", pre_filter,
      weights, max_results, min_score, verbose,
      label = "custom_addresses (locality)"
    )
    res <- .combine_path_results(res, custom, max_results)
  }
  res
}

# Street-only fallback: match against alias_type = 'street_only' records only.
# Fired for inputs that survived all other paths without a match.
# number_first on these records is NULL, so numbered inputs score 0 for number
# but can still reach min_score via postcode + suburb + street name/type.
.match_street_only_duckdb <- function(con, inputs_dt, max_results, min_score,
                                      weights, verbose = FALSE) {
  if (nrow(inputs_dt) == 0L) return(.empty_path_result())

  duckdb::duckdb_register(con, "__gnafr_so_inputs__", inputs_dt, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_so_inputs__"), silent = TRUE))

  join_on  <- "g.postcode = i.in_postcode AND g.alias_type = 'street_only'"
  pre_filt <- "i.in_postcode IS NOT NULL AND g.alias_type = 'street_only'"

  .run_duckdb_score_query(
    con, "__gnafr_so_inputs__", "gnaf_addresses",
    join_on, pre_filt, weights, max_results, min_score, verbose,
    label = "gnaf_addresses (street_only)"
  )
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
    address_site_name = character(), building_name = character(),
    flat_type = character(), flat_number = character(),
    level_type = character(), level_number = character(), lot_number = character(),
    number_first = integer(), number_last = integer(),
    street_name = character(), street_type = character(),
    street_suffix = character(),
    locality_name = character(), state = character(),
    postcode = integer(), longitude = numeric(), latitude = numeric(),
    source = character(), alias_type = character(),
    alias_principal = character(), principal_pid = character(),
    primary_secondary = character(), primary_pid = character(),
    geocode_type = character(), date_created = as.Date(character()),
    legal_parcel_id = character(), mb_code = character(),
    in_postcode = integer(), in_state = character(), in_locality = character(),
    in_street_name = character(), in_street_type = character(),
    in_street_suffix = character(), in_number_first = integer(),
    in_number_last = integer(), in_flat_type = character(),
    in_flat_number = character(), in_level_type = character(),
    in_level_number = character(), in_lot_number = character(),
    in_building_name = character()
  )
}

.empty_path_result <- function() {
  list(matches = NULL, diagnostics = NULL)
}

.deduplicate_match_inputs <- function(parsed) {
  if (nrow(parsed) == 0L) {
    return(list(inputs = parsed, fanout = data.table(
      input_id = integer(), match_input_id = integer()
    )))
  }
  signature_cols <- c(
    setdiff(
      grep("^in_", names(parsed), value = TRUE),
      c("input_id", "input_raw", "input_standardised")
    ),
    "input_standardised"
  )
  signature_cols <- unique(signature_cols[signature_cols %in% names(parsed)])
  inputs <- unique(parsed, by = signature_cols)
  lookup <- inputs[, c("input_id", signature_cols), with = FALSE]
  setnames(lookup, "input_id", "match_input_id")
  fanout <- lookup[parsed, on = signature_cols, nomatch = 0L, allow.cartesian = TRUE,
                   .(input_id = i.input_id, match_input_id)]
  list(inputs = inputs, fanout = fanout)
}

.fanout_match_rows <- function(rows, fanout) {
  if (is.null(rows) || nrow(rows) == 0L || nrow(fanout) == 0L) return(rows)
  out <- merge(
    fanout, rows,
    by.x = "match_input_id", by.y = "input_id",
    all = FALSE, sort = FALSE, allow.cartesian = TRUE
  )
  out[, match_input_id := NULL]
  setcolorder(out, c("input_id", setdiff(names(out), "input_id")))
  out
}

.standardise_input <- function(parsed) {
  missing_column <- function(name, mode = "character") {
    if (name %in% names(parsed)) return(parsed[[name]])
    switch(mode,
      integer = rep(NA_integer_, nrow(parsed)),
      rep(NA_character_, nrow(parsed))
    )
  }
  postcode_chr <- ifelse(is.na(parsed$in_postcode), NA_character_, as.character(parsed$in_postcode))
  number_suffix <- missing_column("in_number_suffix")
  suffix_chr <- ifelse(is.na(number_suffix), "", number_suffix)
  number_chr <- ifelse(
    is.na(parsed$in_number_first),
    NA_character_,
    ifelse(
      is.na(parsed$in_number_last),
      paste0(parsed$in_number_first, suffix_chr),
      paste0(parsed$in_number_first, suffix_chr, "-", parsed$in_number_last)
    )
  )
  flat_chr <- ifelse(
    !is.na(parsed$in_flat_type) & !is.na(parsed$in_flat_number),
    paste(parsed$in_flat_type, parsed$in_flat_number),
    ifelse(!is.na(parsed$in_flat_number), parsed$in_flat_number, NA_character_)
  )
  in_level_type <- missing_column("in_level_type")
  in_level_number <- missing_column("in_level_number")
  in_lot_number <- missing_column("in_lot_number")
  level_chr <- ifelse(
    !is.na(in_level_type) & !is.na(in_level_number),
    paste(in_level_type, in_level_number),
    ifelse(!is.na(in_level_number), in_level_number, NA_character_)
  )
  lot_chr <- ifelse(
    !is.na(in_lot_number), paste("LOT", in_lot_number),
    NA_character_
  )

  line_one <- .collapse_address_parts(
    parsed$in_building_name,
    flat_chr,
    level_chr,
    lot_chr,
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
.exact_label_match <- function(con, parsed, include_custom, alias_types = NULL,
                                weights = .default_match_weights(),
                                min_score = 0L) {
  raw_upper <- unique(toupper(trimws(parsed$input_raw)))
  raw_upper <- raw_upper[nzchar(raw_upper) & !is.na(raw_upper)]
  if (length(raw_upper) == 0L) return(NULL)

  # Register as a virtual table so DuckDB can hash-join instead of scanning
  # with a 50k-item IN() literal (which kills the query planner at scale).
  lkp <- data.table(lbl_key = raw_upper)
  duckdb::duckdb_register(con, "__gnafr_exact_lkp__", lkp, overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "__gnafr_exact_lkp__"), silent = TRUE))

  alias_sql   <- .alias_type_sql(alias_types)
  alias_where <- if (!is.null(alias_sql)) sprintf("\n     AND %s", alias_sql) else ""

  sql <- sprintf(
    "WITH exact_keys AS MATERIALIZED (
       SELECT g.address_detail_pid
       FROM gnaf_addresses g
       JOIN __gnafr_exact_lkp__ l ON g.address_label = l.lbl_key%s
     )
     SELECT %s FROM gnaf_addresses g
     JOIN exact_keys k USING (address_detail_pid)",
    alias_where,
    .GNAF_SELECT_COLS
  )
  cands <- setDT(DBI::dbGetQuery(con, sql))

  if (include_custom && .table_has_rows(con, "custom_addresses")) {
    sql2 <- sprintf(
      "WITH exact_keys AS MATERIALIZED (
         SELECT g.address_detail_pid
         FROM custom_addresses g
         JOIN __gnafr_exact_lkp__ l ON g.address_label = l.lbl_key%s
       )
       SELECT %s FROM custom_addresses g
       JOIN exact_keys k USING (address_detail_pid)",
      alias_where, .GNAF_SELECT_COLS
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

  joined <- .score_pairs(joined, weights = weights)
  joined <- joined[total_score >= min_score]
  if (nrow(joined) == 0L) return(NULL)
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
