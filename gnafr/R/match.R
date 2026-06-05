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
#' @param verbose If \code{TRUE}, prints colored progress, timings, and match
#'   summary information using the \pkg{cli} package.
#' @return A \code{data.table} ordered by \code{input_id} then descending
#'   \code{total_score}. Includes a standardised input string for every row and
#'   retains unmatched inputs with missing match columns.
#' @export
gnaf_match <- function(con, addresses, max_results = 3L, min_score = 40L,
                       include_custom = TRUE,
                       locality_fallback = TRUE,
                       fallback_threshold = 80L,
                       weights = .default_match_weights(),
                       verbose = FALSE) {

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
  parsed[, input_standardised := .standardise_input(parsed)]
  parse_elapsed <- proc.time()[["elapsed"]] - parse_timer

  has_pc <- parsed[!is.na(in_postcode)]
  no_pc  <- parsed[is.na(in_postcode)]

  results <- list()
  diagnostics <- list()

  # ------------------------------------------------------------------
  # Path 1: postcode-blocked matching
  # ------------------------------------------------------------------
  if (nrow(has_pc) > 0L) {
    pcs <- unique(has_pc$in_postcode)
    postcode_word <- if (length(pcs) == 1L) "postcode" else "postcodes"
    .cli_match_step(
      verbose,
      sprintf(
        "Fetching postcode candidates for %s unique %s.",
        cli::col_cyan(format(length(pcs), big.mark = ",")),
        postcode_word
      )
    )
    nfs   <- unique(na.omit(has_pc$in_number_first))
    cands <- .fetch_by_postcode(
      con, pcs, include_custom,
      # Targeted fetch when every input has a parseable street number; falls back
      # to a full postcode fetch for batches that include numberless addresses.
      number_firsts = if (!anyNA(has_pc$in_number_first)) nfs else NULL
    )
    candidate_word <- if (nrow(cands) == 1L) "row" else "rows"
    .cli_match_detail(
      verbose,
      sprintf(
        "Postcode path loaded %s candidate %s.",
        cli::col_green(format(nrow(cands), big.mark = ",")),
        candidate_word
      )
    )

    if (nrow(cands) > 0L) {
      postcode_path <- .match_postcode(
        has_pc, cands, max_results, min_score, weights = weights, diagnostics = TRUE
      )
      results[["postcode"]] <- postcode_path$matches
      diagnostics[["postcode"]] <- postcode_path$diagnostics
    }
  }

  # ------------------------------------------------------------------
  # Path 2: no-postcode inputs — try state-level fallback
  # ------------------------------------------------------------------
  if (nrow(no_pc) > 0L) {
    input_word <- if (nrow(no_pc) == 1L) "row" else "rows"
    .cli_match_step(
      verbose,
      sprintf(
        "Attempting state fallback for %s input %s without a postcode.",
        cli::col_yellow(format(nrow(no_pc), big.mark = ",")),
        input_word
      )
    )
    state_path <- .match_state(
      con, no_pc, max_results, min_score, include_custom, weights = weights,
      verbose = verbose
    )
    results[["no_postcode"]] <- state_path$matches
    diagnostics[["no_postcode"]] <- state_path$diagnostics
  }

  # ------------------------------------------------------------------
  # Path 3: locality fallback for weak / wrong-postcode results
  # ------------------------------------------------------------------
  if (locality_fallback) {
    pc_res <- results[["postcode"]]

    # Best score per input from path 1
    best_by_input <- if (!is.null(pc_res) && nrow(pc_res) > 0L) {
      pc_res[, .(best_score = max(total_score)), by = input_id]
    } else {
      data.table(input_id = integer(0), best_score = integer(0))
    }

    # Trigger fallback for: (a) weak postcode match, (b) no postcode match,
    # (c) no_pc inputs that have a locality name to search on
    weak_ids      <- best_by_input[best_score < fallback_threshold, input_id]
    matched_ids   <- best_by_input$input_id
    unmatched_ids <- has_pc[!input_id %in% matched_ids, input_id]
    no_pc_loc_ids <- no_pc[!is.na(in_locality), input_id]

    fallback_ids   <- unique(c(weak_ids, unmatched_ids, no_pc_loc_ids))
    fallback_parse <- parsed[input_id %in% fallback_ids & !is.na(in_locality)]

    if (nrow(fallback_parse) > 0L) {
      fallback_word <- if (nrow(fallback_parse) == 1L) "row" else "rows"
      .cli_match_step(
        verbose,
        sprintf(
          "Running locality fallback for %s input %s.",
          cli::col_magenta(format(nrow(fallback_parse), big.mark = ",")),
          fallback_word
        )
      )
      locality_path <- .match_locality_fallback(
        con, fallback_parse, max_results, min_score, include_custom,
        weights = weights,
        verbose = verbose
      )
      results[["locality"]] <- locality_path$matches
      diagnostics[["locality"]] <- locality_path$diagnostics
    }
  }

  # ------------------------------------------------------------------
  # Combine paths, deduplicate, re-rank
  # ------------------------------------------------------------------
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
  .cli_match_summary(verbose, parsed, out, total_timer, parse_elapsed)
  out[]
}

# ---------------------------------------------------------------------------
# Locality fallback: discover correct postcodes via DuckDB jaro_winkler_similarity
# ---------------------------------------------------------------------------

.match_locality_fallback <- function(con, parsed_sub, max_results, min_score,
                                     include_custom, weights, verbose = FALSE) {
  locs   <- parsed_sub[!is.na(in_locality), unique(in_locality)]
  states <- parsed_sub[!is.na(in_state),    unique(in_state)]
  if (length(locs) == 0L) return(.empty_path_result())

  locs_esc <- gsub("'", "''", locs)
  state_clause <- if (length(states) > 0L)
    sprintf("AND g.state IN (%s)", paste0("'", states, "'", collapse = ","))
  else ""

  # DuckDB UNNEST to create a row per query locality, then cross-join with the
  # compact locality index (~3 000 rows for QLD vs the full address table).
  sql <- sprintf("
    SELECT DISTINCT locs.q AS in_locality, g.postcode
    FROM gnaf_locality_index g
    CROSS JOIN (SELECT unnest([%s]) AS q) locs
    WHERE jaro_winkler_similarity(g.locality_name, locs.q) >= 0.85
    %s
  ", paste0("'", locs_esc, "'", collapse = ","), state_clause)

  loc_map <- tryCatch(
    setDT(DBI::dbGetQuery(con, sql)),
    error = function(e) {
      .cli_match_alert(
        verbose,
        "warning",
        "Locality fallback query failed: {conditionMessage(e)}"
      )
      NULL
    }
  )
  if (is.null(loc_map) || nrow(loc_map) == 0L) return(.empty_path_result())

  # For each input expand to all candidate postcodes found for its locality
  # loc_map: (in_locality, postcode)  ×  parsed_sub: (..., in_locality, ...)
  i_expanded <- loc_map[parsed_sub, on = "in_locality", allow.cartesian = TRUE, nomatch = 0L]
  # After join: in_locality (key), postcode (alt from loc_map), + all in_* from parsed_sub
  if (nrow(i_expanded) == 0L) return(.empty_path_result())

  # Fetch GNAF candidates for all discovered alt postcodes, targeted by number
  alt_pcs  <- unique(i_expanded$postcode)
  nfs_loc  <- unique(na.omit(i_expanded$in_number_first))
  cands    <- .fetch_by_postcode(
    con, alt_pcs, include_custom,
    number_firsts = if (!anyNA(i_expanded$in_number_first)) nfs_loc else NULL
  )
  if (nrow(cands) == 0L) return(.empty_path_result())

  # Tight join on (alt postcode, number_first)
  # i_expanded already carries `postcode` (the alt postcode) as a regular column,
  # which serves directly as the join key against cands$postcode.
  i_tight <- i_expanded[!is.na(in_number_first)]

  tight <- if (nrow(i_tight) > 0L) {
    r <- cands[i_tight,
               on = c("postcode", "number_first" = "in_number_first"),
               allow.cartesian = TRUE, nomatch = 0L]
    r[, in_number_first := number_first]  # restore consumed join key
    r
  } else cands[0L]

  range_cands <- cands[!is.na(number_last)]
  if (nrow(range_cands) > 0L && nrow(i_tight) > 0L) {
    rj <- range_cands[i_tight, on = "postcode", allow.cartesian = TRUE, nomatch = 0L]
    rj <- rj[in_number_first > number_first & in_number_first <= number_last]
    if (nrow(rj) > 0L) tight <- rbindlist(list(tight, rj), fill = TRUE, use.names = TRUE)
  }

  tight_ids   <- if (nrow(tight) > 0L) unique(tight$input_id) else integer(0L)
  i_unmatched <- i_expanded[!input_id %in% tight_ids]

  broad <- if (nrow(i_unmatched) > 0L) {
    cands[i_unmatched, on = "postcode", allow.cartesian = TRUE, nomatch = 0L]
  } else cands[0L]

  all_pairs <- rbindlist(list(tight, broad), fill = TRUE, use.names = TRUE)
  if (nrow(all_pairs) == 0L) return(.empty_path_result())

  all_pairs <- .score_pairs(all_pairs, weights = weights)
  diagnostic_dt <- .build_path_diagnostics(all_pairs, min_score)
  all_pairs <- all_pairs[total_score >= min_score]
  if (nrow(all_pairs) == 0L) {
    return(list(matches = NULL, diagnostics = diagnostic_dt))
  }

  setorder(all_pairs, input_id, -total_score)
  list(
    matches = all_pairs[, .SD[seq_len(min(.N, max_results))], by = input_id],
    diagnostics = diagnostic_dt
  )
}

# ---------------------------------------------------------------------------
# Existing internal helpers (unchanged)
# ---------------------------------------------------------------------------

.fetch_by_postcode <- function(con, postcodes, include_custom, number_firsts = NULL) {
  pc_csv <- paste(postcodes, collapse = ",")
  where <- if (!is.null(number_firsts) && length(number_firsts) > 0L) {
    nf_csv <- paste(number_firsts, collapse = ",")
    # Fetch only rows whose number_first matches a query number, plus all range
    # records (number_last IS NOT NULL) which are needed for the range join.
    # This avoids pulling every alias variant for every address in the postcode.
    sprintf(
      "postcode IN (%s) AND (number_first IN (%s) OR number_last IS NOT NULL)",
      pc_csv, nf_csv
    )
  } else {
    sprintf("postcode IN (%s)", pc_csv)
  }
  .fetch_sql(con, include_custom, where)
}

.fetch_by_state <- function(con, states, include_custom) {
  st_csv <- paste0("'", states, "'", collapse = ",")
  .fetch_sql(con, include_custom, sprintf("state IN (%s)", st_csv))
}

.fetch_sql <- function(con, include_custom, where_clause) {
  sel <- "address_detail_pid, address_label, building_name,
          flat_type, flat_number, number_first, number_last,
          street_name, street_type, street_suffix, locality_name,
          state, postcode, longitude, latitude, source, alias_type"
  sql <- sprintf("SELECT %s FROM gnaf_addresses WHERE %s", sel, where_clause)
  dt  <- setDT(DBI::dbGetQuery(con, sql))

  if (include_custom && DBI::dbExistsTable(con, "custom_addresses")) {
    sql2 <- sprintf("SELECT %s FROM custom_addresses WHERE %s", sel, where_clause)
    dt   <- rbindlist(list(dt, setDT(DBI::dbGetQuery(con, sql2))), fill = TRUE)
  }
  dt
}

.prep_i <- function(parsed_sub) {
  dt <- copy(parsed_sub)
  dt[, postcode := in_postcode]
  dt
}

.match_postcode <- function(parsed_pc, cands, max_results, min_score,
                            weights, diagnostics = FALSE) {
  i_all <- .prep_i(parsed_pc)

  i_tight <- i_all[!is.na(in_number_first)]
  tight <- if (nrow(i_tight) > 0L) {
    r <- cands[i_tight,
               on = c("postcode", "number_first" = "in_number_first"),
               allow.cartesian = TRUE, nomatch = 0L]
    r[, in_number_first := number_first]
    r
  } else cands[0L]

  # Range join: catch GNAF range records (e.g. 110-120 MUSGRAVE) where
  # in_number_first falls strictly inside the range [number_first, number_last].
  # The strict > avoids re-adding records already captured by the exact tight join.
  range_cands <- cands[!is.na(number_last)]
  if (nrow(range_cands) > 0L && nrow(i_tight) > 0L) {
    rj <- range_cands[i_tight, on = "postcode", allow.cartesian = TRUE, nomatch = 0L]
    rj <- rj[in_number_first > number_first & in_number_first <= number_last]
    if (nrow(rj) > 0L) tight <- rbindlist(list(tight, rj), fill = TRUE, use.names = TRUE)
  }

  tight_ids   <- if (nrow(tight) > 0L) unique(tight$input_id) else integer(0L)
  i_unmatched <- i_all[!input_id %in% tight_ids]

  broad <- if (nrow(i_unmatched) > 0L) {
    cands[i_unmatched, on = "postcode", allow.cartesian = TRUE, nomatch = 0L]
  } else cands[0L]

  all_pairs <- rbindlist(list(tight, broad), fill = TRUE, use.names = TRUE)
  if (nrow(all_pairs) == 0L) {
    if (isTRUE(diagnostics)) return(.empty_path_result())
    return(NULL)
  }

  all_pairs <- .score_pairs(all_pairs, weights = weights)
  diagnostic_dt <- if (isTRUE(diagnostics)) .build_path_diagnostics(all_pairs, min_score) else NULL
  all_pairs <- all_pairs[total_score >= min_score]
  if (nrow(all_pairs) == 0L) {
    if (isTRUE(diagnostics)) return(list(matches = NULL, diagnostics = diagnostic_dt))
    return(NULL)
  }

  setorder(all_pairs, input_id, -total_score)
  matched <- all_pairs[, .SD[seq_len(min(.N, max_results))], by = input_id]
  if (isTRUE(diagnostics)) {
    return(list(matches = matched, diagnostics = diagnostic_dt))
  }
  matched
}

.match_state <- function(con, no_pc, max_results, min_score, include_custom,
                         weights, verbose = FALSE) {
  states <- no_pc[!is.na(in_state), unique(in_state)]
  if (length(states) == 0L) {
    .cli_match_alert(verbose, "warning", "No postcode or state found; skipping these inputs.")
    return(.empty_path_result())
  }

  cands <- .fetch_by_state(con, states, include_custom)
  if (nrow(cands) == 0L) return(.empty_path_result())

  i_all <- .prep_i(no_pc[!is.na(in_state)])
  i_all[, state := in_state]

  pairs <- cands[i_all, on = "state", allow.cartesian = TRUE, nomatch = 0L]
  if (nrow(pairs) == 0L) return(.empty_path_result())

  pairs <- .score_pairs(pairs, weights = weights)
  diagnostic_dt <- .build_path_diagnostics(pairs, min_score)
  pairs <- pairs[total_score >= min_score]
  if (nrow(pairs) == 0L) return(list(matches = NULL, diagnostics = diagnostic_dt))

  setorder(pairs, input_id, -total_score)
  list(
    matches = pairs[, .SD[seq_len(min(.N, max_results))], by = input_id],
    diagnostics = diagnostic_dt
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

.build_path_diagnostics <- function(scored_pairs, min_score) {
  if (nrow(scored_pairs) == 0L) return(NULL)
  cc <- scored_pairs[, .(candidate_count = .N), by = input_id]
  rc <- scored_pairs[total_score >= min_score,
                     .(retained_count = .N, best_score = max(total_score)),
                     by = input_id]
  cc[rc, on = "input_id"]
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

.cli_match_summary <- function(verbose, parsed, out, total_timer, parse_elapsed) {
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
  if (!is.na(avg_best)) {
    cli::cli_li(sprintf("Average top-match score: %s.", cli::col_magenta(sprintf("%.1f", avg_best))))
  }
  cli::cli_text(
    sprintf(
      "Timings: parse %s, total %s.",
      cli::col_cyan(sprintf("%.2fs", parse_elapsed)),
      cli::col_cyan(sprintf("%.2fs", total_elapsed))
    )
  )
}
