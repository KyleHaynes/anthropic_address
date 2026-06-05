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
#' @return A \code{data.table} ordered by \code{input_id} then descending
#'   \code{total_score}.
#' @export
gnaf_match <- function(con, addresses, max_results = 3L, min_score = 40L,
                       include_custom = TRUE,
                       locality_fallback = TRUE,
                       fallback_threshold = 80L) {

  if (!is.character(addresses) || length(addresses) == 0L)
    stop("'addresses' must be a non-empty character vector")

  message(sprintf("Parsing %s addresses ...", format(length(addresses), big.mark = ",")))
  parsed <- address_parse(addresses)

  has_pc <- parsed[!is.na(in_postcode)]
  no_pc  <- parsed[is.na(in_postcode)]

  results <- list()

  # ------------------------------------------------------------------
  # Path 1: postcode-blocked matching
  # ------------------------------------------------------------------
  if (nrow(has_pc) > 0L) {
    pcs <- unique(has_pc$in_postcode)
    message(sprintf("Fetching GNAF candidates for %d unique postcode(s) ...", length(pcs)))
    cands <- .fetch_by_postcode(con, pcs, include_custom)

    if (nrow(cands) > 0L)
      results[["postcode"]] <- .match_postcode(has_pc, cands, max_results, min_score)
  }

  # ------------------------------------------------------------------
  # Path 2: no-postcode inputs — try state-level fallback
  # ------------------------------------------------------------------
  if (nrow(no_pc) > 0L) {
    message(sprintf(
      "%d input(s) have no postcode; attempting state-level fallback ...", nrow(no_pc)
    ))
    results[["no_postcode"]] <- .match_state(con, no_pc, max_results, min_score, include_custom)
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
      message(sprintf("Running locality fallback for %d input(s) ...", nrow(fallback_parse)))
      results[["locality"]] <- .match_locality_fallback(
        con, fallback_parse, max_results, min_score, include_custom
      )
    }
  }

  # ------------------------------------------------------------------
  # Combine paths, deduplicate, re-rank
  # ------------------------------------------------------------------
  out <- rbindlist(results, fill = TRUE, use.names = TRUE)
  if (nrow(out) == 0L) return(.empty_result())

  # Deduplicate: same GNAF record may appear from multiple paths; keep higher score
  setorder(out, input_id, -total_score)
  out <- unique(out, by = c("input_id", "address_detail_pid"))

  # Re-apply max_results and assign final rank
  setorder(out, input_id, -total_score)
  out <- out[, .SD[seq_len(min(.N, max_results))], by = input_id]
  out[, match_rank := seq_len(.N), by = input_id]

  cols_first <- c("input_id", "input_raw", "match_rank", "total_score",
                  "score_postcode", "score_suburb", "score_street_name",
                  "score_street_type", "score_number", "score_flat")
  setcolorder(out, c(cols_first, setdiff(names(out), cols_first)))
  out[]
}

# ---------------------------------------------------------------------------
# Locality fallback: discover correct postcodes via DuckDB jaro_winkler_similarity
# ---------------------------------------------------------------------------

.match_locality_fallback <- function(con, parsed_sub, max_results, min_score, include_custom) {
  locs   <- parsed_sub[!is.na(in_locality), unique(in_locality)]
  states <- parsed_sub[!is.na(in_state),    unique(in_state)]
  if (length(locs) == 0L) return(NULL)

  locs_esc <- gsub("'", "''", locs)
  state_clause <- if (length(states) > 0L)
    sprintf("AND g.state IN (%s)", paste0("'", states, "'", collapse = ","))
  else ""

  # DuckDB UNNEST to create a row per query locality, then cross-join with GNAF
  sql <- sprintf("
    SELECT DISTINCT locs.q AS in_locality, g.postcode
    FROM gnaf_addresses g
    CROSS JOIN (SELECT unnest([%s]) AS q) locs
    WHERE jaro_winkler_similarity(g.locality_name, locs.q) >= 0.85
    %s
  ", paste0("'", locs_esc, "'", collapse = ","), state_clause)

  loc_map <- tryCatch(
    setDT(DBI::dbGetQuery(con, sql)),
    error = function(e) {
      message("Locality fallback query failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(loc_map) || nrow(loc_map) == 0L) return(NULL)

  # For each input expand to all candidate postcodes found for its locality
  # loc_map: (in_locality, postcode)  ×  parsed_sub: (..., in_locality, ...)
  i_expanded <- loc_map[parsed_sub, on = "in_locality", allow.cartesian = TRUE, nomatch = 0L]
  # After join: in_locality (key), postcode (alt from loc_map), + all in_* from parsed_sub
  if (nrow(i_expanded) == 0L) return(NULL)

  # Fetch GNAF candidates for all discovered alt postcodes
  alt_pcs <- unique(i_expanded$postcode)
  cands   <- .fetch_by_postcode(con, alt_pcs, include_custom)
  if (nrow(cands) == 0L) return(NULL)

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

  tight_ids   <- if (nrow(tight) > 0L) unique(tight$input_id) else integer(0L)
  i_unmatched <- i_expanded[!input_id %in% tight_ids]

  broad <- if (nrow(i_unmatched) > 0L) {
    cands[i_unmatched, on = "postcode", allow.cartesian = TRUE, nomatch = 0L]
  } else cands[0L]

  all_pairs <- rbindlist(list(tight, broad), fill = TRUE, use.names = TRUE)
  if (nrow(all_pairs) == 0L) return(NULL)

  all_pairs <- .score_pairs(all_pairs)
  all_pairs <- all_pairs[total_score >= min_score]
  if (nrow(all_pairs) == 0L) return(NULL)

  setorder(all_pairs, input_id, -total_score)
  all_pairs[, .SD[seq_len(min(.N, max_results))], by = input_id]
}

# ---------------------------------------------------------------------------
# Existing internal helpers (unchanged)
# ---------------------------------------------------------------------------

.fetch_by_postcode <- function(con, postcodes, include_custom) {
  pc_csv <- paste(postcodes, collapse = ",")
  .fetch_sql(con, include_custom, sprintf("postcode IN (%s)", pc_csv))
}

.fetch_by_state <- function(con, states, include_custom) {
  st_csv <- paste0("'", states, "'", collapse = ",")
  .fetch_sql(con, include_custom, sprintf("state IN (%s)", st_csv))
}

.fetch_sql <- function(con, include_custom, where_clause) {
  sel <- "address_detail_pid, address_label, building_name,
          flat_type, flat_number, number_first, number_last,
          street_name, street_type, street_suffix, locality_name,
          state, postcode, longitude, latitude, source"
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

.match_postcode <- function(parsed_pc, cands, max_results, min_score) {
  i_all <- .prep_i(parsed_pc)

  i_tight <- i_all[!is.na(in_number_first)]
  tight <- if (nrow(i_tight) > 0L) {
    r <- cands[i_tight,
               on = c("postcode", "number_first" = "in_number_first"),
               allow.cartesian = TRUE, nomatch = 0L]
    r[, in_number_first := number_first]
    r
  } else cands[0L]

  tight_ids   <- if (nrow(tight) > 0L) unique(tight$input_id) else integer(0L)
  i_unmatched <- i_all[!input_id %in% tight_ids]

  broad <- if (nrow(i_unmatched) > 0L) {
    cands[i_unmatched, on = "postcode", allow.cartesian = TRUE, nomatch = 0L]
  } else cands[0L]

  all_pairs <- rbindlist(list(tight, broad), fill = TRUE, use.names = TRUE)
  if (nrow(all_pairs) == 0L) return(NULL)

  all_pairs <- .score_pairs(all_pairs)
  all_pairs <- all_pairs[total_score >= min_score]
  if (nrow(all_pairs) == 0L) return(NULL)

  setorder(all_pairs, input_id, -total_score)
  all_pairs[, .SD[seq_len(min(.N, max_results))], by = input_id]
}

.match_state <- function(con, no_pc, max_results, min_score, include_custom) {
  states <- no_pc[!is.na(in_state), unique(in_state)]
  if (length(states) == 0L) {
    message("No postcode or state found; skipping these inputs.")
    return(NULL)
  }

  cands <- .fetch_by_state(con, states, include_custom)
  if (nrow(cands) == 0L) return(NULL)

  i_all <- .prep_i(no_pc[!is.na(in_state)])
  i_all[, state := in_state]

  pairs <- cands[i_all, on = "state", allow.cartesian = TRUE, nomatch = 0L]
  if (nrow(pairs) == 0L) return(NULL)

  pairs <- .score_pairs(pairs)
  pairs <- pairs[total_score >= min_score]
  if (nrow(pairs) == 0L) return(NULL)

  setorder(pairs, input_id, -total_score)
  pairs[, .SD[seq_len(min(.N, max_results))], by = input_id]
}

.empty_result <- function() {
  data.table(
    input_id = integer(), input_raw = character(), match_rank = integer(),
    total_score = integer(),
    score_postcode = integer(), score_suburb = integer(),
    score_street_name = integer(), score_street_type = integer(),
    score_number = integer(), score_flat = integer(),
    address_detail_pid = character(), address_label = character(),
    flat_type = character(), flat_number = character(),
    number_first = integer(), number_last = integer(),
    street_name = character(), street_type = character(),
    locality_name = character(), state = character(),
    postcode = integer(), longitude = numeric(), latitude = numeric(),
    source = character()
  )
}
