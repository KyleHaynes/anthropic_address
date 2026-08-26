# Attached single-letter flat designators ("F8", "A6", "U110"); any other
# letter defaults to UNIT at the use sites.
.ATT_FLAT_MAP <- c(U = "UNIT", F = "FLAT", A = "APARTMENT")

# Street-type dictionary words that are also common locality-name words (e.g.
# "Red HILL", "Bushland PARK"). When one of these is the rightmost apparent
# street-type match, it is frequently the suburb rather than the real street
# type; both the vectorized fast path and the scalar fallback parser search
# for an earlier, unambiguous street-type token before trusting it.
.LOCALITY_COLLISION_WORDS <- c(
  "ST", "NORTH", "NTH", "SOUTH", "STH", "EAST", "WEST", "HILL",
  "HILLS", "HEIGHTS", "BAY", "BEACH", "ISLAND", "PARK", "POINT",
  "PORT", "VALLEY"
)

# Abbreviation-expansion tables for .expand_abbreviations. Patterns are applied
# sequentially (vectorize_all = FALSE), preserving the original gsub order:
# MNT/MT -> MOUNT; ST -> SAINT (safe after street_type is stripped); NTH/STH
# and leading N/S/E/W -> compass words; CK -> CREEK; & -> AND.
.EXPAND_PATTERNS <- c(
  "\\bMNT\\b", "\\bMT\\b", "\\bST\\b", "\\bNTH\\b", "\\bSTH\\b",
  "^N\\b", "^S\\b", "^E\\b", "^W\\b", "\\bCK\\b", "\\s+&\\s+"
)
.EXPAND_REPLACEMENTS <- c(
  "MOUNT", "MOUNT", "SAINT", "NORTH", "SOUTH",
  "NORTH", "SOUTH", "EAST", "WEST", "CREEK", " AND "
)

.ORDINAL_PATTERNS <- sprintf("\\b%s\\b", c(
  "1ST", "2ND", "3RD", "4TH", "5TH", "6TH", "7TH", "8TH", "9TH", "10TH",
  "11TH", "12TH", "13TH", "14TH", "15TH", "16TH", "17TH", "18TH", "19TH", "20TH"
))
.ORDINAL_REPLACEMENTS <- c(
  "FIRST", "SECOND", "THIRD", "FOURTH", "FIFTH", "SIXTH", "SEVENTH",
  "EIGHTH", "NINTH", "TENTH", "ELEVENTH", "TWELFTH", "THIRTEENTH",
  "FOURTEENTH", "FIFTEENTH", "SIXTEENTH", "SEVENTEENTH", "EIGHTEENTH",
  "NINETEENTH", "TWENTIETH"
)

# Expands common abbreviations in in_locality and in_street_name after the
# structured fields (street type etc.) have already been extracted.  Runs only
# when normalize = TRUE in address_parse / gnaf_match.
.expand_abbreviations <- function(dt) {
  exp_common <- function(x) {
    fast.string::gsub_all(.EXPAND_PATTERNS, .EXPAND_REPLACEMENTS, x,
                          sequential = TRUE)
  }
  # Locality names such as ST LUCIA use ST literally in G-NAF; only street
  # names use the ST -> SAINT expansion.
  dt[, in_locality := fast.string::gsub_all(
    .EXPAND_PATTERNS[-3L], .EXPAND_REPLACEMENTS[-3L], in_locality,
    sequential = TRUE
  )]
  dt[, in_street_name := fast.string::gsub_all(
    .ORDINAL_PATTERNS, .ORDINAL_REPLACEMENTS, exp_common(in_street_name),
    sequential = TRUE
  )]
  dt
}

#' Parse Australian addresses into G-NAF components
#'
#' Parses each unique normalised structural input once and expands results back
#' to the original order. Comma boundaries are retained while the street and
#' locality sections are classified.
#'
#' @param addresses Character vector of raw address strings.
#' @param normalize Scalar logical. If \code{TRUE}, common street-name and
#'   locality abbreviations are expanded after parsing.
#' @return A \code{data.table} with one row per input. Existing parser columns
#'   are retained and \code{in_level_type}, \code{in_level_number}, and
#'   \code{in_lot_number} describe independent G-NAF sub-address components.
#' @export
address_parse <- function(addresses, normalize = TRUE) {
  if (!is.character(addresses)) {
    stop("'addresses' must be a character vector", call. = FALSE)
  }
  if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
    stop("'normalize' must be TRUE or FALSE", call. = FALSE)
  }
  if (length(addresses) == 0L) return(.empty_parse_result())

  resources <- .get_parser_resources()
  structural <- .normalize_addr_keep_commas(addresses)
  unique_idx <- !duplicated(structural)
  unique_structural <- structural[unique_idx]
  boundaries <- .parse_comma_boundaries(unique_structural, resources)
  normalized <- fast.string::fgsub(",", " ", unique_structural, fixed = TRUE)
  normalized <- fast.string::ftrimws(fast.string::fgsub("\\s+", " ", normalized))

  parsed <- .parse_vectorized(
    normalized, addresses[unique_idx], resources$st_map, resources$st_regex,
    resources$ft_map, resources$ft_re, resources$ft_alt, boundaries,
    resources$level_map, resources$level_alt
  )
  if (normalize) parsed <- .expand_abbreviations(parsed)

  dt <- parsed[match(structural, unique_structural)]
  dt[, `:=`(input_id = seq_along(addresses), input_raw = addresses)]
  setcolorder(dt, c("input_id", "input_raw",
                    "in_postcode", "in_state", "in_locality",
                    "in_street_name", "in_street_type", "in_street_suffix",
                    "in_number_first", "in_number_last", "in_number_suffix",
                    "in_flat_type", "in_flat_number", "in_level_type",
                    "in_level_number", "in_lot_number", "in_building_name"))
  dt
}

.empty_parse_result <- function() {
  data.table(
    input_id = integer(), input_raw = character(), in_postcode = integer(),
    in_state = character(), in_locality = character(),
    in_street_name = character(), in_street_type = character(),
    in_street_suffix = character(), in_number_first = integer(),
    in_number_last = integer(), in_number_suffix = character(),
    in_flat_type = character(), in_flat_number = character(),
    in_level_type = character(), in_level_number = character(),
    in_lot_number = character(), in_building_name = character()
  )
}

.parse_comma_boundaries <- function(x, resources) {
  n <- length(x)
  has_comma <- !is.na(x) & fast.string::fgrepl(",", x, fixed = TRUE)
  left <- right <- rep(NA_character_, n)
  if (any(has_comma)) {
    idx <- which(has_comma)
    left[idx] <- fast.string::ftrimws(stringi::stri_replace_last_regex(
      x[idx], ",[^,]*$", ""
    ))
    left[idx] <- fast.string::ftrimws(fast.string::fgsub(
      "\\s*,\\s*", " ", left[idx]
    ))
    right[idx] <- fast.string::ftrimws(stringi::stri_match_last_regex(
      x[idx], ",([^,]*)$"
    )[, 2L])
  }
  locality <- right
  locality <- fast.string::ftrimws(fast.string::fsub(
    "(?:\\s+|^)(?:QLD|NSW|VIC|SA|WA|TAS|NT|ACT)\\s+\\d{4}\\s*$", "", locality
  ))
  locality <- fast.string::ftrimws(fast.string::fsub(
    "(?:\\s+|^)\\d{4}\\s+(?:QLD|NSW|VIC|SA|WA|TAS|NT|ACT)\\s*$", "", locality
  ))
  locality <- fast.string::ftrimws(fast.string::fsub(
    "(?:\\s+|^)(?:QLD|NSW|VIC|SA|WA|TAS|NT|ACT|\\d{4})\\s*$", "", locality
  ))
  marker_re <- paste0(
    "^(?:\\d|LOT\\b|(?:", resources$ft_alt, "|", resources$level_alt,
    ")\\s+[A-Z0-9])"
  )
  meaningful <- has_comma & !is.na(left) & nzchar(left) &
    !is.na(locality) & nzchar(locality) &
    !fast.string::fgrepl(marker_re, right)

  resolved <- .resolve_boundary_street_types(left, meaningful, resources)
  list(
    meaningful = meaningful, street = left, locality = locality,
    type_start = resolved$start, type_end = resolved$end,
    type = resolved$canonical
  )
}

.resolve_boundary_street_types <- function(street, meaningful, resources) {
  n <- length(street)
  start <- end <- rep(NA_integer_, n)
  canonical <- rep(NA_character_, n)
  type_less <- rep(FALSE, n)
  idx <- which(meaningful)
  if (length(idx) == 0L) {
    return(list(start = start, end = end, canonical = canonical))
  }

  base <- fast.string::ftrimws(fast.string::fsub(
    "\\s+(?:NORTH|SOUTH|EAST|WEST|UPPER|LOWER|INNER|OUTER)$", "", street[idx]
  ))
  loc <- stringi::stri_locate_last_regex(base, resources$st_regex)
  exact <- !is.na(loc[, 1L]) & loc[, 2L] == nchar(base)
  if (any(exact)) {
    take_exact <- which(exact)
    raw <- fast.string::fsubstr(base[exact], loc[exact, 1L], loc[exact, 2L])
    prefix <- fast.string::ftrimws(fast.string::fsubstr(
      base[exact], 1L, pmax(0L, loc[exact, 1L] - 1L)
    ))
    sole_name <- fast.string::fgrepl(
      "\\b\\d+[A-Z]?(?:-\\d+[A-Z]?)?$", prefix
    ) |
      fast.string::fgrepl("\\bTHE$", prefix)
    type_less[idx[take_exact[sole_name]]] <- TRUE
    take <- take_exact[!sole_name]
    if (length(take) > 0L) {
      raw_take <- raw[!sole_name]
      out_idx <- idx[take]
      start[out_idx] <- loc[take, 1L]
      end[out_idx] <- loc[take, 2L]
      canonical[out_idx] <- unname(resources$st_map[raw_take])
    }
  }

  unresolved <- which(is.na(canonical[idx]) & !type_less[idx])
  if (length(unresolved) > 0L) {
    base_u <- base[unresolved]
    token <- stringi::stri_extract_last_regex(base_u, "[A-Z][A-Z-]*$")
    type_less_name <- fast.string::fgrepl(
      "^\\d+[A-Z]?(?:-\\d+[A-Z]?)?\\s+(?:THE\\s+)?[A-Z-]+$", base_u
    )
    can_fuzzy <- !is.na(token) & nchar(token) >= 3L & !type_less_name
    if (any(can_fuzzy)) {
      tokens <- unique(token[can_fuzzy])
      keys <- names(resources$st_map)
      values <- vapply(tokens, function(value) {
        sims <- fast.string::jaro_winkler_matrix(value, keys, p = 0.1)[1L, ]
        j <- which.max(sims)
        threshold <- if (nchar(value) <= 3L) 0.90 else 0.86
        if (sims[[j]] >= threshold) unname(resources$st_map[[keys[[j]]]]) else NA_character_
      }, character(1L))
      names(values) <- tokens
      fuzzy_pos <- which(can_fuzzy & !is.na(values[token]))
      if (length(fuzzy_pos) > 0L) {
        out_idx <- idx[unresolved[fuzzy_pos]]
        canonical[out_idx] <- unname(values[token[fuzzy_pos]])
        end[out_idx] <- nchar(base_u[fuzzy_pos])
        start[out_idx] <- end[out_idx] - nchar(token[fuzzy_pos]) + 1L
      }
    }
  }
  list(start = start, end = end, canonical = canonical)
}

.resolve_fuzzy_street_types_vec <- function(text, rows, st_map) {
  out <- list(
    rows = integer(), start = integer(), end = integer(),
    canonical = character()
  )
  if (length(rows) == 0L) return(out)

  words <- strsplit(text[rows], "\\s+", perl = TRUE)
  eligible <- lapply(words, function(value) {
    which(!fast.string::fgrepl("^[0-9]", value) & nchar(value) >= 3L)
  })
  tokens <- unique(unlist(Map(function(value, pos) value[pos], words, eligible),
                          use.names = FALSE))
  if (length(tokens) == 0L) return(out)
  lookup <- .fuzzy_type_lookup(tokens, st_map)

  picked <- lapply(seq_along(words), function(j) {
    pos <- eligible[[j]]
    if (length(pos) == 0L) return(NULL)
    token <- words[[j]][pos]
    sim <- lookup$similarity[match(token, lookup$token)]
    thresholds <- ifelse(
      pos == length(words[[j]]), 0.92,
      ifelse(nchar(token) <= 3L, 0.90, 0.86)
    )
    sim[sim < thresholds] <- NA_real_
    if (all(is.na(sim))) return(NULL)
    best <- utils::tail(which(sim == max(sim, na.rm = TRUE)), 1L)
    word_pos <- pos[[best]]
    start <- if (word_pos == 1L) 1L else
      sum(nchar(words[[j]][seq_len(word_pos - 1L)])) + word_pos
    list(
      row = rows[[j]], start = start,
      end = start + nchar(token[[best]]) - 1L,
      canonical = lookup$canonical[match(token[[best]], lookup$token)]
    )
  })
  picked <- Filter(Negate(is.null), picked)
  if (length(picked) == 0L) return(out)
  list(
    rows = vapply(picked, `[[`, integer(1L), "row"),
    start = vapply(picked, `[[`, integer(1L), "start"),
    end = vapply(picked, `[[`, integer(1L), "end"),
    canonical = vapply(picked, `[[`, character(1L), "canonical")
  )
}

.fuzzy_type_lookup <- function(tokens, st_map) {
  if (is.null(.gnafr_env$fuzzy_type_cache)) {
    .gnafr_env$fuzzy_type_cache <- new.env(hash = TRUE, parent = emptyenv())
  }
  cache <- .gnafr_env$fuzzy_type_cache
  missing <- tokens[!vapply(tokens, exists, logical(1L), envir = cache,
                            inherits = FALSE)]
  if (length(missing) > 0L) {
    keys <- names(st_map)
    chunks <- split(missing, ceiling(seq_along(missing) / 1000L))
    for (chunk in chunks) {
      similarity <- fast.string::jaro_winkler_matrix(chunk, keys, p = 0.1)
      best <- max.col(similarity, ties.method = "first")
      for (j in seq_along(chunk)) {
        key <- keys[[best[[j]]]]
        assign(chunk[[j]], list(
          similarity = similarity[j, best[[j]]],
          canonical = unname(st_map[[key]])
        ), envir = cache)
      }
    }
  }
  values <- lapply(tokens, get, envir = cache, inherits = FALSE)
  data.table(
    token = tokens,
    similarity = vapply(values, `[[`, numeric(1L), "similarity"),
    canonical = vapply(values, `[[`, character(1L), "canonical")
  )
}

#' Parse a vector of address strings into structured components
#'
#' Handles common Australian address formats including unit/flat prefixes,
#' slash notation (110/120), attached prefixes (U110), building names, and
#' street number ranges (13-27).
#'
#' @param addresses Character vector of raw address strings.
#' @param normalize If \code{TRUE} (the default), common abbreviations in
#'   \code{in_locality} and \code{in_street_name} are expanded to their GNAF
#'   canonical forms after parsing: \code{MT}/\code{MNT} \eqn{\to}
#'   \code{MOUNT}; \code{ST} \eqn{\to} \code{SAINT}; \code{NTH}/\code{STH}
#'   and leading \code{N}/\code{S}/\code{E}/\code{W} \eqn{\to} full compass
#'   words; \code{CK} \eqn{\to} \code{CREEK}; \code{\&} \eqn{\to}
#'   \code{AND}; ordinal numerals (\code{1ST}, \code{2ND}, \ldots) \eqn{\to}
#'   written words (street name only).  Set to \code{FALSE} to skip.
#' @return A \code{data.table} with one row per input and columns:
#'   \code{input_id}, \code{input_raw}, \code{in_postcode}, \code{in_state},
#'   \code{in_locality}, \code{in_street_name}, \code{in_street_type},
#'   \code{in_street_suffix}, \code{in_number_first}, \code{in_number_last},
#'   \code{in_flat_type}, \code{in_flat_number}, \code{in_building_name}.
#' @noRd
.address_parse_legacy <- function(addresses, normalize = TRUE) {
  return(address_parse(addresses, normalize = normalize))
  st_map   <- .get_street_type_map()
  st_regex <- .build_street_type_regex(st_map)
  ft_map   <- .get_flat_type_map()
  # Longest-first alternation of flat-type abbreviations, built once per call
  # and threaded through every parser level that needs it.
  ft_alt   <- paste(names(ft_map)[order(-nchar(names(ft_map)))], collapse = "|")
  ft_re    <- paste0("^(", ft_alt, ")\\s+(\\d+[A-Z]?)\\s+")

  # Comma hint: capture the word immediately before the LAST comma in each
  # raw address (uppercased, periods stripped) before .normalize_addr
  # discards comma positions. In "STREET ADDRESS, LOCALITY ..." formats that
  # word is structurally the street type — useful for disambiguating
  # coincidental abbreviation collisions (e.g. "St" in "St James") and for
  # prioritising fuzzy-matching of misspelt types (e.g. "STX" -> "ST").
  # The LAST comma is used so flat/unit notations like "Unit 5, 10 Smith St,
  # Brisbane ..." don't pick up the flat number instead.
  addr_upper <- fast.string::fgsub(
    ".", " ", stringi::stri_trans_toupper(fast.string::ftrimws(addresses)),
    fixed = TRUE
  )
  addr_upper <- fast.string::fgsub("\\s+", " ", addr_upper)
  comma_word <- stringi::stri_match_first_regex(
    addr_upper, "\\b([A-Z0-9]+)\\s*,[^,]*$"
  )[, 2L]

  normalized <- .normalize_addr(addresses)
  dt <- .parse_vectorized(normalized, addresses, st_map, st_regex, ft_map,
                          ft_re, ft_alt, comma_word)
  if (isTRUE(normalize)) .expand_abbreviations(dt)
  setcolorder(dt, c("input_id", "input_raw",
                    "in_postcode", "in_state", "in_locality",
                    "in_street_name", "in_street_type", "in_street_suffix",
                    "in_number_first", "in_number_last", "in_number_suffix",
                    "in_flat_type", "in_flat_number", "in_building_name"))
  dt
}

# ---------------------------------------------------------------------------
# Vectorized parser — handles the full address vector in bulk stringi calls.
# Falls back to .parse_single only for addresses that don't match any fast
# pattern (typically <5% for well-formed Australian addresses).
# ---------------------------------------------------------------------------
.parse_vectorized <- function(normalized, addresses, st_map, st_regex, ft_map,
                              ft_re, ft_alt, boundary, level_map, level_alt) {
  n <- length(normalized)

  in_state         <- rep(NA_character_, n)
  in_postcode      <- rep(NA_integer_,   n)
  in_locality      <- rep(NA_character_, n)
  in_street_name   <- rep(NA_character_, n)
  in_street_suffix <- rep(NA_character_, n)
  in_number_first  <- rep(NA_integer_,   n)
  in_number_last   <- rep(NA_integer_,   n)
  in_number_suffix <- rep(NA_character_, n)
  in_flat_type     <- rep(NA_character_, n)
  in_flat_number   <- rep(NA_character_, n)
  in_level_type    <- rep(NA_character_, n)
  in_level_number  <- rep(NA_character_, n)
  in_lot_number    <- rep(NA_character_, n)
  in_building_name <- rep(NA_character_, n)

  # NA / empty inputs produce an all-NA parse row and skip every stage,
  # including the fallback parser.
  valid <- !is.na(normalized) & nzchar(normalized)

  # ------------------------------------------------------------------
  # Stage 1: extract trailing geo-suffix from end.
  # Handles both "STATE POSTCODE" and "POSTCODE STATE" orderings since
  # address_perturb_sample produces both. Each pattern is only tried on
  # the rows the previous ones didn't claim.
  # ------------------------------------------------------------------
  st_abbr <- "(?:QLD|NSW|VIC|SA|WA|TAS|NT|ACT)"
  a_re <- paste0("(\\b", st_abbr, "\\b)\\s+(\\b\\d{4}\\b)\\s*$")  # STATE POSTCODE$
  b_re <- paste0("(\\b\\d{4}\\b)\\s+(\\b", st_abbr, "\\b)\\s*$")  # POSTCODE STATE$
  c_re <- paste0("(\\b", st_abbr, "\\b)\\s*$")                    # STATE$
  d_re <- "(\\b\\d{4}\\b)\\s*$"                                   # POSTCODE$

  work <- normalized
  rem  <- which(valid)

  m <- stringi::stri_match_first_regex(work[rem], a_re)
  hit <- !is.na(m[, 1L])
  idx <- rem[hit]
  if (length(idx) > 0L) {
    in_state[idx]    <- m[hit, 2L]
    in_postcode[idx] <- as.integer(m[hit, 3L])
    work[idx] <- fast.string::ftrimws(fast.string::fsub(a_re, "", work[idx]))
  }
  rem <- rem[!hit]

  m <- stringi::stri_match_first_regex(work[rem], b_re)
  hit <- !is.na(m[, 1L])
  idx <- rem[hit]
  if (length(idx) > 0L) {
    in_postcode[idx] <- as.integer(m[hit, 2L])
    in_state[idx]    <- m[hit, 3L]
    work[idx] <- fast.string::ftrimws(fast.string::fsub(b_re, "", work[idx]))
  }
  rem <- rem[!hit]

  m <- stringi::stri_match_first_regex(work[rem], c_re)
  hit <- !is.na(m[, 1L])
  idx <- rem[hit]
  if (length(idx) > 0L) {
    in_state[idx] <- m[hit, 2L]
    work[idx] <- fast.string::ftrimws(fast.string::fsub(c_re, "", work[idx]))
  }
  rem <- rem[!hit]

  m <- stringi::stri_match_first_regex(work[rem], d_re)
  hit <- !is.na(m[, 1L])
  idx <- rem[hit]
  if (length(idx) > 0L) {
    in_postcode[idx] <- as.integer(m[hit, 2L])
    work[idx] <- fast.string::ftrimws(fast.string::fsub(d_re, "", work[idx]))
  }

  has_boundary <- boundary$meaningful & valid
  if (any(has_boundary)) {
    work[has_boundary] <- boundary$street[has_boundary]
    in_locality[has_boundary] <- boundary$locality[has_boundary]
  }

  # ------------------------------------------------------------------
  # Stage 2: rightmost street type in the remaining string.
  # ------------------------------------------------------------------
  loc_st <- stringi::stri_locate_last_regex(work, st_regex)
  st_pos <- loc_st[, 1L]
  st_end <- loc_st[, 2L]
  forced_type_less <- rep(FALSE, n)
  initial_raw <- rep(NA_character_, n)
  located <- !is.na(st_pos)
  initial_raw[located] <- fast.string::fsubstr(
    work[located], st_pos[located], st_end[located]
  )
  locality_collision <- initial_raw %in% .LOCALITY_COLLISION_WORDS &
    !boundary$meaningful
  if (any(locality_collision, na.rm = TRUE)) {
    collision_idx <- which(locality_collision)
    prefix_text <- fast.string::ftrimws(fast.string::fsubstr(
      work[collision_idx], 1L, pmax(0L, st_pos[collision_idx] - 1L)
    ))
    previous <- stringi::stri_locate_last_regex(prefix_text, st_regex)
    has_previous <- !is.na(previous[, 1L])
    if (any(has_previous)) {
      idx <- collision_idx[has_previous]
      st_pos[idx] <- previous[has_previous, 1L]
      st_end[idx] <- previous[has_previous, 2L]
    }
    no_previous <- !has_previous
    if (any(no_previous)) {
      idx <- collision_idx[no_previous]
      words_after_number <- fast.string::fgrepl(
        "^.*?\\d+[A-Z]?(?:-\\d+[A-Z]?)?\\s+\\S+\\s+\\S+", work[idx]
      )
      invalidate <- words_after_number & st_end[idx] == nchar(work[idx])
      forced_type_less[idx[invalidate]] <- TRUE
      st_pos[idx[invalidate]] <- NA_integer_
      st_end[idx[invalidate]] <- NA_integer_
    }
  }
  fuzzy_type <- rep(NA_character_, n)
  fuzzy_rows <- which(
    valid & !has_boundary & is.na(st_pos) & !forced_type_less
  )
  fuzzy <- .resolve_fuzzy_street_types_vec(work, fuzzy_rows, st_map)
  if (length(fuzzy$rows) > 0L) {
    st_pos[fuzzy$rows] <- fuzzy$start
    st_end[fuzzy$rows] <- fuzzy$end
    fuzzy_type[fuzzy$rows] <- fuzzy$canonical
  }
  no_type <- fuzzy_rows[!fuzzy_rows %in% fuzzy$rows]
  forced_type_less[no_type] <- TRUE

  non_boundary <- valid & !has_boundary
  has_st <- !is.na(st_pos) & non_boundary
  if (any(has_st)) {
    prefix <- fast.string::ftrimws(fast.string::fsubstr(
      work[has_st], 1L, pmax(0L, st_pos[has_st] - 1L)
    ))
    type_is_name <- fast.string::fgrepl(
      "^\\d+[A-Z]?(?:-\\d+[A-Z]?)?$|\\b\\d+[A-Z]?(?:-\\d+[A-Z]?)?\\s+THE$",
      prefix
    )
    type_name_idx <- which(has_st)[type_is_name]
    forced_type_less[type_name_idx] <- TRUE
    has_st[type_name_idx] <- FALSE
  }
  btype <- has_boundary & !is.na(boundary$type)
  st_pos[btype] <- boundary$type_start[btype]
  st_end[btype] <- boundary$type_end[btype]
  has_st[btype] <- TRUE
  st_raw <- rep(NA_character_, n)
  exact_st <- has_st & !btype
  st_raw[exact_st] <- fast.string::fsubstr(
    work[exact_st], st_pos[exact_st], st_end[exact_st]
  )
  in_street_type <- unname(st_map[st_raw])
  in_street_type[!is.na(fuzzy_type)] <- fuzzy_type[!is.na(fuzzy_type)]
  in_street_type[btype] <- boundary$type[btype]

  # Comma hint: when the word immediately before the (last) comma in the
  # original input differs from the rightmost exact-match street type and
  # plausibly looks like a type itself (exact key, or a close fuzzy match —
  # e.g. "Rode" ~ "Road"), the rightmost-match search has likely landed on a
  # coincidental collision (e.g. "St" inside "St James Rode"). Route these to
  # .parse_single, which re-resolves the type using the comma hint directly.
  before_st_end <- ifelse(has_st, st_pos - 1L, fast.string::fnchar(work))
  before_st     <- fast.string::ftrimws(fast.string::fsubstr(work, 1L, before_st_end))
  before_st[!nzchar(before_st)] <- NA_character_

  after_st_start <- ifelse(has_st, st_end + 1L, fast.string::fnchar(work) + 1L)
  after_st_raw   <- fast.string::ftrimws(fast.string::fsubstr(work, after_st_start, fast.string::fnchar(work)))
  after_st_raw[!nzchar(after_st_raw) | !has_st] <- NA_character_

  # ------------------------------------------------------------------
  # Stage 3: optional street suffix then locality from after_st_raw.
  # ------------------------------------------------------------------
  sfx_re <- "^(NORTH|SOUTH|EAST|WEST|UPPER|LOWER|INNER|OUTER)\\b"
  sfx_m  <- stringi::stri_match_first_regex(after_st_raw, sfx_re)
  has_sfx <- !is.na(sfx_m[, 1L])

  in_street_suffix[has_sfx] <- sfx_m[has_sfx, 2L]
  loc_raw <- after_st_raw
  loc_raw[has_sfx] <- fast.string::ftrimws(fast.string::fsub(
    sfx_re, "", after_st_raw[has_sfx]))
  parsed_locality <- ifelse(
    !is.na(loc_raw) & nzchar(loc_raw), loc_raw, NA_character_
  )
  in_locality[non_boundary] <- parsed_locality[non_boundary]

  # ------------------------------------------------------------------
  # Stage 4: parse the before-street-type for number / flat / name.
  # Four vectorized patterns cover the common cases, each tried only on
  # rows the previous patterns didn't claim:
  #   4a — slash notation "FLAT/NUM STREETNAME" (unit addresses)
  #   4b — flat-type prefix "UNIT 3 NUM STREETNAME"
  #   4b2 — attached single-letter flat prefix "F8 536 STREETNAME"
  #   4c — simple "NUM[-NUM] STREETNAME"  ← ~70% of all addresses
  # Anything else falls back to .parse_single.
  # ------------------------------------------------------------------
  bst <- before_st
  bst[is.na(bst)] <- ""

  fast <- rep(FALSE, n)
  cand <- which(has_st | has_boundary | forced_type_less)

  level_re <- paste0(
    "\\b(", level_alt, ")\\s+([A-Z0-9]+(?:-[A-Z0-9]+)?)\\b"
  )
  level_m <- stringi::stri_match_first_regex(bst[cand], level_re)
  has_level <- !is.na(level_m[, 1L])
  if (any(has_level)) {
    idx <- cand[has_level]
    in_level_type[idx] <- unname(level_map[level_m[has_level, 2L]])
    in_level_number[idx] <- level_m[has_level, 3L]
    bst[idx] <- fast.string::ftrimws(stringi::stri_replace_first_regex(
      bst[idx], level_re, ""
    ))
  }

  lot_re <- "\\bLOT\\s+([A-Z0-9]+(?:-[A-Z0-9]+)?)\\b"
  lot_m <- stringi::stri_match_first_regex(bst[cand], lot_re)
  has_lot <- !is.na(lot_m[, 1L])
  if (any(has_lot)) {
    idx <- cand[has_lot]
    in_lot_number[idx] <- lot_m[has_lot, 2L]
    lot_parts <- stringi::stri_match_first_regex(
      bst[idx],
      "^(.*?)\\bLOT\\s+[A-Z0-9]+(?:-[A-Z0-9]+)?\\s*(.*)$"
    )
    split_lot <- !is.na(lot_parts[, 1L])
    if (any(split_lot)) {
      split_idx <- idx[split_lot]
      building <- fast.string::ftrimws(lot_parts[split_lot, 2L])
      in_building_name[split_idx] <- fifelse(
        nzchar(building), building, in_building_name[split_idx]
      )
      bst[split_idx] <- fast.string::ftrimws(lot_parts[split_lot, 3L])
    }
    if (any(!split_lot)) {
      unsplit_idx <- idx[!split_lot]
      bst[unsplit_idx] <- fast.string::ftrimws(stringi::stri_replace_first_regex(
        bst[unsplit_idx], lot_re, ""
      ))
    }
  }

  # 4a: slash notation (unit/flat numbers may carry trailing alpha e.g. 3A/190B)
  m <- stringi::stri_match_first_regex(
    bst[cand], "^(.*?)(\\d+[A-Z]?)/(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s*(.*)$")
  hit <- !is.na(m[, 1L])
  idx <- cand[hit]
  if (length(idx) > 0L) {
    bld <- trimws(m[hit, 2L])
    # A lone letter before the slash (e.g. "U6019/6") is the attached
    # flat-prefix marker, not a building name — see .ATT_FLAT_MAP.
    is_att <- grepl("^[A-Z]$", bld)
    mapped <- unname(.ATT_FLAT_MAP[bld])
    in_flat_type[idx]     <- fifelse(is_att, fifelse(is.na(mapped), "UNIT", mapped), "UNIT")
    in_building_name[idx] <- fifelse(!is_att & nzchar(bld), bld, NA_character_)
    in_flat_number[idx]   <- m[hit, 3L]
    num <- .split_number_vec(m[hit, 4L])
    in_number_first[idx]  <- num$first
    in_number_last[idx]   <- num$last
    in_number_suffix[idx] <- num$suffix
    in_street_name[idx]   <- m[hit, 5L]
    fast[idx] <- TRUE
  }
  cand <- cand[!hit]

  # 4b: flat-type prefix ("UNIT 3 ...")
  m <- stringi::stri_match_first_regex(bst[cand], ft_re)
  hit <- !is.na(m[, 1L])
  idx <- cand[hit]
  if (length(idx) > 0L) {
    in_flat_type[idx]   <- unname(ft_map[m[hit, 2L]])
    in_flat_number[idx] <- m[hit, 3L]
    rest <- fast.string::ftrimws(fast.string::fsubstr(
      bst[idx], fast.string::fnchar(m[hit, 1L]) + 1L, fast.string::fnchar(bst[idx])
    ))
    # parse "NUM[-NUM] STREETNAME" from rest (number may have trailing alpha e.g. 190A)
    m2  <- stringi::stri_match_first_regex(rest, "^(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(.+)$")
    ok2 <- !is.na(m2[, 1L])
    if (any(ok2)) {
      idx2 <- idx[ok2]
      num  <- .split_number_vec(m2[ok2, 2L])
      in_number_first[idx2]  <- num$first
      in_number_last[idx2]   <- num$last
      in_number_suffix[idx2] <- num$suffix
      in_street_name[idx2]   <- m2[ok2, 3L]
    }
    if (any(!ok2)) {
      rest_no <- rest[!ok2]
      in_street_name[idx[!ok2]] <- fifelse(nzchar(rest_no), rest_no, NA_character_)
    }
    fast[idx] <- TRUE
  }
  cand <- cand[!hit]

  # 4b2: attached single-letter flat prefix — "F8 536 STREETNAME".
  # A single capital letter immediately followed by digits (no space) then NUM
  # STREETNAME. Handles informal shorthands like F8 (Flat 8), A6 (Apartment 6),
  # D2, etc. Not reachable by 4b (requires space after marker) or 4c (requires
  # digit at position 0).
  m <- stringi::stri_match_first_regex(
    bst[cand], "^([A-Z])(\\d+[A-Z]?)\\s+(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(.+)$")
  hit <- !is.na(m[, 1L])
  idx <- cand[hit]
  if (length(idx) > 0L) {
    mapped <- unname(.ATT_FLAT_MAP[m[hit, 2L]])
    in_flat_type[idx]   <- fifelse(is.na(mapped), "UNIT", mapped)
    in_flat_number[idx] <- m[hit, 3L]
    num <- .split_number_vec(m[hit, 4L])
    in_number_first[idx]  <- num$first
    in_number_last[idx]   <- num$last
    in_number_suffix[idx] <- num$suffix
    in_street_name[idx]   <- m[hit, 5L]
    fast[idx] <- TRUE
  }
  cand <- cand[!hit]

  # 4c: simple "NUM[-NUM] STREETNAME" — with implied-flat sub-case:
  #   "NUM1 NUM2[-NUM3] STREETNAME" where NUM1 is unit and NUM2 is street number.
  #   (.parse_before detects this; we replicate it vectorized to avoid fallback overhead.)
  simple_re <- "^(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(.+)$"
  m <- stringi::stri_match_first_regex(bst[cand], simple_re)
  hit <- !is.na(m[, 1L])

  # Exclude candidates whose remainder hides an explicit flat-type marker —
  # e.g. "6 UNIT 6019 Parkland Bvd" or "5 Blind Road Unit 6019 6 Parkland
  # Bvd" — these need .parse_before's full marker-search logic, so route them
  # to the (slower) fallback parser instead of mis-reading the marker as part
  # of the street name.
  if (any(hit)) {
    embed_re <- paste0("\\b(?:", ft_alt, ")\\s+\\d+|\\bU\\d+\\b")
    hit[hit] <- !fast.string::fgrepl(embed_re, m[hit, 3L])
  }
  idx <- cand[hit]
  if (length(idx) > 0L) {
    num_s  <- m[hit, 2L]
    rest_s <- m[hit, 3L]
    impl    <- stringi::stri_match_first_regex(rest_s, simple_re)
    is_impl <- !is.na(impl[, 1L])

    if (any(is_impl)) {
      idx_i <- idx[is_impl]
      in_flat_type[idx_i]   <- "UNIT"
      in_flat_number[idx_i] <- num_s[is_impl]
      num <- .split_number_vec(impl[is_impl, 2L])
      in_number_first[idx_i]  <- num$first
      in_number_last[idx_i]   <- num$last
      in_number_suffix[idx_i] <- num$suffix
      in_street_name[idx_i]   <- impl[is_impl, 3L]
    }

    if (any(!is_impl)) {
      idx_n <- idx[!is_impl]
      num <- .split_number_vec(num_s[!is_impl])
      in_number_first[idx_n]  <- num$first
      in_number_last[idx_n]   <- num$last
      in_number_suffix[idx_n] <- num$suffix
      in_street_name[idx_n]   <- rest_s[!is_impl]

      # Post-process: if street_name starts with a letter+digit flat designator
      # (e.g. "A1 TAVISTOCK" from "36 A1 TAVISTOCK ST"), extract it as flat.
      fnm <- stringi::stri_match_first_regex(
        rest_s[!is_impl], "^([A-Z])(\\d+[A-Z]?)\\s+(\\S.*)$")
      is_fn <- !is.na(fnm[, 1L])
      if (any(is_fn)) {
        idx_f  <- idx_n[is_fn]
        mapped <- unname(.ATT_FLAT_MAP[fnm[is_fn, 2L]])
        in_flat_type[idx_f]   <- fifelse(is.na(mapped), "UNIT", mapped)
        in_flat_number[idx_f] <- fnm[is_fn, 3L]
        in_street_name[idx_f] <- fnm[is_fn, 4L]
      }
    }
    fast[idx] <- TRUE
  }
  cand <- cand[!hit]

  # Plain building/site prefix followed by the street number. This was one of
  # the largest row-wise fallback groups in canonical G-NAF labels.
  building_m <- stringi::stri_match_first_regex(
    bst[cand], "^(.+?\\D)\\s+(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(.+)$"
  )
  building_hit <- !is.na(building_m[, 1L])
  if (any(building_hit)) {
    embedded_marker <- paste0("\\b(?:", ft_alt, ")\\s+\\d+|\\bU\\d+\\b")
    building_hit[building_hit] <- !fast.string::fgrepl(
      embedded_marker, bst[cand[building_hit]]
    )
  }
  idx <- cand[building_hit]
  if (length(idx) > 0L) {
    in_building_name[idx] <- fast.string::ftrimws(building_m[building_hit, 2L])
    num <- .split_number_vec(building_m[building_hit, 3L])
    in_number_first[idx] <- num$first
    in_number_last[idx] <- num$last
    in_number_suffix[idx] <- num$suffix
    in_street_name[idx] <- building_m[building_hit, 4L]
    fast[idx] <- TRUE
  }

  # Building/site name followed by an embedded flat marker, flat number,
  # street number, and street name. Canonical G-NAF labels commonly use this
  # form (for example "WILLOW GLEN UNIT 21 11 DONAHUE STREET").
  remaining <- which(valid & !fast & (has_st | has_boundary | forced_type_less))
  embedded_re <- paste0(
    "^(.+?)\\s+(", ft_alt,
    ")\\s+(\\d+[A-Z]?)\\s+(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(.+)$"
  )
  embedded_m <- stringi::stri_match_first_regex(bst[remaining], embedded_re)
  embedded_hit <- !is.na(embedded_m[, 1L])
  idx <- remaining[embedded_hit]
  if (length(idx) > 0L) {
    in_building_name[idx] <- embedded_m[embedded_hit, 2L]
    in_flat_type[idx] <- unname(ft_map[embedded_m[embedded_hit, 3L]])
    in_flat_number[idx] <- embedded_m[embedded_hit, 4L]
    num <- .split_number_vec(embedded_m[embedded_hit, 5L])
    in_number_first[idx] <- num$first
    in_number_last[idx] <- num$last
    in_number_suffix[idx] <- num$suffix
    in_street_name[idx] <- embedded_m[embedded_hit, 6L]
    fast[idx] <- TRUE
  }

  # Number-less G-NAF rows still have a valid street name. Treat the complete
  # pre-type section as that name instead of routing it through a scalar parse.
  remaining <- which(valid & !fast & (has_st | has_boundary | forced_type_less))
  plain_name <- !fast.string::fgrepl("[0-9]", bst[remaining]) &
    !is.na(bst[remaining]) & nzchar(bst[remaining])
  idx <- remaining[plain_name]
  if (length(idx) > 0L) {
    in_street_name[idx] <- bst[idx]
    fast[idx] <- TRUE
  }

  type_less_fast <- fast & forced_type_less & !has_boundary
  if (any(type_less_fast)) {
    idx <- which(type_less_fast)
    split <- stringi::stri_match_first_regex(
      in_street_name[idx], "^(\\S+)\\s+(.+)$"
    )
    take <- !is.na(split[, 1L]) & split[, 2L] != "THE"
    if (any(take)) {
      out_idx <- idx[take]
      in_street_name[out_idx] <- split[take, 2L]
      in_locality[out_idx] <- split[take, 3L]
    }
  }

  # ------------------------------------------------------------------
  # Fallback: any valid address that didn't hit a fast path above.
  # Typically: no street type found, complex building names, fuzzy street.
  # ------------------------------------------------------------------
  fallback <- which(valid & !fast)
  boundary_fallback <- fallback[has_boundary[fallback]]
  if (length(boundary_fallback) > 0L) {
    bp <- lapply(boundary_fallback, function(i) {
      .parse_before(bst[[i]], ft_re, ft_map, ft_alt, level_map, level_alt)
    })
    in_street_name[boundary_fallback] <- vapply(bp, `[[`, character(1L), "street_name")
    in_number_first[boundary_fallback] <- vapply(bp, `[[`, integer(1L), "number_first")
    in_number_last[boundary_fallback] <- vapply(bp, `[[`, integer(1L), "number_last")
    in_number_suffix[boundary_fallback] <- vapply(bp, `[[`, character(1L), "number_suffix")
    in_flat_type[boundary_fallback] <- vapply(bp, `[[`, character(1L), "flat_type")
    in_flat_number[boundary_fallback] <- vapply(bp, `[[`, character(1L), "flat_number")
    in_building_name[boundary_fallback] <- vapply(bp, `[[`, character(1L), "building_name")
    fallback <- fallback[!has_boundary[fallback]]
  }
  if (length(fallback) > 0L) {
    fb <- lapply(fallback, function(i) {
      .parse_single(normalized[[i]], st_regex, st_map, ft_re, ft_map, ft_alt,
                    NA_character_, level_map, level_alt)
    })
    in_postcode[fallback]      <- vapply(fb, `[[`, integer(1),   "in_postcode")
    in_state[fallback]         <- vapply(fb, `[[`, character(1), "in_state")
    in_locality[fallback]      <- vapply(fb, `[[`, character(1), "in_locality")
    in_street_name[fallback]   <- vapply(fb, `[[`, character(1), "in_street_name")
    in_street_type[fallback]   <- vapply(fb, `[[`, character(1), "in_street_type")
    in_street_suffix[fallback] <- vapply(fb, `[[`, character(1), "in_street_suffix")
    in_number_first[fallback]  <- vapply(fb, `[[`, integer(1),   "in_number_first")
    in_number_last[fallback]   <- vapply(fb, `[[`, integer(1),   "in_number_last")
    in_number_suffix[fallback] <- vapply(fb, `[[`, character(1), "in_number_suffix")
    in_flat_type[fallback]     <- vapply(fb, `[[`, character(1), "in_flat_type")
    in_flat_number[fallback]   <- vapply(fb, `[[`, character(1), "in_flat_number")
    in_level_type[fallback]    <- vapply(fb, `[[`, character(1), "in_level_type")
    in_level_number[fallback]  <- vapply(fb, `[[`, character(1), "in_level_number")
    in_lot_number[fallback]    <- vapply(fb, `[[`, character(1), "in_lot_number")
    in_building_name[fallback] <- vapply(fb, `[[`, character(1), "in_building_name")
  }

  data.table(
    input_id         = seq_len(n),
    input_raw        = addresses,
    in_postcode      = in_postcode,
    in_state         = in_state,
    in_locality      = in_locality,
    in_street_name   = in_street_name,
    in_street_type   = in_street_type,
    in_street_suffix = in_street_suffix,
    in_number_first  = in_number_first,
    in_number_last   = in_number_last,
    in_number_suffix = in_number_suffix,
    in_flat_type     = in_flat_type,
    in_flat_number   = in_flat_number,
    in_level_type    = in_level_type,
    in_level_number  = in_level_number,
    in_lot_number    = in_lot_number,
    in_building_name = in_building_name
  )
}

# ---------------------------------------------------------------------------
# Internal: parse a single normalised address string
# ---------------------------------------------------------------------------
.parse_single <- function(addr, st_regex, st_map, ft_re, ft_map, ft_alt,
                          comma_word = NA_character_,
                          level_map = .get_level_type_map(),
                          level_alt = paste(names(level_map), collapse = "|")) {
  out <- list(
    in_postcode      = NA_integer_,
    in_state         = NA_character_,
    in_locality      = NA_character_,
    in_street_name   = NA_character_,
    in_street_type   = NA_character_,
    in_street_suffix = NA_character_,
    in_number_first  = NA_integer_,
    in_number_last   = NA_integer_,
    in_number_suffix = NA_character_,
    in_flat_type     = NA_character_,
    in_flat_number   = NA_character_,
    in_level_type    = NA_character_,
    in_level_number  = NA_character_,
    in_lot_number    = NA_character_,
    in_building_name = NA_character_
  )

  if (is.na(addr) || !nzchar(addr)) return(out)

  # 1. Postcode: last 4-digit token in string. Remove it by position — a
  # pattern-based removal would also delete a street number that happens to
  # share the same digits (e.g. "4000 SMITH ST BRISBANE QLD 4000").
  pc_all <- gregexpr("\\b\\d{4}\\b", addr, perl = TRUE)[[1L]]
  if (pc_all[1L] > 0L) {
    pc_start <- pc_all[length(pc_all)]
    out$in_postcode <- as.integer(substr(addr, pc_start, pc_start + 3L))
    addr <- trimws(paste0(substr(addr, 1L, pc_start - 1L),
                          substr(addr, pc_start + 4L, nchar(addr))))
    addr <- gsub("\\s+", " ", addr)
  }

  # 2. State abbreviation
  m_st <- regexpr("\\b(QLD|NSW|VIC|SA|WA|TAS|NT|ACT)\\b", addr, perl = TRUE)
  if (m_st > 0L) {
    out$in_state <- substr(addr, m_st, m_st + attr(m_st, "match.length") - 1L)
    addr <- trimws(sub("\\b(?:QLD|NSW|VIC|SA|WA|TAS|NT|ACT)\\b", "", addr, perl = TRUE))
    addr <- gsub("\\s+", " ", addr)
  }

  # 3. Street type resolution.
  #
  # When the original input had a comma separating the street address from
  # the locality, the word immediately before it is structurally the street
  # type. That comma hint takes priority over the generic rightmost-exact-
  # match search, which can mis-fire on:
  #   - coincidental abbreviation collisions inside multi-word street names
  #     (e.g. "St" inside "St James Rode, Tamborine Mountain..." matches the
  #     STREET abbreviation, hiding the real, misspelt type "Rode")
  #   - street-name words that merely resemble a type during fuzzy fallback
  #     (e.g. "Parkland" ~ "Parade")
  #
  # Falls through to the rightmost-exact-match / fuzzy search when there is
  # no comma hint, the hinted word isn't present in `addr`, or it doesn't
  # plausibly look like a street type (exact key, or close fuzzy match).
  resolved_by_comma <- FALSE
  if (!is.na(comma_word)) {
    cw_all <- gregexpr(paste0("\\b", comma_word, "\\b"), addr, perl = TRUE)[[1L]]
    if (cw_all[1L] > 0L) {
      cw_canon <- unname(st_map[comma_word])
      if (is.na(cw_canon)) {
        sims <- fast.string::jaro_winkler_matrix(comma_word, names(st_map), p = 0.1)[1L, ]
        j <- which.max(sims)
        if (sims[[j]] >= 0.85) cw_canon <- unname(st_map[[names(st_map)[[j]]]])
      }
      if (!is.na(cw_canon)) {
        cw_start <- utils::tail(cw_all[cw_all > 0L], 1L)
        cw_len   <- nchar(comma_word)
        out$in_street_type <- cw_canon
        before    <- trimws(substr(addr, 1L, cw_start - 1L))
        after_raw <- trimws(substr(addr, cw_start + cw_len, nchar(addr)))
        resolved_by_comma <- TRUE
      }
    }
  }

  if (!resolved_by_comma) {
    st_all <- gregexpr(st_regex, addr, perl = TRUE)[[1L]]
    if (st_all[1L] <= 0L) {
      fuzzy <- .parse_fuzzy_street(addr, st_map)
      if (is.null(fuzzy)) {
        # No street-type token at all (uncommon but real — e.g. "190 MUSGRAVE
        # RED HILL QLD 4059"). Still extract number/flat/building so number-
        # and postcode-based scoring isn't crippled, then take a best guess at
        # the street/locality split: first remaining word is the street name,
        # the rest is the locality (most AU street names are a single word when
        # the type is dropped; localities are typically 1-3 words).
        bp <- .parse_before(addr, ft_re, ft_map, ft_alt, level_map, level_alt)
        out$in_number_first  <- bp$number_first
        out$in_number_last   <- bp$number_last
        out$in_number_suffix <- bp$number_suffix
        out$in_flat_type     <- bp$flat_type
        out$in_flat_number   <- bp$flat_number
        out$in_level_type    <- bp$level_type
        out$in_level_number  <- bp$level_number
        out$in_lot_number    <- bp$lot_number
        out$in_building_name <- bp$building_name

        if (!is.na(bp$street_name)) {
          words <- strsplit(bp$street_name, "\\s+", perl = TRUE)[[1L]]
          if (length(words) >= 2L) {
            out$in_street_name <- words[[1L]]
            out$in_locality    <- paste(words[-1L], collapse = " ")
          } else {
            out$in_street_name <- bp$street_name
          }
        }
        return(out)
      }
      out$in_street_type <- fuzzy$canonical
      before    <- fuzzy$before
      after_raw <- fuzzy$after
    } else {
      last <- length(st_all)
      st_start  <- st_all[last]
      st_len    <- attr(st_all, "match.length")[last]
      st_raw    <- substr(addr, st_start, st_start + st_len - 1L)

      # A rightmost match that is itself a common locality-name word (e.g.
      # "Red HILL", "Bushland PARK") is often the actual suburb, not the
      # street type - prefer an earlier, unambiguous street-type token when
      # one exists, mirroring the same disambiguation the vectorized fast
      # path applies via .LOCALITY_COLLISION_WORDS.
      if (st_raw %in% .LOCALITY_COLLISION_WORDS) {
        prefix_text <- substr(addr, 1L, st_start - 1L)
        prev_all <- gregexpr(st_regex, prefix_text, perl = TRUE)[[1L]]
        if (prev_all[1L] > 0L) {
          p_last   <- length(prev_all)
          st_start <- prev_all[p_last]
          st_len   <- attr(prev_all, "match.length")[p_last]
          st_raw   <- substr(prefix_text, st_start, st_start + st_len - 1L)
        }
      }

      out$in_street_type <- unname(st_map[st_raw])
      before    <- trimws(substr(addr, 1L, st_start - 1L))
      after_raw <- trimws(substr(addr, st_start + st_len, nchar(addr)))
    }
  }

  # 4. Street suffix (NORTH/SOUTH/EAST/WEST immediately after street type)
  sfx_re <- "^(NORTH|SOUTH|EAST|WEST|UPPER|LOWER|INNER|OUTER)\\b"
  m_sfx <- regexpr(sfx_re, after_raw, perl = TRUE)
  if (m_sfx > 0L) {
    out$in_street_suffix <- substr(after_raw, m_sfx, m_sfx + attr(m_sfx, "match.length") - 1L)
    after_raw <- trimws(sub(sfx_re, "", after_raw, perl = TRUE))
  }
  out$in_locality <- if (nzchar(after_raw)) after_raw else NA_character_

  # 5. Parse "before" section: [building] [flat] [number] street_name
  bp <- .parse_before(before, ft_re, ft_map, ft_alt, level_map, level_alt)
  out$in_street_name   <- bp$street_name
  out$in_number_first  <- bp$number_first
  out$in_number_last   <- bp$number_last
  out$in_number_suffix <- bp$number_suffix
  out$in_flat_type     <- bp$flat_type
  out$in_flat_number   <- bp$flat_number
  out$in_level_type    <- bp$level_type
  out$in_level_number  <- bp$level_number
  out$in_lot_number    <- bp$lot_number
  out$in_building_name <- bp$building_name

  out
}

# ---------------------------------------------------------------------------
# Internal: parse the text that precedes the street type
# Returns a list with: street_name, number_first, number_last,
#                      flat_type, flat_number, building_name
# ---------------------------------------------------------------------------
.extract_special_designators <- function(s, level_map, level_alt) {
  level_type <- level_number <- lot_number <- NA_character_
  level_re <- paste0(
    "\\b(", level_alt, ")\\s+([A-Z0-9]+(?:-[A-Z0-9]+)?)\\b"
  )
  level <- regmatches(s, regexec(level_re, s, perl = TRUE))[[1L]]
  if (length(level) == 3L) {
    level_type <- unname(level_map[[level[[2L]]]])
    level_number <- level[[3L]]
    s <- trimws(sub(level_re, "", s, perl = TRUE))
  }
  lot_re <- "\\bLOT\\s+([A-Z0-9]+(?:-[A-Z0-9]+)?)\\b"
  lot <- regmatches(s, regexec(lot_re, s, perl = TRUE))[[1L]]
  if (length(lot) == 2L) {
    lot_number <- lot[[2L]]
    s <- trimws(sub(lot_re, "", s, perl = TRUE))
  }
  list(
    text = fast.string::ftrimws(fast.string::fgsub("\\s+", " ", s)),
    level_type = level_type, level_number = level_number,
    lot_number = lot_number
  )
}

.parse_before <- function(s, ft_re, ft_map, ft_alt,
                          level_map = .get_level_type_map(),
                          level_alt = paste(names(level_map), collapse = "|")) {
  out <- list(
    street_name   = NA_character_,
    number_first  = NA_integer_,
    number_last   = NA_integer_,
    number_suffix = NA_character_,
    flat_type     = NA_character_,
    flat_number   = NA_character_,
    level_type    = NA_character_,
    level_number  = NA_character_,
    lot_number    = NA_character_,
    building_name = NA_character_
  )

  s <- trimws(s)
  if (!nzchar(s)) return(out)

  special <- .extract_special_designators(s, level_map, level_alt)
  s <- special$text
  out$level_type <- special$level_type
  out$level_number <- special$level_number
  out$lot_number <- special$lot_number
  if (!nzchar(s)) return(out)

  # Case A: slash notation anywhere — "building 110/120 street" or "110/120 street"
  # Unit and street numbers may have trailing alpha (e.g. 3A/190B).
  m_slash <- regexpr("(\\d+[A-Z]?)/(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)", s, perl = TRUE)
  if (m_slash > 0L) {
    slash_end <- m_slash + attr(m_slash, "match.length") - 1L
    slash_str <- substr(s, m_slash, slash_end)

    pre_slash <- if (m_slash > 1L) trimws(substr(s, 1L, m_slash - 1L)) else ""
    # A lone letter before the slash (e.g. "U6019/6") is the attached
    # flat-prefix marker, not a building name — see .ATT_FLAT_MAP.
    if (grepl("^[A-Z]$", pre_slash)) {
      mapped <- unname(.ATT_FLAT_MAP[pre_slash])
      out$flat_type <- if (!is.na(mapped)) mapped else "UNIT"
    } else {
      out$flat_type <- "UNIT"
      if (nzchar(pre_slash)) out$building_name <- pre_slash
    }

    parts <- strsplit(slash_str, "/", fixed = TRUE)[[1L]]
    out$flat_number <- parts[1L]
    out <- .apply_parsed_number(out, parts[2L])

    out$street_name <- trimws(substr(s, slash_end + 1L, nchar(s)))
    if (!nzchar(out$street_name)) out$street_name <- NA_character_
    return(out)
  }

  # Case A2: attached single-letter flat prefix — "F8 536 STREETNAME" (F=Flat,
  # A=Apartment, U=Unit, other letters default to Unit). The letter is
  # immediately followed by the flat number with NO space, then the normal
  # NUM STREETNAME pattern. Handles informal shorthands common in user data
  # (e.g. "F8", "D2", "A6") that the flat-type keyword regex can't reach
  # because it requires a space between marker and number, and the implied-pair
  # regex's \\b word-boundary can't match between a letter and digit in "F8".
  att_m <- regmatches(s, regexec(
    "^([A-Z])(\\d+[A-Z]?)\\s+(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(\\S.*)$", s,
    perl = TRUE))[[1L]]
  if (length(att_m) == 5L) {
    letter_att <- att_m[[2L]]
    out$flat_type   <- if (!is.na(.ATT_FLAT_MAP[letter_att]))
                         unname(.ATT_FLAT_MAP[letter_att]) else "UNIT"
    out$flat_number <- att_m[[3L]]
    out <- .apply_parsed_number(out, att_m[[4L]])
    out$street_name <- att_m[[5L]]
    return(out)
  }

  # Case B: "implied pair" — NUM1 NUM2 STREETNAME, the rightmost such sequence
  # in the text (mirrors the rightmost-street-type heuristic elsewhere). NUM1
  # is the flat/unit number and NUM2 the street number — the common Australian
  # convention of writing "<unit> <number> <street>" without an explicit UNIT
  # marker (e.g. "10 120 Musgrave Rd"). When the text immediately before NUM1
  # ends in an explicit flat-type keyword (UNIT, APT, FLAT, ...), that keyword
  # supplies in_flat_type and is excluded from the building name — this also
  # lets noisy prefixes like "U10 BLAH UNIT 6019 6 Parkland Bvd" resolve to the
  # trailing "6019 6 Parkland" pair instead of the leading "U10".
  pair_re <- "^(.*)\\b(\\d+[A-Z]?)\\s+(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(\\D.*)$"
  pair_m  <- regmatches(s, regexec(pair_re, s, perl = TRUE))[[1L]]
  if (length(pair_m) == 5L) {
    prefix  <- pair_m[[2L]]
    num1    <- pair_m[[3L]]
    num2    <- pair_m[[4L]]
    st_name <- trimws(pair_m[[5L]])

    ftm <- regmatches(prefix, regexec(paste0("^(.*?)\\b(", ft_alt, ")\\s*$"), prefix, perl = TRUE))[[1L]]
    if (length(ftm) == 3L) {
      out$flat_type     <- unname(ft_map[[ftm[[3L]]]])
      out$building_name <- if (nzchar(trimws(ftm[[2L]]))) trimws(ftm[[2L]]) else NA_character_
    } else {
      out$flat_type     <- "UNIT"
      out$building_name <- if (nzchar(trimws(prefix))) trimws(prefix) else NA_character_
    }
    out$flat_number <- num1
    out <- .apply_parsed_number(out, num2)
    out$street_name <- if (nzchar(st_name)) st_name else NA_character_
    return(out)
  }

  # Case C: explicit flat-type marker anywhere — "UNIT 6019 ..." or attached
  # "U6019 ..." — possibly preceded by a building name and/or the street
  # number (e.g. "6 Unit 6019 Parkland Bvd" or "5 Blind Road Unit 6019 6
  # Parkland Bvd"). Whichever marker sits closest to the street name wins.
  ft_alt_re <- paste0("\\b(", ft_alt, ")\\s+(\\d+[A-Z]?)\\b")
  ft_alt_m  <- regexpr(ft_alt_re, s, perl = TRUE)
  u_re <- "\\bU(\\d+)\\b"
  u_m  <- regexpr(u_re, s, perl = TRUE)

  use_kw <- ft_alt_m > 0L && (u_m <= 0L || ft_alt_m >= u_m)
  use_u  <- !use_kw && u_m > 0L

  if (use_kw || use_u) {
    if (use_kw) {
      cap  <- regmatches(s, regexec(ft_alt_re, s, perl = TRUE))[[1L]]
      mpos <- ft_alt_m
      mlen <- attr(ft_alt_m, "match.length")
      out$flat_type   <- unname(ft_map[[cap[[2L]]]])
      out$flat_number <- cap[[3L]]
    } else {
      cap  <- regmatches(s, regexec(u_re, s, perl = TRUE))[[1L]]
      mpos <- u_m
      mlen <- attr(u_m, "match.length")
      out$flat_type   <- "UNIT"
      out$flat_number <- cap[[2L]]
    }
    pre  <- trimws(substr(s, 1L, mpos - 1L))
    post <- trimws(substr(s, mpos + mlen, nchar(s)))

    # The street number sits on whichever side of the marker carries one —
    # immediately before it ("6 UNIT 6019 Parkland") or immediately after
    # ("U10 ... UNIT 6019 6 Parkland" / "U6019 6 Parkland").
    pre_m  <- regmatches(pre,  regexec("^(.*?)\\b(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s*$", pre,  perl = TRUE))[[1L]]
    post_m <- regmatches(post, regexec("^(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(.+)$",      post, perl = TRUE))[[1L]]

    if (length(pre_m) == 3L) {
      out$building_name <- if (nzchar(trimws(pre_m[[2L]]))) trimws(pre_m[[2L]]) else NA_character_
      out <- .apply_parsed_number(out, pre_m[[3L]])
      out$street_name <- if (nzchar(post)) post else NA_character_
    } else if (length(post_m) == 3L) {
      if (nzchar(pre)) out$building_name <- pre
      out <- .apply_parsed_number(out, post_m[[2L]])
      out$street_name <- if (nzchar(post_m[[3L]])) post_m[[3L]] else NA_character_
    } else {
      if (nzchar(pre))  out$building_name <- pre
      out$street_name  <- if (nzchar(post)) post else NA_character_
    }
    return(out)
  }

  # Case D: plain "[building] NUM[-NUM] STREETNAME" — no flat info present.
  # Numbers may carry a trailing alpha suffix (e.g. 190A); .apply_parsed_number
  # strips it before as.integer().
  m_num <- regexpr("(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+", s, perl = TRUE)
  if (m_num > 0L) {
    num_end <- m_num + attr(m_num, "match.length") - 1L

    if (m_num > 1L) {
      bld <- trimws(substr(s, 1L, m_num - 1L))
      if (nzchar(bld)) out$building_name <- bld
    }

    num_str <- trimws(substr(s, m_num, num_end))
    rest    <- trimws(substr(s, num_end + 1L, nchar(s)))
    out <- .apply_parsed_number(out, num_str)
    out$street_name <- if (nzchar(rest)) rest else NA_character_

    # If the street name starts with a letter+digit flat designator — e.g.
    # "A1 TAVISTOCK" from "36 A1 TAVISTOCK ST" — split it off as a flat
    # identifier. Only fires when no flat has already been captured and a
    # word follows the designator (so "36 B4" without a name is left alone).
    if (!is.na(out$street_name) && is.na(out$flat_number)) {
      fn_m <- regmatches(out$street_name,
        regexec("^([A-Z])(\\d+[A-Z]?)\\s+(\\S.*)$", out$street_name,
                perl = TRUE))[[1L]]
      if (length(fn_m) == 4L) {
        letter_fn <- fn_m[[2L]]
        out$flat_type   <- if (!is.na(.ATT_FLAT_MAP[letter_fn]))
                             unname(.ATT_FLAT_MAP[letter_fn]) else "UNIT"
        out$flat_number <- fn_m[[3L]]
        out$street_name <- fn_m[[4L]]
      }
    }
  } else {
    # No numeric token — entire remaining is street name (or building name fallback)
    out$street_name <- if (nzchar(s)) s else NA_character_
  }

  out
}

# Vectorized split of "NUM[-NUM]" tokens (numbers may carry a trailing alpha
# suffix, e.g. "190A", "3A-5B") into first / last / suffix components.
.split_number_vec <- function(x) {
  head_tok <- sub("-.*$", "", x)
  first    <- as.integer(sub("[A-Z]+$", "", head_tok))
  sfx      <- sub("^\\d+([A-Z]?).*$", "\\1", head_tok)
  suffix   <- fifelse(nzchar(sfx), sfx, NA_character_)
  last     <- rep(NA_integer_, length(x))
  has_r    <- grepl("-", x, fixed = TRUE)
  last[has_r] <- as.integer(sub("[A-Z]+$", "", sub("^.*-", "", x[has_r])))
  list(first = first, last = last, suffix = suffix)
}

# Fill number_first / number_last / number_suffix on a `.parse_before` result
# list from a "NUM[-NUM]" token.
.apply_parsed_number <- function(out, num_str) {
  p <- .split_number_vec(num_str)
  out$number_first  <- p$first
  out$number_last   <- p$last
  out$number_suffix <- p$suffix
  out
}

# Fuzzy street-type fallback: score every word's best Jaro-Winkler similarity
# to a known type key and take the GLOBAL best (ties broken by the rightmost
# word, since the street-type token structurally sits closest to the
# locality). Handles common misspellings: RODE → ROAD, STEET → STREET,
# AVNUE → AVENUE, BVDZ → BVD.
#
# Picking the first word to merely clear the threshold (rather than the best
# overall) misfires on streetnames that happen to resemble a type abbreviation
# — e.g. "PARKLAND" ~ "PARADE" (sim 0.87) would be chosen over the actual
# misspelled type "BVDZ" ~ "BVD" (sim 0.94) later in the same address.
# Returns list(before, canonical, after) or NULL if no confident match found.
.parse_fuzzy_street <- function(addr, st_map, threshold = 0.85) {
  words <- strsplit(trimws(addr), "\\s+", perl = TRUE)[[1L]]
  n     <- length(words)
  if (n < 2L) return(NULL)

  cand <- which(!grepl("^[0-9]", words, perl = TRUE))
  if (length(cand) == 0L) return(NULL)

  st_keys  <- names(st_map)
  sims     <- fast.string::jaro_winkler_matrix(words[cand], st_keys, p = 0.1)
  key_j    <- max.col(sims, ties.method = "first")
  best_per <- sims[cbind(seq_along(cand), key_j)]

  best_sim <- max(best_per)
  if (best_sim < threshold) return(NULL)

  pick <- utils::tail(which(best_per == best_sim), 1L)  # rightmost word on ties
  best_idx <- cand[pick]
  best_key <- st_keys[key_j[pick]]

  canonical <- unname(st_map[best_key])
  before    <- trimws(paste(words[seq_len(best_idx - 1L)], collapse = " "))
  after     <- if (best_idx < n) trimws(paste(words[seq(best_idx + 1L, n)], collapse = " ")) else ""
  list(before = before, canonical = canonical, after = after)
}
