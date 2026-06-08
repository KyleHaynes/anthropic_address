#' Parse a vector of address strings into structured components
#'
#' Handles common Australian address formats including unit/flat prefixes,
#' slash notation (110/120), attached prefixes (U110), building names, and
#' street number ranges (13-27).
#'
#' @param addresses Character vector of raw address strings.
#' @return A \code{data.table} with one row per input and columns:
#'   \code{input_id}, \code{input_raw}, \code{in_postcode}, \code{in_state},
#'   \code{in_locality}, \code{in_street_name}, \code{in_street_type},
#'   \code{in_street_suffix}, \code{in_number_first}, \code{in_number_last},
#'   \code{in_flat_type}, \code{in_flat_number}, \code{in_building_name}.
#' @export
address_parse <- function(addresses) {
  st_map    <- .get_street_type_map()
  st_regex  <- .build_street_type_regex(st_map)
  ft_map    <- .get_flat_type_map()
  ft_re     <- paste0(
    "^(", paste(names(ft_map)[order(-nchar(names(ft_map)))], collapse = "|"), ")\\s+(\\d+[A-Z]?)\\s+"
  )

  # Comma hint: capture the word immediately before the LAST comma in each
  # raw address (uppercased, periods stripped) before .normalize_addr
  # discards comma positions. In "STREET ADDRESS, LOCALITY ..." formats that
  # word is structurally the street type — useful for disambiguating
  # coincidental abbreviation collisions (e.g. "St" in "St James") and for
  # prioritising fuzzy-matching of misspelt types (e.g. "STX" -> "ST").
  # The LAST comma is used so flat/unit notations like "Unit 5, 10 Smith St,
  # Brisbane ..." don't pick up the flat number instead.
  addr_upper <- gsub("\\.", " ", toupper(trimws(addresses)), perl = TRUE)
  addr_upper <- gsub("\\s+", " ", addr_upper, perl = TRUE)
  cw_cap <- regmatches(addr_upper, regexec("\\b([A-Z0-9]+)\\s*,(?:[^,]*)$", addr_upper, perl = TRUE))
  comma_word <- vapply(cw_cap, function(x) if (length(x) == 2L) x[[2L]] else NA_character_, character(1))

  normalized <- .normalize_addr(addresses)
  dt <- .parse_vectorized(normalized, addresses, st_map, st_regex, ft_map, ft_re, comma_word)
  setcolorder(dt, c("input_id", "input_raw",
                    "in_postcode", "in_state", "in_locality",
                    "in_street_name", "in_street_type", "in_street_suffix",
                    "in_number_first", "in_number_last", "in_number_suffix",
                    "in_flat_type", "in_flat_number", "in_building_name"))
  dt
}

# ---------------------------------------------------------------------------
# Vectorized parser — handles the full address vector in bulk C operations.
# Falls back to .parse_single only for addresses that don't match any fast
# pattern (typically <5% for well-formed Australian addresses).
# ---------------------------------------------------------------------------
.parse_vectorized <- function(normalized, addresses, st_map, st_regex, ft_map, ft_re, comma_word) {
  n <- length(normalized)

  # ------------------------------------------------------------------
  # Stage 1: extract trailing geo-suffix from end.
  # Handles both "STATE POSTCODE" and "POSTCODE STATE" orderings since
  # address_perturb_sample produces both.
  # ------------------------------------------------------------------
  st_abbr <- "(?:QLD|NSW|VIC|SA|WA|TAS|NT|ACT)"
  # Order A: ...STATE POSTCODE$
  a_re <- paste0("(\\b", st_abbr, "\\b)\\s+(\\b\\d{4}\\b)\\s*$")
  a_m  <- regmatches(normalized, regexec(a_re, normalized, perl = TRUE))
  has_a <- lengths(a_m) == 3L

  # Order B: ...POSTCODE STATE$
  b_re <- paste0("(\\b\\d{4}\\b)\\s+(\\b", st_abbr, "\\b)\\s*$")
  b_m  <- regmatches(normalized, regexec(b_re, normalized, perl = TRUE))
  has_b <- lengths(b_m) == 3L & !has_a

  # Order C: ...STATE$ (state present, no postcode)
  c_re <- paste0("(\\b", st_abbr, "\\b)\\s*$")
  c_m  <- regmatches(normalized, regexec(c_re, normalized, perl = TRUE))
  has_c <- lengths(c_m) == 2L & !has_a & !has_b

  # Order D: ...POSTCODE$ (postcode present, no state)
  d_re <- "(\\b\\d{4}\\b)\\s*$"
  d_m  <- regmatches(normalized, regexec(d_re, normalized, perl = TRUE))
  has_d <- lengths(d_m) == 2L & !has_a & !has_b & !has_c

  has_tail <- has_a | has_b | has_c | has_d
  in_state    <- rep(NA_character_, n)
  in_postcode <- rep(NA_integer_,   n)
  if (any(has_a)) {
    in_state[has_a]    <- vapply(a_m[has_a], `[[`, character(1), 2L)
    in_postcode[has_a] <- as.integer(vapply(a_m[has_a], `[[`, character(1), 3L))
  }
  if (any(has_b)) {
    in_postcode[has_b] <- as.integer(vapply(b_m[has_b], `[[`, character(1), 2L))
    in_state[has_b]    <- vapply(b_m[has_b], `[[`, character(1), 3L)
  }
  if (any(has_c)) {
    in_state[has_c] <- vapply(c_m[has_c], `[[`, character(1), 2L)
  }
  if (any(has_d)) {
    in_postcode[has_d] <- as.integer(vapply(d_m[has_d], `[[`, character(1), 2L))
  }

  work <- normalized
  work[has_a] <- trimws(sub(a_re, "", work[has_a], perl = TRUE))
  work[has_b] <- trimws(sub(b_re, "", work[has_b], perl = TRUE))
  work[has_c] <- trimws(sub(c_re, "", work[has_c], perl = TRUE))
  work[has_d] <- trimws(sub(d_re, "", work[has_d], perl = TRUE))

  # ------------------------------------------------------------------
  # Stage 2: rightmost street type in the remaining string.
  # gregexpr applies to the full vector in one compiled-regex pass.
  # ------------------------------------------------------------------
  st_m   <- gregexpr(st_regex, work, perl = TRUE)
  st_pos <- vapply(st_m, function(m) {
    p <- m[m > 0L]; if (length(p) == 0L) NA_integer_ else tail(p, 1L)
  }, integer(1))
  st_len <- mapply(function(m, pos) {
    if (is.na(pos)) return(NA_integer_)
    attr(m, "match.length")[length(attr(m, "match.length"))]
  }, st_m, st_pos, SIMPLIFY = TRUE)

  has_st         <- !is.na(st_pos)
  st_raw         <- ifelse(has_st, substr(work, st_pos, st_pos + st_len - 1L), NA_character_)
  in_street_type <- unname(st_map[st_raw])

  # Comma hint: when the word immediately before the (last) comma in the
  # original input differs from the rightmost exact-match street type and
  # plausibly looks like a type itself (exact key, or a close fuzzy match —
  # e.g. "Rode" ~ "Road"), the rightmost-match search has likely landed on a
  # coincidental collision (e.g. "St" inside "St James Rode"). Route these to
  # .parse_single, which re-resolves the type using the comma hint directly.
  has_comma <- !is.na(comma_word)
  mismatch  <- has_st & has_comma & comma_word != st_raw
  needs_fix <- rep(FALSE, n)
  if (any(mismatch)) {
    midx <- which(mismatch)
    cw <- comma_word[midx]
    plausible <- !is.na(unname(st_map[cw]))
    chk <- which(!plausible)
    if (length(chk) > 0L) {
      st_keys <- names(st_map)
      sims <- vapply(cw[chk], function(w)
        max(1 - stringdist::stringdist(w, st_keys, method = "jw", p = 0.1)), numeric(1))
      plausible[chk] <- sims >= 0.85
    }
    needs_fix[midx[plausible]] <- TRUE
  }
  has_st <- has_st & !needs_fix

  before_st_end <- ifelse(has_st, st_pos - 1L, nchar(work))
  before_st     <- trimws(substr(work, 1L, before_st_end))
  before_st[!nzchar(before_st)] <- NA_character_

  after_st_start <- ifelse(has_st, st_pos + st_len, nchar(work) + 1L)
  after_st_raw   <- trimws(substr(work, after_st_start, nchar(work)))
  after_st_raw[!nzchar(after_st_raw) | !has_st] <- NA_character_

  # ------------------------------------------------------------------
  # Stage 3: optional street suffix then locality from after_st_raw.
  # ------------------------------------------------------------------
  sfx_re <- "^(NORTH|SOUTH|EAST|WEST|UPPER|LOWER|INNER|OUTER)\\b"
  sfx_m  <- regexpr(sfx_re, ifelse(is.na(after_st_raw), "", after_st_raw), perl = TRUE)
  has_sfx <- sfx_m > 0L & !is.na(after_st_raw)

  in_street_suffix <- ifelse(has_sfx,
    substr(after_st_raw, 1L, attr(sfx_m, "match.length")),
    NA_character_)
  loc_raw <- ifelse(has_sfx,
    trimws(sub(sfx_re, "", after_st_raw, perl = TRUE)),
    after_st_raw)
  in_locality <- ifelse(!is.na(loc_raw) & nzchar(loc_raw), loc_raw, NA_character_)

  # ------------------------------------------------------------------
  # Stage 4: parse the before-street-type for number / flat / name.
  # Three vectorized patterns cover the common cases:
  #   4a — slash notation "FLAT/NUM STREETNAME" (unit addresses)
  #   4b — flat-type prefix "UNIT 3 NUM STREETNAME"
  #   4c — simple "NUM[-NUM] STREETNAME"  ← ~70% of all addresses
  # Anything else falls back to .parse_single.
  # ------------------------------------------------------------------
  bst <- ifelse(is.na(before_st), "", before_st)

  in_street_name   <- rep(NA_character_, n)
  in_number_first  <- rep(NA_integer_,   n)
  in_number_last   <- rep(NA_integer_,   n)
  in_number_suffix <- rep(NA_character_, n)
  in_flat_type     <- rep(NA_character_, n)
  in_flat_number   <- rep(NA_character_, n)
  in_building_name <- rep(NA_character_, n)

  # 4a: slash notation (unit/flat numbers may carry trailing alpha e.g. 3A/190B)
  slash_m  <- regmatches(bst, regexec("^(.*?)(\\d+[A-Z]?)/(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s*(.*)$", bst, perl = TRUE))
  is_slash <- lengths(slash_m) == 6L & has_st
  if (any(is_slash)) {
    bld <- vapply(slash_m[is_slash], `[[`, character(1), 2L)
    in_building_name[is_slash] <- ifelse(nzchar(trimws(bld)), trimws(bld), NA_character_)
    in_flat_type[is_slash]    <- "UNIT"
    in_flat_number[is_slash]  <- vapply(slash_m[is_slash], `[[`, character(1), 3L)
    num_s <- vapply(slash_m[is_slash], `[[`, character(1), 4L)
    in_street_name[is_slash]  <- vapply(slash_m[is_slash], `[[`, character(1), 5L)
    has_r <- grepl("-", num_s, fixed = TRUE)
    in_number_first[is_slash] <- as.integer(sub("[A-Z]+$", "", sub("-.*$", "", num_s)))
    sfx_a <- sub("^\\d+([A-Z]?).*$", "\\1", sub("-.*$", "", num_s))
    in_number_suffix[is_slash] <- ifelse(nzchar(sfx_a), sfx_a, NA_character_)
    tmp <- rep(NA_integer_, sum(is_slash))
    tmp[has_r] <- as.integer(sub("[A-Z]+$", "", sub("^.*-", "", num_s[has_r])))
    in_number_last[is_slash] <- tmp
  }

  # 4b: flat-type prefix ("UNIT 3 ...")  — not slash, has a street type
  remaining_b <- !is_slash & has_st
  flat_m  <- regmatches(bst, regexec(ft_re, bst, perl = TRUE))
  is_flat <- lengths(flat_m) > 2L & remaining_b
  if (any(is_flat)) {
    in_flat_type[is_flat]   <- unname(ft_map[vapply(flat_m[is_flat], `[[`, character(1), 2L)])
    in_flat_number[is_flat] <- vapply(flat_m[is_flat], `[[`, character(1), 3L)
    ft_len <- vapply(flat_m[is_flat], function(m) nchar(m[[1L]]), integer(1))
    rest   <- trimws(substr(bst[is_flat], ft_len + 1L, nchar(bst[is_flat])))
    # parse "NUM[-NUM] STREETNAME" from rest (number may have trailing alpha e.g. 190A)
    sm2 <- regmatches(rest, regexec("^(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(.+)$", rest, perl = TRUE))
    ok2 <- lengths(sm2) == 3L
    num2_raw <- vapply(sm2[ok2], `[[`, character(1), 2L)
    in_number_first[is_flat][ok2]  <- as.integer(sub("[A-Z]+$", "", sub("-.*$", "", num2_raw)))
    sfx_b <- sub("^\\d+([A-Z]?).*$", "\\1", sub("-.*$", "", num2_raw))
    in_number_suffix[is_flat][ok2] <- ifelse(nzchar(sfx_b), sfx_b, NA_character_)
    in_number_last[is_flat][ok2]   <- {
      hr2 <- grepl("-", num2_raw, fixed = TRUE)
      tmp2 <- rep(NA_integer_, sum(ok2))
      tmp2[hr2] <- as.integer(sub("[A-Z]+$", "", sub("^.*-", "", num2_raw[hr2])))
      tmp2
    }
    in_street_name[is_flat][ok2]  <- vapply(sm2[ok2], `[[`, character(1), 3L)
    in_street_name[is_flat][!ok2] <- ifelse(nzchar(rest[!ok2]), rest[!ok2], NA_character_)
  }

  # 4c: simple "NUM[-NUM] STREETNAME" — with implied-flat sub-case:
  #   "NUM1 NUM2[-NUM3] STREETNAME" where NUM1 is unit and NUM2 is street number.
  #   (.parse_before detects this; we replicate it vectorized to avoid fallback overhead.)
  #   Numbers may carry a trailing alpha suffix (e.g. 190A, 10B); strip it before as.integer().
  remaining_c <- !is_slash & !is_flat & has_st
  simple_m <- regmatches(bst, regexec("^(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(.+)$", bst, perl = TRUE))
  is_simple_cand <- lengths(simple_m) == 3L & remaining_c

  # Exclude candidates whose remainder hides an explicit flat-type marker —
  # e.g. "6 UNIT 6019 Parkland Bvd" or "5 Blind Road Unit 6019 6 Parkland
  # Bvd" — these need .parse_before's full marker-search logic, so route them
  # to the (slower) fallback parser instead of mis-reading the marker as part
  # of the street name.
  ft_alt_c <- paste(names(ft_map)[order(-nchar(names(ft_map)))], collapse = "|")
  embed_re <- paste0("\\b(?:", ft_alt_c, ")\\s+\\d+|\\bU\\d+\\b")
  rest_chk <- rep(NA_character_, length(bst))
  rest_chk[is_simple_cand] <- vapply(simple_m[is_simple_cand], `[[`, character(1), 3L)
  has_embedded_flat <- !is.na(rest_chk) & grepl(embed_re, rest_chk, perl = TRUE)
  is_simple <- is_simple_cand & !has_embedded_flat

  if (any(is_simple)) {
    num_s  <- vapply(simple_m[is_simple], `[[`, character(1), 2L)
    rest_s <- vapply(simple_m[is_simple], `[[`, character(1), 3L)
    impl_m <- regmatches(rest_s, regexec("^(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)\\s+(.+)$", rest_s, perl = TRUE))
    is_impl <- lengths(impl_m) == 3L

    if (any(is_impl)) {
      idx <- which(is_simple)[is_impl]
      in_flat_type[idx]   <- "UNIT"
      in_flat_number[idx] <- num_s[is_impl]
      num_s2 <- vapply(impl_m[is_impl], `[[`, character(1), 2L)
      has_r2 <- grepl("-", num_s2, fixed = TRUE)
      in_number_first[idx] <- as.integer(sub("[A-Z]+$", "", sub("-.*$", "", num_s2)))
      sfx_c_impl <- sub("^\\d+([A-Z]?).*$", "\\1", sub("-.*$", "", num_s2))
      in_number_suffix[idx] <- ifelse(nzchar(sfx_c_impl), sfx_c_impl, NA_character_)
      tmp2 <- rep(NA_integer_, sum(is_impl))
      tmp2[has_r2] <- as.integer(sub("[A-Z]+$", "", sub("^.*-", "", num_s2[has_r2])))
      in_number_last[idx]  <- tmp2
      in_street_name[idx]  <- vapply(impl_m[is_impl], `[[`, character(1), 3L)
    }

    ni <- !is_impl
    if (any(ni)) {
      idx <- which(is_simple)[ni]
      num_s_ni <- num_s[ni]
      in_street_name[idx]  <- rest_s[ni]
      has_r <- grepl("-", num_s_ni, fixed = TRUE)
      in_number_first[idx] <- as.integer(sub("[A-Z]+$", "", sub("-.*$", "", num_s_ni)))
      sfx_c_ni <- sub("^\\d+([A-Z]?).*$", "\\1", sub("-.*$", "", num_s_ni))
      in_number_suffix[idx] <- ifelse(nzchar(sfx_c_ni), sfx_c_ni, NA_character_)
      tmp <- rep(NA_integer_, sum(ni))
      tmp[has_r] <- as.integer(sub("[A-Z]+$", "", sub("^.*-", "", num_s_ni[has_r])))
      in_number_last[idx] <- tmp
    }
  }

  # ------------------------------------------------------------------
  # Fallback: any address that didn't hit a fast path above.
  # Typically: no street type found, complex building names, fuzzy street.
  # ------------------------------------------------------------------
  fallback <- which(!is_slash & !is_flat & !is_simple)
  if (length(fallback) > 0L) {
    fb <- lapply(fallback, function(i) {
      .parse_single(normalized[[i]], st_regex, st_map, ft_re, ft_map, comma_word[[i]])
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
    in_building_name = in_building_name
  )
}

# ---------------------------------------------------------------------------
# Internal: parse a single normalised address string
# ---------------------------------------------------------------------------
.parse_single <- function(addr, st_regex, st_map, ft_re, ft_map, comma_word = NA_character_) {
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
    in_building_name = NA_character_
  )

  if (!nzchar(addr)) return(out)

  # 1. Postcode: last 4-digit token in string
  pc_all <- gregexpr("\\b\\d{4}\\b", addr, perl = TRUE)[[1L]]
  if (pc_all[1L] > 0L) {
    last <- length(pc_all)
    pc_start <- pc_all[last]
    out$in_postcode <- as.integer(substr(addr, pc_start, pc_start + 3L))
    addr <- trimws(gsub(substr(addr, pc_start, pc_start + 3L), "", addr, fixed = TRUE))
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
        sims <- 1 - stringdist::stringdist(comma_word, names(st_map), method = "jw", p = 0.1)
        j <- which.max(sims)
        if (sims[[j]] >= 0.85) cw_canon <- unname(st_map[[names(st_map)[[j]]]])
      }
      if (!is.na(cw_canon)) {
        cw_start <- tail(cw_all[cw_all > 0L], 1L)
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
        bp <- .parse_before(addr, ft_re, ft_map)
        out$in_number_first  <- bp$number_first
        out$in_number_last   <- bp$number_last
        out$in_number_suffix <- bp$number_suffix
        out$in_flat_type     <- bp$flat_type
        out$in_flat_number   <- bp$flat_number
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
  bp <- .parse_before(before, ft_re, ft_map)
  out$in_street_name   <- bp$street_name
  out$in_number_first  <- bp$number_first
  out$in_number_last   <- bp$number_last
  out$in_number_suffix <- bp$number_suffix
  out$in_flat_type     <- bp$flat_type
  out$in_flat_number   <- bp$flat_number
  out$in_building_name <- bp$building_name

  out
}

# ---------------------------------------------------------------------------
# Internal: parse the text that precedes the street type
# Returns a list with: street_name, number_first, number_last,
#                      flat_type, flat_number, building_name
# ---------------------------------------------------------------------------
.parse_before <- function(s, ft_re, ft_map) {
  out <- list(
    street_name   = NA_character_,
    number_first  = NA_integer_,
    number_last   = NA_integer_,
    number_suffix = NA_character_,
    flat_type     = NA_character_,
    flat_number   = NA_character_,
    building_name = NA_character_
  )

  s <- trimws(s)
  if (!nzchar(s)) return(out)

  ft_alt <- paste(names(ft_map)[order(-nchar(names(ft_map)))], collapse = "|")

  # Case A: slash notation anywhere — "building 110/120 street" or "110/120 street"
  # Unit and street numbers may have trailing alpha (e.g. 3A/190B).
  m_slash <- regexpr("(\\d+[A-Z]?)/(\\d+[A-Z]?(?:-\\d+[A-Z]?)?)", s, perl = TRUE)
  if (m_slash > 0L) {
    slash_end <- m_slash + attr(m_slash, "match.length") - 1L
    slash_str <- substr(s, m_slash, slash_end)

    if (m_slash > 1L) {
      out$building_name <- trimws(substr(s, 1L, m_slash - 1L))
      if (!nzchar(out$building_name)) out$building_name <- NA_character_
    }

    parts <- strsplit(slash_str, "/", fixed = TRUE)[[1L]]
    out$flat_type   <- "UNIT"
    out$flat_number <- parts[1L]
    out <- .apply_parsed_number(out, parts[2L])

    out$street_name <- trimws(substr(s, slash_end + 1L, nchar(s)))
    if (!nzchar(out$street_name)) out$street_name <- NA_character_
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
  } else {
    # No numeric token — entire remaining is street name (or building name fallback)
    out$street_name <- if (nzchar(s)) s else NA_character_
  }

  out
}

# Fill number_first / number_last / number_suffix on a `.parse_before` result
# list from a "NUM[-NUM]" token (numbers may carry a trailing alpha suffix,
# e.g. "190A", "3A-5B").
.apply_parsed_number <- function(out, num_str) {
  num_parts <- strsplit(num_str, "-", fixed = TRUE)[[1L]]
  out$number_first  <- as.integer(sub("[A-Z]+$", "", num_parts[1L]))
  sfx <- sub("^\\d+([A-Z]?).*$", "\\1", num_parts[1L])
  out$number_suffix <- if (nzchar(sfx)) sfx else NA_character_
  if (length(num_parts) > 1L) out$number_last <- as.integer(sub("[A-Z]+$", "", num_parts[2L]))
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

  st_keys <- names(st_map)

  best_sim <- -1
  best_idx <- NA_integer_
  best_key <- NA_character_

  for (i in seq_len(n)) {
    w <- words[[i]]
    if (grepl("^[0-9]", w, perl = TRUE)) next

    sims <- 1 - stringdist::stringdist(w, st_keys, method = "jw", p = 0.1)
    j <- which.max(sims)

    if (sims[[j]] >= best_sim) {
      best_sim <- sims[[j]]
      best_idx <- i
      best_key <- st_keys[[j]]
    }
  }

  if (is.na(best_idx) || best_sim < threshold) return(NULL)

  canonical <- unname(st_map[best_key])
  before    <- trimws(paste(words[seq_len(best_idx - 1L)], collapse = " "))
  after     <- if (best_idx < n) trimws(paste(words[seq(best_idx + 1L, n)], collapse = " ")) else ""
  list(before = before, canonical = canonical, after = after)
}

