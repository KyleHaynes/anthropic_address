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

  normalized <- .normalize_addr(addresses)

  # Build one 1-row data.table per address so rbindlist always gets data.tables
  # (rbindlist on plain R lists has inconsistent class behaviour across versions)
  parsed <- lapply(seq_along(normalized), function(i) {
    r <- .parse_single(normalized[i], st_regex, st_map, ft_re, ft_map)
    r$input_id  <- i
    r$input_raw <- addresses[i]
    do.call(data.table, r)
  })

  dt <- rbindlist(parsed)
  setcolorder(dt, c("input_id", "input_raw",
                    "in_postcode", "in_state", "in_locality",
                    "in_street_name", "in_street_type", "in_street_suffix",
                    "in_number_first", "in_number_last",
                    "in_flat_type", "in_flat_number", "in_building_name"))
  dt
}

# ---------------------------------------------------------------------------
# Internal: parse a single normalised address string
# ---------------------------------------------------------------------------
.parse_single <- function(addr, st_regex, st_map, ft_re, ft_map) {
  out <- list(
    in_postcode      = NA_integer_,
    in_state         = NA_character_,
    in_locality      = NA_character_,
    in_street_name   = NA_character_,
    in_street_type   = NA_character_,
    in_street_suffix = NA_character_,
    in_number_first  = NA_integer_,
    in_number_last   = NA_integer_,
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

  # 3. Street type — rightmost match with word boundaries
  st_all <- gregexpr(st_regex, addr, perl = TRUE)[[1L]]
  if (st_all[1L] <= 0L) {
    # No street type found — store whole remaining as locality
    out$in_locality <- trimws(addr)
    return(out)
  }

  last <- length(st_all)
  st_start  <- st_all[last]
  st_len    <- attr(st_all, "match.length")[last]
  st_raw    <- substr(addr, st_start, st_start + st_len - 1L)
  out$in_street_type <- unname(st_map[st_raw])

  before    <- trimws(substr(addr, 1L, st_start - 1L))
  after_raw <- trimws(substr(addr, st_start + st_len, nchar(addr)))

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
  out$in_street_name  <- bp$street_name
  out$in_number_first <- bp$number_first
  out$in_number_last  <- bp$number_last
  out$in_flat_type    <- bp$flat_type
  out$in_flat_number  <- bp$flat_number
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
    street_name  = NA_character_,
    number_first = NA_integer_,
    number_last  = NA_integer_,
    flat_type    = NA_character_,
    flat_number  = NA_character_,
    building_name = NA_character_
  )

  s <- trimws(s)
  if (!nzchar(s)) return(out)

  # Case A: slash notation anywhere — "building 110/120 street" or "110/120 street"
  m_slash <- regexpr("(\\d+)/(\\d+(?:-\\d+)?)", s, perl = TRUE)
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

    num_parts <- strsplit(parts[2L], "-", fixed = TRUE)[[1L]]
    out$number_first <- as.integer(num_parts[1L])
    if (length(num_parts) > 1L) out$number_last <- as.integer(num_parts[2L])

    out$street_name <- trimws(substr(s, slash_end + 1L, nchar(s)))
    if (!nzchar(out$street_name)) out$street_name <- NA_character_
    return(out)
  }

  # Case B: "U\d+" attached (e.g. "U110 120 STREET")
  m_u <- regexpr("^U(\\d+)\\s+", s, perl = TRUE)
  if (m_u > 0L) {
    cap <- regmatches(s, regexec("^U(\\d+)\\s+", s, perl = TRUE))[[1L]]
    out$flat_type   <- "UNIT"
    out$flat_number <- cap[2L]
    s <- trimws(substr(s, attr(m_u, "match.length") + 1L, nchar(s)))
  } else {
    # Case C: "UNIT 110 ..." or "APT 3 ..."
    m_ft <- regexpr(ft_re, s, perl = TRUE)
    if (m_ft > 0L) {
      cap <- regmatches(s, regexec(ft_re, s, perl = TRUE))[[1L]]
      out$flat_type   <- unname(ft_map[cap[2L]])
      out$flat_number <- cap[3L]
      s <- trimws(substr(s, attr(m_ft, "match.length") + 1L, nchar(s)))
    }
  }

  # Remaining: [building_name] number[-number] street_name
  # Find first numeric token
  m_num <- regexpr("(\\d+(?:-\\d+)?)\\s+", s, perl = TRUE)
  if (m_num > 0L) {
    num_end <- m_num + attr(m_num, "match.length") - 1L

    if (m_num > 1L) {
      bld <- trimws(substr(s, 1L, m_num - 1L))
      if (nzchar(bld)) out$building_name <- bld
    }

    num_str <- trimws(substr(s, m_num, num_end))
    rest    <- trimws(substr(s, num_end + 1L, nchar(s)))

    # Implied flat: no flat number found yet and rest begins with another numeric token.
    # "10 120 MUSGRAVE" and "10 110-120 MUSGRAVE" follow Australian convention where
    # the first bare number is the unit/flat and the second is the street number.
    m_num2 <- if (is.na(out$flat_number) && nzchar(rest))
      regexpr("^(\\d+(?:-\\d+)?)\\s+", rest, perl = TRUE)
    else -1L

    if (m_num2 > 0L) {
      num2_end  <- m_num2 + attr(m_num2, "match.length") - 1L
      out$flat_type   <- "UNIT"
      out$flat_number <- num_str
      num2_str  <- trimws(substr(rest, 1L, num2_end))
      rest      <- trimws(substr(rest, num2_end + 1L, nchar(rest)))
      num_parts <- strsplit(num2_str, "-", fixed = TRUE)[[1L]]
      out$number_first <- as.integer(num_parts[1L])
      out$number_last  <- if (length(num_parts) > 1L) as.integer(num_parts[2L]) else NA_integer_
    } else {
      num_parts <- strsplit(num_str, "-", fixed = TRUE)[[1L]]
      out$number_first <- as.integer(num_parts[1L])
      if (length(num_parts) > 1L) out$number_last <- as.integer(num_parts[2L])
    }

    out$street_name <- if (nzchar(rest)) rest else NA_character_
  } else {
    # No numeric token — entire remaining is street name (or building name fallback)
    out$street_name <- if (nzchar(s)) s else NA_character_
  }

  out
}
