#' Sample and perturb address labels for bulk-match testing
#'
#' Draws a sample of addresses from a GNAF-like `data.table` and applies small,
#' realistic string perturbations to the address label so you can test fuzzy and
#' bulk matching on messier inputs.
#'
#' Supported perturbations include abbreviation swaps (`ROAD` -> `RD`), comma
#' removal, case changes, whitespace noise, dropped unit/building prefixes, and
#' small single-character typos.
#'
#' @param x A `data.table` or `data.frame` containing either `ADDRESS_LABEL` or
#'   `address_label`.
#' @param n Number of rows to sample. Defaults to `min(1000L, nrow(x))`.
#' @param replace Sample with replacement. Default `FALSE`.
#' @param seed Optional random seed for reproducible output.
#' @param max_changes Maximum number of perturbations to apply per address.
#'   Default `2L`.
#' @param keep_original If `TRUE` (default), includes the original label in the
#'   output.
#' @return A `data.table` containing sampled rows plus a `simulated_address`
#'   column and a `perturbations` column describing what changed.
#' @export
address_perturb_sample <- function(x, n = min(1000L, nrow(x)), replace = FALSE,
                                   seed = NULL, max_changes = 2L,
                                   keep_original = TRUE) {

  if (!is.data.frame(x)) {
    stop("'x' must be a data.frame or data.table")
  }
  if (nrow(x) == 0L) {
    stop("'x' must have at least one row")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  label_col <- if ("ADDRESS_LABEL" %in% names(x)) {
    "ADDRESS_LABEL"
  } else if ("address_label" %in% names(x)) {
    "address_label"
  } else {
    stop("'x' must contain 'ADDRESS_LABEL' or 'address_label'")
  }

  n <- as.integer(n)
  max_changes <- as.integer(max_changes)
  if (is.na(n) || n <= 0L) {
    stop("'n' must be a positive integer")
  }
  if (!replace && n > nrow(x)) {
    stop("'n' cannot exceed nrow(x) when replace = FALSE")
  }
  if (is.na(max_changes) || max_changes < 1L) {
    stop("'max_changes' must be at least 1")
  }

  dt <- as.data.table(x)
  idx <- sample.int(nrow(dt), size = n, replace = replace)
  out <- copy(dt[idx])

  if (!keep_original && label_col != "simulated_address") {
    out[, (label_col) := NULL]
  }

  perturbed <- lapply(dt[[label_col]][idx], function(label) {
    .perturb_address_label(label, max_changes = max_changes)
  })

  out[, simulated_address := vapply(perturbed, `[[`, character(1), "label")]
  out[, perturbations := vapply(perturbed, function(item) {
    if (length(item$changes) == 0L) "none" else paste(item$changes, collapse = ", ")
  }, character(1))]
  out[]
}

.perturb_address_label <- function(label, max_changes = 2L) {
  current <- as.character(label %||% "")
  if (!nzchar(trimws(current))) {
    return(list(label = current, changes = character()))
  }

  ops <- list(
    street_type_abbrev = .perturb_street_type_abbrev,
    remove_commas = .perturb_remove_commas,
    case_noise = .perturb_case_noise,
    spacing_noise = .perturb_spacing_noise,
    drop_unit_prefix = .perturb_drop_unit_prefix,
    drop_building_name = .perturb_drop_building_name,
    suburb_typo = .perturb_locality_typo,
    street_typo = .perturb_street_typo,
    state_postcode_swap = .perturb_state_postcode_swap
  )

  op_names <- sample(names(ops), length(ops))
  changes <- character()
  attempts <- 0L
  limit <- min(max_changes, length(op_names))

  for (op_name in op_names) {
    if (length(changes) >= limit) break
    attempts <- attempts + 1L
    candidate <- ops[[op_name]](current)
    if (!identical(candidate, current)) {
      current <- candidate
      changes <- c(changes, op_name)
    }
  }

  list(label = current, changes = changes)
}

.perturb_street_type_abbrev <- function(label) {
  st_map <- .get_street_type_map()
  canon_to_abbrev <- tapply(names(st_map), st_map, function(values) values[which.min(nchar(values))])
  canonicals <- names(canon_to_abbrev)
  hits <- canonicals[vapply(canonicals, function(value) {
    grepl(paste0("\\b", value, "\\b"), label)
  }, logical(1))]
  if (length(hits) == 0L) return(label)
  chosen <- sample(hits, 1L)
  sub(paste0("\\b", chosen, "\\b"), canon_to_abbrev[[chosen]], label)
}

.perturb_remove_commas <- function(label) {
  if (!grepl(",", label, fixed = TRUE)) return(label)
  gsub(",", "", label, fixed = TRUE)
}

.perturb_case_noise <- function(label) {
  mode <- sample(c("lower", "title"), 1L)
  if (mode == "lower") {
    tolower(label)
  } else {
    tokens <- strsplit(tolower(label), "\\s+")[[1L]]
    paste(tools::toTitleCase(tokens), collapse = " ")
  }
}

.perturb_spacing_noise <- function(label) {
  spaced <- gsub(",", ", ", label, fixed = TRUE)
  spaced <- gsub("/", " / ", spaced, fixed = TRUE)
  gsub("\\s+", " ", spaced)
}

.perturb_drop_unit_prefix <- function(label) {
  patterns <- c(
    "\\bUNIT\\s+([A-Z0-9]+)\\b",
    "\\bAPARTMENT\\s+([A-Z0-9]+)\\b",
    "\\bAPT\\s+([A-Z0-9]+)\\b",
    "\\bSUITE\\s+([A-Z0-9]+)\\b",
    "\\bLEVEL\\s+([A-Z0-9]+)\\b"
  )
  for (pattern in patterns) {
    candidate <- sub(pattern, "\\1", label, perl = TRUE)
    if (!identical(candidate, label)) return(candidate)
  }
  label
}

.perturb_drop_building_name <- function(label) {
  candidate <- sub("^[^,]+?\\s+(UNIT|APT|APARTMENT|SUITE|LEVEL)\\b", "\\1", label, perl = TRUE)
  if (identical(candidate, label)) return(label)
  candidate
}

.perturb_locality_typo <- function(label) {
  .perturb_segment_typo(label, which = "locality")
}

.perturb_street_typo <- function(label) {
  .perturb_segment_typo(label, which = "street")
}

.perturb_segment_typo <- function(label, which = c("locality", "street")) {
  which <- match.arg(which)
  parts <- strsplit(label, ",", fixed = TRUE)[[1L]]
  target_idx <- if (which == "street") {
    if (length(parts) >= 1L) 1L else NA_integer_
  } else {
    if (length(parts) >= 2L) 2L else NA_integer_
  }
  if (is.na(target_idx)) return(label)

  tokens <- strsplit(trimws(parts[target_idx]), "\\s+")[[1L]]
  editable <- which(nchar(tokens) >= 4L & !grepl("^\\d+$", tokens))
  if (length(editable) == 0L) return(label)

  token_idx <- sample(editable, 1L)
  tokens[token_idx] <- .introduce_typo(tokens[token_idx])
  parts[target_idx] <- paste(tokens, collapse = " ")
  trimws(paste(parts, collapse = ", "))
}

.introduce_typo <- function(token) {
  chars <- strsplit(token, "", fixed = TRUE)[[1L]]
  if (length(chars) < 2L) return(token)

  op <- sample(c("swap", "drop"), 1L)
  if (op == "swap" && length(chars) >= 2L) {
    pos <- sample.int(length(chars) - 1L, 1L)
    tmp <- chars[pos]
    chars[pos] <- chars[pos + 1L]
    chars[pos + 1L] <- tmp
  } else {
    pos <- sample.int(length(chars), 1L)
    chars <- chars[-pos]
  }
  paste(chars, collapse = "")
}

.perturb_state_postcode_swap <- function(label) {
  candidate <- sub(
    "\\b(QLD|NSW|VIC|SA|WA|TAS|NT|ACT)\\s+(\\d{4})\\b",
    "\\2 \\1",
    label,
    perl = TRUE
  )
  if (identical(candidate, label)) return(label)
  candidate
}