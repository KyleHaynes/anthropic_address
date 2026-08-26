# Reproducible parser benchmark. Run from the package root with:
#   Rscript bench_parse.R
devtools::load_all(".", quiet = TRUE)
data.table::setDTthreads(1L)

n <- as.integer(Sys.getenv("GNAFR_PARSE_BENCH_N", "10000"))
stopifnot(!is.na(n), n > 0L)

corpora <- list(
  canonical = sprintf("%d SMITH STREET, ST LUCIA QLD 4067", seq_len(n)),
  comma_collision = sprintf(
    "%d MAIN ROAD, SOUTH BRISBANE QLD 4101", seq_len(n)
  ),
  fuzzy_boundary = sprintf(
    "%d SAINT JAMES RODE, TAMBORINE MOUNTAIN QLD 4272", seq_len(n)
  ),
  missing_type = sprintf("%d MUSGRAVE BRISBANE QLD 4000", seq_len(n)),
  flat_level = sprintf(
    "SHOP %d LEVEL 3 52 DAVENPORT ROAD, SOUTH BRISBANE QLD 4101",
    seq_len(n)
  ),
  duplicate_heavy = rep(c(
    "10 SMITH STREET, ST LUCIA QLD 4067",
    "LOT 7 KREIS ROAD, WESTBROOK QLD 4350",
    "SHOP 14 LEVEL 3 52 DAVENPORT ROAD, SOUTH BRISBANE QLD 4101",
    "10 THE AVENUE, WINDSOR QLD 4030"
  ), length.out = max(100000L, n))
)

results <- data.table::rbindlist(lapply(names(corpora), function(name) {
  values <- corpora[[name]]
  gc(FALSE)
  elapsed <- system.time(parsed <- address_parse(values, normalize = FALSE))[["elapsed"]]
  stopifnot(nrow(parsed) == length(values), identical(parsed$input_id, seq_along(values)))
  data.table::data.table(
    corpus = name,
    rows = length(values),
    unique_structures = data.table::uniqueN(values),
    elapsed_seconds = elapsed,
    rows_per_second = round(length(values) / elapsed)
  )
}))

print(results)
