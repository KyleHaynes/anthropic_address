# Focused fast-format regression benchmark.
devtools::load_all(".", quiet = TRUE)
n <- as.integer(Sys.getenv("GNAFR_PARSE_BENCH_N", "10000"))

formats <- list(
  simple = sprintf("%d SMITH ST, BRISBANE QLD 4000", seq_len(n)),
  slash = sprintf("UNIT %d/10 SMITH ST, BRISBANE QLD 4000", seq_len(n)),
  flat = sprintf("UNIT %d 10 SMITH ST, BRISBANE QLD 4000", seq_len(n)),
  range = sprintf("%d-%d SMITH ST, BRISBANE QLD 4000", seq_len(n), seq_len(n) + 2L)
)

timings <- data.table::rbindlist(lapply(names(formats), function(name) {
  elapsed <- system.time(address_parse(formats[[name]], normalize = FALSE))[["elapsed"]]
  data.table::data.table(
    format = name, rows = n, elapsed_seconds = elapsed,
    rows_per_second = round(n / elapsed)
  )
}))
print(timings)
