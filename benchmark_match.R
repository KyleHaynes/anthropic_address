# Optional seeded round-trip benchmark against a real database.
# Set GNAFR_BENCH_DB to a gnafr DuckDB path before running this script.
devtools::load_all(".", quiet = TRUE)
library(data.table)

db_path <- Sys.getenv("GNAFR_BENCH_DB")
if (!nzchar(db_path)) {
  message("GNAFR_BENCH_DB is not set; skipping the database benchmark.")
  quit(save = "no", status = 0L)
}

run_benchmark <- function() {
n <- as.integer(Sys.getenv("GNAFR_MATCH_BENCH_N", "1000"))
seed <- as.integer(Sys.getenv("GNAFR_MATCH_BENCH_SEED", "42"))
con <- gnaf_connect(db_path, read_only = TRUE)
on.exit(gnaf_disconnect(con), add = TRUE)

sample_sql <- sprintf(
  paste(
    "SELECT * FROM (",
    "  SELECT * FROM gnaf_addresses",
    "  WHERE alias_type IS NULL AND address_label IS NOT NULL",
    ") core USING SAMPLE reservoir(%d ROWS) REPEATABLE (%d)"
  ), n, seed
)
source_rows <- as.data.table(DBI::dbGetQuery(con, sample_sql))
inputs <- address_perturb_sample(
  source_rows, n = min(n, nrow(source_rows)), seed = seed, max_changes = 2L
)

elapsed <- system.time(matches <- gnaf_match(
  inputs$simulated_address, con,
  max_results = 1L, min_score = 60L,
  cache = FALSE, verbose = FALSE
))[["elapsed"]]

top <- matches[match_rank %in% 1L]
resolved_pid <- fifelse(
  !is.na(top$principal_pid), top$principal_pid, top$address_detail_pid
)
summary <- data.table(
  inputs = nrow(inputs),
  elapsed_seconds = elapsed,
  inputs_per_second = round(nrow(inputs) / elapsed),
  matched_rate = uniqueN(top$input_id) / nrow(inputs),
  exact_pid_rate = sum(
    top$address_detail_pid == inputs$address_detail_pid[top$input_id],
    na.rm = TRUE
  ) / nrow(inputs),
  resolved_pid_rate = sum(
    resolved_pid == inputs$address_detail_pid[top$input_id],
    na.rm = TRUE
  ) / nrow(inputs)
)
print(summary)

if (identical(Sys.getenv("GNAFR_EXPLAIN"), "1") && nrow(inputs) > 0L) {
  probe <- address_parse(inputs$simulated_address[[1L]], normalize = FALSE)
  plan <- DBI::dbGetQuery(con, sprintf(
    paste(
      "EXPLAIN ANALYZE SELECT address_detail_pid",
      "FROM gnaf_addresses",
      "WHERE postcode = %d AND number_first = %d"
    ), probe$in_postcode, probe$in_number_first
  ))
  print(plan)
}
}

run_benchmark()
