# ---- Benchmark setup ----

# This script assumes the following objects already exist in the session:
# - d: a data.table with ADDRESS_LABEL or address_label.
# - con: an open DuckDB connection created with gnaf_connect().

library(data.table)
library(gnafr)

benchmark_sizes <- c(100L, 1000L, 10000L, 100000L)

# ---- Simulate benchmark inputs ----

simulated_inputs <- lapply(benchmark_sizes, function(n_rows) {
    address_perturb_sample(
        d,
        n = n_rows,
        seed = n_rows
    )
})
names(simulated_inputs) <- as.character(benchmark_sizes)

# ---- Run one-shot benchmarks ----

benchmark_results <- rbindlist(lapply(benchmark_sizes, function(n_rows) {
    dt_sim <- simulated_inputs[[as.character(n_rows)]]

    started_at <- Sys.time()
    result_dt <- gnaf_match(
        con,
        dt_sim$simulated_address,
        max_results = 1L,
        min_score = 40L,
        verbose = FALSE
    )
    elapsed_secs <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))

    data.table(
        n = n_rows,
        elapsed_secs = elapsed_secs,
        matched_inputs = uniqueN(result_dt[matched %in% TRUE, input_id]),
        total_inputs = uniqueN(result_dt$input_id),
        match_rate = round(mean(result_dt$matched %in% TRUE) * 100, 1)
    )
}))

# ---- Inspect results ----

benchmark_results[]