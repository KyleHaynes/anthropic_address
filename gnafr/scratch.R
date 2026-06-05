require(data.table)
d = fread("C:\\temp\\gnaf.qld.csv")

## Benchmarks
# 100k
result_dt <- gnaf_match(
        con,
        simulated_inputs[[1]]$simulated_address,
        max_results = 1L,
        min_score = 40L,
        verbose = TRUE
)

result_dt[]
