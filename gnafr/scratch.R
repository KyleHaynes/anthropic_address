require(data.table)

con <- gnaf_connect("C:/temp/gnaf.duckdb")

## Benchmarks
# 100k
d = fread("C:\\temp\\gnaf.qld.csv")
simulated_inputs = address_perturb_sample(
    d,
    n = 1E3,
    seed = 1
)
result_dt <- gnaf_match(
        con,
        simulated_inputs$simulated_address[1:100],
        max_results = 1L,
        min_score = 80L,
        verbose = TRUE
)

result_dt[]


## App -----------------------------------
gnaf_app(db_path = "C:/temp/gnaf.duckdb")