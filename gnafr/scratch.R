require(data.table)
# require(gnafr)
devtools::document()

con <- gnaf_connect("C:/temp/gnaf.duckdb")

## Benchmarks
# 100k
d = fread("C:\\temp\\gnaf.qld.csv")
simulated_inputs = address_perturb_sample(
    d,
    n = 2E3,
    seed = 2
)
result_dt <- gnaf_match(
        con,
        c(simulated_inputs$simulated_address[1:200], d$ADDRESS_LABEL[400:700]),
        max_results = 1L,
        min_score = 80L,
        verbose = TRUE
)

# Test "perfect" addresses
result_dt <- gnaf_match(
        con,
        d$ADDRESS_LABEL[1:20000],
        max_results = 1L,
        min_score = 80L,
        verbose = TRUE
)


result_dt[]

require(data.table.ext)

result_dt[]            
Error in class_colors[[tok]] : subscript out of bounds

## App -----------------------------------
gnaf_app(db_path = "C:/temp/gnaf.duckdb")

# ---- 2026-06-06 verbose timing breakdown ----
# Checks that gnaf_match() now reports parse, input standardisation,
# exact/cache/slow-path, and final wrangling timings separately.
result_dt <- gnaf_match(
        con,
        c(simulated_inputs$simulated_address[1:25], d$ADDRESS_LABEL[1:25]),
        max_results = 1L,
        min_score = 80L,
        verbose = TRUE
)


gnaf_match("190 MUSGRAVE ROAD, RED 4060", con = con, weights = list(postcode = 20L, suburb = 15L, street_name = 40L, street_type = 10L, number = 10L, flat = 5L))
