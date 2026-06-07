require(data.table)
# require(gnafr)
devtools::document()

con <- gnaf_connect("C:/temp/gnaf.duckdb")

## Benchmarks
# 100k
d = fread("C:\\temp\\gnaf.qld.csv")
simulated_inputs = address_perturb_sample(
    d,
    n = 1E6,
    seed = 3
)
saveRDS(simulated_inputs, "simulated_inputs.rds")

simulated_inputs <- readRDS("simulated_inputs.rds")
result_dt <- gnaf_match(
        c(simulated_inputs$simulated_address[200000:300000]),
        con,
        max_results = 1L,
        min_score = 70L,
        verbose = TRUE,
        cache = FALSE # Turning off to benchmark bad addresses
)

simulated_inputs[, no_match := fifelse(simulated_address %in% result_dt[(!matched)]$input_raw, T, F)]
simulated_inputs[(no_match), .(simulated_address, ADDRESS_LABEL, perturbations)]

gnaf_match(c("43 THE PINNACLE, WORONGARY QLD 4213", ""), con)



# Test "perfect" addresses
result_dt <- gnaf_match(
        d$ADDRESS_LABEL[1:20000],
        con,
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
        c(simulated_inputs$simulated_address[1:25], d$ADDRESS_LABEL[1:25]),
        con,
        max_results = 1L,
        min_score = 80L,
        verbose = TRUE
)


gnaf_match("25 ST JAMES CR EAGLE HEIGHTS QLD 4271", con = con, weights = list(postcode = 20L, suburb = 15L, street_name = 40L, street_type = 10L, number = 10L, flat = 5L))
gnaf_match("25 ST JAMES CR EAGLE HEIGHTS QLD 4271", con = con, max_results = 1)$address_label
 input_id                             input_raw                           input_standardised address_label


test <- gnaf_match(c(
    "25 ST JAMES CR EAGLE HEIGHTS QLD 4271",
    "25 ST JAMES CR EAGLE HEIGHTS 4271 QLD",
    "110-120 MUSGRADE RD RED HILL QLD 4060",
    "112 MUSGRAVE RD RED HILLS QLD 4059",
    "112 MUSGRAVE RD PADDINGTON 4059",
    "PARLAND 6019/6 PARKLAND BVD BRISBANE QLD 4001",
    "PARLAND 6019 6 PARKLAND BVD BRISBANE QLD 4001",
    "U6019 6 PARKLAND BVD BRISBANE QLD 4001"
), con = con, max_results = 1)

# Fall back on street type. Kinda not ideal to pass both arguments
gnaf_match("190 MUSGRAVE RD RED HILL 4059 QLD", con, alias_types = c("street_only"), street_only_fallback = T)
gnaf_match("190a MUSGRAVE RD RED HILL 4059 QLD", con, alias_types = c(NA), street_only_fallback = T)


d[ADDRESS_LABEL %plike% "\\d\\D "]
