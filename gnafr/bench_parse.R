devtools::load_all(quiet = TRUE)
sims  <- readRDS("simulated_inputs.rds")
addrs <- c(sims[["simulated_address"]][1:5000])

cat("--- address_parse (vectorized) ---\n")
t0 <- proc.time()[["elapsed"]]
dt_new <- address_parse(addrs)
cat("address_parse 5k:", round(proc.time()[["elapsed"]] - t0, 3), "s\n")
cat("rows:", nrow(dt_new), "\n")
cat("matched street_name:", sum(!is.na(dt_new$in_street_name)), "\n")
cat("matched postcode:   ", sum(!is.na(dt_new$in_postcode)), "\n")

cat("\n--- old lapply path (for comparison) ---\n")
st_map   <- gnafr:::.get_street_type_map()
st_regex <- gnafr:::.build_street_type_regex(st_map)
ft_map   <- gnafr:::.get_flat_type_map()
ft_re    <- paste0("^(", paste(names(ft_map)[order(-nchar(names(ft_map)))], collapse = "|"),
                   ")\\s+(\\d+[A-Z]?)\\s+")
normalized <- gnafr:::.normalize_addr(addrs)
t0 <- proc.time()[["elapsed"]]
old_lists <- lapply(seq_along(normalized), function(i) {
  r <- gnafr:::.parse_single(normalized[[i]], st_regex, st_map, ft_re, ft_map)
  r[["input_id"]]  <- i
  r[["input_raw"]] <- addrs[[i]]
  r
})
dt_old <- data.table::rbindlist(old_lists, use.names = TRUE)
cat("lapply+rbindlist 5k:", round(proc.time()[["elapsed"]] - t0, 3), "s\n")

cat("\n--- correctness spot-check (mismatched rows) ---\n")
cols <- c("in_postcode", "in_state", "in_locality", "in_street_name",
          "in_street_type", "in_number_first", "in_number_last",
          "in_flat_type", "in_flat_number")
for (col in cols) {
  old_v <- dt_old[[col]]
  new_v <- dt_new[[col]]
  n_diff <- sum(!mapply(identical, old_v, new_v), na.rm = FALSE)
  if (n_diff > 0) cat(sprintf("  DIFF %-20s: %d rows\n", col, n_diff))
}
cat("(no output above = perfect match)\n")
