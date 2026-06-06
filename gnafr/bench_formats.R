devtools::load_all(quiet = TRUE)
sims  <- readRDS("simulated_inputs.rds")
addrs <- c(sims[["simulated_address"]][1:5000])

tail_re  <- "(\\b(?:QLD|NSW|VIC|SA|WA|TAS|NT|ACT)\\b)\\s+(\\b\\d{4}\\b)\\s*$"
no_tail  <- addrs[!grepl(tail_re, addrs, perl = TRUE)]
cat("Not matching STATE+POSTCODE at end:", length(no_tail), "\n\n")
cat("Sample (first 15):\n")
writeLines(head(no_tail, 15))

# Cross-check: what does the OLD parser give for the first 5 no-tail addresses?
cat("\n--- OLD parser output for first 5 no-tail addresses ---\n")
st_map   <- gnafr:::.get_street_type_map()
st_regex <- gnafr:::.build_street_type_regex(st_map)
ft_map   <- gnafr:::.get_flat_type_map()
ft_re    <- paste0("^(", paste(names(ft_map)[order(-nchar(names(ft_map)))], collapse = "|"),
                   ")\\s+(\\d+[A-Z]?)\\s+")
normalized <- gnafr:::.normalize_addr(head(no_tail, 5))
for (i in seq_along(normalized)) {
  r <- gnafr:::.parse_single(normalized[[i]], st_regex, st_map, ft_re, ft_map)
  cat(sprintf("\n  input: %s\n  postcode=%s state=%s locality=%s\n",
              head(no_tail, 5)[[i]],
              r$in_postcode, r$in_state, r$in_locality))
}
