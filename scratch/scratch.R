# Test code / scratch, scratch scratching / scratch pad

## Benchmark timing regressions ----
require(atime)
# https://github.com/tdhock/atime/pull/80

gdir <- "C:\\Users\\kyleh\\GitHub\\anthropic_address"

glist <- atime::atime_versions(
  file.path(gdir,"pkg"),
  current = "1aae646888dcedb128c9076d9bd53fcb4075dcda",
  old     = "51056b9c4363797023da4572bde07e345ce57d9c",
  setup   = {library(gnafr); con <- gnaf_connect("C:/temp/gnaf.duckdb"); },
  expr    = gnaf_match(con, "190 musgrave rd red hill qld"))
plot(glist)




