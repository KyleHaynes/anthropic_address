# Test code / scratch, scratch scratching / scratch pad

## Benchmark timing regressions ----
require(atime)
# https://github.com/tdhock/atime/pull/80

remotes::install_github("KyleHaynes/gnafr")
gdir <- tempfile()
dir.create(gdir)
git2r::clone("https://github.com/KyleHaynes/gnafr", gdir)


glist <- atime::atime_versions(
  file.path(gdir),
  N = c(100,200),
  times = 1,
  current = "04cc71dfe7e23416968149757a72c4e2bbffb68c",
  old     = "8d43e9bedfb9d40b8d4aa3dbeeed79fb775d5260",
  setup   = {con <- gnafr::gnaf_connect("C:/temp/gnaf.duckdb"); },
  expr    = gnafr::gnaf_match(rep(readRDS("c:/temp/temp.rds")$input_raw, 100), con))
plot(glist)




