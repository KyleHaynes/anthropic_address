# gnafr

Fast, fuzzy Australian address matching against the [Geocoded National Address File (GNAF)](https://geoscape.com.au/data/g-naf/).

- **Bulk matching** — 100k+ addresses in a single call
- **Fuzzy matching** — handles abbreviations, typos, missing fields, and messy real-world strings
- **Confidence scoring** — transparent 0–100 score with per-component breakdown
- **DuckDB backend** — embedded, no server, survives restarts
- **Custom addresses** — add your own records and match against them alongside GNAF
- **data.table throughout** — fast in-memory joins and vectorised scoring

---

## Contents

1. [Installation](#installation)
2. [Getting GNAF data](#getting-gnaf-data)
3. [First-time database setup](#first-time-database-setup)
4. [Matching addresses](#matching-addresses)
5. [Understanding the output](#understanding-the-output)
6. [How scoring works](#how-scoring-works)
7. [Address string formats](#address-string-formats)
8. [Bulk matching (100k+)](#bulk-matching-100k)
9. [Custom addresses](#custom-addresses)
10. [Working with results](#working-with-results)
11. [Performance notes](#performance-notes)
12. [Function reference](#function-reference)
13. [Troubleshooting](#troubleshooting)

---

## Installation

```r
# Install dependencies
install.packages(c("data.table", "DBI", "duckdb", "stringdist"))

# Install gnafr from source
devtools::install_local("gnafr")
# or from within the gnafr directory:
devtools::load_all()

# Optional app dependencies
install.packages(c("shiny", "reactable"))
```

---

## Getting GNAF data

GNAF Core is a free, open dataset published by Geoscape. Download it from:

> https://geoscape.com.au/data/g-naf/

After downloading and extracting, the package expects a CSV with the standard GNAF Core columns. The quickest way to get a state-level CSV is to run the `load_gnaf.R` script in this repo, which uses `data.table::fread` to read and optionally filter the raw pipe-delimited GNAF files.

All columns from the standard GNAF Core CSV are loaded into `gnaf_addresses`. Key ones:

| Column | Example |
|--------|---------|
| `ADDRESS_DETAIL_PID` | `GAQLD159783900` |
| `ADDRESS_LABEL` | `UNIT 50 13-27 FAIRWAY DR, CLEAR ISLAND WATERS QLD 4226` |
| `FLAT_TYPE` | `UNIT` |
| `FLAT_NUMBER` | `50` |
| `NUMBER_FIRST` | `13` |
| `NUMBER_LAST` | `27` |
| `STREET_NAME` | `FAIRWAY` |
| `STREET_TYPE` | `DRIVE` |
| `LOCALITY_NAME` | `CLEAR ISLAND WATERS` |
| `STATE` | `QLD` |
| `POSTCODE` | `4226` |
| `LONGITUDE` | `153.4023` |
| `LATITUDE` | `-28.03448` |
| `DATE_CREATED` | `27-07-2017` |
| `LEGAL_PARCEL_ID` | `50/BUP3753` |
| `MB_CODE` | `30293470000` |
| `ALIAS_PRINCIPAL` / `PRINCIPAL_PID` | `PRINCIPAL` / *(blank, or the PID of the principal address for an `ALIAS` record)* |
| `PRIMARY_SECONDARY` / `PRIMARY_PID` | `SECONDARY` / `GAQLD163045373` — distinguishes the main dwelling (`PRIMARY`) from sub-dwellings/units (`SECONDARY`) |
| `GEOCODE_TYPE` | `PROPERTY CENTROID` |

---

## First-time database setup

This is a one-off step. The DuckDB file persists between sessions — you only need to load GNAF once.

```r
library(gnafr)

# 1. Create (or open) the database file
con <- gnaf_connect("C:/temp/gnaf.duckdb")

# 2. Create the tables and indexes
gnaf_init(con)

# 3. Load GNAF CSV(s) — DuckDB reads the file directly, no R import needed
gnaf_load(con, "C:/temp/gnaf.qld.csv")
#> Loading: C:/temp/gnaf.qld.csv
#> Done: C:/temp/gnaf.qld.csv
#> Total GNAF addresses in database: 3,305,035

# Check what's loaded
gnaf_status(con)
#>                  table     rows
#>          gnaf_addresses 3305035
#>        custom_addresses       0

# Close when done
gnaf_disconnect(con)
```

### Loading multiple states

Pass a vector of file paths to load them in sequence:

```r
gnaf_load(con, c(
  "C:/temp/gnaf.qld.csv",
  "C:/temp/gnaf.nsw.csv",
  "C:/temp/gnaf.vic.csv"
))
```

Loading is idempotent — rows with a duplicate `ADDRESS_DETAIL_PID` are silently skipped, so you can safely re-run `gnaf_load` without creating duplicates.

### Reloading from scratch

```r
# Wipe existing GNAF data and reload
gnaf_load(con, "C:/temp/gnaf.qld.csv", overwrite = TRUE)
```

---

## Matching addresses

### Reconnecting in a new session

Once the database is built you never need to call `gnaf_init` or `gnaf_load` again. Just reconnect:

```r
library(gnafr)
con <- gnaf_connect("C:/temp/gnaf.duckdb")
```

### Shiny geocoder

You can launch an interactive geocoding app against the same DuckDB database:

```r
library(gnafr)

gnaf_app(db_path = "C:/temp/gnaf.duckdb")
# or reuse an existing connection:
gnaf_app(con = con)
```

The app accepts one address per line, runs `gnaf_match()`, and shows the output
in a `reactable` table. It also adds full-string `Jaro-Winkler` and `Jaccard`
similarity scores between the input string and the matched `address_label`, with
gradient highlighting so low-confidence lexical matches stand out immediately.

### Basic match

```r
addresses <- c(
  "unit 110 120 musgrave Road red hill 4000 QLD",
  "unit 110 120 musgrave Road red hill 4059 QLD",
  "U110 1120 musgrave rd red hill 4000",
  "Cambridge on the hill 110/120 musgrave road red hill QLD 4000",
  "18-20 drift cl goldsborough QLD 4865",
  "77 broadwater rd mount gravatt east 4122"
)

gnaf_match(con, c("10 110-120 musgrave Road red hill 4000 QLD", "unit 10a 110-120 musgrave Road red hill 4000 QLD", "unit 10 120 musgrave Road red hill 4059 QLD", "10 120 musgrave Road red hill 4059 QLD"), max_results = 2)
gnaf_match(con, c("10 St James Ct, Tamborine Mountain QLD 4272"), max_results = 2)
```

By default `gnaf_match` returns up to **3 matches** per input with a **minimum score of 40**. Both are adjustable:

```r
# Top match only, higher confidence threshold
results <- gnaf_match(con, addresses, max_results = 1, min_score = 60)

# More candidates, accept lower confidence (useful for auditing)
results <- gnaf_match(con, addresses, max_results = 5, min_score = 20)
```

---

## Understanding the output

`gnaf_match` returns a `data.table` with one row per match. Multiple rows per input are possible when `max_results > 1`.

```
input_id  input_raw                           match_rank  total_score  score_postcode  score_suburb  score_street_name  score_street_type  score_number  score_flat
       1  unit 110 120 musgrave Road red...            1           97              25            20                 25                 10            12           5
       1  unit 110 120 musgrave Road red...            2           82              25            20                 25                 10            12           0
       2  U110 1120 musgrave rd red hill...            1           82              25            20                 25                 10             0           5
```

### Identity columns

| Column | Description |
|--------|-------------|
| `input_id` | Integer index into the original `addresses` vector |
| `input_raw` | Original (unmodified) input string |
| `match_rank` | 1 = best match, 2 = second best, etc. |

### Score columns

| Column | Max | Description |
|--------|-----|-------------|
| `total_score` | 100 | Weighted sum of all component scores |
| `score_postcode` | 25 | Exact postcode match |
| `score_suburb` | 20 | Jaro-Winkler similarity of locality name |
| `score_street_name` | 25 | Jaro-Winkler similarity of street name |
| `score_street_type` | 10 | Normalised street type match (RD = ROAD) |
| `score_number` | 12 | Street number: exact (12), in range (8), no match (0) |
| `score_flat` | 8 | Flat/unit: both match or both absent (8), otherwise (0) |

### Matched GNAF fields

| Column | Description |
|--------|-------------|
| `address_detail_pid` | GNAF unique identifier for the matched address |
| `address_label` | Formatted address string from GNAF |
| `flat_type` / `flat_number` | Unit/apartment type and number |
| `number_first` / `number_last` | Street number or range start/end |
| `street_name` / `street_type` | Matched street components |
| `locality_name` | Suburb / locality |
| `state` / `postcode` | State and postcode |
| `longitude` / `latitude` | Geocoordinates from GNAF |
| `source` | `"gnaf"` or `"custom"` |
| `alias_principal` / `principal_pid` | Whether the matched record is the `PRINCIPAL` address or an `ALIAS`, and (for aliases) the `address_detail_pid` of its principal record |
| `primary_secondary` / `primary_pid` | Whether the matched record is the `PRIMARY` (main dwelling) or a `SECONDARY` (sub-dwelling/unit) address, and (for secondaries) the `address_detail_pid` of its primary record |
| `geocode_type` | Geocode method/reliability code (e.g. `PROPERTY CENTROID`) |
| `date_created` | Date the address record was created in GNAF |
| `legal_parcel_id` | Cadastral lot/plan identifier |
| `mb_code` | ABS Mesh Block code |

### Inputs with no match

If an input has no candidates above `min_score`, it will not appear in the output at all. To find which inputs were not matched:

```r
unmatched_ids <- setdiff(seq_along(addresses), results$input_id)
addresses[unmatched_ids]
```

---

## How scoring works

Each input address is parsed into components, then compared against candidate GNAF records field by field. The total score is the sum of six weighted components (max 100). The weights are configurable via the `weights` argument on `gnaf_match()`, which expects a named list that sums to 100.

### Score breakdown

```
total_score = score_postcode + score_suburb + score_street_name
            + score_street_type + score_number + score_flat
```

Default weights:

```r
list(
  postcode = 25,
  suburb = 20,
  street_name = 25,
  street_type = 10,
  number = 12,
  flat = 8
)
```

You can override them when you need a different bias, for example if street numbers matter more than postcode for your use case:

```r
results <- gnaf_match(
  con,
  addresses,
  weights = list(
    postcode = 20,
    suburb = 18,
    street_name = 25,
    street_type = 10,
    number = 20,
    flat = 7
  )
)
```

**Postcode (25 pts)** — all-or-nothing exact match.  
High weight because Australian postcodes are granular and reliable. If the input has no parseable postcode, 0 pts; matching switches to a state-level fallback which is much noisier.

**Suburb / locality (20 pts)** — Jaro-Winkler similarity scaled 0–20.  
Jaro-Winkler rewards prefix agreement, so `RED HILL` vs `REDHILL` scores ~19/20. `MOUNT GRAVATT EAST` vs `MT GRAVATT EAST` scores ~17/20 due to the `MT`/`MOUNT` mismatch.

**Street name (25 pts)** — Jaro-Winkler similarity scaled 0–25.  
`MUSGRAVE` vs `MUSGRAVE` = 25. `MUSGROVE` vs `MUSGRAVE` ≈ 24 (one-character transposition). `BROADWATER` vs `BROADWATER` = 25.

**Street type (10 pts)** — normalised exact match.  
`RD` and `ROAD` are both normalised to `ROAD` before comparison, so they match. If only one side has a type (e.g. input omitted it), 5 pts partial credit.

**Street number (12 pts)** — three tiers:  
- Exact match to `number_first` → 12 pts  
- Number falls within `number_first`..`number_last` range (e.g. input has 15, GNAF has 13–27) → 8 pts  
- No match → 0 pts

**Flat / unit (8 pts)** — binary:  
- Both input and GNAF have no flat, or both have matching flat numbers → 8 pts  
- Mismatch or only one side has a flat → 0 pts

### Interpreting scores

| Score range | Typical meaning |
|-------------|-----------------|
| 90–100 | Near-certain match, all components agree |
| 75–89 | High confidence; minor variation in suburb or street name spelling |
| 60–74 | Reasonable match; one significant discrepancy (e.g. wrong street type or suburb spelling) |
| 40–59 | Low confidence; review manually |
| < 40 | Filtered out by default (`min_score = 40`) |

A `total_score` of 80+ with `score_street_name` of 25 and `score_postcode` of 25 is generally trustworthy for automated pipelines. Add `score_number = 12` for high-value use cases.

---

## Address string formats

The parser handles the messy real-world formats you'll encounter in Australian data. It works left-to-right, stripping postcode and state first, then finding the street type as an anchor.

### Supported patterns

| Input | Parsed as |
|-------|-----------|
| `unit 110 120 musgrave Road red hill 4000 QLD` | flat=110, num=120, street=MUSGRAVE ROAD |
| `U110 1120 musgrave rd red hill 4000` | flat=110, num=1120, street=MUSGRAVE ROAD |
| `Cambridge on the hill 110/120 musgrave road red hill QLD 4000` | building=CAMBRIDGE ON THE HILL, flat=110, num=120 |
| `13/45 smith st brisbane 4000` | flat=13, num=45, street=SMITH STREET |
| `APT 3 200 george st sydney NSW 2000` | flat=3, num=200, street=GEORGE STREET |
| `18-20 drift cl goldsborough QLD 4865` | num_first=18, num_last=20, street=DRIFT CLOSE |
| `level 5 300 ann st brisbane 4000` | flat_type=LEVEL, flat_num=5, num=300 |
| `77 broadwater rd mount gravatt east 4122` | num=77, street=BROADWATER ROAD, suburb=MOUNT GRAVATT EAST |

### Flat/unit prefixes recognised

`UNIT`, `U` (attached, e.g. `U12`), `APARTMENT`, `APT`, `FLAT`, `FL`, `FLT`, `LEVEL`, `LVL`, `SUITE`, `STE`, `SHOP`, `SH`, `VILLA`, `VLA`, `LOT`, `TENANCY`, `TNY`

### Street type abbreviations

All standard abbreviations are normalised to their canonical GNAF form before matching:

| Abbreviation(s) | Canonical |
|-----------------|-----------|
| `RD`, `RDS` | `ROAD` |
| `ST`, `STR` | `STREET` |
| `DR`, `DV` | `DRIVE` |
| `AVE`, `AV` | `AVENUE` |
| `CT`, `CRT` | `COURT` |
| `PL`, `PLC` | `PLACE` |
| `CL` | `CLOSE` |
| `CCT` | `CIRCUIT` |
| `CRES`, `CR` | `CRESCENT` |
| `HWY`, `HY` | `HIGHWAY` |
| `PDE` | `PARADE` |
| `PKWY`, `PWY`, `PKY` | `PARKWAY` |
| `TCE`, `TER`, `TERR` | `TERRACE` |

The full list (90 entries) is in [`inst/extdata/street_types.csv`](inst/extdata/street_types.csv).

### What the parser cannot handle

- **PO Box / GPO Box / Locked Bag** addresses — no street component to anchor on
- **Rural addressing** (`Lot 5 DP 12345`) — lot numbers are stored in GNAF but not matched
- **Non-standard street types** not in the abbreviation table — the address will still match but the street type score component will be 0

Use `address_parse()` directly to inspect how an address is being interpreted:

```r
address_parse(c(
  "unit 110 120 musgrave Road red hill 4000 QLD",
  "Cambridge on the hill 110/120 musgrave road red hill QLD 4000"
))
#>    input_id  in_postcode  in_state  in_locality  in_street_name  in_street_type  in_number_first  in_flat_type  in_flat_number  in_building_name
#>           1         4000       QLD     RED HILL        MUSGRAVE            ROAD              120          UNIT             110                NA
#>           2         4000       QLD     RED HILL        MUSGRAVE            ROAD              120          UNIT             110  CAMBRIDGE ON THE HILL
```

---

## Bulk matching (100k+)

`gnaf_match` is designed for large batches. Pass the full vector in one call — it batches all database queries internally.

```r
library(data.table)
library(gnafr)

con <- gnaf_connect("C:/data/gnaf.duckdb")

# Load your addresses from any source
dt_in <- fread("C:/data/my_addresses.csv")

results <- gnaf_match(con, dt_in$address_string, max_results = 1, min_score = 60)

# Join back to your original data
dt_out <- results[dt_in, on = c("input_id" = "row_id")]
```

### How bulk matching works internally

1. **Parse** — all input strings are parsed into components in R (vectorised regex, no DB calls)
2. **Batch fetch** — one SQL `IN (...)` query retrieves all GNAF records for the relevant postcodes at once
3. **Tight join** — `data.table` joins inputs to candidates on `(postcode, number_first)`; this is highly selective and creates a small candidate set
4. **Broad fallback** — inputs with no tight-join candidates are joined on `postcode` only (all addresses in that postcode become candidates)
5. **Score** — Jaro-Winkler and exact comparisons run on all candidate pairs in one vectorised pass
6. **Rank and filter** — top N per input, then drop rows below `min_score`

### Chunking very large inputs

For inputs exceeding ~500k rows or spanning many postcodes, splitting into chunks avoids peak memory pressure:

```r
chunk_size <- 50000L
ids <- seq_len(nrow(dt_in))
chunks <- split(ids, ceiling(ids / chunk_size))

results_list <- lapply(chunks, function(idx) {
  gnaf_match(con, dt_in$address_string[idx], max_results = 1, min_score = 60)
})

results <- rbindlist(results_list)
```

---

## Custom addresses

Add addresses that are not in GNAF — new developments, internal locations, corrections — and they will be matched alongside GNAF records transparently.

### Adding custom addresses

```r
library(data.table)

custom <- data.table(
  address_label  = "LEVEL 2 123 CUSTOM STREET, BRISBANE QLD 4000",
  flat_type      = "LEVEL",
  flat_number    = "2",
  number_first   = 123L,
  street_name    = "CUSTOM",
  street_type    = "STREET",
  locality_name  = "BRISBANE",
  state          = "QLD",
  postcode       = 4000L,
  longitude      = 153.0234,
  latitude       = -27.4698
)

gnaf_add(con, custom)
#> Inserted 1 custom address(es). Total custom: 1.
```

Only six columns are required (`number_first`, `street_name`, `street_type`, `locality_name`, `state`, `postcode`). All others are optional and default to `NA`.

### Upserting (update if exists)

```r
# Replace existing custom address with same PID
gnaf_add(con, custom_updated, upsert = TRUE)
```

### Removing custom addresses

```r
# Address detail PIDs are shown in the 'address_detail_pid' column of results
gnaf_remove_custom(con, c("CUSTOM_1", "CUSTOM_2"))
```

### Bulk custom import

```r
custom_bulk <- fread("C:/data/my_custom_addresses.csv")

# Ensure required columns exist and types are correct
custom_bulk[, number_first := as.integer(number_first)]
custom_bulk[, postcode     := as.integer(postcode)]

gnaf_add(con, custom_bulk)
#> Inserted 4,832 custom address(es). Total custom: 4,832.
```

Custom addresses are stored in the same DuckDB file as GNAF data and persist across sessions.

---

## Working with results

### Extracting the best match per input

```r
best <- results[match_rank == 1]
```

### Filtering by confidence

```r
# Only high-confidence matches for automated processing
high_conf <- results[match_rank == 1 & total_score >= 80]

# Flag low-confidence for manual review
results[, needs_review := total_score < 60]
```

### Identifying unmatched inputs

```r
matched_ids   <- unique(results$input_id)
unmatched_ids <- setdiff(seq_along(addresses), matched_ids)

cat(sprintf("%d of %d inputs had no match above min_score\n",
            length(unmatched_ids), length(addresses)))
```

### Joining coordinates back to your data

```r
dt_in[, input_id := .I]

geo <- results[match_rank == 1, .(input_id, total_score, longitude, latitude,
                                   address_label, address_detail_pid)]

dt_out <- geo[dt_in, on = "input_id"]
```

### Inspecting score breakdown for diagnostics

```r
# Addresses where postcode matched but street name didn't
suspect <- results[score_postcode == 25 & score_street_name < 15]

# All components for a specific input
results[input_id == 42, .(match_rank, total_score, score_postcode, score_suburb,
                           score_street_name, score_street_type, score_number,
                           score_flat, address_label)]
```

---

## Performance notes

### Typical throughput

| Input size | Estimated time |
|------------|---------------|
| 1,000 | < 1 second |
| 10,000 | 2–5 seconds |
| 100,000 | 15–45 seconds |
| 500,000 | 2–5 minutes (chunk recommended) |

Times assume a laptop with SSD and ~3M GNAF records for QLD. Results vary with CPU, postcode spread, and proportion of addresses without postcodes.

### What drives performance

**Parsing** — the `lapply` loop in `address_parse` is the main bottleneck for large inputs. It can be parallelised:

```r
library(parallel)
n_cores <- detectCores() - 1L
chunks  <- split(addresses, cut(seq_along(addresses), n_cores, labels = FALSE))
parsed_list <- mclapply(chunks, address_parse, mc.cores = n_cores)
parsed <- rbindlist(parsed_list)
# Then re-index input_ids if needed
```

**Postcode spread** — if 100k addresses all share one postcode, the broad fallback join can be large (100k × 2000 GNAF records = 200M pairs). Prefer the tight join path by ensuring street numbers parse correctly.

**DB I/O** — DuckDB reads GNAF candidates in one query per `gnaf_match` call. If your inputs span hundreds of distinct postcodes, this query can involve large scans. The `(postcode, number_first)` compound index is the primary optimisation.

### Keeping the connection open

Re-using a single connection across multiple `gnaf_match` calls is faster than reconnecting each time. For Shiny apps or API services, keep `con` in a global or module-level variable.

---

## Function reference

### Connection

```r
gnaf_connect(path, read_only = FALSE)
```
Opens (or creates) a DuckDB database at `path`. Returns a DBI connection object. Pass `read_only = TRUE` for concurrent read access from multiple R processes.

```r
gnaf_disconnect(con)
```
Closes the connection cleanly. Always call this before your script exits.

---

### Setup

```r
gnaf_init(con)
```
Creates `gnaf_addresses` and `custom_addresses` tables and their indexes. Safe to call on an existing database — uses `CREATE TABLE IF NOT EXISTS`.

```r
gnaf_status(con)
```
Returns a `data.table` with row counts for each table.

---

### Loading data

```r
gnaf_load(con, path, overwrite = FALSE)
```
Loads one or more GNAF CSV files into `gnaf_addresses`. Uses DuckDB's native `read_csv` for speed — does not pull data into R first. Duplicate PIDs are silently skipped unless `overwrite = TRUE`.

---

### Matching

```r
gnaf_match(con, addresses, max_results = 3, min_score = 40, include_custom = TRUE)
```
Matches a character vector of address strings. Returns a `data.table` with matched GNAF fields and score columns. Set `include_custom = FALSE` to exclude custom addresses.

---

### Parsing (standalone)

```r
address_parse(addresses)
```
Parses address strings into structured components without hitting the database. Useful for debugging, data profiling, or pre-processing. Returns a `data.table` with one row per input and columns `in_postcode`, `in_state`, `in_locality`, `in_street_name`, `in_street_type`, `in_number_first`, `in_number_last`, `in_flat_type`, `in_flat_number`, `in_building_name`.

---

### Custom addresses

```r
gnaf_add(con, addresses, upsert = FALSE)
```
Inserts a `data.table` of custom addresses into `custom_addresses`. Required columns: `number_first`, `street_name`, `street_type`, `locality_name`, `state`, `postcode`. PIDs are auto-generated if absent (`CUSTOM_1`, `CUSTOM_2`, …). Set `upsert = TRUE` to replace existing rows.

```r
gnaf_remove_custom(con, pids)
```
Deletes custom addresses by `address_detail_pid`.

---

## Troubleshooting

### "File not found" when calling `gnaf_load`

Ensure the path uses either forward slashes or doubled backslashes:

```r
gnaf_load(con, "C:/temp/gnaf.qld.csv")      # OK
gnaf_load(con, "C:\\temp\\gnaf.qld.csv")    # Also OK
```

### Address parses correctly but gets no match

Use `address_parse()` to confirm the components, then check whether the postcode returns any GNAF records:

```r
# Check the parse
address_parse("15 smith st brisbane 4001")

# Check if postcode is in the database
DBI::dbGetQuery(con, "SELECT COUNT(*) FROM gnaf_addresses WHERE postcode = 4001")
```

If the count is 0, the postcode is not in your loaded data (e.g. you loaded QLD only and the address is NSW).

### Street type not normalised

If `score_street_type` is consistently 5 (partial) instead of 10 (full), the input street type abbreviation may not be in the lookup table. Check `inst/extdata/street_types.csv` and add the missing abbreviation:

```r
# View the table
fread(system.file("extdata", "street_types.csv", package = "gnafr"))
```

### Score is unexpectedly low

Inspect the parsed components vs the GNAF match:

```r
parsed <- address_parse("my problem address")
print(parsed)

result <- gnaf_match(con, "my problem address", max_results = 5, min_score = 0)
print(result[, .(match_rank, total_score, score_suburb, score_street_name,
                  score_number, address_label)])
```

Common causes:
- **Suburb spelling diverges** (`MT GRAVATT` vs `MOUNT GRAVATT EAST`) — Jaro-Winkler handles small differences but not radical abbreviations
- **Number not parsed** — the `in_number_first` column in the parse output will be `NA`; this triggers the broad-join fallback and `score_number = 0`
- **Building name confusing the parser** — long building names before the flat/number can sometimes prevent number extraction

### DuckDB version errors

If you see an error about incompatible database versions after upgrading `duckdb`, the database file needs to be rebuilt:

```r
file.remove("C:/data/gnaf.duckdb")
con <- gnaf_connect("C:/temp/gnaf.duckdb")
gnaf_init(con)
gnaf_load(con, "C:/temp/gnaf.qld.csv")
```

---

## Additional examples

### Loading the raw G-NAF Standard PSV files

If you are working from the Geoscape G-NAF Standard distribution rather than a pre-built CSV, point `gnaf_load_psv()` at the `Standard` directory:

```r
con <- gnaf_connect("C:/temp/gnaf.duckdb")
gnaf_init(con)

gnaf_load_psv(
  con,
  gnaf_dir = "C:/temp/gnaf/G-NAF/G-NAF MAY 2026/Standard"
)

gnaf_status(con)
```

### Cache usage

`gnaf_match()` can reuse high-confidence matches through the built-in `gnaf_match_cache` table.

```r
# Cache is on by default.
results <- gnaf_match(
  con,
  addresses,
  cache = TRUE,
  cache_threshold = 95,
  verbose = TRUE
)
```

Inspect the cache:

```r
gnaf_cache_status(con)
#>    rows oldest_cached newest_cached

gnaf_cache_history(con, by = "day")
#>       bucket rows min_score avg_score max_score
```

Sample cache entries at random:

```r
gnaf_cache_sample(con, n = 10)
```

Sample cache entries from a specific day:

```r
gnaf_cache_sample(con, n = 10, cached_on = "2026-06-05")
```

Sample cache entries from a datetime range:

```r
gnaf_cache_sample(
  con,
  n = 20,
  from = "2026-06-05 00:00:00",
  to = "2026-06-05 23:59:59"
)
```

Roll back or clear cache entries:

```r
gnaf_cache_rollback(con, after = "2026-06-05 14:00:00")
gnaf_cache_clear(con)
```

