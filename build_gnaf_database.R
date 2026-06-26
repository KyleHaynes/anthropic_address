# =============================================================================
# build_gnaf_database.R
#
# How to build a good gnafr database, end to end.
#
# This script is the reference build pipeline: every step below is something
# gnafr actually does (or can do) to turn a raw GNAF Core download into a
# database that gnaf_match() matches well against. It's written to run
# top-to-bottom on a fresh database, with each stage explaining *why* it's
# there, not just what it does, since that's the part that doesn't show up by
# reading the function source.
#
# Paths below match the local convention used elsewhere in this repo
# (see scratch.R): GNAF Core QLD CSV at C:/temp/gnaf.qld.csv, DuckDB file at
# C:/temp/gnaf.duckdb. Adjust for other states/locations.
# =============================================================================

library(gnafr)

con <- gnaf_connect("C:/temp/gnaf.duckdb")

# -----------------------------------------------------------------------------
# Step 0: Schema
#
# gnaf_init() creates gnaf_addresses / custom_addresses (with every GNAF Core
# column — see "Capture all GNAF Core columns" below), the locality search
# index, and the match cache. It's safe to call on an existing database: every
# CREATE is IF NOT EXISTS, and any columns added to the schema since the
# database was first built are migrated in via ALTER TABLE ADD COLUMN IF NOT
# EXISTS. Always call it first, including on databases you've built before —
# it's how older databases pick up schema changes.
# -----------------------------------------------------------------------------
gnaf_init(con)

# -----------------------------------------------------------------------------
# Step 1: Load GNAF Core
#
# gnaf_load() reads the GNAF Core CSV straight into DuckDB with read_csv() —
# the file is never pulled into R, so this scales to state-sized files without
# RAM pressure. It captures every column GNAF Core publishes, not just the
# ones obviously needed for address-string matching:
#   - PRIMARY_SECONDARY / PRIMARY_PID  — distinguishes a main dwelling from
#     its sub-dwellings (units/secondaries), and points a secondary back at
#     its primary record. Without this, "10 EXAMPLE ST" and "UNIT 2 10
#     EXAMPLE ST" are just two unrelated rows with no way to relate them.
#   - ALIAS_PRINCIPAL / PRINCIPAL_PID  — flags alias address records and maps
#     them back to the principal (canonical) record they're an alias of.
#   - DATE_CREATED, LEGAL_PARCEL_ID, MB_CODE, GEOCODE_TYPE — kept because
#     they're free (already in the source file) and useful for downstream
#     filtering/auditing (e.g. excluding very recently created records, or
#     joining to ABS Mesh Block data via MB_CODE) without a second load.
#
# Loading is idempotent on ADDRESS_DETAIL_PID (ON CONFLICT DO NOTHING), so
# re-running this on a database that already has the same file loaded is a
# safe no-op rather than a pile of duplicates.
# -----------------------------------------------------------------------------
gnaf_load(con, "C:/temp/gnaf.qld.csv")

# -----------------------------------------------------------------------------
# Step 2: Canonicalise street types  (usually a no-op — see why below)
#
# Free-text addresses mix abbreviated and full street types ("RD" vs "ROAD",
# "AV" vs "AVENUE"), and gnaf_match()'s street_type score expects both sides
# of a comparison to use the same convention to award full credit. gnaf_load()
# already canonicalises STREET_TYPE inline at insert time, so immediately
# after Step 1 this is a no-op.
#
# It earns its place in this script for one reason: it's the fix for any
# database that was built *before* that inline canonicalisation existed.
# Running it here makes the script idempotent regardless of how old the
# database underneath it is, instead of silently leaving stale, un-normalised
# street types behind on an upgrade.
# -----------------------------------------------------------------------------
gnaf_canonicalize_street_types(con)

# -----------------------------------------------------------------------------
# Step 3: Build street-only aliases
#
# This is the alias derivation: gnaf_build_street_aliases() doesn't read any
# new file — it derives new rows *from gnaf_addresses itself*, one per unique
# (street_name, street_type, street_suffix, locality, state, postcode)
# combination already loaded, with a synthetic, number-free address_label and
# alias_type = "street_only".
#
# Why: gnaf_match()'s number pre-filter requires an input with a parsed house
# number to match a candidate row whose number is NULL or in range — a
# numbered input can never accidentally match a number-free row. That means
# street-only rows are inert for normal matching and only ever get used by
# the street-only *fallback* path (street_only_fallback = TRUE in
# gnaf_match()), which exists for inputs that survive every other path
# unmatched — typically because the specific house number is missing from
# GNAF (new subdivision, GNAF hasn't caught up yet) or the input genuinely
# has no number. Without these rows, those inputs have nothing to fall back
# to and stay unmatched even though the street unambiguously exists.
#
# It's idempotent: derived PIDs are an MD5 of the key fields, so re-running
# without overwrite = TRUE silently skips aliases that already exist.
# -----------------------------------------------------------------------------
gnaf_build_street_aliases(con)

# -----------------------------------------------------------------------------
# Step 4: Rebuild the locality search index  (usually automatic)
#
# gnaf_load(), gnaf_load_psv() and gnaf_add() all rebuild gnaf_locality_index
# for you after they finish, so this is normally nothing to think about. It
# exists as a callable step because gnaf_match()'s locality-fallback path
# (mistyped or wrong postcode, but a recognisable suburb name) runs a
# Jaro-Winkler scan over this ~3k-row index rather than the full multi-million
# row address table — that's the whole reason it's fast. The only time you
# need to call it yourself is after bulk deletes/updates run outside of
# gnafr's own load functions (e.g. raw DBI::dbExecute DELETEs), which is rare
# enough that it's worth calling explicitly here as cheap insurance.
# -----------------------------------------------------------------------------
gnaf_rebuild_locality_index(con)

# -----------------------------------------------------------------------------
# Step 5 (optional): Custom addresses
#
# Anything that isn't in GNAF — a brand-new subdivision GNAF hasn't published
# yet, a PO box, an internal site code — can be added to a separate
# custom_addresses table via gnaf_add(). It's unioned into gnaf_match()
# transparently (include_custom = TRUE, the default), scored with the exact
# same logic as GNAF rows, and kept in its own table specifically so it's
# never confused with or overwritten by a future gnaf_load(overwrite = TRUE).
#
# Left commented out here since it's data-specific, not a universal build
# step — uncomment and adapt when you actually have custom records to add.
# -----------------------------------------------------------------------------
# gnaf_add(con, data.table::data.table(
#   number_first  = 1,
#   street_name   = "EXAMPLE",
#   street_type   = "STREET",
#   locality_name = "SAMPLETON",
#   state         = "QLD",
#   postcode      = 4999
# ))

# -----------------------------------------------------------------------------
# Step 6: Verify
#
# gnaf_status() confirms both tables exist and reports row counts at a glance
# — the cheapest possible sanity check that Step 1 actually loaded what you
# expected. sample_gnaf() pulls a few random rows so you can eyeball that
# columns landed in the right place (street types canonicalised, the new
# PRIMARY_SECONDARY/PRINCIPAL_PID columns populated, etc.) before trusting the
# database with real matching.
# -----------------------------------------------------------------------------
gnaf_status(con)
sample_gnaf(con, n = 5)

gnaf_disconnect(con)

# =============================================================================
# Alternative ingestion path (not run here): gnaf_load_psv()
#
# Everything above starts from the GNAF Core CSV, which is the simplified,
# single-file product Geoscape publishes. If you instead have the full raw
# G-NAF PSV product (the "Standard" directory with ADDRESS_DETAIL,
# ADDRESS_ALIAS, STREET_LOCALITY_ALIAS, LOCALITY_ALIAS, etc.), gnaf_load_psv()
# is a richer alternative to Steps 1 and 3 combined: it derives real locality-
# name and street-name alias records (alias_type "LOCALITY:SYN"/"STREET:SYN")
# directly from GNAF's own official alias tables, rather than the
# number-stripped street_only aliases Step 3 builds. That catches inputs that
# use a *recognised alternative suburb or street name* GNAF itself records as
# a synonym — something Step 3's street-only derivation can't do, since it
# only ever drops the house number, never substitutes a different name.
#
# The two paths are alternatives, not complements — each loads its own
# complete set of source = 'gnaf' rows, so pick one:
#
   gnaf_load_psv(con, "C:\\temp\\gnaf\\G-NAF\\G-NAF MAY 2026\\Standard")
#
# Prefer gnaf_load() (Steps 1+3 above) when the simplified CSV is all you
# have, or you don't need locality/street synonym coverage. Prefer
# gnaf_load_psv() when you have the full raw product and want that extra
# alias coverage out of the box.
# =============================================================================


gnaf_status(con)
sample_gnaf(con, n = 51)
