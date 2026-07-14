# =============================================================================
# SPARES Facility Name Matching → Node Attributes
# =============================================================================
# Goal: fuzzy-merge the 'etablissement' column in the SPARES raw data with the
#       'facility_name' column in the enriched node attributes table.
#
# Why this is non-trivial:
#   • SPARES uses a "CITY - SHORT NAME" convention (e.g. "BOBIGNY - AVICENNE")
#   • Node attributes use full official hospital names (e.g. "CHU AVICENNE …")
#   • Both are in French so accents, hyphens, abbreviations vary a lot
#
# Strategy:
#   1. Normalise both name columns (upper-case, strip accents, punctuation, etc.)
#   2. Build a lookup from the node attributes (candidate pool)
#   3. Fuzzy-match each SPARES name against all candidates using string distance
#      (method = "jw" for Jaro-Winkler, good for short messy strings)
#   4. Keep the best match per SPARES row + flag low-confidence matches for
#      manual review
#   5. Export a merged table AND a flagged review file
#
# Package dependencies (install once if needed):
#   install.packages(c("here", "dplyr", "readr", "stringr", "stringi",
#                       "fuzzyjoin", "tidyr"))
# =============================================================================

library(here)        # project-relative paths — anchored at arcane-temporal-network-new
library(dplyr)       # data wrangling
library(readr)       # fast CSV reading/writing
library(stringr)     # string manipulation
library(stringi)     # accent / unicode normalisation
library(fuzzyjoin)   # fuzzy string joins
library(tidyr)       # reshaping helpers

# =============================================================================
# 0.  PATH SETUP
# =============================================================================

# here() resolves relative to the repo root (arcane-temporal-network-new).
# The two input files live in the same sub-folder.
data_dir  <- here("Datasets", "Output Data", "Enriched Facility Data")
spares_path <- file.path(data_dir, "SPARES_facility_raw.csv")
nodes_path  <- file.path(data_dir, "node_attributes_enriched.csv")
output_path <- file.path(data_dir, "SPARES_nodes_matched.csv")
review_path <- file.path(data_dir, "SPARES_nodes_review_needed.csv")

message("📂  Reading data from: ", data_dir)

# =============================================================================
# 1.  LOAD DATA
# =============================================================================

spares_raw <- read_csv(spares_path, show_col_types = FALSE)
nodes_raw  <- read_csv(nodes_path,  show_col_types = FALSE)

message("✅  SPARES rows: ",  nrow(spares_raw))
message("✅  Node attr rows: ", nrow(nodes_raw))

# =============================================================================
# 2.  TEXT NORMALISATION HELPER
# =============================================================================
# This function is the workhorse for cleaning French hospital names.
# It handles:
#   • Accented characters  → ASCII equivalents  (é→e, ô→o, etc.)
#   • Mixed case           → all uppercase
#   • Hyphens / apostrophes → space
#   • Extra whitespace     → single space, trimmed
#   • Common French abbreviations that appear inconsistently (CH, CHU, AP-HP…)

normalise_name <- function(x) {
  x |>
    # 1. Transliterate accents to plain ASCII (French-safe)
    stri_trans_general("Latin-ASCII") |>
    # 2. Force uppercase
    str_to_upper() |>
    # 3. Replace hyphens, apostrophes, slashes → space
    str_replace_all("[-'/]", " ") |>
    # 4. Drop parentheses and their contents  e.g. "( AP-HP)" noise
    str_replace_all("\\(.*?\\)", "") |>
    # 5. Remove stray punctuation (dots, commas) but keep letters/digits/spaces
    str_replace_all("[^A-Z0-9 ]", "") |>
    # 6. Collapse multiple spaces → single space + trim
    str_squish()
}

# =============================================================================
# 3.  PREPARE SPARES TABLE
# =============================================================================

spares <- spares_raw |>
  # Drop the spurious row-index column if present
  select(-any_of("...1"), -any_of("Unnamed: 0")) |>
  rename(spares_name = etablissement) |>
  mutate(
    # Keep original for reference
    spares_name_original = spares_name,
    # Normalised version for matching
    spares_name_clean    = normalise_name(spares_name),
    # Extract the city token (everything before the first " - " or end of string)
    # Many SPARES names follow "CITY - HOSPITAL_SHORT_NAME"
    spares_city_token    = str_extract(spares_name_clean, "^[A-Z0-9]+"),
    # Extract the hospital fragment (everything after " - " if present)
    spares_hosp_token    = str_extract(spares_name_clean, "(?<=  ).*$") |>
      str_trim() |>
      replace_na("")
  )

# =============================================================================
# 4.  PREPARE NODES TABLE
# =============================================================================

nodes <- nodes_raw |>
  select(-any_of("...1"), -any_of("Unnamed: 0")) |>
  mutate(
    # Original untouched — we'll join on this after matching
    facility_name_original = facility_name,
    # Normalised name for matching
    facility_name_clean    = normalise_name(facility_name)
  )

# =============================================================================
# 5.  FUZZY MATCHING
# =============================================================================
# We use stringdist_join() with method = "jw" (Jaro-Winkler distance).
# Jaro-Winkler rewards common prefixes, which is useful when both sides share
# a city or hospital keyword at the start of the string.
#
# max_dist = 0.25 is a fairly permissive threshold — lower = stricter.
# We'll flag anything above 0.15 as "needs review".

MATCH_THRESHOLD  <- 0.25   # maximum allowed Jaro-Winkler distance (0 = perfect)
REVIEW_THRESHOLD <- 0.15   # above this → flag for human review

message("🔍  Running fuzzy match (Jaro-Winkler, max_dist = ", MATCH_THRESHOLD, ") …")

# stringdist_join returns ALL pairs within the threshold.
# We then pick the single best match per SPARES row.
matched_all <- stringdist_join(
  x          = spares,
  y          = nodes,
  by         = c("spares_name_clean" = "facility_name_clean"),
  method     = "jw",
  max_dist   = MATCH_THRESHOLD,
  mode       = "left",            # keep all SPARES rows even if no match
  distance_col = "jw_distance"    # store the distance score
)

# =============================================================================
# 6.  KEEP BEST MATCH PER SPARES ROW
# =============================================================================

best_match <- matched_all |>
  # For rows with multiple candidates, keep the one with the smallest distance
  group_by(spares_name_original) |>
  slice_min(order_by = jw_distance, n = 1, with_ties = FALSE) |>
  ungroup() |>
  # Create a human-readable match quality flag
  mutate(
    match_quality = case_when(
      is.na(jw_distance)         ~ "NO MATCH FOUND",
      jw_distance == 0           ~ "EXACT",
      jw_distance <= 0.05        ~ "VERY HIGH",
      jw_distance <= 0.10        ~ "HIGH",
      jw_distance <= REVIEW_THRESHOLD ~ "MEDIUM",
      TRUE                       ~ "LOW — REVIEW NEEDED"
    )
  )

# =============================================================================
# 7.  TIDY OUTPUT COLUMNS
# =============================================================================

output <- best_match |>
  select(
    # --- SPARES columns ---
    spares_name_original,     # original SPARES establishment name
    spares_name_clean,        # normalised SPARES name used for matching
    n_mois_annee,             # number of months in the year (SPARES metric)
    n_spec,                   # number of specialties (SPARES metric)
    # --- Match quality ---
    jw_distance,              # Jaro-Winkler distance (0 = identical)
    match_quality,            # human-readable quality label
    # --- Matched node attributes ---
    facility_name_original,   # matched facility name from node attributes
    facility_name_clean,      # normalised node name used for matching
    finess_geo,               # FINESS geographic code (unique hospital ID)
    city,
    department,
    region,
    hospital_type,
    facility_type_pmsi,
    total_beds_mco,
    no_beds,
    longitude,
    latitude
  )

# =============================================================================
# 8.  SEPARATE ROWS NEEDING MANUAL REVIEW
# =============================================================================

review_needed <- output |>
  filter(match_quality %in% c("LOW — REVIEW NEEDED", "NO MATCH FOUND")) |>
  arrange(jw_distance)

message("📊  Match summary:")
output |> count(match_quality) |> print()
message("\n⚠️   Rows flagged for review: ", nrow(review_needed))

# =============================================================================
# 9.  WRITE OUTPUTS
# =============================================================================

write_csv(output,       output_path)
write_csv(review_needed, review_path)

message("\n✅  Full merged table written to:\n    ", output_path)
message("✅  Review file written to:\n    ", review_path)

# =============================================================================
# 10. QUICK SANITY CHECK — print a few examples from each quality bucket
# =============================================================================

message("\n--- Sample matches by quality ---")
output |>
  filter(!is.na(jw_distance)) |>
  group_by(match_quality) |>
  slice_head(n = 2) |>
  select(spares_name_original, facility_name_original, jw_distance, match_quality) |>
  print(n = 30)

