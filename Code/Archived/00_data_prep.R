# ==============================================================================
# 00_data_prep.R
# Hospital Transfer Network — Data Preparation & Coverage Report
# Rany Octaria | Le CNAM
# ==============================================================================
#
# PURPOSE
#   Build the three clean objects needed for all downstream network modelling:
#     (1) node_attributes  — one row per facility, all covariates
#     (2) daily_admission  — raw daily admission counts (temporal node signal)
#     (3) weekly           — weekly rolling-average edge list (kept separate)
#   Then assess how well-covered the network nodes are across each attribute
#   source and write a CSV coverage report.
#
# INPUTS  (paths relative to project root via here())
#   Datasets/MCO_SSR_HBN_2024/MCO_SSR_HBN_Direct_2024/
#       HBN_weekly_sliding_edgelist_2024.csv   ← transfer edge list
#       NO_ADMISSION_DAILY_DIRCT_HBN.csv       ← daily admission counts
#   Datasets/MCO_SSR_HBN_2024/
#       finessgeo_metadata_2024.csv            ← single source: geo (X/Y Lambert-93)
#                                                + PMSI category + all facility info
#
# OUTPUTS  (saved identically to all three destination folders)
#   node_attributes.RDS   — node covariate table (1 row per network facility)
#   weekly.RDS            — edge list with harmonised key names
#   daily_admission.RDS   — full daily panel with harmonised key name
#   coverage_report.csv   — per-node attribute-coverage flags + summary tables
#
# KEY DESIGN DECISIONS
#   • finessgeo_metadata_2024.csv is the only facility attribute source.
#     It carries X/Y coordinates in Lambert-93 (EPSG:2154) which are reprojected
#     to WGS84 (EPSG:4326) lat/lon, then spatially joined to GISCO France admin
#     boundaries to attach city, department, and region labels.
#   • IUM hospital unit data and the separate beds_data file are no longer used —
#     all needed facility attributes come from facility_meta.
#   • weekly is kept as a separate object — it is an edge table and must never
#     be row-joined with node tables.
#   • All facility keys are standardised to "finess_geo" (snake_case).
#     The weekly edge columns become finess_geo_origin / finess_geo_target.
#   • node_attributes is built starting from the unique facilities in weekly,
#     so it is always aligned with the network.
# ==============================================================================


# ── 0. Libraries ──────────────────────────────────────────────────────────────
library(here)
library(tidyverse)
library(lubridate)
library(janitor)   # clean_names()
library(sf)        # Lambert-93 → WGS84 reprojection
library(giscoR)    # France admin boundaries for spatial join (city/dept/region)

options(scipen = 999)


# ── 1. File paths & load raw datasets ─────────────────────────────────────────
# All input paths in one place — update here if files move.

RAW <- list(
  weekly        = here("Datasets", "MCO_SSR_HBN_2024",
                       "MCO_SSR_HBN_Direct_2024",
                       "HBN_weekly_sliding_edgelist_2024.csv"),
  facility_meta = here("Datasets", "MCO_SSR_HBN_2024",
                       "finessgeo_metadata_2024.csv"),
  daily         = here("Datasets", "MCO_SSR_HBN_2024",
                       "MCO_SSR_HBN_Direct_2024",
                       "NO_ADMISSION_DAILY_DIRCT_HBN.csv")
)

# Stop early with a clear message if any file is missing
missing_files <- Filter(Negate(file.exists), RAW)
if (length(missing_files) > 0) {
  stop("Missing input files:\n",
       paste(" ✗", names(missing_files), "→", unlist(missing_files),
             collapse = "\n"))
}
message("── Loading raw data ──")
message("  All input files found ✓")

# 1a. Weekly rolling-average transfer edge list
#     Columns as shipped: finessGeo_origin | finessGeo_target | weight
#     weight = mean daily transfers over a sliding 7-day window
weekly_raw        <- read_csv(RAW$weekly, show_col_types = FALSE)

# 1b. PMSI facility metadata
#     Contains: X/Y coordinates (Lambert-93), PMSI category, and all other
#     facility-level attributes.  Coordinates are reprojected in Section 3.
facility_meta_raw <- read_csv(RAW$facility_meta, show_col_types = FALSE)

# 1c. Daily admissions — facility × date panel, temporal node-activity signal
daily_raw         <- read_delim(RAW$daily, delim = ";", escape_double = FALSE,
                                trim_ws = TRUE, show_col_types = FALSE)

message("  weekly_raw:          ", nrow(weekly_raw), " edges")
message("  facility_meta_raw:   ", nrow(facility_meta_raw), " facilities")
message("  daily_raw:           ", nrow(daily_raw), " facility-day rows")


# ── 2. Helper functions ────────────────────────────────────────────────────────

# safe_rename: rename a column only if one of the candidate names exists.
# rename_with(~ "fixed_name", .cols = any_of(...)) crashes when zero candidates
# match because the lambda returns length 1 but zero columns are selected.
# This helper avoids that by checking with intersect() first.
#   df         : data frame
#   new_name   : desired output column name (string)
#   candidates : character vector of possible current names
safe_rename <- function(df, new_name, candidates) {
  hit <- intersect(names(df), candidates)
  if (length(hit) == 0) {
    warning("safe_rename: none of [", paste(candidates, collapse = ", "),
            "] found — column NOT renamed to '", new_name, "'.\n",
            "  Actual column names: ", paste(names(df), collapse = ", "))
    return(df)
  }
  rename(df, !!new_name := !!hit[1])
}


# ── 3. Harmonise column names ──────────────────────────────────────────────────
message("\n── Harmonising column names ──")

# 3a. weekly — rename camelCase origin/target keys, leave weight as-is
weekly <- weekly_raw %>%
  rename(
    finess_geo_origin = finessGeo_origin,
    finess_geo_target = finessGeo_target
  )
stopifnot(
  "weekly must have finess_geo_origin" = "finess_geo_origin" %in% names(weekly),
  "weekly must have finess_geo_target" = "finess_geo_target" %in% names(weekly),
  "weekly must have weight"            = "weight"            %in% names(weekly)
)
message("  weekly keys OK: finess_geo_origin | finess_geo_target | weight")

# 3b. daily admissions
daily_admission <- daily_raw %>%
  clean_names() %>%
  safe_rename("finess_geo", c("finessgeo", "finess_geo", "finessegeo",
                              "finess_et", "finessgeographique"))
message("  daily_admission columns: ", paste(names(daily_admission), collapse = " | "))

# 3c. facility_meta — print columns first so you can verify the X/Y names
#     and any column that needs renaming below
facility_meta <- facility_meta_raw %>%
  clean_names()
message("  facility_meta columns after clean_names(): ",
        paste(names(facility_meta), collapse = " | "))

facility_meta <- facility_meta %>%
  safe_rename("finess_geo",
              c("finessgeo", "finess_geo", "finessegeo",
                "finess_et", "finessgeographique")) %>%
  # Rename the PMSI category column → "facility_type"
  # If the warning fires, add the actual column name to this list.
  safe_rename("facility_type",
              c("pmsi_category", "categ", "categorie", "type_etab", "category",
                "type_etablissement", "cat_etab", "libelle_categorie",
                "code_categorie"))


# ── 4. Reproject coordinates Lambert-93 → WGS84 ───────────────────────────────
# facility_meta carries X/Y coordinates in Lambert-93 (EPSG:2154), the standard
# French projection.  We reproject to WGS84 (EPSG:4326) to get decimal
# latitude/longitude, which is what igraph, leaflet, and ggplot mapping expect.
#
# The coordinate columns are most commonly named coordxet/coordyet or x/y after
# clean_names().  The message above (facility_meta columns) will confirm this.
# Adjust the two column name strings below if yours differ.

message("\n── Reprojecting Lambert-93 coordinates to WGS84 ──")

# ADJUST THESE two strings if your X/Y column names differ:
coord_x_col <- "coordxet"   # Lambert-93 easting  (X)
coord_y_col <- "coordyet"   # Lambert-93 northing (Y)

if (!all(c(coord_x_col, coord_y_col) %in% names(facility_meta))) {
  stop("Coordinate columns '", coord_x_col, "' / '", coord_y_col,
       "' not found in facility_meta.\n",
       "  Available columns: ", paste(names(facility_meta), collapse = ", "))
}

facility_meta <- facility_meta %>%
  # Drop rows with missing coordinates before building the sf object
  # (st_as_sf will error on NA coordinates)
  mutate(has_coords_raw = !is.na(.data[[coord_x_col]]) &
           !is.na(.data[[coord_y_col]])) %>%
  { 
    # Split: reproject the rows that have coords, keep the rest as-is
    has_xy  <- filter(., has_coords_raw)
    no_xy   <- filter(., !has_coords_raw)
    
    reprojected <- has_xy %>%
      st_as_sf(coords = c(coord_x_col, coord_y_col), crs = 2154, remove = FALSE) %>%
      st_transform(4326) %>%
      mutate(
        longitude = st_coordinates(.)[, 1],
        latitude  = st_coordinates(.)[, 2]
      ) %>%
      st_drop_geometry()
    
    bind_rows(reprojected,
              no_xy %>% mutate(longitude = NA_real_, latitude = NA_real_))
  }

n_reprojected <- sum(!is.na(facility_meta$latitude))
message("  Reprojected: ", n_reprojected, " / ", nrow(facility_meta),
        " facilities have valid WGS84 coordinates")
message("  Coordinate range — lon: [",
        round(min(facility_meta$longitude, na.rm = TRUE), 2), ", ",
        round(max(facility_meta$longitude, na.rm = TRUE), 2), "]  lat: [",
        round(min(facility_meta$latitude,  na.rm = TRUE), 2), ", ",
        round(max(facility_meta$latitude,  na.rm = TRUE), 2), "]")
# Sanity check: French mainland is roughly lon [-5, 10], lat [41, 52]
# Overseas territories will extend this range — that is expected.

# ── 4b. Spatial join → attach city, department, region ────────────────────────
# Now that we have WGS84 coordinates, join the reprojected points against
# GISCO France admin boundary polygons to get commune, department, and region
# labels.  giscoR caches downloads after the first run.
# Facilities without valid coordinates will get NA for all three labels.

# ── 4b. Spatial join → attach city, department, region ────────────────────────
message("  Fetching France admin boundaries from GISCO (cached after first run)…")
communes_fr    <- gisco_get_communes(country = "FR", epsg = "4326")
departments_fr <- gisco_get_nuts(country = "FR", nuts_level = 3, epsg = "4326")

# NUTS-1 = the new 13 régions (post-2016 reform). NUTS-2 would give the OLD 22.
# Pin year so the post-reform hierarchy is guaranteed regardless of giscoR default.
regions_fr     <- gisco_get_nuts(country = "FR", nuts_level = 1,
                                 year = "2021", epsg = "4326")

# Only spatial-join the rows that actually have coordinates
has_coords <- facility_meta %>% filter(!is.na(latitude))
no_coords  <- facility_meta %>% filter( is.na(latitude))

has_coords_sf <- has_coords %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

has_coords_joined <- has_coords_sf %>%
  st_join(communes_fr    %>% select(city       = COMM_NAME), join = st_within, left = TRUE) %>%
  st_join(departments_fr %>% select(department = NAME_LATN), join = st_within, left = TRUE) %>%
  st_join(regions_fr     %>% select(region     = NAME_LATN), join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  # Facilities on region boundaries can match multiple polygons — keep first
  group_by(finess_geo) %>%
  slice(1) %>%
  ungroup()

facility_meta <- bind_rows(
  has_coords_joined,
  no_coords %>% mutate(city = NA_character_,
                       department = NA_character_,
                       region = NA_character_)
)

message("  Admin labels attached:")
message("    city resolved:       ",
        sum(!is.na(facility_meta$city)), " / ", nrow(facility_meta))
message("    department resolved: ",
        sum(!is.na(facility_meta$department)), " / ", nrow(facility_meta))
message("    region resolved:     ",
        sum(!is.na(facility_meta$region)), " / ", nrow(facility_meta))


# ── 5. Build node attribute table ─────────────────────────────────────────────
# Start from the universe of facilities in the weekly transfer network, then
# left-join all attributes.  Every network node gets a row even if some
# attributes are missing — the coverage of those joins is quantified in Section 6.

message("\n── Building node attribute table ──")

network_nodes <- tibble(
  finess_geo = unique(c(weekly$finess_geo_origin, weekly$finess_geo_target))
)
message("  Unique facilities in weekly edge list: ", nrow(network_nodes))

# Yearly admission total per facility (aggregated from daily panel)
admit_yr <- daily_admission %>%
  group_by(finess_geo) %>%
  summarise(admit_yr = sum(no_admissions, na.rm = TRUE), .groups = "drop")

# One row per network facility — facility_meta is the single attribute source
node_attributes <- network_nodes %>%
  left_join(facility_meta, by = "finess_geo") %>%   # all facility covariates + lat/lon
  left_join(admit_yr,      by = "finess_geo")       # yearly admissions aggregate

message("  node_attributes: ", nrow(node_attributes), " rows × ",
        ncol(node_attributes), " columns")


# ── 6. Source-by-source coverage audit ────────────────────────────────────────
#
# GOAL: for every facility in the network, check each raw source INDEPENDENTLY
# before any joining, so we can see exactly how much each source contributes.
#
#   Q1. Which sources cover which share of network nodes?
#   Q2. Within each source, how many values are non-missing for modelling vars?
#   Q3. How many nodes are fully covered (all key attributes present)?
#
# OUTPUT TABLES (also exported to coverage_report.csv)
#   coverage_per_node — one row per facility, all source flags + actual values
#   source_summary    — match rates per source
#   var_fill          — attribute fill rates across all network nodes

message("\n── Source-by-source coverage audit ──")
message("  Network has ", nrow(network_nodes), " unique facilities")

# SOURCE 1: facility_meta — coordinates
src_coords <- network_nodes %>%
  left_join(
    facility_meta %>% select(finess_geo,
                             any_of(c("latitude", "longitude",
                                      "city", "department", "region"))),
    by = "finess_geo"
  ) %>%
  mutate(
    in_coords   = !is.na(latitude),
    has_lat_lon = !is.na(latitude) & !is.na(longitude),
    has_region  = !is.na(region)
  )

# SOURCE 2: facility_meta — facility type
src_type <- network_nodes %>%
  left_join(
    facility_meta %>% select(finess_geo, facility_type),
    by = "finess_geo"
  ) %>%
  mutate(
    in_facility_meta  = !is.na(facility_type),
    has_facility_type = !is.na(facility_type)
  )

# SOURCE 3: daily_admission
#   Captures both whether a facility appears at all and how active it was.
#   n_active_days = days with at least one recorded admission.
src_admissions <- network_nodes %>%
  left_join(
    daily_admission %>%
      group_by(finess_geo) %>%
      summarise(
        admit_yr      = sum(no_admissions, na.rm = TRUE),
        n_active_days = sum(!is.na(no_admissions) & no_admissions > 0),
        n_days_total  = n(),
        .groups = "drop"
      ),
    by = "finess_geo"
  ) %>%
  mutate(
    in_daily           = !is.na(admit_yr),
    admissions_nonzero = !is.na(admit_yr) & admit_yr > 0,
    pct_active_days    = ifelse(n_days_total > 0,
                                n_active_days / n_days_total, NA_real_)
  )
# ── Node attributes: NETWORK version (existing, keep as-is) ───────────────────
# Only facilities present in the weekly transfer network
node_attributes <- network_nodes %>%
  left_join(facility_meta, by = "finess_geo") %>%
  left_join(admit_yr,      by = "finess_geo")

# ── Node attributes: FULL version (for SPARES estimation) ─────────────────────
# All facilities in facility_meta, regardless of transfer network membership
# Use this when linking SPARES surveillance data — hospitals reporting AMR
# cases may not appear in the transfer network but are still valid for
# incidence estimation
node_attributes_full <- facility_meta %>%
  left_join(admit_yr, by = "finess_geo") %>%
  mutate(
    # Flag whether this facility is part of the transfer network
    in_transfer_network = finess_geo %in% network_nodes$finess_geo
  )

message("  node_attributes (network only): ", nrow(node_attributes),      " facilities")
message("  node_attributes_full (all):     ", nrow(node_attributes_full), " facilities")
message("  In transfer network:            ",
        sum(node_attributes_full$in_transfer_network), " / ", nrow(node_attributes_full))
# ── 6b. Per-node coverage matrix ──────────────────────────────────────────────
coverage_per_node <- network_nodes %>%
  left_join(src_coords    %>% select(finess_geo, in_coords, has_lat_lon,
                                     has_region, latitude, longitude,
                                     city, department, region),
            by = "finess_geo") %>%
  left_join(src_type      %>% select(finess_geo, in_facility_meta,
                                     has_facility_type, facility_type),
            by = "finess_geo") %>%
  left_join(src_admissions %>% select(finess_geo, in_daily,
                                      admissions_nonzero, admit_yr,
                                      n_active_days, n_days_total,
                                      pct_active_days),
            by = "finess_geo") %>%
  mutate(
    # Fully modelable: has coordinates + facility type + admission activity
    fully_covered = has_lat_lon & has_facility_type & admissions_nonzero
  )

# ── 6c. Summary tables ────────────────────────────────────────────────────────
pct <- function(x) round(mean(x, na.rm = TRUE) * 100, 1)
n_t <- function(x) sum(x, na.rm = TRUE)

source_summary <- tribble(
  ~source,           ~n_matched,                                       ~pct_matched,
  "facility_meta",   n_t(coverage_per_node$in_facility_meta),          pct(coverage_per_node$in_facility_meta),
  "coords (in meta)",n_t(coverage_per_node$in_coords),                 pct(coverage_per_node$in_coords),
  "daily_admission", n_t(coverage_per_node$in_daily),                  pct(coverage_per_node$in_daily)
) %>%
  mutate(n_total   = nrow(network_nodes),
         n_missing = n_total - n_matched)

var_fill <- tibble(
  attribute = c(
    "lat/lon — WGS84 after reprojection",
    "region resolved",
    "facility type (PMSI category)",
    "admissions > 0 in 2024",
    "fully covered (coords + type + admissions)"
  ),
  n_present = c(
    n_t(coverage_per_node$has_lat_lon),
    n_t(coverage_per_node$has_region),
    n_t(coverage_per_node$has_facility_type),
    n_t(coverage_per_node$admissions_nonzero),
    n_t(coverage_per_node$fully_covered)
  )
) %>%
  mutate(
    n_missing   = nrow(network_nodes) - n_present,
    pct_present = round(n_present / nrow(network_nodes) * 100, 1)
  )

# ── 6d. Console diagnostics ───────────────────────────────────────────────────
message("\n  ┌── Source match rates ──────────────────────────────────┐")
walk2(source_summary$source, source_summary$pct_matched, ~ {
  bar <- strrep("█", round(.y / 5))
  message(sprintf("  │  %-26s %5.1f%%  %s", .x, .y, bar))
})
message("  └────────────────────────────────────────────────────────┘")

message("\n  ┌── Attribute fill rates ─────────────────────────────────┐")
walk2(var_fill$attribute, var_fill$pct_present, ~ {
  message(sprintf("  │  %-42s %5.1f%%  (%d missing)",
                  .x, .y, nrow(network_nodes) - round(.y / 100 * nrow(network_nodes))))
})
message("  └────────────────────────────────────────────────────────┘")

message("\n  Fully covered nodes: ",
        n_t(coverage_per_node$fully_covered), " / ", nrow(network_nodes),
        " (", pct(coverage_per_node$fully_covered), "%)")

# ── 6e. Assemble export CSV ───────────────────────────────────────────────────
# Sections stacked in one file, separated by a SECTION header row, so it's
# self-documenting when opened in Excel.

coverage_csv <- bind_rows(
  tibble(SECTION = "PER_NODE_DETAIL — one row per network facility"),
  coverage_per_node %>%
    mutate(across(where(is.logical), as.integer),
           across(where(is.numeric), ~ round(., 4))),
  tibble(SECTION = ""),
  
  tibble(SECTION = "SOURCE_MATCH_RATES — % of network nodes found in each source"),
  source_summary,
  tibble(SECTION = ""),
  
  tibble(SECTION = "ATTRIBUTE_FILL_RATES — non-missing values as % of all network nodes"),
  var_fill
)


# ── 7. Save outputs ────────────────────────────────────────────────────────────
message("\n── Saving outputs ──")

save_rds_multi <- function(obj, filename, folders) {
  for (folder in folders) {
    dir.create(here(folder), showWarnings = FALSE, recursive = TRUE)
    path <- here(folder, filename)
    saveRDS(obj, file = path)
    message("  Saved: ", path)
  }
}

save_csv_multi <- function(df, filename, folders) {
  for (folder in folders) {
    dir.create(here(folder), showWarnings = FALSE, recursive = TRUE)
    path <- here(folder, filename)
    write_csv(df, path)
    message("  Saved: ", path)
  }
}

output_folders <- c(
  "Cluster Jobs",
  "Seeding Jobs",
  file.path("Datasets", "Output Data")
)

save_rds_multi(node_attributes, "node_attributes.RDS", output_folders)
save_rds_multi(weekly,          "weekly.RDS",          output_folders)
save_rds_multi(daily_admission, "daily_admission.RDS", output_folders)
save_csv_multi(coverage_csv,    "coverage_report.csv", output_folders)

message("\n── Done ──")
message("  node_attributes  : ", nrow(node_attributes), " × ", ncol(node_attributes))
message("  weekly           : ", nrow(weekly), " edges")
message("  daily_admission  : ", nrow(daily_admission), " rows")
message("  coverage_per_node: ", nrow(coverage_per_node), " facilities")


save_rds_multi(node_attributes_full, "node_attributes_full.RDS", output_folders)

# ── 8. Clean up ────────────────────────────────────────────────────────────────
rm(list = setdiff(ls(), c(
  "node_attributes",
  "weekly",
  "daily_admission",
  "coverage_per_node",
  "source_summary",
  "var_fill",
  "coverage_csv"
)))

# ==============================================================================
# END OF 00_data_prep.R
#
# LOAD IN DOWNSTREAM SCRIPTS
# ---------------------------
# node_attributes <- readRDS(here("Datasets", "Output Data", "node_attributes.RDS"))
# weekly          <- readRDS(here("Datasets", "Output Data", "weekly.RDS"))
# daily_admission <- readRDS(here("Datasets", "Output Data", "daily_admission.RDS"))
#
# KEY COLUMNS IN node_attributes
# --------------------------------
#   finess_geo     : 9-digit FINESS facility identifier
#   longitude      : WGS84 decimal longitude (reprojected from Lambert-93 X)
#   latitude       : WGS84 decimal latitude  (reprojected from Lambert-93 Y)
#   facility_type  : PMSI category (MCO / SSR / HAD / PSY / …)
#   admit_yr       : total admissions recorded in 2024
#   [+ all other columns from finessgeo_metadata_2024.csv as cleaned by janitor]
#
# COLUMNS IN weekly
# ------------------
#   finess_geo_origin : sending facility
#   finess_geo_target : receiving facility
#   weight            : mean daily transfers over sliding 7-day window
# ==============================================================================


# =============================================================================
# cnr Cleaning Data
# Project: ARCANE
# Author: Rany Octaria
# Description: Cleans cnr surveillance data, builds hospital reference
#              table from FINESS + SAE CAPACT, and links bed counts to
#              node_attributes by finess_geo
# =============================================================================




# =============================================================================
# PART 3: FINESS REFERENCE FILE
# Download once with Option A, then use Option B for day-to-day runs
# =============================================================================

# --- Option A: Download fresh (run once) ---
# finess_url <- "https://www.data.gouv.fr/fr/datasets/r/2ce43ade-8d2c-4d1d-81da-ca06c82abc68"
# download.file(finess_url, destfile = here("Datasets", "Facility Data", "etalab_finess_et.csv"), mode = "wb")

# --- Option B: Load already-downloaded file ---
# NOTE: Raw FINESS file has no header row and mixed row types.
#       Encoding is ISO-8859-1 (not UTF-8).
finess_cols <- c(
  "structure", "nofinesset", "nofinessej", "rs", "rslongue",
  "complrs", "compldistrib", "numvoie", "typvoie", "voie",
  "compvoie", "lieuditbp", "commune", "departement",
  "libdepartement", "ligneacheminement", "telephone", "telecopie",
  "categetab", "libcategetab", "categagretab", "libcategagretab",
  "siret", "codeape", "codemft", "libmft", "codesph", "libsph",
  "dateouv", "dateautor", "datemaj", "numuai"
)

finess_raw <- read_delim(
  here("Datasets", "Facility Data", "etalab_finess_et.csv"),
  delim      = ";",
  col_names  = finess_cols,
  skip       = 1,
  locale     = locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE
)

# Keep only geographic establishment rows, select relevant columns
finess_clean <- finess_raw %>%
  filter(structure == "structureet") %>%
  select(
    finess_geo    = nofinesset,
    finess_ej     = nofinessej,
    facility_name = rs,
    categetab,
    libcategetab
  ) %>%
  mutate(finess_geo = str_pad(as.character(finess_geo), 9, pad = "0"))

# =============================================================================
# PART 4: SAE CAPACT — BED COUNTS BY FINESS GEO
# Download manually from:
# https://data.drees.solidarites-sante.gouv.fr/explore/dataset/708_bases-statistiques-sae/
# Attachments > CAPACT_PM_PNM > unzip > place capact_24.csv in Datasets/Facility Data/
# =============================================================================

sae_raw <- read_csv(
  here("Datasets", "Facility Data", "CAPACT24.csv"),
  show_col_types = FALSE
)

# Check all discipline codes present
sae_raw %>% distinct(DISCI, DISCIPLINE) %>% print(n = 50)

# Classify disciplines into MCO vs SSR and sum beds per facility
beds_by_finess <- sae_raw %>%
  mutate(
    care_type = case_when(
      str_detect(DISCIPLINE, "Médecine|Chirurgie|Gynéco") ~ "MCO",
      str_detect(DISCIPLINE, "Soins de")                  ~ "SSR",
      TRUE                                                 ~ NA_character_
    )
  ) %>%
  filter(!is.na(care_type)) %>%
  group_by(fi, care_type, rs) %>%
  summarise(beds = sum(LIT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from   = care_type,
    values_from  = beds,
    values_fill  = 0,
    names_prefix = "lit_"
  ) %>%
  mutate(
    finess_geo    = str_pad(as.character(fi), 9, pad = "0"),
    facility_type = case_when(
      lit_MCO > 0 & lit_SSR > 0 ~ "MCO/SSR",
      lit_MCO > 0               ~ "MCO",
      lit_SSR > 0               ~ "SSR",
      TRUE                      ~ "Other"
    ),
    no_beds = lit_MCO + lit_SSR
  ) %>%
  select(rs,finess_geo, lit_MCO, lit_SSR, no_beds, facility_type) %>% 
  rename(facility_name = rs)

head(beds_by_finess)


# =============================================================================
# PART 5: JOIN FINESS + BED COUNTS INTO FULL HOSPITAL REFERENCE TABLE
# =============================================================================

hospital_ref <- finess_clean %>%
  left_join(beds_by_finess, by = "finess_geo") %>%
  mutate(
    facility_type = replace_na(facility_type, "Other/Unknown"),
    no_beds       = replace_na(no_beds, 0)
  )

# Quick check
hospital_ref %>% count(facility_type)
hospital_ref %>% filter(no_beds > 0) %>% nrow() %>% paste("facilities with bed data")

# =============================================================================
# PART 6: ENRICH NODE ATTRIBUTES WITH BED COUNTS
# =============================================================================

node_attributes_enriched <- node_attributes %>%
  mutate(finess_geo = str_pad(as.character(finess_geo), 9, pad = "0")) %>%
  rename(facility_type_pmsi = facility_type) %>%
  left_join(
    beds_by_finess %>% rename(facility_type_capact = facility_type),
    by = "finess_geo"
  )
# Check match rate
node_attributes_enriched %>%
  count(hospital_type, total_beds_mco ==0)

node_attributes_enriched %>%
  filter(is.na(no_beds)) %>%
  nrow() %>%
  paste("facilities with no bed match")


write.csv(node_attributes_enriched, file = here("Datasets", "Output Data", 
                                                "Enriched Facility Data","node_attributes_enriched.csv"))
#Get just SSR 
ssr = node_attributes_enriched %>% 
  filter(hospital_type =="SSR") %>% 
  select(finess_geo, hospital_type, facility_type_pmsi,
         facility_type_capact, facility_name,
         total_beds_mco, lit_MCO, lit_SSR, no_beds)

write.csv(ssr, file = here("Datasets", "Output Data", "Enriched Facility Data","SSR_enriched.csv"))

#Get MCO/SSR
mco_ssr = node_attributes_enriched %>% 
  filter(hospital_type =="MCO/SSR") %>% 
  select(finess_geo, hospital_type, facility_type_pmsi,
         facility_type_capact, facility_name,
         total_beds_mco, lit_MCO, lit_SSR, no_beds)

write.csv(mco_ssr, file = here("Datasets", "Output Data", "Enriched Facility Data","MCO_SSR_enriched.csv"))


#Get just MCO
mco = node_attributes_enriched %>% 
  filter(hospital_type =="MCO") %>% 
  select(finess_geo, hospital_type, facility_type_pmsi,
         facility_type_capact, facility_name,
         total_beds_mco, lit_MCO, lit_SSR, no_beds)

write.csv(mco, file = here("Datasets", "Output Data", "Enriched Facility Data","MCO_enriched.csv"))



