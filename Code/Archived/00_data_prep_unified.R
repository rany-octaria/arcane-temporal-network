# ==============================================================================
# 00_data_prep_unified.R
# ARCANE Project — Unified Data Preparation Pipeline
# Rany Octaria | Le CNAM / MESuRS
# ==============================================================================
#
# PURPOSE
#   Single end-to-end script that replaces the three previously separate scripts:
#     • 00_data_prep.R          (network nodes, coordinates, coverage audit)
#     • cnr_cleaning_data.R     (FINESS reference, CAPACT beds, enriched nodes)
#     • LOS + census calc       (daily census, LOS stats, SPARES matching)
#
# SECTION ORDER (re-arranged so every object is built before it is used)
#   Part A — Network data: weekly transfers + daily admissions
#   Part B — Facility metadata: coordinates + spatial join (region/department)
#   Part C — FINESS reference + CAPACT bed counts
#   Part D — Build node attribute tables (network / full / enriched)
#   Part E — Coverage audit
#   Part F — Facility counts by type x region
#   Part G — Patient stays: LOS + daily census
#   Part H — Merge LOS + census + beds -> hospital_stats, assign type_spares
#   Part I — Attach SPARES region x type ESBL incidence to hospital_stats
#            (now correctly AFTER hospital_stats/type_spares exist)
#   Part J — Final facility-level dataset (one row per facility)
#   Part K — Save all outputs
#   Part L — Final summary printout
#
# OUTPUTS — ALL saved to a single folder: Datasets/Cleaned Model Input Data/
#   node_attributes.RDS              — network-only facilities (for simulation)
#   node_attributes_full.RDS         — ALL facilities in facility_meta (for SPARES),
#                                       includes SPARES region x type ESBL incidence
#   node_attributes_enriched.RDS/csv — network nodes + CAPACT beds
#   weekly.RDS                       — harmonised transfer edge list
#   daily_admission.RDS              — daily admissions panel
#   daily_census.RDS                 — daily patient census per facility
#   hospital_stats_los_census.RDS/csv— facility-level LOS + census + bed counts +
#                                       type_spares + SPARES ESBL incidence
#   reg_type_stats_los.RDS/csv       — region x type_spares aggregated LOS
#   facility_level_final.RDS/csv     — ONE ROW PER FACILITY: identity, geography,
#                                       type, beds, admissions, LOS, patient-days,
#                                       census, and SPARES ESBL incidence (overall
#                                       + per-species) — primary downstream input
#   coverage_report.csv              — node coverage audit
#   facility_type_region_summary.csv — facility counts by type x region
#   dataset_node_counts.csv          — node counts across datasets
#   MCO_enriched.csv / SSR_enriched.csv / MCO_SSR_enriched.csv — type subsets
#
# ==============================================================================


# ── 0. Libraries ───────────────────────────────────────────────────────────────
library(here)
library(tidyverse)
library(lubridate)
library(janitor)   # clean_names()
library(sf)        # Lambert-93 -> WGS84 reprojection
library(giscoR)    # GISCO France admin boundaries

here::i_am("Code/00_data_prep_unified.R")
options(scipen = 999)

message("══════════════════════════════════════════════════════════════════")
message("  ARCANE — Unified Data Preparation Pipeline")
message("══════════════════════════════════════════════════════════════════")


# ── 1. File paths ──────────────────────────────────────────────────────────────
message("\n── 1. Checking input files ──")

RAW <- list(
  weekly        = here("Datasets", "MCO_SSR_HBN_2024",
                       "MCO_SSR_HBN_Direct_2024",
                       "HBN_weekly_sliding_edgelist_2024.csv"),
  facility_meta = here("Datasets", "MCO_SSR_HBN_2024",
                       "finessgeo_metadata_2024.csv"),
  daily         = here("Datasets", "MCO_SSR_HBN_2024",
                       "MCO_SSR_HBN_Direct_2024",
                       "NO_ADMISSION_DAILY_DIRCT_HBN.csv"),
  stays         = here("Datasets", "MCO_SSR_HBN_2024",
                       "MCO_SSR_HBN_IP_Direct_2024",
                       "WORK_QUERY_FOR_BEFORE_DIRECT_SSR_MCO_HBN.csv"),
  finess        = here("Datasets", "Facility Data", "etalab_finess_et.csv"),
  capact        = here("Datasets", "Facility Data", "CAPACT24.csv"),
  spares        = here("Datasets", "SPARES", "incidence_eblse.txt")
)

missing_files <- Filter(Negate(file.exists), RAW)
if (length(missing_files) > 0) {
  stop("Missing input files:\n",
       paste(" x", names(missing_files), "->", unlist(missing_files),
             collapse = "\n"))
}
message("  All input files found OK")

# Output folder — single destination for every output of this script
output_dir <- here("Datasets", "Cleaned Model Input Data")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


# ── 2. Helper functions ────────────────────────────────────────────────────────

# Rename a column only if one of the candidate names exists in the data frame.
# Avoids crashes when zero candidates match.
safe_rename <- function(df, new_name, candidates) {
  hit <- intersect(names(df), candidates)
  if (length(hit) == 0) {
    warning("safe_rename: none of [", paste(candidates, collapse = ", "),
            "] found - column NOT renamed to '", new_name, "'.\n",
            "  Actual columns: ", paste(names(df), collapse = ", "))
    return(df)
  }
  rename(df, !!new_name := !!hit[1])
}

# Save an RDS to the output folder.
save_rds_out <- function(obj, filename) {
  path <- file.path(output_dir, filename)
  saveRDS(obj, file = path)
  message("    Saved: ", path)
}

# Save a CSV to the output folder.
save_csv_out <- function(df, filename) {
  path <- file.path(output_dir, filename)
  write_csv(df, path)
  message("    Saved: ", path)
}

# Canonicalise French region spellings to a single standard that matches
# SPARES exactly. Handles three categories of mismatch found in the data:
#   1. Numeric INSEE region codes (from the CAPACT REG fallback in Part B2)
#   2. GISCO NAME_LATN spelling variants (missing accents/hyphens, em-dash)
#   3. Pass-through for anything already correctly spelled
# Implemented as a named-vector lookup (NOT dplyr::case_when) because
# case_when() inside a helper function called from mutate() can fail to
# resolve bare column-name symbols against the function argument.
canonicalise_region <- function(x) {
  lookup <- c(
    # --- numeric INSEE region codes (CAPACT REG fallback) ---
    "11" = "Île-de-France",
    "24" = "Centre-Val de Loire",
    "27" = "Bourgogne-Franche-Comté",
    "28" = "Normandie",
    "32" = "Hauts-de-France",
    "44" = "Grand-Est",
    "52" = "Pays de la Loire",
    "53" = "Bretagne",
    "75" = "Nouvelle-Aquitaine",
    "76" = "Occitanie",
    "84" = "Auvergne-Rhône-Alpes",
    "93" = "Provence-Alpes-Côte d'Azur",
    "94" = "Corse",
    # Overseas (DOM-TOM) - not present in SPARES, mapped for completeness
    "01" = "Guadeloupe",
    "02" = "Martinique",
    "03" = "Guyane",
    "04" = "La Réunion",
    "06" = "Mayotte",
    # --- GISCO name-spelling variants that don't match SPARES ---
    "Ile-de-France"               = "Île-de-France",
    "Centre \u2014 Val de Loire"  = "Centre-Val de Loire",   # em-dash variant
    "Centre-Val-de-Loire"         = "Centre-Val de Loire",
    "Grand Est"                   = "Grand-Est",
    "Hauts de France"             = "Hauts-de-France",
    "Bourgogne Franche-Comté"     = "Bourgogne-Franche-Comté",
    "Provence-Alpes-C\u00f4te d\u2019Azur" = "Provence-Alpes-Côte d'Azur"  # curly apostrophe
  )
  # Vectorised lookup: hit -> canonical spelling, miss -> keep original value
  unname(dplyr::coalesce(lookup[x], x))
}


# ══════════════════════════════════════════════════════════════════════════════
# PART A — NETWORK DATA: WEEKLY TRANSFERS + DAILY ADMISSIONS
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part A: Load transfer network and daily admissions ──")

# A1. Weekly rolling-average transfer edge list
#     Raw columns: finessGeo_origin | finessGeo_target | weight
#     weight = mean daily transfers over a sliding 7-day window
weekly_raw <- read_csv(RAW$weekly, show_col_types = FALSE)

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

# A2. Daily admissions panel
daily_raw <- read_delim(RAW$daily, delim = ";", escape_double = FALSE,
                        trim_ws = TRUE, show_col_types = FALSE)

daily_admission <- daily_raw %>%
  clean_names() %>%
  safe_rename("finess_geo", c("finessgeo", "finess_geo", "finessegeo",
                              "finess_et", "finessgeographique"))

# Unique facilities in the transfer network — this is the NETWORK universe
network_nodes <- tibble(
  finess_geo = unique(c(weekly$finess_geo_origin, weekly$finess_geo_target))
)

# Yearly admission total per facility
admit_yr <- daily_admission %>%
  group_by(finess_geo) %>%
  summarise(admit_yr = sum(no_admissions, na.rm = TRUE), .groups = "drop")

message("  Weekly edges:        ", nrow(weekly))
message("  Network facilities:  ", nrow(network_nodes))
message("  Daily rows:          ", nrow(daily_admission))


# ══════════════════════════════════════════════════════════════════════════════
# PART B — FACILITY METADATA: COORDINATES + SPATIAL JOIN
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part B: Facility metadata + Lambert-93 -> WGS84 reprojection ──")

facility_meta_raw <- read_csv(RAW$facility_meta, show_col_types = FALSE)

facility_meta <- facility_meta_raw %>%
  clean_names() %>%
  safe_rename("finess_geo",
              c("finessgeo", "finess_geo", "finessegeo",
                "finess_et", "finessgeographique")) %>%
  safe_rename("facility_type",
              c("pmsi_category", "categ", "categorie", "type_etab", "category",
                "type_etablissement", "cat_etab", "libelle_categorie",
                "code_categorie"))

message("  facility_meta columns: ", paste(names(facility_meta), collapse = " | "))

# B1. Reproject Lambert-93 (EPSG:2154) -> WGS84 (EPSG:4326)
# ADJUST these two strings if your coordinate column names differ:
coord_x_col <- "coordxet"   # Lambert-93 easting  (X)
coord_y_col <- "coordyet"   # Lambert-93 northing (Y)

if (!all(c(coord_x_col, coord_y_col) %in% names(facility_meta))) {
  stop("Coordinate columns '", coord_x_col, "' / '", coord_y_col,
       "' not found.\n  Available: ", paste(names(facility_meta), collapse = ", "))
}

facility_meta <- facility_meta %>%
  mutate(has_coords_raw = !is.na(.data[[coord_x_col]]) &
           !is.na(.data[[coord_y_col]])) %>%
  {
    has_xy <- filter(., has_coords_raw)
    no_xy  <- filter(., !has_coords_raw)

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

message("  Reprojected: ", sum(!is.na(facility_meta$latitude)),
        " / ", nrow(facility_meta), " facilities have WGS84 coords")

# B2. Spatial join -> city, department, region
#     Strategy:
#       Pass 1 — st_within on GISCO polygons (exact containment, best for mainland)
#       Pass 2 — CAPACT dep/reg code fallback for anything still missing after pass 1
#                (matches finess_geo = fi in sae_raw, pulls dep and reg columns)
#     NOTE: sae_raw is needed again in Part C. We load it here once and reuse
#     the same object there — no double I/O cost.

message("  Fetching France admin boundaries from GISCO...")
communes_fr    <- gisco_get_communes(country = "FR", epsg = "4326")
departments_fr <- gisco_get_nuts(country = "FR", nuts_level = 3,
                                 epsg = "4326", year = "2021")
regions_fr     <- gisco_get_nuts(country = "FR", nuts_level = 1,
                                 epsg = "4326", year = "2021")

has_coords <- facility_meta %>% filter(!is.na(latitude))
no_coords  <- facility_meta %>% filter( is.na(latitude))

# ── Pass 1: st_within (strict spatial containment) ────────────────────────────
pass1 <- has_coords %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_join(communes_fr    %>% select(city       = COMM_NAME), join = st_within, left = TRUE) %>%
  st_join(departments_fr %>% select(department = NAME_LATN), join = st_within, left = TRUE) %>%
  st_join(regions_fr     %>% select(region     = NAME_LATN), join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  group_by(finess_geo) %>%
  slice(1) %>%
  ungroup()

message("  Pass 1 (st_within): ",
        sum(!is.na(pass1$region)), " matched, ",
        sum( is.na(pass1$region)), " still missing region")

# ── Pass 2: CAPACT dep/reg fallback ───────────────────────────────────────────
# Load sae_raw here (reused again in Part C — no extra I/O cost).
# Pull dep and reg columns keyed on fi -> finess_geo.
# These fill in department and region for facilities that fell outside GISCO
# polygons (boundary-sitting, overseas, or missing coordinates).
sae_raw <- read_csv(RAW$capact, show_col_types = FALSE)

capact_geo <- sae_raw %>%
  mutate(finess_geo = str_pad(as.character(fi), 9, pad = "0")) %>%
  select(finess_geo,
         dep_capact = dep,
         reg_capact = reg) %>%
  distinct(finess_geo, .keep_all = TRUE)

# Combine pass1 + no_coords rows, then fill missing geo fields from CAPACT
facility_meta <- bind_rows(
  pass1,
  no_coords %>% mutate(city       = NA_character_,
                       department = NA_character_,
                       region     = NA_character_)
) %>%
  left_join(capact_geo, by = "finess_geo") %>%
  mutate(
    # coalesce: keep GISCO name if present, fall back to CAPACT code otherwise
    department = coalesce(department, as.character(dep_capact)),
    region     = coalesce(region,     as.character(reg_capact))
  ) %>%
  select(-dep_capact, -reg_capact) %>%
  # Standardise region spelling/codes so it matches SPARES exactly — fixes:
  #   - numeric INSEE codes left over from the CAPACT fallback (e.g. "84")
  #   - GISCO spelling variants (missing accents/hyphens, em-dash)
  mutate(region = canonicalise_region(region))

message("  After CAPACT fallback: ",
        sum(!is.na(facility_meta$region)), " / ", nrow(facility_meta),
        " (", round(mean(!is.na(facility_meta$region)) * 100, 1), "%) have region")


# ══════════════════════════════════════════════════════════════════════════════
# PART C — FINESS REFERENCE + CAPACT BED COUNTS
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part C: FINESS reference + CAPACT beds ──")

# C1. FINESS geographic establishment reference
#     Raw file: ISO-8859-1, semicolon-delimited, no header, skip=1
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
  RAW$finess,
  delim          = ";",
  col_names      = finess_cols,
  skip           = 1,
  locale         = locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE
)

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

message("  FINESS geo establishments: ", nrow(finess_clean))

# C2. SAE CAPACT bed counts — classify into MCO vs SSR, sum per facility
# NOTE: sae_raw was already loaded in Part B2 for the CAPACT geo fallback.
# Reusing the same object here — no second read needed.

message("  CAPACT discipline codes found:")
sae_raw %>% distinct(DISCI, DISCIPLINE) %>% print(n = 50)

beds_by_finess <- sae_raw %>%
  mutate(
    care_type = case_when(
      str_detect(DISCIPLINE, "Medecine|Chirurgie|Gyneco|Médecine|Chirurgie|Gynéco") ~ "MCO",
      str_detect(DISCIPLINE, "Soins de") ~ "SSR",
      TRUE ~ NA_character_
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
    facility_type_capact = case_when(
      lit_MCO > 0 & lit_SSR > 0 ~ "MCO/SSR",
      lit_MCO > 0               ~ "MCO",
      lit_SSR > 0               ~ "SSR",
      TRUE                      ~ "Other"
    ),
    capact_total_beds = lit_MCO + lit_SSR
  ) %>%
  select(finess_geo,
         facility_name_capact = rs,
         capact_beds_mco      = lit_MCO,
         capact_beds_ssr      = lit_SSR,
         capact_total_beds,
         facility_type_capact)

message("  CAPACT facilities with bed data: ",
        sum(beds_by_finess$capact_total_beds > 0))


# ══════════════════════════════════════════════════════════════════════════════
# PART D — BUILD NODE ATTRIBUTE TABLES
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part D: Build node attribute tables ──")

# D1. NETWORK version — only facilities in the weekly transfer network
#     Used for simulation / seeding jobs
node_attributes <- network_nodes %>%
  left_join(facility_meta, by = "finess_geo") %>%
  left_join(admit_yr,      by = "finess_geo")

# D2. FULL version — ALL facilities in facility_meta
#     Used for SPARES incidence estimation (hospitals reporting AMR cases
#     may not appear in the transfer network but are still valid)
node_attributes_full <- facility_meta %>%
  left_join(admit_yr, by = "finess_geo") %>%
  mutate(in_transfer_network = finess_geo %in% network_nodes$finess_geo)

# D3. ENRICHED version — network nodes + CAPACT beds
#     Adds CAPACT bed counts and disambiguates facility_type source labels
node_attributes_enriched <- node_attributes %>%
  mutate(finess_geo = str_pad(as.character(finess_geo), 9, pad = "0")) %>%
  rename(facility_type_pmsi  = facility_type,
         pmsi_total_beds_mco = total_beds_mco) %>%
  left_join(beds_by_finess, by = "finess_geo")

message("  node_attributes (network):  ", nrow(node_attributes),
        " facilities x ", ncol(node_attributes), " columns")
message("  node_attributes_full (all): ", nrow(node_attributes_full),
        " facilities x ", ncol(node_attributes_full), " columns")
message("  node_attributes_enriched:   ", nrow(node_attributes_enriched),
        " facilities x ", ncol(node_attributes_enriched), " columns")
message("  In transfer network:        ",
        sum(node_attributes_full$in_transfer_network),
        " / ", nrow(node_attributes_full))


# ══════════════════════════════════════════════════════════════════════════════
# PART E — COVERAGE AUDIT
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part E: Coverage audit ──")

# Per-source flags for network nodes
src_coords <- network_nodes %>%
  left_join(facility_meta %>%
              select(finess_geo, any_of(c("latitude","longitude",
                                          "city","department","region"))),
            by = "finess_geo") %>%
  mutate(in_coords   = !is.na(latitude),
         has_lat_lon = !is.na(latitude) & !is.na(longitude),
         has_region  = !is.na(region))

src_type <- network_nodes %>%
  left_join(facility_meta %>% select(finess_geo, facility_type),
            by = "finess_geo") %>%
  mutate(in_facility_meta  = !is.na(facility_type),
         has_facility_type = !is.na(facility_type))

src_admissions <- network_nodes %>%
  left_join(
    daily_admission %>%
      group_by(finess_geo) %>%
      summarise(admit_yr      = sum(no_admissions,       na.rm = TRUE),
                n_active_days = sum(!is.na(no_admissions) & no_admissions > 0),
                n_days_total  = n(), .groups = "drop"),
    by = "finess_geo"
  ) %>%
  mutate(in_daily           = !is.na(admit_yr),
         admissions_nonzero = !is.na(admit_yr) & admit_yr > 0,
         pct_active_days    = ifelse(n_days_total > 0,
                                     n_active_days / n_days_total, NA_real_))

coverage_per_node <- network_nodes %>%
  left_join(src_coords     %>% select(finess_geo, in_coords, has_lat_lon,
                                      has_region, latitude, longitude,
                                      city, department, region),
            by = "finess_geo") %>%
  left_join(src_type       %>% select(finess_geo, in_facility_meta,
                                      has_facility_type, facility_type),
            by = "finess_geo") %>%
  left_join(src_admissions %>% select(finess_geo, in_daily, admissions_nonzero,
                                      admit_yr, n_active_days,
                                      n_days_total, pct_active_days),
            by = "finess_geo") %>%
  mutate(fully_covered = has_lat_lon & has_facility_type & admissions_nonzero)

pct_fn <- function(x) round(mean(x, na.rm = TRUE) * 100, 1)
n_fn   <- function(x) sum(x, na.rm = TRUE)

source_summary <- tribble(
  ~source,            ~n_matched,                                      ~pct_matched,
  "facility_meta",    n_fn(coverage_per_node$in_facility_meta),        pct_fn(coverage_per_node$in_facility_meta),
  "coords (in meta)", n_fn(coverage_per_node$in_coords),               pct_fn(coverage_per_node$in_coords),
  "daily_admission",  n_fn(coverage_per_node$in_daily),                pct_fn(coverage_per_node$in_daily)
) %>%
  mutate(n_total   = nrow(network_nodes),
         n_missing = n_total - n_matched)

var_fill <- tibble(
  attribute = c("lat/lon (WGS84)", "region resolved",
                "facility type (PMSI)", "admissions > 0",
                "fully covered (all three)"),
  n_present = c(n_fn(coverage_per_node$has_lat_lon),
                n_fn(coverage_per_node$has_region),
                n_fn(coverage_per_node$has_facility_type),
                n_fn(coverage_per_node$admissions_nonzero),
                n_fn(coverage_per_node$fully_covered))
) %>%
  mutate(n_missing   = nrow(network_nodes) - n_present,
         pct_present = round(n_present / nrow(network_nodes) * 100, 1))

message("\n  Source match rates:")
walk2(source_summary$source, source_summary$pct_matched,
      ~ message(sprintf("    %-26s %5.1f%%", .x, .y)))
message("\n  Attribute fill rates:")
walk2(var_fill$attribute, var_fill$pct_present,
      ~ message(sprintf("    %-42s %5.1f%%", .x, .y)))
message("  Fully covered nodes: ",
        n_fn(coverage_per_node$fully_covered), " / ", nrow(network_nodes),
        " (", pct_fn(coverage_per_node$fully_covered), "%)")


# ══════════════════════════════════════════════════════════════════════════════
# PART F — FACILITY COUNTS: BY TYPE x REGION
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part F: Facility counts by type and region ──")

# F1. Dataset-level node counts
dataset_node_counts <- tibble(
  dataset                  = c("weekly transfer network",
                               "facility_meta (all)",
                               "daily_admission",
                               "CAPACT beds",
                               "FINESS reference",
                               "node_attributes_enriched"),
  n_facilities             = c(nrow(network_nodes),
                               nrow(facility_meta),
                               n_distinct(daily_admission$finess_geo),
                               nrow(beds_by_finess),
                               nrow(finess_clean),
                               nrow(node_attributes_enriched)),
  n_in_transfer_network    = c(nrow(network_nodes),
                               sum(facility_meta$finess_geo %in% network_nodes$finess_geo),
                               sum(unique(daily_admission$finess_geo) %in% network_nodes$finess_geo),
                               sum(beds_by_finess$finess_geo %in% network_nodes$finess_geo),
                               sum(finess_clean$finess_geo %in% network_nodes$finess_geo),
                               nrow(node_attributes_enriched))
) %>%
  mutate(pct_in_network = round(n_in_transfer_network / n_facilities * 100, 1))

message("\n  Node counts per dataset:")
print(dataset_node_counts, n = Inf)

# F2. Facility type breakdown for NETWORK nodes
type_counts_network <- node_attributes_enriched %>%
  count(hospital_type, name = "n_network") %>%
  arrange(desc(n_network))

message("\n  Facility types in network:")
print(type_counts_network, n = Inf)

# F3. Facility type x region breakdown (network nodes)
type_region_counts <- node_attributes_enriched %>%
  count(hospital_type, region, name = "n") %>%
  arrange(hospital_type, desc(n))

message("\n  Facility type x region (network):")
print(type_region_counts, n = Inf)

# F4. Same breakdown for the FULL (non-network-filtered) table
type_region_counts_full <- node_attributes_full %>%
  count(facility_type, region, name = "n_full") %>%
  arrange(facility_type, desc(n_full))

# Combine network vs full for comparison
facility_summary_combined <- type_region_counts %>%
  full_join(type_region_counts_full,
            by = c("hospital_type" = "facility_type", "region" = "region")) %>%
  mutate(
    n          = replace_na(n, 0),
    n_full     = replace_na(n_full, 0),
    pct_in_net = round(n / n_full * 100, 1)
  ) %>%
  arrange(hospital_type, desc(n_full))

message("\n  Combined type x region — network vs full meta:")
print(facility_summary_combined, n = Inf)


# ══════════════════════════════════════════════════════════════════════════════
# PART G — PATIENT STAYS: LOS + DAILY CENSUS
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part G: Patient stays — LOS and daily census ──")

# G1. Load and parse stays
stays_raw <- read_delim(RAW$stays, delim = ";", escape_double = FALSE,
                        trim_ws = TRUE, show_col_types = FALSE)

stays <- stays_raw %>%
  mutate(
    date_entree = as.Date(date_entree, format = "%d/%m/%Y"),
    date_sortie = as.Date(date_sortie, format = "%d/%m/%Y")
  ) %>%
  filter(LOS_Days > 0)   # drop same-day stays (zero LOS)

message("  Stays loaded: ", nrow(stays), " (after removing LOS = 0)")
message("  Date range: ", min(stays$date_entree, na.rm = TRUE),
        " to ", max(stays$date_sortie, na.rm = TRUE))

# G2. LOS stats per facility
los <- stays %>%
  group_by(FinessGeo) %>%
  summarise(
    los_mean     = mean(LOS_Days,                       na.rm = TRUE),
    los_median   = median(LOS_Days,                     na.rm = TRUE),
    los_q1       = quantile(LOS_Days, probs = 0.25,     na.rm = TRUE),
    los_q3       = quantile(LOS_Days, probs = 0.75,     na.rm = TRUE),
    los_ci_low   = quantile(LOS_Days, probs = 0.05,     na.rm = TRUE),
    los_ci_hi    = quantile(LOS_Days, probs = 0.95,     na.rm = TRUE),
    los_sd       = sd(LOS_Days,                         na.rm = TRUE),
    pt_days_total= sum(LOS_Days,                        na.rm = TRUE),
    patient_total= n(),
    .groups = "drop"
  )

message("  LOS stats computed for ", nrow(los), " facilities")

# G3. Daily census — count patients present for each calendar day
#     Feb 1 to Dec 23 to exclude edge-effect outliers at year boundaries
message("  Computing daily census (this takes a few minutes)...")
date_seq <- seq(as.Date("2024-02-01"), as.Date("2024-12-23"), by = "day")

count_day <- function(d) {
  stays %>%
    filter(date_entree <= d & date_sortie >= d) %>%
    group_by(FinessGeo) %>%
    summarise(n_patients = n(), .groups = "drop") %>%
    mutate(day = d)
}

daily_census <- map_dfr(date_seq, count_day) %>%
  complete(FinessGeo, day = date_seq, fill = list(n_patients = 0))

message("  Daily census rows: ", nrow(daily_census))

# G4. Census summary stats per facility
hospital_census <- daily_census %>%
  group_by(FinessGeo) %>%
  summarise(
    census_min      = min(n_patients,                     na.rm = TRUE),
    census_max      = max(n_patients,                     na.rm = TRUE),
    census_mean     = round(mean(n_patients,              na.rm = TRUE), 2),
    census_median   = median(n_patients,                  na.rm = TRUE),
    census_95ci_low = quantile(n_patients, probs = 0.05,  na.rm = TRUE),
    census_95ci_hi  = quantile(n_patients, probs = 0.95,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(finess_geo = FinessGeo)


# ══════════════════════════════════════════════════════════════════════════════
# PART H — MERGE: LOS + CENSUS + BEDS -> hospital_stats, ASSIGN type_spares
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part H: Merge LOS + census + bed counts ──")

# Join census to enriched nodes (which already has CAPACT beds + PMSI beds)
census_enriched <- full_join(hospital_census, node_attributes_enriched,
                             by = "finess_geo")

# Merge with LOS
hospital_stats <- full_join(
  los,
  census_enriched,
  by = c("FinessGeo" = "finess_geo")
) %>%
  rename(finess_geo = FinessGeo)

message("  hospital_stats rows: ", nrow(hospital_stats))

# H1. Assign type_spares — SPARES-compatible facility type labels
#     Priority: activity axis (SSR = Rehabilitation) over ownership axis
hospital_stats <- hospital_stats %>%
  mutate(
    type_spares = case_when(
      # Activity axis: SSR-only -> Rehabilitation regardless of ownership
      hospital_type == "SSR" | facility_type_pmsi == "SSR"    ~ "Rehabilitation hospital",
      # Ownership axis for acute (MCO / MCO-SSR) facilities
      facility_type_pmsi == "General public hospital (CH)"         ~ "General public hospital",
      facility_type_pmsi == "Regional/University hospital (CHR/U)" ~ "University hospital",
      facility_type_pmsi == "Private"                              ~ "Private for profit hospital",
      facility_type_pmsi == "Non-profit facility (PSPH/EBNL)"      ~ "Private not-for-profit hospital",
      facility_type_pmsi == "Cancer centre (CLCC)"                 ~ "Cancer centre (CLCC)",
      TRUE ~ NA_character_
    )
  )

# H2. Region x type_spares aggregated LOS (kept for diagnostics / reporting)
region_type_stats <- hospital_stats %>%
  filter(!is.na(type_spares), !is.na(region)) %>%
  group_by(region, type_spares) %>%
  summarise(
    reg_type_pt_days    = sum(pt_days_total,  na.rm = TRUE),
    reg_type_n_patients = sum(patient_total,  na.rm = TRUE),
    n_facilities        = n(),
    .groups = "drop"
  ) %>%
  mutate(
    reg_type_los_avg = reg_type_pt_days / reg_type_n_patients
  )

message("  region_type_stats rows: ", nrow(region_type_stats))


# ══════════════════════════════════════════════════════════════════════════════
# PART I — SPARES REGION x TYPE ESBL INCIDENCE -> hospital_stats
# ══════════════════════════════════════════════════════════════════════════════
# Runs AFTER Part H so hospital_stats + type_spares already exist.
# hospital_stats then carries SPARES incidence from this point onward, and
# node_attributes_full simply inherits it via finess_geo (no recomputation).
message("\n── Part I: Attach SPARES ESBL incidence to hospital_stats ──")

# I1. Load SPARES and compute row-wise all-ESBL incidence
spares <- read.delim(RAW$spares) %>%
  mutate(
    # Row-wise sum across the three ESBL-E species, scaled to per-1000 bed-days
    # (matches the existing per-species incidence_* columns' scale).
    # NOTE: must be row-wise `+`, NOT sum() across whole columns.
    incidence_esbl_all = (n_esbl_ecloacaecomplex + n_esbl_ecoli + n_esbl_kpneumoniae) /
      n_bed_days * 1000,
    region = canonicalise_region(as.character(region))
  )

# I2. Average across years -> one row per region x type cell
spares_by_cell <- spares %>%
  group_by(region, type) %>%
  summarise(
    incidence_region_type_ESBL_all         = mean(incidence_esbl_all,             na.rm = TRUE),
    incidence_region_type_ESBL_ecoli       = mean(incidence_esbl_ecoli,           na.rm = TRUE),
    incidence_region_type_ESBL_kpneumoniae = mean(incidence_esbl_kpneumoniae,     na.rm = TRUE),
    incidence_region_type_ESBL_ecloacae    = mean(incidence_esbl_ecloacaecomplex, na.rm = TRUE),
    n_bed_days_spares                      = sum(n_bed_days,                      na.rm = TRUE),
    .groups = "drop"
  )

message("  SPARES types:   ", paste(sort(unique(spares$type)),   collapse = " | "))
message("  SPARES regions: ", paste(sort(unique(spares$region)), collapse = " | "))
message("  hospital_stats types (type_spares): ",
        paste(sort(unique(hospital_stats$type_spares)), collapse = " | "))
message("  hospital_stats regions:             ",
        paste(sort(unique(hospital_stats$region)), collapse = " | "))

# I3. Join onto hospital_stats via region + type_spares (direct match,
#     no crosswalk needed — type_spares was built to mirror SPARES `type`)
hospital_stats <- hospital_stats %>%
  left_join(
    spares_by_cell,
    by = c("region" = "region", "type_spares" = "type")
  )

n_total_hs   <- nrow(hospital_stats)
n_matched_hs <- sum(!is.na(hospital_stats$incidence_region_type_ESBL_all))

message("\n  SPARES match summary (hospital_stats):")
message(sprintf("    Total facilities  : %d", n_total_hs))
message(sprintf("    Matched to SPARES : %d (%.0f%%)", n_matched_hs, 100 * n_matched_hs / n_total_hs))

unmatched_combos <- hospital_stats %>%
  filter(is.na(incidence_region_type_ESBL_all)) %>%
  count(region, type_spares, name = "n_facilities") %>%
  arrange(desc(n_facilities))

if (nrow(unmatched_combos) > 0) {
  message("\n  Unmatched region x type_spares combinations (facility counts):")
  print(as.data.frame(unmatched_combos), row.names = FALSE)

  message("\n  Regions in SPARES not found in hospital_stats:")
  missing_reg <- setdiff(unique(spares$region), unique(hospital_stats$region))
  if (length(missing_reg) > 0) print(missing_reg) else message("    None (all regions matched)")

  message("  Types in SPARES not found in hospital_stats$type_spares:")
  missing_type <- setdiff(unique(spares$type), unique(hospital_stats$type_spares))
  if (length(missing_type) > 0) print(missing_type) else message("    None (all types matched)")
}

# I4. Propagate the same incidence columns onto node_attributes_full,
#     so both objects stay in sync without recomputing anything.
incidence_cols <- c(
  "finess_geo",
  "incidence_region_type_ESBL_all",
  "incidence_region_type_ESBL_ecoli",
  "incidence_region_type_ESBL_kpneumoniae",
  "incidence_region_type_ESBL_ecloacae",
  "n_bed_days_spares"
)

node_attributes_full <- node_attributes_full %>%
  left_join(
    hospital_stats %>% select(all_of(incidence_cols)),
    by = "finess_geo"
  )

message("\n  node_attributes_full incidence coverage: ",
        sum(!is.na(node_attributes_full$incidence_region_type_ESBL_all)),
        " / ", nrow(node_attributes_full))


# ══════════════════════════════════════════════════════════════════════════════
# PART J — FINAL FACILITY-LEVEL DATASET
# ══════════════════════════════════════════════════════════════════════════════
# One row per finess_geo with everything needed for downstream calibration /
# seeding work in a single place: identity, geography, type, beds, LOS,
# patient-days, admissions, census, and SPARES ESBL incidence.
message("\n── Part J: Build final facility-level dataset ──")

facility_level_final <- hospital_stats %>%
  select(
    # Identity
    finess_geo, facility_name, facility_name_capact,

    # Geography
    city, department, region,

    # Type (multiple source labels kept for traceability)
    hospital_type, facility_type_pmsi, facility_type_capact, type_spares,

    # Beds
    pmsi_total_beds_mco, capact_beds_mco, capact_beds_ssr, capact_total_beds,

    # Admissions (yearly total, from Part A)
    admit_yr,

    # Length of stay (facility-level, from Part G)
    los_mean, los_median, los_q1, los_q3, los_ci_low, los_ci_hi, los_sd,

    # Patient volume / patient-days (facility-level, from Part G)
    patient_total, pt_days_total,

    # Daily census stats (facility-level, from Part G)
    census_min, census_max, census_mean, census_median,
    census_95ci_low, census_95ci_hi,

    # SPARES region x type ESBL incidence (ecological estimate, from Part I)
    incidence_region_type_ESBL_all,
    incidence_region_type_ESBL_ecoli,
    incidence_region_type_ESBL_kpneumoniae,
    incidence_region_type_ESBL_ecloacae,
    n_bed_days_spares
  ) %>%
  # in_transfer_network lives only on node_attributes_full — attach via finess_geo
  left_join(
    node_attributes_full %>% select(finess_geo, in_transfer_network),
    by = "finess_geo"
  ) %>%
  relocate(in_transfer_network, .after = type_spares)

message("  facility_level_final: ", nrow(facility_level_final), " facilities x ",
        ncol(facility_level_final), " columns")
message("  Columns: ", paste(names(facility_level_final), collapse = " | "))


# ══════════════════════════════════════════════════════════════════════════════
# PART K — SAVE ALL OUTPUTS
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part K: Saving outputs to ", output_dir, " ──")

# K1. Core node-level objects
save_rds_out(node_attributes,           "node_attributes.RDS")
save_rds_out(node_attributes_full,      "node_attributes_full.RDS")
save_rds_out(node_attributes_enriched,  "node_attributes_enriched.RDS")
save_rds_out(weekly,                    "weekly.RDS")
save_rds_out(daily_admission,           "daily_admission.RDS")
save_rds_out(daily_census,              "daily_census.RDS")

# K2. Facility-level / aggregate analysis objects
save_rds_out(hospital_stats,            "hospital_stats_los_census.RDS")
save_rds_out(region_type_stats,         "reg_type_stats_los.RDS")
save_rds_out(facility_level_final,      "facility_level_final.RDS")

# K3. CSV mirrors of the above (for non-R use / quick inspection)
save_csv_out(node_attributes_enriched,  "node_attributes_enriched.csv")
save_csv_out(hospital_stats,            "hospital_stats_los_census.csv")
save_csv_out(region_type_stats,         "reg_type_stats_los.csv")
save_csv_out(dataset_node_counts,       "dataset_node_counts.csv")
save_csv_out(facility_summary_combined, "facility_type_region_summary.csv")
save_csv_out(facility_level_final,      "facility_level_final.csv")

# K4. Coverage report
coverage_csv <- bind_rows(
  tibble(SECTION = "PER_NODE_DETAIL"),
  coverage_per_node %>%
    mutate(across(where(is.logical), as.integer),
           across(where(is.numeric), ~ round(., 4))),
  tibble(SECTION = ""),
  tibble(SECTION = "SOURCE_MATCH_RATES"),
  source_summary,
  tibble(SECTION = ""),
  tibble(SECTION = "ATTRIBUTE_FILL_RATES"),
  var_fill
)
save_csv_out(coverage_csv, "coverage_report.csv")

# K5. Hospital type subsets
for (htype in c("MCO", "SSR", "MCO/SSR")) {
  fname <- paste0(gsub("/", "_", htype), "_enriched.csv")
  node_attributes_enriched %>%
    filter(hospital_type == htype) %>%
    select(finess_geo, hospital_type, facility_type_pmsi, facility_type_capact,
           facility_name, pmsi_total_beds_mco,
           capact_beds_mco, capact_beds_ssr, capact_total_beds) %>%
    save_csv_out(fname)
}


# ══════════════════════════════════════════════════════════════════════════════
# PART L — FINAL SUMMARY PRINTOUT
# ══════════════════════════════════════════════════════════════════════════════
message("\n══════════════════════════════════════════════════════════════════")
message("  FINAL SUMMARY")
message("══════════════════════════════════════════════════════════════════")

message("\n  NODE COUNTS ACROSS DATASETS:")
print(dataset_node_counts, n = Inf)

message("\n  FACILITY TYPES IN NETWORK:")
print(type_counts_network, n = Inf)

message("\n  FACILITY TYPE x REGION (network vs full meta):")
print(facility_summary_combined, n = Inf)

message("\n  SPARES MATCH RESULT (hospital_stats):")
message(sprintf("    Matched: %d / %d facilities (%.0f%%)",
                n_matched_hs, n_total_hs, 100 * n_matched_hs / n_total_hs))
if (nrow(unmatched_combos) > 0) {
  message("    Unmatched combos:")
  print(as.data.frame(unmatched_combos), row.names = FALSE)
}

message("\n  FINAL FACILITY-LEVEL DATASET:")
message("    facility_level_final: ", nrow(facility_level_final), " facilities x ",
        ncol(facility_level_final), " columns")

message("\n  OUTPUTS SAVED TO:")
message("    ", output_dir)

message("\n══════════════════════════════════════════════════════════════════")
message("  DONE — 00_data_prep_unified.R completed successfully")
message("══════════════════════════════════════════════════════════════════")


# ── Clean up: keep only final outputs in the environment ──────────────────────
rm(list = setdiff(ls(), c(
  "node_attributes", "node_attributes_full", "node_attributes_enriched",
  "weekly", "daily_admission", "daily_census",
  "hospital_stats", "region_type_stats", "facility_level_final",
  "coverage_per_node", "source_summary", "var_fill",
  "dataset_node_counts", "facility_summary_combined",
  "type_counts_network", "type_region_counts"
)))

# ==============================================================================
# END OF 00_data_prep_unified.R
#
# LOAD IN DOWNSTREAM SCRIPTS
# ---------------------------
# output_dir <- here("Datasets", "Cleaned Model Input Data")
#
# node_attributes          <- readRDS(file.path(output_dir, "node_attributes.RDS"))
# node_attributes_full     <- readRDS(file.path(output_dir, "node_attributes_full.RDS"))
# node_attributes_enriched <- readRDS(file.path(output_dir, "node_attributes_enriched.RDS"))
# weekly                   <- readRDS(file.path(output_dir, "weekly.RDS"))
# daily_admission          <- readRDS(file.path(output_dir, "daily_admission.RDS"))
# daily_census             <- readRDS(file.path(output_dir, "daily_census.RDS"))
# hospital_stats           <- readRDS(file.path(output_dir, "hospital_stats_los_census.RDS"))
# region_type_stats        <- readRDS(file.path(output_dir, "reg_type_stats_los.RDS"))
# facility_level_final     <- readRDS(file.path(output_dir, "facility_level_final.RDS"))
# ==============================================================================
