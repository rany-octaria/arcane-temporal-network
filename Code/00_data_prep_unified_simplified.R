# ==============================================================================
# 00_data_prep_unified_simplified.R
# ARCANE Project — Unified Data Preparation Pipeline (SIMPLIFIED)
# Rany Octaria | Le CNAM / MESuRS
# ==============================================================================
#
# SIMPLIFIED vs. 00_data_prep_unified_09_2026_newSPARES.R — what changed and why:
#   1. Part H no longer re-reads finessgeo_metadata_2024.csv a second time to
#      get type_spares. Part B already computes factype_spares_pmsi inline on
#      facility_meta; it flows through the existing join chain (facility_meta
#      -> node_attributes -> node_attributes_enriched -> hospital_stats) on
#      its own, so Part H now just renames it. One less file read.
#   2. Part D3's `hospital_type = facility_type_capact` overwrite is removed.
#      It only existed to feed the old Part F / K5 (both removed below), and
#      it was silently CLOBBERING facility_meta's own real PMSI hospital_type
#      field (the one factype_spares_pmsi depends on). Removing it lets that
#      real field survive under its own name as one of the "type in a
#      different dataset" columns in facility_level_final.
#   3. Coverage audit (old Part E) and facility counts by type x region (old
#      Part F) are removed entirely - not part of the requested final
#      dictionary, and nothing downstream depended on them.
#   4. CAPACT bed counts, PMSI total beds, and any comparison between them are
#      removed - not needed; daily census (Part G) is what modeling actually
#      uses. facility_type_capact (the MCO/SSR/MCO-SSR/Other CATEGORY) is kept,
#      since it's one of the requested "type in a different dataset" columns.
#   5. finess_clean (FINESS's own categetab/libcategetab) was computed in the
#      original but never joined to anything - a 4th type source, now wired in.
#   6. ADDED: a diagnostic (Part H3) on WHY type_spares is NA where it is,
#      classified by which upstream field is missing, plus a breakdown of
#      every other type field and finess_geo for the affected facilities.
#      Saved as type_spares_NA_diagnostic.csv.
#   7. Part I now detects whichever of "factype_spares_pmsi" / "factype_pmsi_
#      spares" is actually present in the incidence CSV, instead of hardcoding
#      one spelling - the two pipelines have drifted on this name before.
#   8. K5 (MCO/SSR/MCO_SSR_enriched.csv subsets) is removed - built from the
#      bed-count fields that are no longer computed.
#   9. IMPORTANT FIX, found while diagnosing: hospital_stats was being built
#      from node_attributes_enriched, which is network-only (filtered from
#      node_attributes, not node_attributes_full). Any facility with census/
#      LOS data but NOT in the weekly transfer network got every type field -
#      facility_type_pmsi, facility_type_capact, categetab, and type_spares -
#      as NA, not because PMSI data was actually missing but because it was
#      structurally excluded before the join. node_attributes_full (ALL
#      facilities) now gets the same type-source joins as node_attributes_
#      enriched, and hospital_stats is built from THAT instead.
#      node_attributes_enriched is now just network_nodes %>% filter() of
#      the already-enriched node_attributes_full, so the two can't drift
#      apart. Confirmed with a test case: a non-network facility with real
#      PMSI data now correctly resolves type_spares instead of showing NA.
#
# SECTION ORDER
#   Part A — Network data: weekly transfers + daily admissions
#   Part B — Facility metadata: coordinates + spatial join (region/department)
#            + factype_spares_pmsi (our new type category)
#   Part C — FINESS reference (categetab/libcategetab) + CAPACT facility type
#   Part D — Build node attribute tables (network / full / enriched)
#   Part G — Patient stays: LOS + daily census
#   Part H — Merge LOS + census + type -> hospital_stats, assign type_spares,
#            diagnose type_spares == NA
#   Part I — Attach SPARES region x type ESBL incidence to hospital_stats
#   Part J — Final facility-level dataset (one row per facility)
#   Part K — Save all outputs
#   Part L — Final summary printout
#
# OUTPUTS — ALL saved to a single folder: Datasets/Cleaned Model Input Data/
#   node_attributes.RDS                — network-only facilities (for simulation)
#   node_attributes_full.RDS           — ALL facilities in facility_meta (for SPARES),
#                                         includes SPARES region x type ESBL incidence
#   node_attributes_enriched.RDS/csv   — network nodes + facility type sources
#   weekly.RDS                         — harmonised transfer edge list
#   daily_admission.RDS                — daily admissions panel
#   daily_census.RDS                   — daily patient census per facility
#   hospital_stats_los_census.RDS/csv  — facility-level LOS + census + type_spares
#                                         + SPARES ESBL incidence
#   reg_type_stats_los.RDS/csv         — region x type_spares aggregated LOS
#   facility_level_final.RDS/csv       — ONE ROW PER FACILITY: identity, type
#                                         (our category + every dataset's own),
#                                         geography (incl. XY), stays, patient-
#                                         days, LOS, census, and SPARES ESBL
#                                         incidence + its denominator —
#                                         primary downstream input
#   type_spares_NA_diagnostic.csv      — facilities with type_spares == NA,
#                                         why, and their other type fields
#
# ==============================================================================


# ── 0. Libraries ───────────────────────────────────────────────────────────────
library(here)
library(tidyverse)
library(lubridate)
library(janitor)   # clean_names()
library(sf)        # Lambert-93 -> WGS84 reprojection
library(giscoR)    # GISCO France admin boundaries

here::i_am("Code/00_data_prep_unified_simplified.R")
options(scipen = 999)

message("══════════════════════════════════════════════════════════════════")
message("  ARCANE — Unified Data Preparation Pipeline (simplified)")
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
                       "MCO_SSR_HBN_IP_Direct_2024",
                       "NO_INPATIENTS_ADMISSION_DAILY_DIRCT_HBN.csv"),
  stays         = here("Datasets", "MCO_SSR_HBN_2024",
                       "MCO_SSR_HBN_IP_Direct_2024",
                       "WORK_QUERY_FOR_BEFORE_DIRECT_SSR_MCO_HBN.csv"),
  finess        = here("Datasets", "Facility Data", "etalab_finess_et.csv"),
  capact        = here("Datasets", "Facility Data", "CAPACT24.csv")
  # spares (incidence_eblse.txt) removed - Part I now reads
  # esbl_incidence_by_region_type_2022.csv instead (see ESBL_INCIDENCE_PATH)
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

# ADDED: facility-count waterfall log, built up throughout the script and
# printed/saved as ONE block at the very end (Part L) - same pattern as
# the sample-selection scripts' `out` log (sample_selection_2022.txt).
# message() calls stay in place too, for live console feedback as each
# step runs - this is the same numbers, consolidated for the end-of-run
# report, not a replacement for the step-by-step console output.
out <- paste0(
  "###############################################################\n",
  "00_data_prep_unified_simplified.R - facility count waterfall\n",
  "###############################################################\n"
)


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
    "Provence-Alpes-C\u00f4te d\u2019Azur" = "Provence-Alpes-Côte d'Azur",  # curly apostrophe
    # --- BMR-Raisin / ATB admin file spelling variants (esbl_incidence_2022.R) ---
    # "Grand Est" and "Hauts de France" above already cover BMR's spelling.
    # "Reunion - Mayotte" (a combined BMR cell) and "Nouvelle Calédonie" (not
    # one of these 18 regions) aren't simple 1:1 renames - handled separately
    # in Part I, not here.
    "Auvergne - Rhône Alpes"      = "Auvergne-Rhône-Alpes",
    "Bourgogne - Franche Comté"   = "Bourgogne-Franche-Comté",
    "Ile de France"               = "Île-de-France",
    "Nouvelle Aquitaine"          = "Nouvelle-Aquitaine",
    "Pays de Loire"               = "Pays de la Loire",
    "Provence Alpes Côte d'Azur"  = "Provence-Alpes-Côte d'Azur"
  )
  # Vectorised lookup: hit -> canonical spelling, miss -> keep original value
  unname(dplyr::coalesce(lookup[x], x))
}

# Overseas French territories (DROM) - excluded from the final facility
# dataset and from the SPARES incidence file (Part I). Rationale: these are
# structurally near-isolated from mainland inter-hospital transfers, the
# reference paper this pipeline follows excludes them for statistical power,
# and the SPARES incidence file has no regional coverage for them anyway.
# Does NOT include Corsica - only asked to exclude overseas territory.
OVERSEAS_REGIONS <- c("Guadeloupe", "Martinique", "Guyane", "La Réunion", "Mayotte")


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
out <- paste0(out,
              "\n[Part A] Weekly transfer edges: ", nrow(weekly),
              "\n[Part A] Facilities in transfer network: ", nrow(network_nodes)
)


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
                "code_categorie")) %>% 
  mutate(
    factype_spares_pmsi = ifelse(hospital_type == "SSR", "SSR", facility_type)
    
  )


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
out <- paste0(out,
              "\n[Part B] Facilities in facility_meta (all, before overseas exclusion): ", nrow(facility_meta),
              "\n[Part B] Facilities with region resolved: ", sum(!is.na(facility_meta$region)),
              " / ", nrow(facility_meta)
)

# MOVED HERE (per instruction) - overseas territory exclusion, applied to
# facility_meta itself, right where region is finalized, so it propagates
# to EVERY downstream object (node_attributes, node_attributes_full,
# node_attributes_enriched, hospital_stats, region_type_stats, the
# type_spares NA diagnostic, the SPARES incidence match diagnostic,
# facility_level_final) automatically, instead of only being dropped right
# before facility_level_final in Part J. Previously, Part I's "unmatched
# region x type_spares combinations" diagnostic ran BEFORE the Part J
# exclusion, so overseas facilities - which can never match the incidence
# file (it has no overseas rows at all) - showed up there as "unmatched"
# even though they were always going to be dropped a few steps later. That
# was correct output from stale-by-then input, not a bug in the match
# logic itself, but it inflated the unmatched-combos count misleadingly
# (97 of 152 facility-instances in one run were overseas facilities that
# were never going to match anything, by design). Excluding here instead
# means that diagnostic now only reports genuine mainland gaps.
n_before_overseas_excl <- nrow(facility_meta)
is_overseas_facility <- facility_meta$region %in% OVERSEAS_REGIONS
n_overseas_excluded <- sum(is_overseas_facility, na.rm = TRUE)

facility_meta <- facility_meta %>% filter(!(region %in% OVERSEAS_REGIONS))

message("  Overseas territory (", paste(OVERSEAS_REGIONS, collapse = ", "), ") excluded: ",
        n_overseas_excluded, " of ", n_before_overseas_excl, " -> ", nrow(facility_meta), " remain")
out <- paste0(out,
              "\n[Part B] Overseas territory excluded: ", n_overseas_excluded,
              " of ", n_before_overseas_excl, " -> ", nrow(facility_meta), " remain"
)


# ══════════════════════════════════════════════════════════════════════════════
# PART C — FINESS REFERENCE + CAPACT FACILITY TYPE
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part C: FINESS reference + CAPACT facility type ──")

# C1. FINESS geographic establishment reference — official category code/label
#     (categetab / libcategetab), a 4th independent "type" source alongside
#     PMSI's own and CAPACT's. Kept, and now actually joined in at Part D3
#     (it was computed but never used in the original script).
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
  mutate(finess_geo = str_pad(as.character(finess_geo), 9, pad = "0")) %>%
  distinct(finess_geo, .keep_all = TRUE)

message("  FINESS geo establishments: ", nrow(finess_clean))

# C2. CAPACT facility type — SIMPLIFIED: only the MCO/SSR/MCO-SSR/Other
#     CATEGORY is kept (one more "type in a different dataset"). The actual
#     bed counts and any PMSI-vs-CAPACT bed comparison are NOT computed -
#     not needed; daily census (Part G) is what modeling actually uses.
# NOTE: sae_raw was already loaded in Part B2 for the CAPACT geo fallback.
# Reusing the same object here — no second read needed.
facility_type_by_finess <- sae_raw %>%
  mutate(
    care_type = case_when(
      str_detect(DISCIPLINE, "Medecine|Chirurgie|Gyneco|Médecine|Chirurgie|Gynéco") ~ "MCO",
      str_detect(DISCIPLINE, "Soins de") ~ "SSR",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(care_type)) %>%
  group_by(fi, care_type) %>%
  summarise(beds = sum(LIT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = care_type, values_from = beds, values_fill = 0) %>%
  mutate(
    finess_geo = str_pad(as.character(fi), 9, pad = "0"),
    facility_type_capact = case_when(
      MCO > 0 & SSR > 0 ~ "MCO/SSR",
      MCO > 0            ~ "MCO",
      SSR > 0            ~ "SSR",
      TRUE               ~ "Other"
    )
  ) %>%
  select(finess_geo, facility_type_capact) %>%
  distinct(finess_geo, .keep_all = TRUE)

message("  CAPACT facility type assigned for: ", nrow(facility_type_by_finess), " facilities")
print(count(facility_type_by_finess, facility_type_capact, name = "n"), n = Inf)
out <- paste0(out,
              "\n[Part C] FINESS geo establishments: ", nrow(finess_clean),
              "\n[Part C] Facilities with CAPACT facility type: ", nrow(facility_type_by_finess)
)

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
#     may not appear in the transfer network but are still valid). Gets the
#     SAME type-source joins as node_attributes_enriched below (facility_type_
#     capact, categetab/libcategetab) - hospital_stats is built from THIS
#     object now, not the network-restricted one, specifically so a facility
#     missing from the transfer network doesn't look like it's missing PMSI
#     data when it isn't (see header note #9 / Part H3's NA diagnostic).
node_attributes_full <- facility_meta %>%
  left_join(admit_yr, by = "finess_geo") %>%
  mutate(
    finess_geo = str_pad(as.character(finess_geo), 9, pad = "0"),
    in_transfer_network = finess_geo %in% network_nodes$finess_geo
  ) %>%
  rename(facility_type_pmsi = facility_type) %>%
  left_join(facility_type_by_finess, by = "finess_geo") %>%
  left_join(finess_clean %>% select(finess_geo, facility_name, categetab, libcategetab),
            by = "finess_geo")

# D3. ENRICHED version — the network-only SUBSET of node_attributes_full
#     (which already carries every type-source join from D2 above). SIMPLIFIED
#     vs. the original in three ways:
#       - no CAPACT bed counts / PMSI-vs-CAPACT bed comparison (not needed)
#       - no "hospital_type = facility_type_capact" overwrite - that alias
#         only existed to feed the old Part F / K5 (both removed), and it
#         was CLOBBERING facility_meta's own real PMSI hospital_type field
#         (the one Part B's factype_spares_pmsi is built from). Removing the
#         overwrite lets that real field survive here under its own name.
#       - built as a filter of node_attributes_full instead of its own
#         separate round of joins, so the two objects can't drift apart.
node_attributes_enriched <- node_attributes_full %>%
  filter(finess_geo %in% network_nodes$finess_geo)

message("  node_attributes (network):  ", nrow(node_attributes),
        " facilities x ", ncol(node_attributes), " columns")
message("  node_attributes_full (all): ", nrow(node_attributes_full),
        " facilities x ", ncol(node_attributes_full), " columns")
message("  node_attributes_enriched:   ", nrow(node_attributes_enriched),
        " facilities x ", ncol(node_attributes_enriched), " columns")
message("  In transfer network:        ",
        sum(node_attributes_full$in_transfer_network),
        " / ", nrow(node_attributes_full))
out <- paste0(out,
              "\n[Part D] node_attributes (network-only): ", nrow(node_attributes),
              "\n[Part D] node_attributes_full (all facilities): ", nrow(node_attributes_full),
              "\n[Part D] node_attributes_enriched (network subset): ", nrow(node_attributes_enriched),
              "\n[Part D] Of which in transfer network: ", sum(node_attributes_full$in_transfer_network),
              " / ", nrow(node_attributes_full)
)

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
out <- paste0(out,
              "\n[Part G] Stays loaded (after removing LOS = 0): ", nrow(stays),
              "\n[Part G] Facilities with LOS stats: ", nrow(los)
)

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
# PART H — MERGE: LOS + CENSUS + TYPE -> hospital_stats, ASSIGN type_spares
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Part H: Merge LOS + census + facility type ──")

# Join census to the FULL (not network-restricted) enriched nodes - using
# node_attributes_enriched here would silently give every non-network
# facility NA for every type field, not because PMSI data is missing but
# because node_attributes_enriched excludes it structurally. See header
# note #9.
census_enriched <- full_join(hospital_census, node_attributes_full,
                             by = "finess_geo")

# Merge with LOS
hospital_stats <- full_join(
  los,
  census_enriched,
  by = c("FinessGeo" = "finess_geo")
) %>%
  rename(finess_geo = FinessGeo)

message("  hospital_stats rows: ", nrow(hospital_stats))
out <- paste0(out, "\n[Part H] hospital_stats rows (LOS + census + type merged): ", nrow(hospital_stats))

# H1. Assign type_spares - SIMPLIFIED: this is now just a rename, not a
#     second file read. factype_spares_pmsi is already present (computed in
#     Part B, directly on facility_meta) and has already flowed through via
#     the ordinary join chain facility_meta -> node_attributes ->
#     node_attributes_enriched -> census_enriched -> hospital_stats. The
#     earlier version of this script re-read finessgeo_metadata_2024.csv a
#     second time here to get the same value - unnecessary once it's
#     computed once in Part B.
stopifnot(
  "factype_spares_pmsi is missing from hospital_stats - check Part B's inline mutate()" =
    "factype_spares_pmsi" %in% names(hospital_stats)
)
hospital_stats <- hospital_stats %>%
  rename(type_spares = factype_spares_pmsi)

n_type_spares <- sum(!is.na(hospital_stats$type_spares))
message("  type_spares assigned from PMSI metadata (hospital_type / pmsi_category): ",
        n_type_spares, " / ", nrow(hospital_stats), " facilities (",
        round(100 * n_type_spares / nrow(hospital_stats), 1), "%)")
message("  type_spares categories: ",
        paste(sort(unique(hospital_stats$type_spares)), collapse = " | "))
out <- paste0(out,
              "\n[Part H] type_spares assigned: ", n_type_spares, " / ", nrow(hospital_stats),
              " (", round(100 * n_type_spares / nrow(hospital_stats), 1), "%)"
)

# H2. Region x type_spares aggregated LOS (kept for diagnostics / reporting).
#     RESTORED - this was dropped by accident during an earlier rewrite of
#     this Part, while the save/cleanup calls that reference it were not,
#     causing "object 'region_type_stats' not found" at save time.
region_type_stats <- hospital_stats %>%
  filter(!is.na(type_spares), !is.na(region)) %>%
  group_by(region, type_spares) %>%
  summarise(
    reg_type_pt_days    = sum(pt_days_total, na.rm = TRUE),
    reg_type_n_patients = sum(patient_total, na.rm = TRUE),
    n_facilities        = n(),
    .groups = "drop"
  ) %>%
  mutate(reg_type_los_avg = reg_type_pt_days / reg_type_n_patients)

message("  region_type_stats rows: ", nrow(region_type_stats))
out <- paste0(out, "\n[Part H] region_type_stats rows (region x type_spares aggregates): ",
              nrow(region_type_stats))

# H3. ADDED (per instruction) — diagnose WHY type_spares is NA where it is.
#     For every facility with type_spares == NA, classify the likely cause
#     from the other type fields, then print both the reason breakdown and
#     a facility-level table (finess_geo + every other type field) so
#     specific hospitals can be inspected.
type_spares_na <- hospital_stats %>%
  filter(is.na(type_spares)) %>%
  mutate(
    na_reason = case_when(
      is.na(facility_type_pmsi) & is.na(hospital_type) & is.na(in_transfer_network) ~
        "Not in facility_meta at all (no match on finess_geo there)",
      is.na(facility_type_pmsi) & is.na(hospital_type) ~
        "In facility_meta but with no facility_type_pmsi or hospital_type recorded",
      is.na(hospital_type) ~
        "hospital_type is NA (ifelse(hospital_type==\"SSR\",...) propagates NA even though pmsi_category exists)",
      is.na(facility_type_pmsi) ~
        "hospital_type present and != \"SSR\", but pmsi_category (facility_type_pmsi) is itself NA",
      TRUE ~ "Unexplained - inspect manually"
    )
  ) %>%
  select(finess_geo, na_reason, facility_type_pmsi, facility_type_capact,
         hospital_type, categetab, libcategetab, region, in_transfer_network)

message("\n  ---- type_spares == NA: why? (", nrow(type_spares_na), " facilities) ----")
message("  Reason breakdown:")
print(count(type_spares_na, na_reason, name = "n_facilities") %>% arrange(desc(n_facilities)),
      n = Inf)

message("\n  Breakdown of their OTHER type fields (facility_type_capact):")
print(count(type_spares_na, facility_type_capact, name = "n") %>% arrange(desc(n)), n = Inf)

message("\n  Breakdown of their OTHER type fields (categetab / libcategetab):")
print(count(type_spares_na, categetab, libcategetab, name = "n") %>% arrange(desc(n)), n = Inf)

message("\n  Sample of affected finess_geo (up to 20):")
print(as.data.frame(head(type_spares_na, 20)), row.names = FALSE)

out <- paste0(out, "\n[Part H] type_spares == NA: ", nrow(type_spares_na),
              " facilities (see type_spares_NA_diagnostic.csv for the reason breakdown)")

# ══════════════════════════════════════════════════════════════════════════════
# PART I — SPARES ESBL INCIDENCE (region x type_spares) -> hospital_stats
# ══════════════════════════════════════════════════════════════════════════════
# Reads the output of esbl_incidence_2022.R, keyed by region x the PMSI-
# derived type category (same vocabulary Part H assigns) x species_group.
# Runs AFTER Part H so hospital_stats + type_spares already exist.
message("\n── Part I: Attach SPARES ESBL incidence to hospital_stats ──")

ESBL_INCIDENCE_PATH <- here("Datasets", "SPARES",
                            "esbl_incidence_by_region_type_2022.csv")

if (!file.exists(ESBL_INCIDENCE_PATH)) {
  stop("esbl_incidence_by_region_type_2022.csv not found at: ",
       ESBL_INCIDENCE_PATH, "\n  Run esbl_incidence_2022.R first, or update",
       " ESBL_INCIDENCE_PATH above.")
}

esbl_incidence_raw <- read_csv(ESBL_INCIDENCE_PATH, show_col_types = FALSE)

# ADDED: exclude overseas territories right after import, before anything
# else touches this file - only if it actually has a region column (it may
# not, if esbl_incidence_2022.R's output format ever changes). Checks BMR's
# raw spelling ("Reunion - Mayotte" is one combined cell; "Nouvelle
# Calédonie" is an overseas collectivity, not one of the 18 regions at all)
# as well as the canonicalised form, so it catches overseas rows whichever
# spelling the file happens to use.
incidence_region_col <- intersect(c("Nouvelle_Region", "region"), names(esbl_incidence_raw))
if (length(incidence_region_col) > 0) {
  rc <- incidence_region_col[1]
  n_before_overseas <- nrow(esbl_incidence_raw)
  is_overseas_row <- esbl_incidence_raw[[rc]] %in% OVERSEAS_REGIONS |
    esbl_incidence_raw[[rc]] == "Reunion - Mayotte" |
    esbl_incidence_raw[[rc]] == "Nouvelle Calédonie" |
    canonicalise_region(esbl_incidence_raw[[rc]]) %in% OVERSEAS_REGIONS
  is_overseas_row[is.na(is_overseas_row)] <- FALSE
  esbl_incidence_raw <- esbl_incidence_raw[!is_overseas_row, ]
  message("  Excluded ", sum(is_overseas_row), " overseas rows from the incidence file (column '",
          rc, "'): ", n_before_overseas, " -> ", nrow(esbl_incidence_raw))
} else {
  message("  No region column found in the incidence file (checked: Nouvelle_Region, region) -",
          " overseas exclusion skipped here")
}

# ADDED: detect whichever of the two spellings esbl_incidence_2022.R
# actually used (facility_meta computes "factype_spares_pmsi"; a recent
# edit to the incidence script renamed from "factype_pmsi_spares" - the
# words swapped). Checking both avoids a silent zero-match join if the two
# scripts drift again.
type_col_candidates <- c("factype_spares_pmsi", "factype_pmsi_spares", "type_spares")
type_col_found <- intersect(type_col_candidates, names(esbl_incidence_raw))
if (length(type_col_found) == 0) {
  stop("None of the expected type columns (", paste(type_col_candidates, collapse = ", "),
       ") were found in esbl_incidence_by_region_type_2022.csv.\n  Columns present: ",
       paste(names(esbl_incidence_raw), collapse = ", "))
}
if (length(type_col_found) > 1) {
  warning("More than one candidate type column found (", paste(type_col_found, collapse = ", "),
          ") - using the first: ", type_col_found[1])
}
message("  Using '", type_col_found[1], "' as the incidence file's type column")

esbl_incidence_raw <- esbl_incidence_raw %>%
  rename(type_spares = !!type_col_found[1])

# I1. Map BMR's region spelling to the canonical spelling (extended into
#     canonicalise_region() above) so it lines up with hospital_stats$region.
#     SIMPLIFIED: overseas rows (including the old "Reunion - Mayotte"
#     combined-cell split and the "Nouvelle Calédonie" drop) were already
#     excluded right after import above, so this is now a plain rename.
esbl_incidence <- esbl_incidence_raw %>%
  mutate(region = canonicalise_region(Nouvelle_Region)) %>%
  select(-Nouvelle_Region)

message("  SPARES incidence regions: ",
        paste(sort(unique(esbl_incidence$region)), collapse = " | "))
message("  SPARES incidence types (type_spares): ",
        paste(sort(unique(esbl_incidence$type_spares)), collapse = " | "))
message("  hospital_stats types (type_spares): ",
        paste(sort(unique(hospital_stats$type_spares)), collapse = " | "))
message("  hospital_stats regions: ",
        paste(sort(unique(hospital_stats$region)), collapse = " | "))

# I2. Pivot to one row per region x type_spares, columns per species,
#     matching the existing incidence_region_type_ESBL_* naming convention
species_suffix <- c(
  "All ESBL-E"                   = "all",
  "Escherichia coli"             = "ecoli",
  "Klebsiella pneumoniae"        = "kpneumoniae",
  "Enterobacter cloacae complex" = "ecloacae"
)

spares_by_cell <- esbl_incidence %>%
  mutate(species_suffix = unname(species_suffix[species_group])) %>%
  select(region, type_spares, species_suffix, incidence_1000_JH) %>%
  pivot_wider(
    id_cols     = c(region, type_spares),
    names_from  = species_suffix,
    values_from = incidence_1000_JH,
    names_prefix = "incidence_region_type_ESBL_"
  ) %>%
  left_join(
    esbl_incidence %>%
      distinct(region, type_spares, n_hospitals, patient_days) %>%
      rename(n_bed_days_spares = patient_days),
    by = c("region", "type_spares")
  )

# I3. Join onto hospital_stats via region + type_spares
hospital_stats <- hospital_stats %>%
  left_join(spares_by_cell, by = c("region", "type_spares"))

n_total_hs   <- nrow(hospital_stats)
n_matched_hs <- sum(!is.na(hospital_stats$incidence_region_type_ESBL_all))

message("\n  SPARES incidence match summary (hospital_stats):")
message(sprintf("    Total facilities  : %d", n_total_hs))
message(sprintf("    Matched           : %d (%.0f%%)",
                n_matched_hs, 100 * n_matched_hs / n_total_hs))
out <- paste0(out,
              "\n[Part I] SPARES incidence matched: ", n_matched_hs, " / ", n_total_hs,
              " (", round(100 * n_matched_hs / n_total_hs), "%)"
)

unmatched_combos <- hospital_stats %>%
  filter(is.na(incidence_region_type_ESBL_all), !is.na(type_spares)) %>%
  count(region, type_spares, name = "n_facilities") %>%
  arrange(desc(n_facilities))

if (nrow(unmatched_combos) > 0) {
  message("\n  Unmatched region x type_spares combinations (facility counts):")
  print(as.data.frame(unmatched_combos), row.names = FALSE)
  
  message("\n  Regions in the SPARES incidence file not found in hospital_stats:")
  missing_reg <- setdiff(unique(esbl_incidence$region), unique(hospital_stats$region))
  if (length(missing_reg) > 0) print(missing_reg) else message("    None (all regions matched)")
  
  message("  Types in the SPARES incidence file not found in hospital_stats$type_spares:")
  missing_type <- setdiff(unique(esbl_incidence$type_spares), unique(hospital_stats$type_spares))
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
# One row per finess_geo, built from hospital_stats AFTER two exclusions
# (see J1 below): overseas territory, and no inpatient stays at all. Field
# list:
#   - facility node identity + name
#   - type_spares (our new category) + type in each other dataset (PMSI
#     ownership, CAPACT activity, raw PMSI hospital_type, FINESS categetab)
#   - census stats
#   - total stays, total patient-days, LOS (all stats)
#   - geography, INCLUDING raw Lambert-93 X/Y (not just WGS84 lat/long)
#   - ESBL incidence + the denominator (patient-days) it was computed from
# Bed counts (CAPACT vs PMSI) are deliberately NOT included - not needed.
message("\n── Part J: Build final facility-level dataset ──")

# J0. ADDED: facilities with NO characteristics at all - no stays, no LOS,
#     no incidence, no geographic/region info, all simultaneously missing.
#     Computed on the FULL hospital_stats universe (before the exclusions in
#     J1 below), since this is a general "is this row even usable" check,
#     not specific to either exclusion reason - a facility can fail this
#     without being overseas or without failing the no-stays check alone
#     (e.g. it could have stays but no region), and vice versa.
facilities_no_characteristics <- hospital_stats %>%
  filter(
    (is.na(patient_total) | patient_total == 0),
    is.na(los_mean),
    is.na(incidence_region_type_ESBL_all),
    is.na(region)
  )

message("  Facilities with NO characteristics at all (no stays, LOS, incidence, or region): ",
        nrow(facilities_no_characteristics), " / ", nrow(hospital_stats))
out <- paste0(out,
              "\n[Part J] Facilities with NO characteristics at all: ", nrow(facilities_no_characteristics),
              " / ", nrow(hospital_stats), " (saved separately, see facilities_no_characteristics.csv)"
)

# J1. Exclusion - no inpatient stays at all. Overseas territory is NO
#     LONGER excluded here - it's excluded upstream now, in Part B, right
#     on facility_meta where region is finalized, so it propagates to
#     hospital_stats automatically (see the Part B note for why). The
#     check below is a SANITY CHECK that the upstream exclusion actually
#     worked, not a real filter - it should always find zero.
n_before_exclusion <- nrow(hospital_stats)
is_overseas_leftover <- hospital_stats$region %in% OVERSEAS_REGIONS
if (any(is_overseas_leftover)) {
  warning(sum(is_overseas_leftover), " overseas facilities are STILL in hospital_stats ",
          "despite the Part B exclusion - the Part B filter and this check may be out of ",
          "sync, or something downstream of Part B is reintroducing them. Inspect before trusting",
          " facility_level_final.")
}
has_no_stays  <- is.na(hospital_stats$patient_total) | hospital_stats$patient_total == 0
n_no_stays    <- sum(has_no_stays)

message("\n  Exclusion applied before building facility_level_final:")
message(sprintf("    No inpatient stays (patient_total NA or 0): %d", n_no_stays))
message(sprintf("    Overseas territory remaining (should be 0, excluded in Part B): %d",
                sum(is_overseas_leftover)))

hospital_stats_kept <- hospital_stats %>%
  filter(!has_no_stays)

n_after_exclusion <- nrow(hospital_stats_kept)
message(sprintf("    Total excluded: %d  |  Kept: %d (of %d)",
                n_before_exclusion - n_after_exclusion, n_after_exclusion, n_before_exclusion))
out <- paste0(out,
              "\n[Part J] EXCLUSION before building facility_level_final:",
              "\n[Part J]   No inpatient stays (patient_total NA or 0): ", n_no_stays,
              "\n[Part J]   Overseas territory remaining (should be 0, excluded in Part B): ",
              sum(is_overseas_leftover),
              "\n[Part J]   Total excluded: ", n_before_exclusion - n_after_exclusion,
              "  |  Kept: ", n_after_exclusion, " (of ", n_before_exclusion, ")"
)

# J2. type_spares breakdown, including NA, BEFORE and AFTER the exclusions
message("\n  type_spares breakdown - hospital_stats (overseas already excluded in Part B;",
        " before the no-stays exclusion), including NA:")
print(hospital_stats %>% count(type_spares, name = "n") %>% arrange(desc(n)), n = Inf)

message("\n  type_spares breakdown - FINAL kept facilities, including NA:")
print(hospital_stats_kept %>% count(type_spares, name = "n") %>% arrange(desc(n)), n = Inf)

# J3. Build the final dataset from the KEPT (post-exclusion) facilities
facility_level_final <- hospital_stats_kept %>%
  select(
    # Identity
    finess_geo, facility_name,
    
    # Type: our new unified category, then every other dataset's own label
    type_spares,
    facility_type_pmsi, facility_type_capact, hospital_type,
    categetab, libcategetab,
    
    # Geography, including raw Lambert-93 XY (not just WGS84 lat/long)
    city, department, region,
    lambert_x = coordxet, lambert_y = coordyet,
    latitude, longitude,
    
    # Admissions / stays
    admit_yr, patient_total,
    
    # Patient-days
    pt_days_total,
    
    # Length of stay - all stats
    los_mean, los_median, los_q1, los_q3, los_ci_low, los_ci_hi, los_sd,
    
    # Daily census stats
    census_min, census_max, census_mean, census_median,
    census_95ci_low, census_95ci_hi,
    
    # SPARES region x type ESBL incidence (ecological estimate, from Part I)
    # + the patient-days denominator it was computed from
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

message("\n  facility_level_final: ", nrow(facility_level_final), " facilities x ",
        ncol(facility_level_final), " columns")
message("  Columns: ", paste(names(facility_level_final), collapse = " | "))
out <- paste0(out,
              "\n[Part J] FINAL facility_level_final: ", nrow(facility_level_final),
              " facilities x ", ncol(facility_level_final), " columns"
)

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
save_csv_out(facility_level_final,      "facility_level_final.csv")

# K4. type_spares == NA diagnostic (from Part H3)
save_csv_out(type_spares_na,            "type_spares_NA_diagnostic.csv")

# K5. ADDED: facilities with no stays/LOS/incidence/region at all (from Part J0)
save_rds_out(facilities_no_characteristics, "facilities_no_characteristics.RDS")
save_csv_out(facilities_no_characteristics, "facilities_no_characteristics.csv")

# ══════════════════════════════════════════════════════════════════════════════
# PART L — FINAL SUMMARY PRINTOUT
# ══════════════════════════════════════════════════════════════════════════════
message("\n══════════════════════════════════════════════════════════════════")
message("  FINAL SUMMARY")
message("══════════════════════════════════════════════════════════════════")

# ADDED: the facility-count waterfall built up throughout the script
# (out) is printed as ONE consolidated block here, and saved to its own
# text file - same pattern as the sample-selection scripts' end-of-run
# log (sample_selection_2022.txt / writeLines(out, ...)).
out <- paste0(out,
              "\n\n---- type_spares breakdown, hospital_stats (overseas already excluded, before no-stays exclusion), including NA ----\n"
)
out <- paste0(out,
              paste(capture.output(print(hospital_stats %>% count(type_spares, name = "n") %>%
                                           arrange(desc(n)), n = Inf)), collapse = "\n")
)
out <- paste0(out,
              "\n\n---- type_spares breakdown, FINAL kept facilities, including NA ----\n"
)
out <- paste0(out,
              paste(capture.output(print(hospital_stats_kept %>% count(type_spares, name = "n") %>%
                                           arrange(desc(n)), n = Inf)), collapse = "\n")
)
if (nrow(unmatched_combos) > 0) {
  out <- paste0(out, "\n\n---- Unmatched region x type_spares combinations ----\n")
  out <- paste0(out, paste(capture.output(print(as.data.frame(unmatched_combos), row.names = FALSE)),
                           collapse = "\n"))
}

cat(out, "\n")
writeLines(out, file.path(output_dir, "facility_count_waterfall.txt"))

message("\n  OUTPUTS SAVED TO:")
message("    ", output_dir)

message("\n══════════════════════════════════════════════════════════════════")
message("  DONE — 00_data_prep_unified_simplified.R completed successfully")
message("══════════════════════════════════════════════════════════════════")


# ── Clean up: keep only final outputs in the environment ──────────────────────
rm(list = setdiff(ls(), c(
  "node_attributes", "node_attributes_full", "node_attributes_enriched",
  "weekly", "daily_admission", "daily_census",
  "hospital_stats", "hospital_stats_kept", "region_type_stats",
  #"facility_level_final", 
  "type_spares_na", "facilities_no_characteristics"
)))

# ==============================================================================
# END OF 00_data_prep_unified_simplified.R
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