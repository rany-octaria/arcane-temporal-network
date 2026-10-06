# ============================================================
# ESBL-E Incidence per 1,000 Patient-Days, 2022
# By Region x Hospital Type
#
# Numerator:   cleaned, deduplicated BLSE-tested isolates (E. coli,
#              K. pneumoniae, Enterobacter cloacae complex only - see
#              2b_esbl_sample_selection_2022.R for the full cleaning/
#              exclusion/deduplication logic), from
#              Datasets/SPARES/esbl_isolates_cleaned_2022.RDS
# Denominator: whole-facility patient-days, from ATB_admindata2022,
#              linked to BMR hospitals via FINESS through
#              ATB_participants2022 (crosswalk)
#
# Run 2b_esbl_sample_selection_2022.R first - it also produces the
# corrected admin table and hospital cohort this script depends on.
#
# ATB_admindata2022 and ATB_participants2022 are used from your R
# session if already loaded, otherwise read from Datasets/SPARES/*.RDS
# (cached there the first time this script runs with them loaded).
# BMR_Covid_2022_admin_cleaned.RDS and hospital_cohort_2022.RDS (both
# produced by 2b_esbl_sample_selection_2022.R) are always read from disk.
# ============================================================

library(here)
library(tidyverse)

# ------------------------------------------------------------
# 0. Load data
# ------------------------------------------------------------
spares_dir <- here("Datasets", "SPARES")
dir.create(spares_dir, showWarnings = FALSE, recursive = TRUE)

esbl_isolates <- readRDS(here("Datasets", "SPARES", "combined", "resistance_cohort2022.RDS"))

#BMR Files consist of admin files of the Resistance Report Data
bmr_admin_cleaned_path <- file.path(spares_dir, "BMR_Covid_2022_admin_cleaned.RDS")
if (!file.exists(bmr_admin_cleaned_path)) {
  stop("BMR_Covid_2022_admin_cleaned.RDS not found at: ", bmr_admin_cleaned_path,
       "\n  Run 2b_esbl_sample_selection_2022.R first - it applies the",
       " hand-corrected name/FINESS/type fixes and saves this file.")
}
# Using the CORRECTED admin table (name/finess/finess_juridique/groupe
# fixes from 2b_esbl_sample_selection_2022.R's hospital cohort step),
# not the raw BMR_Covid_2022_admin - so those corrections actually feed
# the FINESS join and type_spares categorization below, not just which
# hospitals get excluded.
bmr_admin <- readRDS(bmr_admin_cleaned_path)
atb_admin <- readRDS("./Datasets/SPARES/ATB_admindata2022.RDS")
atb_part  <- readRDS("./Datasets/SPARES/ATB_participants2022.RDS")
pmsi_facs = read.csv("./Datasets/MCO_SSR_HBN_2024/finessgeo_metadata_2024.csv")
# ------------------------------------------------------------
# 1. Numerator: ESBL-positive isolates per hospital x species
#    esbl_isolates is already cleaned and deduplicated by
#    2b_esbl_sample_selection_2022.R (exact-duplicate resolution,
#    sector-duplicate resolution, 30-day episode dedup, untested/
#    incorrectly-coded/out-of-sector/out-of-species exclusions already
#    applied). esbl_positive uses the WIDENED case definition (BLSE
#    positive OR R phenotype on a 3GC) - the only thing left to do here
#    is count.
# ------------------------------------------------------------
species_groups_all <- c("All ESBL-E", "Escherichia coli",
                        "Klebsiella pneumoniae",
                        "Enterobacter cloacae complex")

numerator_by_species <- esbl_isolates %>%
 # filter(esbl_positive) %>%
  count(code, bacterie, name = "n_esbl_positive") %>%
  rename(species_group = bacterie)
head(numerator_by_species)

numerator_all <- esbl_isolates %>%
  #filter(esbl_positive) %>%
  mutate(species_group = "All ESBL-E") %>%
  count(code, species_group, name = "n_esbl_positive")
head(numerator_all)

numerator <- bind_rows(numerator_by_species, numerator_all) %>%
  rename(IdEtablissement = code)

# ------------------------------------------------------------
# 2. Denominator: whole-facility patient-days (JH) per hospital,
#    restricted to hospitals with ANY actual 2022 reporting
# ------------------------------------------------------------
# Pad FINESS geo code to 9 digits so it matches BMR admin's `finess`
atb_part <- atb_part %>%
  mutate(finess_geo_9 = sprintf("%09.0f", `finess géographique`))

n_atb_total <- atb_admin %>% filter(secteur == "Total établissement") %>% nrow()
n_atb_zero  <- atb_admin %>%
  filter(secteur == "Total établissement", !is.na(Nbhosp), Nbhosp == 0) %>%
  nrow()
cat("\n", n_atb_zero, "of", n_atb_total,
    "ATB 'Total établissement' rows report exactly 0 patient-days -",
    "treated as no reporting, not a genuine zero, and excluded below.\n")

patient_days <- atb_admin %>%
  filter(secteur == "Total établissement") %>%   # whole-facility total,
  filter(!is.na(Nbhosp), Nbhosp > 0) %>%          # ANY reporting in 2022:
  select(code, Nbhosp) %>%                        # NA or 0 = not treated
  rename(patient_days = Nbhosp) %>%                # as having reported
  left_join(atb_part %>% select(code, finess_geo_9), by = "code")

# ------------------------------------------------------------
# 3. Restrict to BMR-file hospitals IN THE HOSPITAL COHORT (from
#    2b_esbl_sample_selection_2022.R's hospital selection step -
#    excludes overseas/Corsica/no-identifier hospitals, same as the
#    numerator side, so a hospital isn't counted for patient-days but
#    silently dropped from ESBL case counts), attach type/region, join
#    denom
# ------------------------------------------------------------
hospital_cohort_path <- file.path(spares_dir, "hospital_cohort_2022.RDS")
if (!file.exists(hospital_cohort_path)) {
  stop("hospital_cohort_2022.RDS not found at: ", hospital_cohort_path,
       "\n  Run 2b_esbl_sample_selection_2022.R first.")
}
hospital_cohort_2022 <- readRDS(hospital_cohort_path)

hospital_base <- bmr_admin %>%
  filter(idetablissement %in% hospital_cohort_2022) %>%
  select(IdEtablissement = idetablissement, groupe, Nouvelle_Region, finess)

denom_by_hospital_all <- hospital_base %>%
  left_join(patient_days, by = c("finess" = "finess_geo_9")) %>%
  select(IdEtablissement, finess, groupe, Nouvelle_Region, patient_days)

n_no_2022_reporting <- sum(is.na(denom_by_hospital_all$patient_days))
cat("\n", n_no_2022_reporting, "of", nrow(denom_by_hospital_all),
    "cohort hospitals have no 2022 ATB reporting (no patient-days match",
    "via FINESS) and are excluded from the denominator.\n")

# Only hospitals with actual 2022 ATB reporting go into the denominator -
# this filter used to happen implicitly, later, inside the aggregation
# step below; it's now explicit here instead.
denom_by_hospital <- denom_by_hospital_all %>%
  filter(!is.na(patient_days))
head(denom_by_hospital)


### Added Section by Rany - matching Finess in denom data with PMSI database to get the 
## PMSI -defined Hospital type

#Merge this with PMSI data
pmsi_metadata = read.csv("./Datasets/MCO_SSR_HBN_2024/finessgeo_metadata_2024.csv") %>% 
  mutate(factype_new = ifelse(hospital_type == "SSR", "SSR", pmsi_category))

denom_by_hospital_pmsi = left_join(denom_by_hospital, pmsi_metadata,
                                   by = c("finess"=  "finessgeo"))
#because of the different years of data, it is expected sme may not match, let's see te breakdown
# denom_by_hospital_pmsi = denom_by_hospital_pmsi %>% 
#   mutate( 
#     factype_pmsi_spares = ifelse(is.na(factype_new)& groupe == "CH", "General public hospital (CH)" ,
#                                  ifelse(is.na(factype_new)& groupe == "CLCC", "Cancer centre (CLCC)",
#                                         ifelse(is.na(factype_new)& groupe == "ESSR", "SSR",
#                                                ifelse(is.na(factype_new)& groupe == "MCO", "Private",
#                                                       ifelse(is.na(factype_new)& groupe =="CHU","Regional/University hospital (CHR/U)",
#                                                              ifelse(is.na(factype_new), "Others", factype_new )))))))
groupe_to_pmsi_label <- c(
  CH   = "General public hospital (CH)",
  CLCC = "Cancer centre (CLCC)",
  ESSR = "SSR",
  MCO  = "Private",
  CHU  = "Regional/University hospital (CHR/U)"
)

denom_by_hospital_pmsi <- denom_by_hospital_pmsi %>%
  mutate(
    factype_pmsi_spares = case_when(
      is.na(groupe) & is.na(factype_new) ~ NA_character_,  # matches the original's edge-case behavior
      .default = coalesce(factype_new, groupe_to_pmsi_label[groupe], "Others")
    )
  )

table( denom_by_hospital_pmsi$factype_pmsi_spares, useNA = "always")

# 4. Combine and aggregate by Region x Hospital Type x species
# ------------------------------------------------------------
# (already restricted to hospitals with 2022 ATB reporting, see above)
# cross every hospital with every species group so hospitals with
# zero ESBL isolates still contribute their patient-days

combined <- denom_by_hospital_pmsi %>%
  tidyr::crossing(species_group = species_groups_all) %>%
  left_join(numerator, by = c("IdEtablissement", "species_group")) %>%
  mutate(n_esbl_positive = replace_na(n_esbl_positive, 0))

esbl_incidence_region_type <- combined %>%
  group_by(Nouvelle_Region,factype_pmsi_spares , species_group) %>%
  summarise(
    n_hospitals        = n_distinct(IdEtablissement),
    n_esbl_positive     = sum(n_esbl_positive),
    patient_days        = sum(patient_days),
    incidence_1000_JH   = round(n_esbl_positive / patient_days * 1000, 3),
    .groups = "drop"
  ) %>%
  arrange(species_group, Nouvelle_Region, factype_pmsi_spares )

# ------------------------------------------------------------
# 5. Save output
# ------------------------------------------------------------
write.csv(
  esbl_incidence_region_type,
  here("Datasets", "SPARES", "esbl_incidence_by_region_type_2022.csv"),
  row.names = FALSE
)

print(esbl_incidence_region_type, n = 60)

