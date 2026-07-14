# =============================================================================
# cnr Cleaning Data
# Project: ARCANE
# Author: Rany Octaria
# Description: Cleans cnr surveillance data, builds hospital reference
#              table from FINESS + SAE CAPACT, and links bed counts to
#              node_attributes by finess_geo
# =============================================================================

library(tidyverse)
library(readxl)
library(httr)
library(here)


# =============================================================================
# PART 1: IMPORT & CLEAN cnr DATA
# =============================================================================

cnr <- read_excel(here("Datasets", "Output Data", "CNR_2023_2024_DEIDENTIFIED_shareable_FR.xlsx"))

# Normalize all column names to ASCII lowercase with underscores
cnr <- cnr %>%
  rename_with(~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT") %>%
                tolower() %>%
                gsub(" ", "_", .))

colnames(cnr)

# Keep only patient specimens (exclude environmental)
patient_cnr <- filter(cnr, est_environnement == FALSE)

# Summary: distinct month-year combos and specimen count per facility
facility_breakdown <- patient_cnr %>%
  mutate(mois_annee = paste(mois, annee, sep = "-")) %>%
  group_by(etablissement) %>%
  summarise(
    n_mois_annee = n_distinct(mois_annee),
    n_spec       = n_distinct(specimen_id),
    .groups      = "drop"
  ) %>% 
  arrange(-n_mois_annee, etablissement)

write.csv(facility_breakdown, file = here("Datasets", "Output Data", "Enriched Facility Data","cnr_facility_raw.csv"))

# =============================================================================
# PART 2: LOAD HOSPITAL NETWORK NODE ATTRIBUTES
# =============================================================================

node_attributes <- readRDS(
  here("Datasets", "Output Data", "node_attributes.RDS")
)

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

 

