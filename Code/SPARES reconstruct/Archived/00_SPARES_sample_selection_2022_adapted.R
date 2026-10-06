##################################################
## ESBL-E SAMPLE SELECTION & CLEANING, 2022 ONLY
## Adapted from 2_patient_sample_selection.R, with a hospital-level
## cohort selection step merged in from 0_hospital_sample_selection.R
##################################################
#
# Differences from the original scripts, per instruction:
#   1. 2022 only - no year loop, no foreach/doParallel (single year,
#      no parallelism benefit)
#   2. Hospital exclusion is back, but narrower than either original
#      script: excludes ONLY overseas hospitals, Corsica hospitals, and
#      hospitals without a usable FINESS identifier (see section 0c).
#      Cancer centres (CLCC) and psychiatric hospitals (PSY), which the
#      original 0_hospital_sample_selection.R excluded, are explicitly
#      KEPT here, and no other hospital-type exclusion is applied.
#   3. Age exclusion IS applied again (idtrancheage >= 3, i.e. 15y+) and
#      newborn-site samples ARE excluded again (IdSite != 13) - this
#      reverses an earlier instruction to include all ages. Flag if that
#      reversal wasn't intended.
#   4. NO exclusion by ICU status or PMSI occupancy - the original
#      0_hospital_sample_selection.R's entire ICU-cohort section
#      (icu_cohort19202122) is not ported at all
#   5. All other sample-level cleaning/exclusion/deduplication logic is
#      kept as close to the original as could be reconstructed (see
#      MISSING DEPENDENCIES below for where that wasn't fully possible)
#
# MISSING DEPENDENCIES - the originals source
# R/helper/helper_functions.R and R/helper/dictionaries.R, load
# data/cohort19202122.rda, and (for hospital selection) read the
# official government FINESS registry
# (data-raw/spares/hospital_location/etalab-...csv) and
# data-raw/spares/finess_issues/all_chu_finess_annotated.xlsx - none of
# which are available here. Substituted or simplified where needed -
# flagged with "ASSUMPTION" comments below, and summarised here:
#   - enzymes: DERIVED FROM THE DATA rather than hardcoded - a molecule
#     is treated as an enzyme/phenotype test when most of its Resultat
#     values are O/N (see section 1). The script stops with a clear
#     message if BLSE is not detected this way. Whatever the real
#     `enzymes` dictionary contains, this should match it as long as
#     O/N-coding is how enzyme tests are distinguished from S/I/R
#     susceptibility tests in your data.
#   - dict_id_age, dict_id_site, dict_secteur_spares, dict_molecule_class,
#     dict_hospital_type: not available. age_cat, site, and molecule_class
#     are created as IDENTITY pass-throughs of idtrancheage, IdSite, and
#     molecule respectively (same column names as your real code, raw
#     values instead of recoded labels) - secteur is left as its raw
#     French value too. This changes display labels only, not which
#     samples/hospitals are kept or how they're grouped for dedup.
#   - bacteria_of_interest: confirmed as the 8-species MDRO list (E. coli,
#     K. pneumoniae, E. cloacae complex, S. aureus, and two more tested
#     with carbapenems + two Enterococcus species tested with
#     Vancomycine) from how it's indexed in the code you provided. This
#     script only uses indices [1:3] + BLSE (the ESBL-E slice) - the
#     other four resistance mechanisms (MRSA, CRPA/CRAB, VRE) are out of
#     scope for this script and not applied.
#   - selection_30days_sequence(): not available. Within a 30-day
#     sequence of BLSE samples from the same patient/organism/sector,
#     this script keeps the EARLIEST sample (a standard "first isolate
#     of the episode" rule) rather than whatever your actual function
#     selects. Send helper_functions.R if you want this to match
#     exactly.
#   - Overseas/Corsica identification: the original looks up each
#     hospital's FINESS department code (2A/2B, 9A-9F) against the
#     official government registry. That registry isn't available here,
#     so this script identifies overseas/Corsica hospitals from
#     BMR_Covid_2022_admin's own Nouvelle_Region field instead (Corse;
#     Guadeloupe; Martinique; Reunion - Mayotte; Nouvelle Calédonie) -
#     same practical effect, different mechanism. Flag if you'd rather
#     this be checked against the real FINESS registry.
#   - "Without identifiers": the original checks each FINESS number
#     against the official registry. Substituted with a check on
#     BMR_Covid_2022_admin's own `finess` field (missing, blank, or not
#     a 9-digit code) - an internal completeness check, not validated
#     against the government registry.
#   - The ~20 hand-corrected hospital name/FINESS/city/type fixes in the
#     original (specific to known 2019-2022 data issues, e.g. code 7115,
#     9709, 2009...) ARE ported, keyed on idetablissement/etablissement
#     exactly as in the original (see section 0c). I can't independently
#     verify each one is still accurate for your current data, since I
#     don't have the original context for why each was made - worth a
#     spot check if any of these codes matter a lot to your results.
#   - The original also excludes hospitals not reporting in the ATB AND
#     resistance databases across all 4 years (`missing_year_or_database`)
#     and requires reporting in all 4 years (`n == 4`) - both are
#     inherently multi-year checks and don't apply to a 2022-only cohort,
#     so neither is applied here.
#   - `geographic_entities` (excludes legal-entity-level FINESS reporting
#     except for CHUs) is NOT applied - not one of the exclusions named,
#     and "keep all other hospital" reads as keeping these too. Flag if
#     you actually want this one applied.
#
# SCOPE NARROWING - the original script's own ESBL/BLSE definition (see
# its `bacteria_of_interest[1:3]` usage around its line 199) is
# E. coli + K. pneumoniae + Enterobacter cloacae complex ONLY - not all
# Enterobacterales like the earlier esbl_incidence_2022.R used. Matched
# to THIS script's narrower, more standard "ESBL-E" convention here.
#
# CASE DEFINITION - an isolate counts as ESBL-positive when the
# dedicated BLSE test (molecule == "BLSE") is positive (Resultat == "O",
# French Oui). No other molecule/antibiotic result is used to determine
# ESBL status.
##################################################

library(tidyverse)
library(here)

##################################################
# 0. Load data
##################################################
spares_dir <- here("Datasets", "SPARES")
dir.create(spares_dir, showWarnings = FALSE, recursive = TRUE)
souches_path <- file.path(spares_dir, "BMR_Covid_souches_2022.RDS")

if (exists("BMR_Covid_souches_2022")) {
  # Cache to RDS if this is the first time it's been available this session
  # (esbl_incidence_2022.R no longer owns this save step - it now consumes
  # this script's cleaned output instead of raw souches data)
  if (!file.exists(souches_path)) saveRDS(BMR_Covid_souches_2022, souches_path)
} else {
  BMR_Covid_souches_2022 <- readRDS(souches_path)
}

res <- BMR_Covid_souches_2022 %>%
  rename(code = IdEtablissement) %>%
  mutate(molecule = iconv(molecule, from = "latin1", to = "UTF-8"))

y <- 2022

out <- paste0(
  "###############################################################\n",
  "ESBL-E sample selection - ", y, "\n",
  "###############################################################\n"
)

n_isolates <- res %>%
  select(-c(IdMolecule, molecule, nosocomial, Resultat, IdArchiveLaboratoire)) %>%
  distinct() %>%
  nrow()
out <- paste0(out, "Number of isolates (all species, all molecules): ", n_isolates)

##################################################
# 0c. Hospital cohort selection (merged in from
#     0_hospital_sample_selection.R, adapted to 2022 only - see
#     MISSING DEPENDENCIES in the header for what changed and why)
##################################################
if (!exists("BMR_Covid_2022_admin")) {
  BMR_Covid_2022_admin <- readRDS(file.path(spares_dir, "BMR_Covid_2022_admin.RDS"))
}

hospital_meta <- BMR_Covid_2022_admin %>%
  mutate(
    # Generic name cleaning
    etablissement = gsub("  ", " ", etablissement),
    etablissement = gsub(" \\(fermé\\)", "", etablissement),
    # Hand-corrected name fixes, ported from 0_hospital_sample_selection.R
    etablissement = case_when(
      etablissement == "CHU GRENOBLE" ~ "CHU GRENOBLE-HOPITAL NORD",
      etablissement == "CENTRE DE REEDUCATION LA LANDE" ~ "SSR LA LANDE",
      etablissement == "CENTRE DE READAPTATION DU CONFLUENT" ~ "CENTRE DE SSR DU CONFLUENT",
      etablissement == "CLINIQUE J.VERNE- POLE HOSP MUTUALISTE" ~ "CLINIQUE MUTUALISTE JULES VERNE",
      etablissement == "CTRE READAPTATION VILLA NOTRE DAME" ~ "SSR VILLA NOTRE DAME",
      etablissement == "CENTRE HOSPITALIER DE MONTLUCON NERIS LES BAINS" ~ "CENTRE HOSPITALIER DE MONTLUCON",
      etablissement == "CLINIQUE CHANTECLER (hacking 2020 : abs conso ATB)" ~ "CLINIQUE CHANTECLER",
      etablissement == "CLINEA CRF DU BESSILLON" ~ "CRF DU BESSILLON",
      etablissement == "CLINIQUE MALARTIC" ~ "POLYCLINIQUE MALARTIC",
      etablissement == "C.H.I.C. COTE BASQUE - BAYONNE" ~ "CH DE LA COTE BASQUE - BAYONNE",
      etablissement == "EPSYLAN" ~ "CHS BLAIN",
      etablissement == "HOPITAUX DE GRAND COGNAC" ~ "CH INTERCOMMUNAL DU PAYS DE COGNAC",
      etablissement == "CLINIQUE FSEF RENNES BEAULIEU" ~ "CENTRE MEDICAL ET PEDAGOGIQUE BEAULIEU",
      idetablissement == 9960  ~ "INSTITUT DE READAPTATION D'ACHERES",
      idetablissement == 11330 ~ "CENTRE MEDICAL SANCELLEMOZ",
      idetablissement == 2727  ~ "CH DE PRIVAS ARDECHE",
      idetablissement == 10650 ~ "CLINIQUE SAINT JEAN SUD DE FRANCE",
      idetablissement == 10839 ~ "HOPITAL DU PAYS SALONAIS",
      idetablissement == 2406  ~ "HOPITAL DES COLLINES VENDEENNES",
      idetablissement == 2009  ~ "HOPITAL ROBERT SCHUMAN DE VANTOUX",
      idetablissement == 7115  ~ "CHU G. MONTPIED",
      idetablissement == 9709  ~ "CLINIQUE BLAGNAC",
      idetablissement == 10406 ~ "CLINIQUE LES HAUTS DE CENON",
      .default = etablissement
    ),
    # Hand-corrected FINESS fixes, ported from 0_hospital_sample_selection.R
    finess = case_when(
      idetablissement == 2406  ~ "850000647",
      idetablissement == 12672 ~ "940110042",
      idetablissement == 2453  ~ "440059319",
      idetablissement == 2009  ~ "570026252",
      idetablissement == 7115  ~ "630000404",
      idetablissement == 9657  ~ "690781810",
      idetablissement == 9709  ~ "310025010",
      .default = as.character(finess)
    ),
    finess_juridique = case_when(
      idetablissement == 2478  ~ "440041895",
      idetablissement == 10597 ~ "920029527",
      idetablissement == 8984  ~ "420784878",
      idetablissement == 8836  ~ "350001137",
      idetablissement == 9984  ~ "950042994",
      idetablissement == 12672 ~ "940110042",
      idetablissement == 2453  ~ "440059301",
      idetablissement == 2009  ~ "570023630",
      idetablissement == 9709  ~ "310025010",
      .default = as.character(finess_juridique)
    ),
    # Hand-corrected city fixes, ported from 0_hospital_sample_selection.R
    ville = case_when(
      idetablissement == 2420  ~ "LE LOUROUX BECONNAIS",
      idetablissement == 2839  ~ "LA TESTE DE BUCH",
      idetablissement == 9960  ~ "ACHERES",
      idetablissement == 10806 ~ "CANNES",
      idetablissement == 11330 ~ "PASSY",
      idetablissement == 12420 ~ "NEVILLE",
      idetablissement == 10650 ~ "SAINT JEAN DE VEDAS",
      idetablissement == 9984  ~ "ENNERY",
      idetablissement == 2009  ~ "VANTOUX",
      idetablissement == 9709  ~ "BLAGNAC",
      .default = ville
    ),
    # Hand-corrected type (groupe) fixes, ported from
    # 0_hospital_sample_selection.R - these feed straight into type_spares
    # in esbl_incidence_2022.R, so they matter for categorization, not
    # just display
    groupe = case_when(
      idetablissement == 11014 ~ "CLCC", # physical Finess code: 840000350
      idetablissement == 1913  ~ "MCO",  # physical Finess code: 210011847
      idetablissement == 11180 ~ "MCO",  # physical Finess code: 420000192
      .default = groupe
    )
  )

# Save the corrected admin table (ALL hospitals, before the cohort
# exclusion filter below) so esbl_incidence_2022.R can use the corrected
# name/finess/type values too, not just this script's isolate data -
# otherwise these fixes would only affect which hospitals are excluded,
# not the FINESS join or type_spares categorization downstream.
saveRDS(hospital_meta, file.path(spares_dir, "BMR_Covid_2022_admin_cleaned.RDS"))

# Overseas (DROM/COM) and Corsica, identified from Nouvelle_Region -
# substitute for the FINESS-department lookup (see header note)
overseas_regions <- c("Guadeloupe", "Martinique", "Reunion - Mayotte",
                      "Nouvelle Calédonie")
corsica_region <- "Corse"

# "Without identifiers": no usable FINESS geo code - substitute for the
# data.gouv.fr registry check (see header note)
is_missing_finess <- function(x) {
  is.na(x) | trimws(as.character(x)) == "" |
    !grepl("^[0-9]{9}$", trimws(as.character(x)))
}

hospital_exclusions <- hospital_meta %>%
  mutate(
    excl_overseas = Nouvelle_Region %in% overseas_regions,
    excl_corsica  = Nouvelle_Region == corsica_region,
    excl_no_id    = is_missing_finess(finess)
    # groupe == "PSY" / "CLCC" intentionally NOT excluded (kept, per
    # instruction) - no hospital-type exclusion is applied at all
  )

out <- paste0(out,
              "\n\n---- Hospital cohort exclusions (2022) ----",
              "\nOverseas: ", sum(hospital_exclusions$excl_overseas),
              "\nCorsica: ", sum(hospital_exclusions$excl_corsica),
              "\nMissing/invalid FINESS: ", sum(hospital_exclusions$excl_no_id)
)

hospital_cohort_2022 <- hospital_exclusions %>%
  filter(!excl_overseas, !excl_corsica, !excl_no_id) %>%
  pull(idetablissement)

out <- paste0(out, "\nHospitals kept: ", length(hospital_cohort_2022),
              " of ", nrow(hospital_meta))

##################################################
# 1. Sample-level filtering
##################################################
# `enzymes` is DERIVED FROM THE DATA rather than hardcoded: a molecule is
# treated as an enzyme/phenotype test (O/N coded) when most of its results
# are O or N. (Majority rule, not "all results are O/N", so a few miscoded
# rows can't make a real enzyme test look like an ordinary antibiotic and
# cause every one of its rows to be dropped.)
molecule_resultat_share <- res %>%
  group_by(molecule) %>%
  summarise(share_on = mean(Resultat %in% c("O", "N")), .groups = "drop")

# enzymes <- molecule_resultat_share %>%
#   filter(share_on > 0.5) %>%
#   pull(molecule)
enzymes = "BLSE"

# if (!"BLSE" %in% enzymes) {
#   stop("BLSE was not detected as an O/N-coded (enzyme) molecule - check the ",
#        "molecule and Resultat columns before continuing.")
# }

out <- paste0(out, "\n\nMolecules treated as enzyme/phenotype tests (O/N-coded): ",
              paste(enzymes, collapse = ", "))

res <- res %>%
  # Filter out Pediatry, Psychiatry and SLD
  filter(secteur %in% c("Chirurgie", "Gynécologie-Obstétrique", "Médecine",
                        "Réanimation", "SSR")) %>%
  # Filter out samples isolated in new borns
  filter(IdSite != 13) %>%
  # Filter out samples from patients under 15 years old
  filter(idtrancheage >= 3) %>%
  # Hospital exclusion, using the cohort from 0c above (overseas / Corsica /
  # no-identifier only - PSY and CLCC are kept)
  filter(code %in% hospital_cohort_2022) %>%
  # Filter out test results/phenotypes that are incorrectly coded
  filter(
    !((Resultat %in% c("O", "N") & !molecule %in% enzymes)# |
       # (Resultat %in% c("S", "I", "R") & molecule %in% enzymes)
      )
  ) %>%
  # Remove unnecessary columns that were previously checked for
  # potential coding issues
  select(-c(IdArchiveLaboratoire, IdPeriode, IdBacterie, IdMolecule, nosocomial,
            suppr_dedoublonnage2, iduf, idactivite, code_de, code_ta,
            IdetablissementLaboratoire)) %>%
  mutate(
    # age_cat, site, molecule_class: dict_id_age / dict_id_site /
    # dict_molecule_class are not available - identity pass-throughs of
    # idtrancheage / IdSite / molecule instead of recoded labels (see header)
    age_cat = idtrancheage,
    molecule_class = molecule,
    site = IdSite,
    # secteur: dict_secteur_spares not available - kept as raw French value
    Date_year = y,
    bacterie = case_when(
      bacterie == "Enterobacter cloacae co"      ~ "Enterobacter cloacae complex",
      bacterie == "Enterobacter cloacae"          ~ "Enterobacter cloacae complex",
      bacterie == "Enterobacter asburiae"         ~ "Enterobacter cloacae complex",
      bacterie == "Enterobacter hormaechei"       ~ "Enterobacter cloacae complex",
      bacterie == "Enterobacter kobei"            ~ "Enterobacter cloacae complex",
      bacterie == "Enterobacter ludwigii"         ~ "Enterobacter cloacae complex",
      bacterie == "Enterobacter nimipressuralis"  ~ "Enterobacter cloacae complex",
      .default = bacterie
    )
  ) %>%
  select(code, site, prelevement, ligne, Resultat, Num_Patient,
         age_cat, bacterie, molecule, molecule_class, secteur, Date_year) %>%
  distinct()
res %>% filter(molecule == "BLSE", bacterie == "Escherichia coli", Resultat=="O") %>% distinct() %>% nrow()
##################################################
# 2. Narrow to ESBL-E: E. coli / K. pneumoniae / Enterobacter cloacae
#    complex tested for BLSE (bacteria_of_interest[1:3] + "BLSE" - the
#    other species / molecules in the full bacteria_of_interest list are
#    out of scope for this script, see header)
##################################################
esbl_species <- c("Escherichia coli", "Klebsiella pneumoniae",
                  "Enterobacter cloacae complex")

n_esbl_species_samples <- res %>%
  filter(bacterie %in% esbl_species) %>%
  select(code, site, prelevement, Num_Patient, bacterie, secteur, ligne) %>%
  distinct() %>%
  nrow()

res <- res %>%
  filter(bacterie %in% esbl_species & molecule %in% "BLSE")

n_tested <- res %>%
  select(code, site, prelevement, Num_Patient, bacterie, secteur, ligne) %>%
  distinct() %>%
  nrow()

out <- paste0(out,
              "\nNumber of E. coli / K. pneumoniae / E. cloacae complex samples: ", n_esbl_species_samples,
              "\nNumber tested for BLSE: ", n_tested,
              "\nNumber NOT tested for BLSE (excluded): ", n_esbl_species_samples - n_tested
)

##################################################
# 3. Verify one phenotype per sample at the "ligne" level (kept as-is)
##################################################
ligne_level <- res %>%
  group_by(code, site, prelevement, ligne, Num_Patient, age_cat,
           bacterie, secteur, Date_year) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

if (nrow(ligne_level) > 0) {
  stop("Some isolates have multiple BLSE phenotypes at the ligne level - ",
       "inspect `ligne_level` before continuing")
}

##################################################
# 4. Exact duplicates: same patient/organism/sector/day, different
#    ligne IDs -> keep lowest ligne if results agree; if BLSE results
#    conflict, resolve O (positive) wins over N
##################################################
dup_key <- c("code", "site", "prelevement", "Num_Patient", "age_cat",
             "bacterie", "secteur")

ids_duplicates <- res %>%
  select(all_of(dup_key), ligne) %>%
  distinct() %>%
  group_by(across(all_of(dup_key))) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  ungroup() %>%
  select(-n)

out <- paste0(out, "\nNumber of potential duplicates: ",
              ids_duplicates %>% select(-ligne) %>% distinct() %>% nrow())

exact_duplicates <- res %>%
  inner_join(ids_duplicates, by = c(dup_key, "ligne")) %>%
  group_by(across(all_of(dup_key))) %>%
  mutate(n_diff = length(unique(Resultat))) %>%
  ungroup()

exact_duplicates_to_remove <- exact_duplicates %>%
  filter(n_diff == 1) %>%   # identical BLSE result across duplicate ligne IDs
  group_by(across(all_of(dup_key))) %>%
  mutate(m = min(ligne)) %>%
  filter(ligne != m) %>%
  ungroup() %>%
  select(all_of(dup_key), ligne) %>%
  distinct()

different_result_duplicates <- exact_duplicates %>%
  filter(n_diff > 1)  # same isolate, conflicting BLSE results - resolved below

out <- paste0(out,
              "\nNumber of exact duplicate rows removed: ", nrow(exact_duplicates_to_remove),
              "\nNumber of same-isolate BLSE result conflicts resolved: ",
              different_result_duplicates %>% select(all_of(dup_key)) %>% distinct() %>% nrow()
)

res <- res %>% anti_join(exact_duplicates_to_remove, by = c(dup_key, "ligne"))

if (nrow(different_result_duplicates) > 0) {
  # The dup_key columns are the grouping columns, so they are carried
  # through automatically and must not be re-specified inside summarise().
  resolved <- different_result_duplicates %>%
    group_by(across(all_of(dup_key))) %>%
    summarise(
      Resultat       = ifelse(any(Resultat == "O"), "O", "N"),
      molecule       = first(molecule),
      molecule_class = first(molecule_class),
      ligne          = min(ligne),
      Date_year      = first(Date_year),
      .groups = "drop"
    )
  
  res <- res %>%
    anti_join(different_result_duplicates %>% select(all_of(dup_key), ligne),
              by = c(dup_key, "ligne")) %>%
    bind_rows(resolved)
}

##################################################
# 5. Sector duplicates: same isolate reported under more than one sector
#    the same day -> keep the highest-priority sector only.
#    Priority: Réanimation > Chirurgie > Gynécologie-Obstétrique >
#    Médecine > SSR
##################################################
sector_key <- c("code", "site", "prelevement", "Num_Patient", "age_cat",
                "bacterie", "Date_year")

sector_priority <- c("Réanimation"             = 1,
                     "Chirurgie"               = 2,
                     "Gynécologie-Obstétrique" = 3,
                     "Médecine"                = 4,
                     "SSR"                     = 5)

res <- res %>%
  mutate(sector_rank = unname(sector_priority[secteur])) %>%
  group_by(across(all_of(sector_key))) %>%
  mutate(n_sectors = n_distinct(secteur)) %>%
  ungroup()

out <- paste0(out, "\nNumber of isolates reported in two sectors the same day: ",
              res %>% filter(n_sectors > 1) %>%
                distinct(across(all_of(sector_key))) %>% nrow())

res <- res %>%
  group_by(across(all_of(sector_key))) %>%
  filter(sector_rank == min(sector_rank, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-sector_rank, -n_sectors)

##################################################
# 6. 30-day sequences: repeat BLSE samples from the same
#    patient/organism/sector within 30 days -> keep the EARLIEST one
#    ASSUMPTION: selection_30days_sequence() is not available - this
#    substitutes a standard "first isolate of the episode" rule. See
#    MISSING DEPENDENCIES note in the header.
##################################################
episode_key <- c("code", "site", "Num_Patient", "age_cat", "bacterie", "secteur")

samples_30days <- res %>%
  select(all_of(episode_key), ligne, prelevement) %>%
  distinct() %>%
  mutate(prelevement = as.Date(prelevement, "%d/%m/%Y")) %>%
  arrange(across(all_of(episode_key)), prelevement) %>%
  group_by(across(all_of(episode_key))) %>%
  mutate(
    prelevement_lag = lag(prelevement),
    time_diff = as.numeric(prelevement - prelevement_lag)
  ) %>%
  ungroup()

redundant_profiles <- samples_30days %>%
  filter(!is.na(time_diff), time_diff <= 30) %>%
  select(all_of(episode_key), ligne, prelevement)

out <- paste0(out,
              "\nNumber of samples within 30 days of a prior sample from the same patient/organism/sector: ",
              nrow(redundant_profiles)
)

res <- res %>%
  mutate(prelevement = as.Date(prelevement, "%d/%m/%Y")) %>%
  anti_join(redundant_profiles, by = c(episode_key, "ligne", "prelevement"))

##################################################
# 7. Drop the final partial week of the study period (kept as in the
#    original; the original also drops the FIRST week of its 2019-2022
#    span, which isn't applicable here since this run is 2022-only)
##################################################
res <- res %>%
  mutate(Date_week = as.Date(cut(prelevement, "week")))

out <- paste0(out, "\nNumber of isolates in the last (partial) week of 2022: ",
              res %>% filter(Date_week == as.Date("2022-12-26")) %>%
                select(all_of(episode_key), ligne) %>% distinct() %>% nrow())

res <- res %>% filter(Date_week != as.Date("2022-12-26"))

##################################################
# 8. Final cleaned dataset
##################################################
res_final <- res %>%
  rename(Date_day = prelevement) %>%
  select(code, site, secteur, Date_day, ligne, Num_Patient, age_cat,
         bacterie, molecule, molecule_class, Resultat, Date_year)

out <- paste0(out,
              "\nFinal number of BLSE-tested ESBL-E isolates: ",
              res_final %>% select(-molecule, -molecule_class, -Resultat) %>% distinct() %>% nrow(),
              "\nOf which BLSE-positive: ",
              res_final %>% filter(Resultat == "O") %>%
                select(-molecule, -molecule_class, -Resultat) %>% distinct() %>% nrow()
)

cat(out, "\n")
writeLines(out, here("Datasets", "SPARES", "esbl_sample_selection_2022.txt"))

##################################################
# 9. Save
##################################################
dir.create(here("Datasets", "SPARES"), showWarnings = FALSE, recursive = TRUE)
saveRDS(res_final, here("Datasets", "SPARES", "esbl_isolates_cleaned_2022.RDS"))
saveRDS(hospital_cohort_2022, here("Datasets", "SPARES", "hospital_cohort_2022.RDS"))

dir.create(here("Datasets", "Output Data"), showWarnings = FALSE, recursive = TRUE)
write.csv(res_final, here("Datasets", "Output Data", "esbl_isolates_cleaned_2022.csv"),
          row.names = FALSE)

cat("\nSaved cleaned ESBL-E isolate data to Datasets/SPARES/esbl_isolates_cleaned_2022.RDS\n")