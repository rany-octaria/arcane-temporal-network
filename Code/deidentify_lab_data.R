# De-identification code for Lab Surveillance data
# rRany Octaria, continuing the work started by Laura

############################################################
## 0) Packages
############################################################
library(dplyr)
library(stringr)
library(lubridate)
library(digest)
library(here)
library(readxl)

############################################################
## HIPAA‑COMPLIANT ANONYMIZATION OF LABORATORY DATASET
##
## Goal:
##  - Preserve original columns internally
##  - Create a unique PERSON ID (one per patient)
##  - Create a unique SPECIMEN ID (one per sample)
##  - Generate a shareable anonymized dataset
##
## IMPORTANT:
##  - NEVER SHARE THE SECRET SALT
##  - NEVER SHARE THE ORIGINAL DATASET
############################################################


#########################
## 1. Load dependencies
#########################

# dplyr : data manipulation
# digest: cryptographic hashing (irreversible IDs)


#Importing dataset
options(scipen = 999)
df_2023<- read_excel(here::here("Datasets", "CNR 2023-2024 pour Cpias_anonyme_mois_année.xlsx"),
                     sheet = "2023")

df_2024<- read_excel(here::here("Datasets", "CNR 2023-2024 pour Cpias_anonyme_mois_année.xlsx"),
                     sheet = "2024")

df_2023_2024 <- bind_rows(df_2023, df_2024)

# Work on a copy
df_internal <- df_2023_2024

############################################################
## 2) Define QC/Control and Environment regex rules
##    - QC/control: exclude
##    - Environment: keep but mark + ID as Env_#
############################################################

# QC/control keywords (case-insensitive)
qc_regex <- "(?i)\\b(qualit[eé]|cq|quality|aglae|control|contr[oô]le|controle|qc)\\b"

# Environment keywords (case-insensitive)
# You can tune this list anytime.
env_regex <- "(?i)\\b(surface|cnr_env|environnement|chambre|siphon|douche|lavabo|[eé]vier|robinet|wc|toilettes|eau|air)\\b"

############################################################
## 3) Flag QC/control + Environment (using BOTH columns)
############################################################

df_internal <- df_internal %>%
  mutate(
    # QC/control flag looks at Nature Prélèvement OR Objet/patient
    est_qc_control =
      str_detect(coalesce(`Nature Prélèvement`, ""), qc_regex) |
      str_detect(coalesce(`Objet/patient`, ""), qc_regex),
    
    # Environment flag looks at Nature Prélèvement OR Objet/patient
    est_environnement =
      str_detect(coalesce(`Nature Prélèvement`, ""), env_regex) |
      str_detect(coalesce(`Objet/patient`, ""), env_regex)
  )

# (Optional) Keep QC/control rows separately for review
qc_specimen <- df_internal %>% filter(est_qc_control)

# Exclude QC/control from the working dataset
df_internal1 <- df_internal %>%
  filter(!est_qc_control)

############################################################
## 4) Parse `Objet/patient` to extract:
##    - sexe_extrait: "(M)" / "(F)" if present
##    - date_extrait: trailing dd/mm/yyyy if present
##    - libelle_extrait: label/name text with sex + trailing date removed
##
## IMPORTANT:
## - If environment: date_extrait is considered sampling date (date_prelev_env)
## - If patient: date_extrait is DOB (DDN_extrait)
##
## FIX: Use if_else() (not ifelse()) to preserve Date class
############################################################

df_internal1 <- df_internal1 %>%
  mutate(
    objet_clean = str_squish(coalesce(`Objet/patient`, "")),
    
    # Extract sex letter if present
    sexe_lettre  = str_match(objet_clean, "\\(([MF])\\)")[, 2],
    sexe_extrait = if_else(is.na(sexe_lettre), NA_character_, paste0("(", sexe_lettre, ")")),
    
    # Extract trailing date if present (dd/mm/yyyy at end)
    date_texte_fin = str_extract(objet_clean, "\\d{2}/\\d{2}/\\d{4}\\s*$"),
    date_extrait   = dmy(date_texte_fin),
    
    # Remove gender token and trailing date to get a clean label
    libelle_extrait = objet_clean %>%
      str_remove("\\s*\\([MF]\\)\\s*") %>%                 # remove "(M)" or "(F)" if present
      str_remove("\\s*\\d{2}/\\d{2}/\\d{4}\\s*$") %>%      # remove trailing date if present
      str_squish() %>%
      str_remove(",\\s*$") %>%                             # drop trailing comma
      str_trim(),
    
    # Patient name extracted only for non-environment rows
    nom_extrait = if_else(est_environnement, NA_character_, str_to_upper(libelle_extrait)),
    
    # DOB extracted only for non-environment rows (Date class preserved)
    DDN_extrait = if_else(est_environnement, as.Date(NA), as.Date(date_extrait)),
    
    # Environment sampling date (Date class preserved)
    date_prelev_env = if_else(est_environnement, as.Date(date_extrait), as.Date(NA))
  )

############################################################
## 5) Build patient/entity IDs
##
## REQUIREMENT UPDATE:
## - Include patients even when DOB is missing (e.g., "..., (M)")
##
## Key logic:
## - If DOB exists: key = NAME | DOB   (no sex used)
## - If DOB missing: key = NAME | NO_DOB | SEX(or NO_SEX)  (tie-breaker only)
##
## Environment rows:
## - key = ENV | LABEL
##
## FIX: entite_levels must be built from df_internal1 (NOT df_internal)
############################################################

df_internal1 <- df_internal1 %>%
  mutate(
    # safe string keys (never NA)
    dob_key = if_else(!is.na(DDN_extrait), as.character(DDN_extrait), "NO_DOB"),
    sex_key = coalesce(sexe_extrait, "NO_SEX"),
    
    patient_key = if_else(
      !est_environnement,
      if_else(
        dob_key != "NO_DOB",
        paste(nom_extrait, dob_key, sep = " | "),
        paste(nom_extrait, dob_key, sex_key, sep = " | ")
      ),
      NA_character_
    ),
    
    env_key = if_else(
      est_environnement,
      paste("ENV", str_to_upper(libelle_extrait), sep = " | "),
      NA_character_
    ),
    
    entite_key = coalesce(patient_key, env_key)
  )

# FIX: build levels from df_internal1
entite_levels <- sort(unique(df_internal1$entite_key))

df_internal1 <- df_internal1 %>%
  mutate(
    entite_num = match(entite_key, entite_levels),
    patient_id = if_else(
      est_environnement,
      paste0("Env_", entite_num),
      paste0("Patient_", entite_num)
    )
  )

############################################################
## 6) Specimen IDs: deduplicate per patient using:
##    patient_id + Mois + Année + Nature Prélèvement + Espèce
##
## specimen_id format: Patient_#_spec_#
############################################################

spec_map <- df_internal1 %>%
  distinct(
    patient_id,
    Mois,
    Année,
    `Nature Prélèvement`,
    Espèce
  ) %>%
  arrange(patient_id, Année, Mois, `Nature Prélèvement`, Espèce) %>%
  group_by(patient_id) %>%
  mutate(
    spec_num = row_number(),
    specimen_id = paste0(patient_id, "_spec_", spec_num)
  ) %>%
  ungroup()

df_internal1 <- df_internal1 %>%
  left_join(
    spec_map,
    by = c("patient_id", "Mois", "Année", "Nature Prélèvement", "Espèce")
  )

############################################################
## 7) Final sort (as requested): by patient_id then specimen #
############################################################

df_internal1 <- df_internal1 %>%
  arrange(entite_num, spec_num)

############################################################
## 8) Diagnostics
##    - Missing nom_extrait is the only "true" parsing problem for patients
##    - Missing DOB is allowed and tracked separately if needed
############################################################

# True parse problems: patient row but no extracted name
bad_parse <- df_internal1 %>%
  filter(!est_environnement & is.na(nom_extrait)) %>%
  select(`Objet/patient`, `Nature Prélèvement`, nom_extrait, sexe_extrait, DDN_extrait)

print(head(bad_parse, 50))

# Optional: track missing DOB (NOT an error)
missing_dob <- df_internal1 %>%
  filter(!est_environnement & is.na(DDN_extrait)) %>%
  select(`Objet/patient`, `Nature Prélèvement`, nom_extrait, sexe_extrait, DDN_extrait)

print(head(missing_dob, 50))

# Ensure specimen IDs exist
stopifnot(!any(is.na(df_internal1$specimen_id)))


############################################################
## 8b) CLEAN & STANDARDIZE SPECIES FROM `Culture`
############################################################

df_internal1 <- df_internal1 %>%
  mutate(
    ########################################################
    # 1) Start from Culture
    ########################################################
    culture_raw = str_squish(Culture),
    
    
    ########################################################
    # 2) Remove positivity markers ONLY at end of string
    #    Examples removed:
    #      "* P", "*P", " P", "P.", "P"
    #    Examples preserved:
    #      "P. aeruginosa", "Proteus mirabilis"
    ########################################################
    culture_clean = culture_raw %>%
      str_replace("\\s*\\*?\\s*P\\.?\\s*$", "") %>%  # ← KEY FIX
      str_squish(),
    
    
    ########################################################
    # 3) If multiple species are listed, keep the FIRST one
    #    (before comma)
    ########################################################
    culture_clean = str_split_fixed(culture_clean, ",", 2)[, 1],
    
    ########################################################
    # 4) Normalize spacing and case
    ########################################################
    culture_clean = culture_clean %>%
      str_squish() %>%
      str_replace_all("\\s+", " "),
    
    ########################################################
    # 5) Standardize abbreviated genus names
    #    E coli  -> E. coli
    #    K pneumoniae -> K. pneumoniae
    ########################################################
    culture_clean = culture_clean %>%
      str_replace("^([A-Z])\\s+", "\\1. "),
    
    ########################################################
    # 6) Standardize common patterns / spelling
    ########################################################
    culture_clean = culture_clean %>%
      str_replace("^CI\\. ", "Citrobacter ") %>%
      str_replace("^CI ", "Citrobacter ") %>%
      str_replace("^Entrb ", "Enterobacter ") %>%
      str_replace("^Entrbc_", "Enterobacter ") %>%
      str_replace("^E\\. cloacae complex$", "Enterobacter cloacae complex") %>%
      str_replace("^E\\. cloacae$", "Enterobacter cloacae") %>%
      str_replace("^E\\. coli$", "Escherichia coli") %>%
      str_replace("^K\\. pneumoniae$", "Klebsiella pneumoniae") %>%
      str_replace("^K\\. oxytoca$", "Klebsiella oxytoca") %>%
      str_replace("^K\\. aerogenes$", "Klebsiella aerogenes") %>%
      str_replace("^S\\. marcescens$", "Serratia marcescens") %>%
      str_replace("^S\\. maltophilia$", "Stenotrophomonas maltophilia") %>%
      str_replace("^P\\. aeruginosa$", "Pseudomonas aeruginosa") %>%
      str_replace("^Pseudomonas sp\\.$", "Pseudomonas sp.") %>%
      str_replace("^Acinetobacter sp\\.$", "Acinetobacter sp.") %>%
      str_replace("^Salmonella sp\\.$", "Salmonella sp."),
    
    ########################################################
    # 7) Standardize non-species culture results
    ########################################################
    Espèce_clean = case_when(
      str_detect(culture_clean, regex("culture négative", ignore_case = TRUE)) ~ "Culture négative",
      str_detect(culture_clean, regex("polymicrob", ignore_case = TRUE)) ~ "Flore polymicrobienne",
      str_detect(culture_clean, regex("polymorphe", ignore_case = TRUE)) ~ "Flore polymorphe",
      TRUE ~ culture_clean
    )
  ) %>%
  select(-culture_raw, -culture_clean)

############################################################
## 8c) FINAL TAXONOMIC STANDARDIZATION
##     Espèce_clean  -->  Espèce_final
############################################################

df_internal_clean <- df_internal1 %>%
  mutate(
    ########################################################
    # 1) Normalize empty / meaningless values
    ########################################################
    Espèce_final = case_when(
      Espèce_clean %in% c("-", ".", "?", "") ~ NA_character_,
      TRUE ~ Espèce_clean
    ),
    
    ########################################################
    # 2) Expand abbreviated genus names
    ########################################################
    Espèce_final = case_when(
      # Escherichia
      str_detect(Espèce_final, "^E\\. coli$|^E coli$") ~ "Escherichia coli",
      
      # Klebsiella
      str_detect(Espèce_final, "^K\\. pneumoniae$|^K pneumoniae$") ~ "Klebsiella pneumoniae",
      str_detect(Espèce_final, "^K\\. oxytoca$|^K oxytoca$") ~ "Klebsiella oxytoca",
      str_detect(Espèce_final, "^K\\. aerogenes$|^K aerogenes$") ~ "Klebsiella aerogenes",
      str_detect(Espèce_final, "^K\\. variicola$|^K variicola$") ~ "Klebsiella variicola",
      str_detect(Espèce_final, "^K\\. cowanii$|^K cowanii$") ~ "Klebsiella cowanii",
      str_detect(Espèce_final, "^K\\. cryocrescens$|^K cryocrescens$") ~ "Klebsiella cryocrescens",
      
      # Enterobacter
      str_detect(Espèce_final, "^E\\. cloacae complex$|^E cloacae complex$") ~
        "Enterobacter cloacae complex",
      str_detect(Espèce_final, "^E\\. cloacae$|^E cloacae$") ~
        "Enterobacter cloacae",
      str_detect(Espèce_final, "^Enterobacter hormch.*") ~
        "Enterobacter hormaechei",
      str_detect(Espèce_final, "^Enterobacter kobei$") ~
        "Enterobacter kobei",
      
      # Citrobacter
      str_detect(Espèce_final, "^C\\. freundii$|^Citrobacter freundii$") ~
        "Citrobacter freundii",
      str_detect(Espèce_final, "^C\\. freundii complex$") ~
        "Citrobacter freundii complex",
      str_detect(Espèce_final, "^C\\. koseri$") ~
        "Citrobacter koseri",
      str_detect(Espèce_final, "^C\\. braakii$") ~
        "Citrobacter braakii",
      str_detect(Espèce_final, "^C\\. farmeri$") ~
        "Citrobacter farmeri",
      str_detect(Espèce_final, "^C\\. amalonaticus$") ~
        "Citrobacter amalonaticus",
      str_detect(Espèce_final, "^C\\. youngae$") ~
        "Citrobacter youngae",
      str_detect(Espèce_final, "^C\\. murliniae$") ~
        "Citrobacter murliniae",
      str_detect(Espèce_final, "^C\\. werkmanii$") ~
        "Citrobacter werkmanii",
      
      # Serratia
      str_detect(Espèce_final, "^S\\. marcescens$|^S marcescens$") ~
        "Serratia marcescens",
      
      # Proteus
      str_detect(Espèce_final, "^P\\. mirabilis$") ~
        "Proteus mirabilis",
      str_detect(Espèce_final, "^P\\. vulgaris$") ~
        "Proteus vulgaris",
      str_detect(Espèce_final, "^P\\. penneri$") ~
        "Proteus penneri",
      
      # Morganella
      str_detect(Espèce_final, "^M\\. morganii$") ~
        "Morganella morganii",
      
      # Providencia
      str_detect(Espèce_final, "^P\\. rettgeri$") ~
        "Providencia rettgeri",
      str_detect(Espèce_final, "^P\\. stuartii$") ~
        "Providencia stuartii",
      
      # Pseudomonas
      str_detect(Espèce_final, "^P\\. aeruginosa$") ~
        "Pseudomonas aeruginosa",
      str_detect(Espèce_final, "^Pseudomonas sp\\.$") ~
        "Pseudomonas sp.",
      
      # Stenotrophomonas
      str_detect(Espèce_final, "^S\\. maltophilia$") ~
        "Stenotrophomonas maltophilia",
      
      # Hafnia
      str_detect(Espèce_final, "^H\\. alvei$") ~
        "Hafnia alvei",
      
      # Acinetobacter
      str_detect(Espèce_final, "^A\\. baumannii$") ~
        "Acinetobacter baumannii",
      str_detect(Espèce_final, "^A\\. pittii$") ~
        "Acinetobacter pittii",
      str_detect(Espèce_final, "^A\\. caviae$") ~
        "Aeromonas caviae",
      str_detect(Espèce_final, "^A\\. veronii$") ~
        "Aeromonas veronii",
      
      # Salmonella
      str_detect(Espèce_final, "^Salmonella sp\\.$") ~
        "Salmonella sp.",
      
      ########################################################
      # Default: keep as-is
      ########################################################
      TRUE ~ Espèce_final
    )
  )

############################################################
## 8) REMOVE UNNAMED / PLACEHOLDER COLUMNS (…1, …23, etc.)
##    Keep EVERYTHING else for internal dataset
############################################################

df_internal_clean <- df_internal_clean %>%
  select(-matches("^\\.\\.\\."))

############################################################
## 9) CREATE DE‑IDENTIFIED SHAREABLE DATASET
##    - Patient identifiers removed
##    - Hospital / unit / ZIP / dates KEPT
############################################################

df_shareable <- df_internal_clean %>%
  select(
    # --- Synthetic IDs ---
    patient_id,
    specimen_id,
    
    # --- Sample classification ---
    est_environnement,
    
    # --- Demographics (non‑identifying) ---
    #Sexe,
    #Age,
    #`Année de naissance`,
    
    # --- Time ---
    Mois,
    Année,
    `Mois de réception au CNR`,
    
    # --- Hospital / laboratory information (KEPT) ---
    Etablissement,
    `Nom de l'experiteur`,
    `HOP / LAM`,
    `Dép.`,
    `ZIP code`,
    ZIP,
    
    # --- Specimen / microbiology ---
    `Nature Prélèvement`,
    Culture,
    Espèce_final,
    `C.alb MLST`,
    `NP Test`,
    Séquençage,
    `Résist. carba Conc`,
    Conclusion,
    `Nb souches`
  )

############################################################
## 10) EXPORT INTERNAL CLEANED DATASET (RESTRICTED)
##     No columns removed except unnamed placeholders
############################################################

write.csv(
  df_internal_clean,
 here::here("Datasets", "Output Data", "CNR_2023_2024_INTERNAL_cleaned.csv"),
  row.names = FALSE
)
############################################################
## 11) EXPORT DE‑IDENTIFIED SHAREABLE DATASET (FR)
############################################################

write.csv(
  df_shareable,
  here::here("Datasets", "Output Data", "CNR_2023_2024_DEIDENTIFIED_shareable.csv"),
  row.names = FALSE
)

############################################################
## 12) CREATE ENGLISH / STANDARDIZED VERSION (_clean)
############################################################

df_shareable_clean <- df_shareable %>%
  rename(
    # Identifiers
    patient_id        = patient_id,
    specimen_id       = specimen_id,
    
    # Sample type
    is_environment    = est_environnement,
    
    # Demographics
    #sex               = Sexe,
    #age               = Age,
    #birth_year        = `Année de naissance`,
    
    # Time
    sample_month      = Mois,
    sample_year       = Année,
    reception_month   = `Mois de réception au CNR`,
    
    # Hospital / lab
    facility_name     = Etablissement,
    sending_lab       = `Nom de l'experiteur`,
    facility_type     = `HOP / LAM`,
    department_code   = `Dép.`,
    zip_code_sender   = `ZIP code`,
    zip_code_facility = ZIP,
    
    # Microbiology
    specimen_type     = `Nature Prélèvement`,
    culture_raw       = Culture,
    species           = Espèce_final,
    carb_mlst         = `C.alb MLST`,
    np_test           = `NP Test`,
    sequencing        = Séquençage,
    resistance_profile= `Résist. carba Conc`,
    interpretation    = Conclusion,
    isolate_count     = `Nb souches`
  )

############################################################
## 12) Export CLEAN ENGLISH version
############################################################

write.csv(
  df_shareable_clean,
  here::here("Datasets", "Output Data","CNR_2023_2024_anonymized_SHAREABLE_clean.csv"),
  row.names = FALSE
)



############################################################
## 13) DATA DICTIONARY — FRENCH
############################################################

dict_fr <- tibble(
  variable = names(df_shareable),
  type = sapply(df_shareable, class) |> sapply(`[`, 1),
  description = c(
    "Identifiant synthétique du patient",
    "Identifiant synthétique du prélèvement",
    "Indique si le prélèvement est environnemental",
    "Sexe du patient",
    "Âge du patient (>=89 regroupé)",
    "Année de naissance",
    "Mois du prélèvement",
    "Année du prélèvement",
    "Mois de réception au CNR",
    "Nom de l'établissement",
    "Laboratoire expéditeur",
    "Type de structure (Hôpital / Laboratoire)",
    "Code département",
    "Code postal du laboratoire",
    "Code postal de l'établissement",
    "Type de prélèvement",
    "Résultat brut de culture",
    "Espèce bactérienne standardisée",
    "MLST carbapénémase",
    "Test NP",
    "Résultat du séquençage",
    "Profil de résistance",
    "Interprétation microbiologique",
    "Nombre de souches"
  )
)

write.csv(
  dict_fr,
  "DATA_DICTIONARY_CNR_SHAREABLE_FR.csv",
  row.names = FALSE
)

############################################################
## 11) DATA DICTIONARY — ENGLISH
############################################################

dict_en <- tibble(
  variable = names(df_shareable_clean),
  type = sapply(df_shareable_clean, class) |> sapply(`[`, 1),
  description = c(
    "Synthetic patient identifier",
    "Synthetic specimen identifier",
    "Environmental sample flag",
    "Patient sex",
    "Patient age (>=89 grouped)",
    "Year of birth",
    "Specimen collection month",
    "Specimen collection year",
    "Month received at National Reference Center",
    "Facility name",
    "Sending laboratory",
    "Facility type (hospital / laboratory)",
    "Department code",
    "Laboratory ZIP code",
    "Facility ZIP code",
    "Specimen type",
    "Raw culture result",
    "Standardized bacterial species",
    "Carbapenemase MLST",
    "NP test result",
    "Sequencing result",
    "Resistance profile",
    "Clinical interpretation",
    "Number of isolates"
  )
)

write.csv(
  dict_en,
  "DATA_DICTIONARY_CNR_SHAREABLE_EN.csv",
  row.names = FALSE
)