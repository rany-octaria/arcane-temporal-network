############################################################
# De-identification code for Lab Surveillance data
# Rany Octaria – continuation of Laura’s work
############################################################

############################################################
## 0) Packages
############################################################
library(dplyr)
library(stringr)
library(lubridate)
library(here)
library(readxl)

options(scipen = 999)

############################################################
## 1) Import data
############################################################

df_2023 <- read_excel(
  here("Datasets", "CNR 2023-2024 pour Cpias_anonyme_mois_année.xlsx"),
  sheet = "2023"
)

df_2024 <- read_excel(
  here("Datasets", "CNR 2023-2024 pour Cpias_anonyme_mois_année.xlsx"),
  sheet = "2024"
)

df_raw <- bind_rows(df_2023, df_2024)

############################################################
## 2) Define QC / Environment rules
############################################################

qc_regex  <- "(?i)\\b(qualit[eé]|cq|quality|aglae|control|contr[oô]le|controle|qc)\\b"
env_regex <- "(?i)\\b(surface|cnr_env|environnement|chambre|siphon|douche|lavabo|[eé]vier|robinet|wc|toilettes|eau|air)\\b"

############################################################
## 3) Flag QC / Environment
############################################################

df_flagged <- df_raw %>%
  mutate(
    est_qc_control =
      str_detect(coalesce(`Nature Prélèvement`, ""), qc_regex) |
      str_detect(coalesce(`Objet/patient`, ""), qc_regex),
    
    est_environnement =
      str_detect(coalesce(`Nature Prélèvement`, ""), env_regex) |
      str_detect(coalesce(`Objet/patient`, ""), env_regex)
  )

qc_specimen <- df_flagged %>% filter(est_qc_control)

df_flagged <- df_flagged %>% filter(!est_qc_control)

############################################################
## 4) Parse Objet/patient
############################################################

df_parsed <- df_flagged %>%
  mutate(
    objet_clean = str_squish(coalesce(`Objet/patient`, "")),
    
    sexe_lettre  = str_match(objet_clean, "\\(([MF])\\)")[,2],
    sexe_extrait = if_else(is.na(sexe_lettre), NA_character_, paste0("(", sexe_lettre, ")")),
    
    date_texte_fin = str_extract(objet_clean, "\\d{2}/\\d{2}/\\d{4}\\s*$"),
    date_extrait   = dmy(date_texte_fin),
    
    libelle_extrait = objet_clean %>%
      str_remove("\\s*\\([MF]\\)\\s*") %>%
      str_remove("\\s*\\d{2}/\\d{2}/\\d{4}\\s*$") %>%
      str_squish() %>%
      str_remove(",\\s*$"),
    
    nom_extrait = if_else(est_environnement, NA_character_, str_to_upper(libelle_extrait)),
    DDN_extrait = if_else(est_environnement, as.Date(NA), as.Date(date_extrait)),
    date_prelev_env = if_else(est_environnement, as.Date(date_extrait), as.Date(NA))
  )

############################################################
## 5) Patient / entity IDs
############################################################

df_parsed <- df_parsed %>%
  mutate(
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

entite_levels <- sort(unique(df_parsed$entite_key))

df_parsed <- df_parsed %>%
  mutate(
    entite_num = match(entite_key, entite_levels),
    patient_id = if_else(
      est_environnement,
      paste0("Env_", entite_num),
      paste0("Patient_", entite_num)
    )
  )

############################################################
## 6) Specimen IDs
############################################################

spec_map <- df_parsed %>%
  distinct(
    patient_id, Mois, Année, `Nature Prélèvement`, Espèce
  ) %>%
  arrange(patient_id, Année, Mois, `Nature Prélèvement`, Espèce) %>%
  group_by(patient_id) %>%
  mutate(
    spec_num = row_number(),
    specimen_id = paste0(patient_id, "_spec_", spec_num)
  ) %>%
  ungroup()

df_specimen <- df_parsed %>%
  left_join(
    spec_map,
    by = c("patient_id","Mois","Année","Nature Prélèvement","Espèce")
  ) %>%
  arrange(entite_num, spec_num)

############################################################
## 7) Species cleaning (Culture → Espèce_clean)
############################################################

df_species_clean <- df_specimen %>%
  mutate(
    culture_raw = str_squish(Culture),
    
    culture_clean = culture_raw %>%
      str_replace("\\s*\\*?\\s*P\\.?\\s*$", "") %>%
      str_squish(),
    
    culture_clean = str_split_fixed(culture_clean, ",", 2)[,1],
    
    culture_clean = culture_clean %>%
      str_replace("^([A-Z])\\s+", "\\1. "),
    
    Espèce_clean = case_when(
      str_detect(culture_clean, regex("culture négative", TRUE)) ~ "Culture négative",
      str_detect(culture_clean, regex("polymicrob", TRUE)) ~ "Flore polymicrobienne",
      str_detect(culture_clean, regex("polymorphe", TRUE)) ~ "Flore polymorphe",
      TRUE ~ culture_clean
    )
  ) %>%
  select(-culture_raw, -culture_clean)

############################################################
## 8) Final taxonomic standardization
############################################################

df_internal_clean <- df_species_clean %>%
  mutate(
    Espèce_final = case_when(
      Espèce_clean %in% c("-", ".", "?", "") ~ NA_character_,
      Espèce_clean %in% c("E. coli") ~ "Escherichia coli",
      Espèce_clean %in% c("K. pneumoniae") ~ "Klebsiella pneumoniae",
      Espèce_clean %in% c("S. marcescens") ~ "Serratia marcescens",
      TRUE ~ Espèce_clean
    )
  ) %>%
  select(-matches("^\\.\\.\\."))

############################################################
## 9) Shareable dataset (FR)
############################################################

df_shareable <- df_internal_clean %>%
  select(
    patient_id,
    specimen_id,
    Sexe,
    Age,
    est_environnement,
    Mois,
    Année,
    `Mois de réception au CNR`,
    Etablissement,
    `Nom de l'experiteur`,
    `HOP / LAM`,
    `Dép.`,
    `ZIP code`,
    ZIP,
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
## 10) Shareable dataset (EN clean)
############################################################

df_shareable_clean <- df_shareable %>%
  rename(
    patient_id = patient_id,
    specimen_id = specimen_id,
    is_environment = est_environnement,
    sample_month = Mois,
    sample_year = Année,
    Sex = Sexe,
    Age = Age,
    reception_month = `Mois de réception au CNR`,
    facility_name = Etablissement,
    sending_lab = `Nom de l'experiteur`,
    facility_type = `HOP / LAM`,
    department_code = `Dép.`,
    zip_code_sender = `ZIP code`,
    zip_code_facility = ZIP,
    specimen_type = `Nature Prélèvement`,
    culture_raw = Culture,
    species = Espèce_final,
    carb_mlst = `C.alb MLST`,
    np_test = `NP Test`,
    sequencing = Séquençage,
    resistance_profile = `Résist. carba Conc`,
    interpretation = Conclusion,
    number_of_strain = `Nb souches`
  )

############################################################
## 11) Exports
############################################################

write.csv(df_internal_clean,
          here("Datasets","Output Data","CNR_2023_2024_INTERNAL_cleaned.csv"),
          row.names = FALSE)

write.csv(df_shareable,
          here("Datasets","Output Data","CNR_2023_2024_DEIDENTIFIED_shareable_FR.csv"),
          row.names = FALSE)

write.csv(df_shareable_clean,
          here("Datasets","Output Data","CNR_2023_2024_DEIDENTIFIED_shareable_EN_clean.csv"),
          row.names = FALSE)

#install.packages("writexl")
library(writexl)

write_xlsx(
  df_internal_clean,
  here("Datasets", "Output Data", "CNR_2023_2024_INTERNAL_cleaned.xlsx")
)

write_xlsx(
  df_shareable,
  here("Datasets", "Output Data", "CNR_2023_2024_DEIDENTIFIED_shareable_FR.xlsx")
)

write_xlsx(
  df_shareable_clean,
  here("Datasets", "Output Data", "CNR_2023_2024_DEIDENTIFIED_shareable_EN_clean.xlsx")
)

############################################################
## 12) Data dictionaries
############################################################

dict_fr <- tibble(
  variable = names(df_shareable),
  type = sapply(df_shareable, class) |> sapply(`[`,1),
  description = c(
    "Identifiant synthétique du patient",
    "Identifiant synthétique du prélèvement",
    "Prélèvement environnemental",
    "Mois du rapport",
    "Année du rapport",
    "Sexe",
    "Age",
    "Mois de réception au CNR",
    "Nom de l'établissement",
    "Laboratoire expéditeur",
    "Type de structure",
    "Code département",
    "Code postal expéditeur",
    "Code postal établissement",
    "Type de prélèvement",
    "Culture brute",
    "Espèce bactérienne standardisée",
    "MLST carbapénémase",
    "Test NP",
    "Séquençage",
    "Profil de résistance",
    "Interprétation",
    "Nombre de souches"
  )
)

dict_en <- tibble(
  variable = names(df_shareable_clean),
  type = sapply(df_shareable_clean, class) |> sapply(`[`,1),
  description = c(
    "Synthetic patient identifier",
    "Synthetic specimen identifier",
    "Environmental sample flag",
    "Month report was received",
    "Year the report was received",
    "Sex",
    "Age",
    "Month received at reference center",
    "Facility name",
    "Sending laboratory",
    "Facility type",
    "Department code",
    "Sender ZIP code",
    "Facility ZIP code",
    "Specimen type",
    "Raw culture result",
    "Standardized bacterial species",
    "Carbapenemase MLST",
    "NP test result",
    "Sequencing result",
    "Resistance profile",
    "Clinical interpretation",
    "Number of strains"
)
)

write_xlsx(dict_fr,
          here("Datasets","Output Data","DATA_DICTIONARY_CNR_SHAREABLE_FR.xlsx"))

write_xlsx(dict_en,
          here("Datasets","Output Data","DATA_DICTIONARY_CNR_SHAREABLE_EN.xlsx"))

############################################################
## END
############################################################