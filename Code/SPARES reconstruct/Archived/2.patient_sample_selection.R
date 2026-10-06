##################################################
## SELECTION OF BACTERIAL SAMPLES FOR THE
## FIVE SPECIES OF INTEREST  --  2022 ONLY
## Adapted from 2_patient_sample_selection.R
##################################################
# CHANGES vs the original (the selection/deduplication logic itself is the authors', unchanged):
#  0. Paths are in one block at the top; missing files are reported clearly BEFORE anything runs; the
#     resistance file can be .csv ("|"-separated), .RDS or .xlsx.
#  1. 2022 only; foreach/doParallel replaced by a plain for-loop (one year, and it avoids
#     parallel-export problems on Windows). foreach, doParallel and gt are still loaded but not used.
#  2. Hospital cohort is Datasets/SPARES/cohort2022.rda from 0_hospital_sample_selection_2022.R (no
#     multi-year reporting requirement). Object and output names use "cohort2022" instead of
#     "cohort19202122", so the authors' own files are never overwritten.
#  3. ADDED (diagnostic only, changes nothing): the log now reports the number of BLSE-positive
#     isolates (E. coli, K. pneumoniae, E. cloacae complex) at each stage, so you can see
#     exactly which step removes positives.
#  4. Removed: everything after the per-year loop (tables/plots that need data/cohort_final.rda
#     and PMSI data, carbapenem and macrolide analyses) - not part of sample selection.
# Needs (as in the original): helper_functions.R (selection_30days_sequence) and
# dictionaries.R (enzymes, bacteria_of_interest, dict_id_age, dict_molecule_class,
# dict_secteur_spares, dict_id_site) - paths are in the block below.
##################################################

#rm(list = ls())
library(tidyverse)
library(foreach)
library(doParallel)
library(gt)

# ---- Paths: edit to where YOUR files are (relative to R's working directory = the project root) ----
path_cohort  = "./Datasets/SPARES/cohort2022.rda"                 # made by 0_hospital_sample_selection_2022.R
path_helpers = "./Code/SPARES Reconstruct/helper_functions.R"
path_dicts   = "./Code/SPARES Reconstruct/dictionaries.R"
path_res22   = "./Datasets/SPARES/BMR_Covid_souches_2022.RDS"     # .csv ("|" separated), .RDS or .xlsx
# Outputs are written to Datasets/SPARES/combined/ (created below if it does not exist)
path_pmsi = "./Datasets/MCO_SSR_HBN_2024/finessgeo_metadata_2024.csv"
# ---- Helpers (added): clear error messages for missing files + flexible readers ----------------
# Stops BEFORE anything runs and lists EVERY missing file at once, with similarly named files found
# under the working directory, instead of a bare "cannot open the connection" from inside the loop.
check_inputs = function(paths, hints = NULL) {
  missing = paths[!file.exists(paths)]
  if (length(missing) > 0) {
    msg = paste0("Input file(s) not found. Edit the path block at the top of this script.\n",
                 "Working directory: ", getwd(), "\n")
    for (nm in names(missing)) {
      msg = paste0(msg, "  - ", nm, ": ", missing[[nm]], "\n")
      key = if (!is.null(hints) && nm %in% names(hints)) hints[[nm]] else sub("\\.[^.]*$", "", basename(missing[[nm]]))
      cand = list.files(".", pattern = key, recursive = TRUE, ignore.case = TRUE)
      if (length(cand) > 0) msg = paste0(msg, "      similar file(s) found: ", paste(head(cand, 5), collapse = "  |  "), "\n")
    }
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

# Reads the 2022 resistance ("souches") file from .csv ("|"-separated, the authors' format), .RDS or .xlsx,
# and names the hospital column `code`, as the authors' script does right after reading.
read_res22 = function(path) {
  ext = tolower(tools::file_ext(path))
  if (ext %in% c("csv", "txt")) {
    d = read.table(path, sep = "|", header = TRUE, encoding = "latin1")   # change sep if yours differs
  } else {
    d = if (ext == "rds") readRDS(path)
    else if (ext %in% c("xlsx", "xls")) readxl::read_excel(path)
    else stop("Unsupported file type '", ext, "' for ", path, " (use .csv, .RDS or .xlsx)", call. = FALSE)
    d = as.data.frame(d)
    # repair text stored with the wrong encoding (e.g. "C\xe9phalosporinase" -> "Céphalosporinase")
    d[] = lapply(d, function(x) {
      if (is.character(x)) { bad = !validUTF8(x); x[bad] = iconv(x[bad], "latin1", "UTF-8") }
      x
    })
  }
  d = as.data.frame(d)
  if ("IdEtablissement" %in% names(d)) names(d)[names(d) == "IdEtablissement"] = "code"
  # the authors' code expects `prelevement` as "dd/mm/yyyy" text; convert if it was stored differently
  if ("prelevement" %in% names(d)) {
    p = d$prelevement
    if (inherits(p, c("Date", "POSIXt"))) {
      d$prelevement = format(as.Date(p), "%d/%m/%Y")
    } else if (is.numeric(p)) {                                                   # Excel serial numbers
      d$prelevement = format(as.Date(p, origin = "1899-12-30"), "%d/%m/%Y")
    } else if (is.character(p) && any(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", p))) {  # yyyy-mm-dd
      d$prelevement = format(as.Date(substr(p, 1, 10)), "%d/%m/%Y")
    }
    ok = sum(!is.na(as.Date(d$prelevement, "%d/%m/%Y")))
    message("[ADDED] `prelevement` dates readable as dd/mm/yyyy: ", ok, " of ", nrow(d))
    if (ok == 0) stop("None of the `prelevement` values can be read as dd/mm/yyyy dates. First values: ",
                      paste(head(unique(d$prelevement), 5), collapse = ", "), call. = FALSE)
  }
  d
}

check_inputs(c(cohort = path_cohort, helper_functions = path_helpers, dictionaries = path_dicts,
               resistance_2022 = path_res22),
             hints = c(cohort = "cohort", helper_functions = "helper", dictionaries = "dictionar",
                       resistance_2022 = "souches"))
dir.create("Datasets/SPARES/combined", showWarnings = FALSE, recursive = TRUE)

loaded_objects = load(path_cohort)
# ADDED: the cohort file must contain an object called `cohort2022` and it must not be empty
if (!"cohort2022" %in% loaded_objects) {
  stop("The cohort file does not contain an object called `cohort2022` (it contains: ",
       paste(loaded_objects, collapse = ", "), "). Rerun 0_hospital_sample_selection_2022.R.", call. = FALSE)
}
if (length(cohort2022) == 0) {
  stop("`cohort2022` is empty. Rerun 0_hospital_sample_selection_2022.R and look at its ",
       "'excluded by ...' counts to see which criterion removes every hospital.", call. = FALSE)
}
message("[ADDED] cohort2022: ", length(cohort2022), " hospitals (class ", class(cohort2022)[1],
        "), first codes: ", paste(head(cohort2022, 8), collapse = ", "))
source(path_helpers)
source(path_dicts)
needed_objects = c("enzymes", "bacteria_of_interest", "dict_id_age", "dict_molecule_class",
                   "dict_secteur_spares", "dict_id_site")
if (!all(sapply(needed_objects, exists))) {
  stop("dictionaries.R does not define: ", paste(needed_objects[!sapply(needed_objects, exists)], collapse = ", "),
       call. = FALSE)
}

# ADDED: the authors' rename + Date_day / Date_week / Date_month step, unchanged, except that it no
# longer stops when the table is empty (cut() fails with "'to' must be a finite number" on 0 rows,
# e.g. when every isolate was tested and the "not tested" table has no rows).
add_dates = function(d) {
  d = d %>%
    rename(Date_day = prelevement, atb_class = molecule_class) %>%
    mutate(Date_day = as.Date(Date_day, "%d/%m/%Y"))
  if (any(!is.na(d$Date_day))) {
    d = d %>% mutate(Date_week = as.Date(cut(Date_day, "week")),
                     Date_month = as.Date(cut(Date_day, "month")))
  } else {
    d = d %>% mutate(Date_week = as.Date(NA), Date_month = as.Date(NA))
  }
  d
}

# ADDED (diagnostic): number of BLSE-positive isolates among the 3 Enterobacterales
count_blse_pos = function(d) {
  d %>%
    filter(bacterie %in% c("Escherichia coli", "Klebsiella pneumoniae", "Enterobacter cloacae complex"),
           molecule == "BLSE", Resultat == "O") %>%
    select(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur, ligne) %>%
    distinct() %>%
    nrow()
}

#Load PMSI data
pmsi = read.csv(path_pmsi) %>% 
  mutate(pmsi_factype_new = ifelse(hospital_type == "SSR", "SSR", pmsi_category))
table(pmsi$pmsi_factype_new)
##################################################
# Process resistance data
##################################################
all_years = 2022

for (y in all_years) {
  
  out = ""
  out = paste0(out,
               "###############################################################\n",
               "Resistance data - ", y, "\n",
               "###############################################################\n"
  )
  
  # Basic checks----------------------------------------------------------------
  if (y == 2022) res = read_res22(path_res22)
  
  n_isolates = res %>%
    select(-c(IdMolecule, molecule, nosocomial,Resultat, IdArchiveLaboratoire)) %>%
    distinct() %>%
    nrow(.)
  
  out = paste0(out,
               "Number of isolates: ",
               n_isolates,
               
               "\nNumber of isolates when removing unwanted columns: ",
               res %>%
                 select(-c(IdMolecule, molecule, nosocomial,Resultat, IdArchiveLaboratoire,
                           IdPeriode, IdBacterie, suppr_dedoublonnage2, iduf, idactivite, code_de, code_ta, IdetablissementLaboratoire)) %>%
                 distinct() %>%
                 nrow(.),
               
               "\nNumber of duplicated isolated (IdArchiveLaboratoire): ",
               nrow(res %>%
                      select(-c(IdMolecule, IdBacterie, IdetablissementLaboratoire, nosocomial, code_ta, code_de, idactivite, iduf, suppr_dedoublonnage2, IdPeriode)) %>%
                      distinct() %>%
                      group_by(across(-c(IdArchiveLaboratoire, Resultat))) %>%
                      mutate(n = n()) %>%
                      filter(n > 1)),
               
               "\nNumber of samples with multiple bacterial species for one isolate: ",
               res %>%
                 select(code, IdSite, secteur, prelevement, ligne, Num_Patient, bacterie) %>%
                 distinct() %>%
                 group_by(across(-bacterie)) %>%
                 mutate(n = n()) %>%
                 filter(n > 1) %>%
                 nrow(.),
               
               "\nNumber of samples in ICUs: ",
               res %>%
                 filter(secteur == "Réanimation") %>%
                 select(-c(IdMolecule, molecule, nosocomial, Resultat, IdArchiveLaboratoire)) %>%
                 distinct() %>%
                 nrow(.)
  )
  
  # ADDED (diagnostic, changes nothing): rows remaining after each of the authors' filters below,
  # printed immediately so you can see which one removes the data.
  waterfall = list(
    "sector (Chirurgie, Gynécologie-Obstétrique, Médecine, Réanimation, SSR)" =
      res$secteur %in% c("Chirurgie", "Gynécologie-Obstétrique", "Médecine", "Réanimation", "SSR"),
    "hospital cohort (cohort2022)" = res$code %in% cohort2022,
    "newborn site (IdSite != 13)" = res$IdSite != 13,
    "age >= 15 years (idtrancheage >= 3)" = res$idtrancheage >= 3,
    "result coding" = !(res$Resultat %in% c("O", "N") & !res$molecule %in% enzymes) |
      (res$Resultat %in% c("S", "I", "R") & res$molecule %in% enzymes)
  )
  keep = rep(TRUE, nrow(res))
  message("[ADDED] rows in the resistance file: ", nrow(res))
  for (nm in names(waterfall)) {
    keep = keep & (waterfall[[nm]] %in% TRUE)
    message("[ADDED] rows remaining after ", nm, ": ", sum(keep))
    out = paste0(out, "\n[ADDED] rows remaining after ", nm, ": ", sum(keep))
  }
  if (sum(keep) == 0) {
    if (!any(res$code %in% cohort2022)) {
      message("[ADDED] The hospital cohort matches NO hospital in the resistance file:\n",
              "  cohort2022            : ", length(cohort2022), " codes, class ", class(cohort2022)[1],
              ", first values: ", paste(head(cohort2022, 8), collapse = ", "), "\n",
              "  resistance file `code`: ", length(unique(res$code)), " codes, class ", class(res$code)[1],
              ", first values: ", paste(head(sort(unique(res$code)), 8), collapse = ", "))
    }
    stop("No rows are left after the filters above - the last [ADDED] line with a ",
         "non-zero count shows the filter that removes everything.", call. = FALSE)
  }
  
  # Filtering based on selected hospitals/departments, patient age, site, and
  # correct coding of results variable------------------------------------------
  res = res %>%
    # Filter out Pediatry, Psychiatry and SSL
    filter(secteur %in% c("Chirurgie", "Gynécologie-Obstétrique", "Médecine", "Réanimation", "SSR")) %>%
    # Filter out excluded facilities
    filter(code %in% cohort2022) %>%
    # Filter out samples isolated in new borns
    filter(IdSite != 13) %>%
    # Filter out samples from patients under 15 years old
    filter(idtrancheage >= 3) %>%
    # Filter out test results/phenotypes that are incorrectly coded
    filter(
      !(Resultat %in% c("O", "N") & !molecule %in% enzymes) |
        (Resultat %in% c("S", "I", "R") & molecule %in% enzymes)
    ) %>%
    # Remove unnecessary columns
    # that were previously checked for potential coding issues
    select(-c(IdArchiveLaboratoire, IdPeriode, IdBacterie, IdMolecule, nosocomial, suppr_dedoublonnage2,
              iduf, idactivite, code_de, code_ta, IdetablissementLaboratoire)) %>%
    mutate(
      age_cat = recode(idtrancheage, !!!dict_id_age),
      molecule_class = recode(molecule, !!!dict_molecule_class),
      secteur = recode(secteur, !!!dict_secteur_spares),
      site = recode(IdSite, !!!dict_id_site),
      Date_year = y,
      bacterie = case_when(
        bacterie == "Enterobacter cloacae co" ~ "Enterobacter cloacae complex",
        bacterie == "Enterobacter cloacae" ~ "Enterobacter cloacae complex",
        bacterie == "Enterobacter asburiae" ~ "Enterobacter cloacae complex",
        bacterie == "Enterobacter hormaechei" ~ "Enterobacter cloacae complex",
        bacterie == "Enterobacter kobei" ~ "Enterobacter cloacae complex",
        bacterie == "Enterobacter ludwigii" ~ "Enterobacter cloacae complex",
        bacterie == "Enterobacter nimipressuralis" ~ "Enterobacter cloacae complex",
        .default = bacterie)
    ) %>%
    select(code, site, prelevement, ligne, Resultat, Num_Patient,
           age_cat, bacterie, molecule, molecule_class, secteur, Date_year) %>%
    distinct()
  
  # Get basic information-------------------------------------------------------
  molecules = sort(unique(res$molecule))
  sites = sort(unique(res$site))
  bacteria = sort(unique(res$bacterie))
  nlines = res %>% select(code, site, secteur, prelevement, ligne, Num_Patient) %>% distinct() %>% nrow(.)
  npatients = res %>% select(code, Num_Patient) %>% distinct() %>% nrow(.)
  
  # Filter out samples that are not from bacteria of interest-------------------
  res = res %>%
    filter(bacterie %in% bacteria_of_interest)
  
  message("[ADDED] rows remaining after keeping bacteria_of_interest: ", nrow(res))
  if (nrow(res) == 0) stop("No rows left after keeping `bacteria_of_interest`.\n  bacteria_of_interest: ",
                           paste(bacteria_of_interest, collapse = " | "), "\n  species in the data at this point: ",
                           paste(bacteria, collapse = " | "), call. = FALSE)
  
  out = paste0(out, "\n[ADDED] BLSE-positive isolates BEFORE any deduplication: ", count_blse_pos(res))
  
  # Get number of samples per bacteria species stratified by whether they were
  # tested for the antibiotic of interest---------------------------------------
  numbers_tested = left_join(
    res %>%
      distinct(code, site, prelevement, ligne, Num_Patient, age_cat, bacterie, Date_year) %>%
      count(Date_year, bacterie) %>%
      rename(all_samples = n),
    res %>%
      filter(
        (bacterie %in% c("Escherichia coli", "Klebsiella pneumoniae", "Enterobacter cloacae complex") & molecule == "BLSE") |
          (bacterie == "Pseudomonas aeruginosa" & molecule %in% c("Imipénème", "Méropénème")) |
          (bacterie == "Staphylococcus aureus" & molecule == "Oxacilline")
      ) %>%
      distinct(code, site, prelevement, ligne, Num_Patient, age_cat, bacterie, Date_year) %>%
      count(Date_year, bacterie) %>%
      rename(tested_samples = n),
    by = c("Date_year", "bacterie")
  )
  
  # Excluded samples
  res %>%
    group_by(code, site, prelevement, ligne, Num_Patient, age_cat, bacterie, secteur, Date_year) %>%
    mutate(to_keep = case_when(
      all(bacterie %in% c("Escherichia coli", "Klebsiella pneumoniae", "Enterobacter cloacae complex")) & any(molecule == "BLSE") ~ 0,
      all(bacterie == "Pseudomonas aeruginosa") & any(molecule %in% c("Imipénème", "Méropénème")) ~ 0,
      all(bacterie == "Staphylococcus aureus") & any(molecule == "Oxacilline") ~ 0,
      .default = 1
    )) %>%
    ungroup() %>%
    filter(to_keep == 1) %>%
    select(-to_keep) %>%
    add_dates() %>%
    write.table(., paste0("Datasets/SPARES/combined/not_tested_cohort2022_detailed_", y, ".txt"),
                sep = "\t", quote = F, row.names = F)
  
  # All samples
  res %>%
    add_dates() %>%
    write.table(., paste0("Datasets/SPARES/combined/cohort2022_detailed_", y, ".txt"),
                sep = "\t", quote = F, row.names = F)
  
  ##############################################################################
  # Get numbers-----------------------------------------------------------------
  ##############################################################################
  # 1. Samples that are not tested for the antibiotics used to define resistances
  n_samples_bacteria_of_interest = res %>%
    select(code, site, prelevement, Num_Patient, bacterie, secteur, ligne) %>%
    distinct() %>%
    nrow()
  
  n_samples_bacteria_of_interest_included = res %>%
    filter(
      (bacterie %in% bacteria_of_interest[1:3] & molecule %in% "BLSE") |
        (bacterie == bacteria_of_interest[4] & molecule %in% "Oxacilline") |
        (bacterie == bacteria_of_interest[5] & molecule %in% c("Imipénème", "Méropénème")) |
        (bacterie == bacteria_of_interest[6] & molecule %in% c("Imipénème", "Méropénème")) |
        (bacterie %in% bacteria_of_interest[7:8] & molecule %in% "Vancomycine")
    ) %>%
    select(code, site, prelevement, Num_Patient, bacterie, secteur, ligne) %>%
    distinct() %>%
    nrow()
  
  out = paste0(out,
               "\nNumber of samples of bacteria of interest: ",
               n_samples_bacteria_of_interest,
               "\nNumber of samples of bacteria of interest excluded because they were not tested for specific antibiotics: ",
               n_samples_bacteria_of_interest - n_samples_bacteria_of_interest_included
  )
  
  # 2. Phenotype of bacteria that are not tested for the antibiotics used to
  # define resistance-----------------------------------------------------------
  res %>%
    group_by(code, site, prelevement, Num_Patient, bacterie, secteur, ligne) %>%
    summarise(atb_test = sum(
      (bacterie %in% bacteria_of_interest[1:3] & molecule %in% "BLSE") |
        (bacterie == bacteria_of_interest[4] & molecule %in% "Oxacilline") |
        (bacterie == bacteria_of_interest[5] & molecule %in% c("Imipénème", "Méropénème")) |
        (bacterie == bacteria_of_interest[6] & molecule %in% c("Imipénème", "Méropénème")) |
        (bacterie %in% bacteria_of_interest[7:8] & molecule %in% "Vancomycine")
    )
    , .groups = "drop") %>%
    group_by(bacterie) %>%
    summarise(n = n(),
              n_not_tested = sum(atb_test == 0),
              p = sum(atb_test == 0) / n() *100,
              .groups = "drop") %>%
    mutate(Date_year = y) %>%
    write.table(., paste0("Datasets/SPARES/combined/not_tested_cohort2022_", y, ".txt"),
                sep = "\t", quote = F, row.names = F)
  
  
  res %>%
    filter(bacterie %in% c("Escherichia coli", "Enterobacter cloacae complex")) %>%
    group_by(code, site, prelevement, Num_Patient, bacterie, secteur, ligne) %>%
    summarise(
      blse_tested = ifelse("BLSE" %in% molecule, "tested", "not tested"),
      blse_production = ifelse(any(molecule %in% "BLSE" & Resultat == "O"), 1, 0),
      n_beta_lactamins = sum(molecule_class %in% c("Broad spectrum Penicillins", "Carbapenems", "Cephalosporins", "Narrow spectrum Penicillins")),
      n_beta_res = sum(molecule_class %in% c("Broad spectrum Penicillins", "Carbapenems", "Cephalosporins", "Narrow spectrum Penicillins") & Resultat == "R"),
      n_beta_int = sum(molecule_class %in% c("Broad spectrum Penicillins", "Carbapenems", "Cephalosporins", "Narrow spectrum Penicillins") & Resultat == "I"),
      n_beta_s = sum(molecule_class %in% c("Broad spectrum Penicillins", "Carbapenems", "Cephalosporins", "Narrow spectrum Penicillins") & Resultat == "S"),
      .groups = "drop"
    ) %>%
    filter(blse_tested == "not tested") %>%
    group_by(bacterie) %>%
    summarise(
      n_r = sum(n_beta_lactamins == n_beta_res),
      n_s = sum(n_beta_lactamins == n_beta_s),
      n_int = sum(n_beta_lactamins != n_beta_s & n_beta_lactamins != n_beta_res),
      .groups = "drop"
    ) %>%
    mutate(Date_year = y) %>%
    write.table(., paste0("Datasets/SPARES/combined/enterobacter_c3g_cohort2022_", y, ".txt"),
                sep = "\t", quote = F, row.names = F)
  
  # 3. Samples that are I among the bacteria of interest and the antibiotics used
  # to define resistance--------------------------------------------------------
  res %>%
    filter(
      (bacterie %in% bacteria_of_interest[1:3] & molecule %in% "BLSE") |
        (bacterie == bacteria_of_interest[4] & molecule %in% "Oxacilline") |
        (bacterie == bacteria_of_interest[5] & molecule %in% c("Imipénème", "Méropénème")) |
        (bacterie == bacteria_of_interest[6] & molecule %in% c("Imipénème", "Méropénème")) |
        (bacterie %in% bacteria_of_interest[7:8] & molecule %in% "Vancomycine")
    ) %>%
    group_by(code, site, prelevement, Num_Patient, bacterie, secteur, ligne, molecule) %>%
    summarise(phenotype = case_when(
      "R" %in% Resultat || "O" %in% "Resultat" ~ "R",
      "I" %in% Resultat ~ "I",
      .default = "S"
    ), .groups = "drop") %>%
    group_by(bacterie, molecule) %>%
    summarise(
      n_tot = n(),
      n_res = sum(phenotype == "R"),
      n_int = sum(phenotype %in% "I"),
      p_int_by_tot = sum(phenotype %in% "I") / n() * 100,
      p_int_by_nots = sum(phenotype %in% "I") / sum(!phenotype %in% "S") * 100,
      n_s = sum(phenotype %in% "S"),
      .groups = "drop"
    ) %>%
    mutate(Date_year = y) %>%
    write.table(., paste0("Datasets/SPARES/combined/phenotype_int_cohort2022_", y, ".txt"),
                sep = "\t", quote = F, row.names = F)
  
  # 4. Verify that there is only one phenotype per sample defined at the "ligne"
  # level ----------------------------------------------------------------------
  ligne_level = res %>%
    group_by(code, site, prelevement, ligne, Num_Patient, age_cat, bacterie, secteur, Date_year, molecule) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1)
  
  if(nrow(ligne_level) > 0) stop("Some isolates defined at the ligne level have multiple phenotypes for an antibiotic")
  
  # 5. Samples that are isolated in the same individual, the same day, at the same
  # site for the same bacteria but with different sample IDs--------------------
  ids_duplicates = res %>%
    select(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur, ligne) %>%
    distinct() %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur) %>%
    mutate(n = n()) %>%
    filter(n>1) %>%
    ungroup() %>%
    select(-n)
  
  out = paste0(out,
               "\nNumber of potential duplicates: ",
               ids_duplicates %>% select(-ligne) %>% distinct() %>% nrow(.)
  )
  
  exact_duplicates = res %>%
    inner_join(., ids_duplicates, by = c("code", "site", "prelevement", "ligne",
                                         "Num_Patient", "age_cat", "bacterie", "secteur")) %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur) %>%
    mutate(n_samples = length(unique(ligne))) %>%
    ungroup() %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur, molecule) %>%
    mutate(n = n(), n_diff = length(unique(Resultat))) %>%
    ungroup() %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur) %>%
    mutate(
      uniqueness = case_when(
        all(n > 1) & length(unique(n)) == 1 & all(n_diff == 1) ~ 1, # Exact duplicates
        any(n < n_samples) & all(n_diff == 1) ~ 2, # More antibiotics tested, otherwise identical phenotypes
        .default = 3 # At least one phenotype that is different
      )) %>%
    ungroup()
  
  duplicates_to_remove = exact_duplicates %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur) %>%
    mutate(m = min(ligne)) %>%
    filter(ligne != m) %>%
    ungroup() %>%
    select(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur, ligne) %>%
    distinct()
  
  rm(ids_duplicates)
  
  # 6. Samples that are part of a 30-day sequence-------------------------------
  samples_30days = res %>%
    anti_join(., duplicates_to_remove,
              by = c("code", "site", "prelevement", "Num_Patient", "age_cat", "bacterie", "secteur", "ligne")) %>%
    select(code, site, Num_Patient, age_cat, bacterie, secteur, ligne, prelevement) %>%
    distinct() %>%
    mutate(prelevement = as.Date(prelevement, "%d/%m/%Y")) %>%
    arrange(code, site, Num_Patient, age_cat, bacterie, secteur, prelevement) %>%
    group_by(code, site, Num_Patient, age_cat, bacterie, secteur) %>%
    mutate(n = n(), prelevement_lag = lag(prelevement)) %>%
    ungroup() %>%
    filter(n > 1) %>%
    mutate(time_diff = ifelse(
      is.na(prelevement_lag),
      0,
      difftime(as.Date(prelevement, "%d/%m/%Y"), as.Date(prelevement_lag, "%d/%m/%Y"), units = "day")
    )) %>%
    filter(!is.na(prelevement), time_diff <= 30) %>%
    group_by(code, site, Num_Patient, age_cat, bacterie, secteur) %>%
    mutate(n2 = n()) %>%
    ungroup() %>%
    filter(n2 > 1)
  
  out = paste0(out,
               "\nNumber of sequences of samples separated by less than 30 days: ",
               samples_30days %>%
                 select(code, site, Num_Patient, age_cat, bacterie, secteur) %>%
                 distinct() %>%
                 nrow(.)
  )
  
  redundant_profiles = samples_30days %>%
    select(-c(n, prelevement_lag, time_diff, n2)) %>%
    left_join(., res %>% mutate(prelevement = as.Date(prelevement, "%d/%m/%Y")),
              by = c("code", "site", "Num_Patient", "age_cat", "bacterie", "secteur", "ligne", "prelevement")) %>%
    group_by(code, site, Num_Patient, age_cat, bacterie, secteur) %>%
    nest() %>%
    mutate(selected = map(data, selection_30days_sequence)) %>%
    select(-data) %>%
    unnest(cols = selected) %>%
    ungroup()
  
  rm(samples_30days)
  
  ##############################################################################
  # Samples and phenotypes selection--------------------------------------------
  ##############################################################################
  # 1. Keep samples with lowest "ligne" ID among exact duplicates---------------
  exact_duplicates_to_remove = exact_duplicates %>%
    filter(uniqueness == 1) %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur) %>%
    mutate(m = min(ligne)) %>%
    filter(ligne != m) %>%
    ungroup() %>%
    select(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur, ligne) %>%
    distinct()
  
  res = res %>%
    anti_join(., exact_duplicates_to_remove,
              by = c("code", "site", "prelevement", "Num_Patient", "age_cat", "bacterie", "secteur", "ligne"))
  
  out = paste0(out,
               "\nNumber of exact duplicates: ",
               exact_duplicates %>%
                 filter(uniqueness == 1) %>%
                 select(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur, ligne) %>%
                 distinct() %>%
                 nrow(),
               "\nNumber of samples from exact duplicates that were removed: ",
               nrow(exact_duplicates_to_remove)
  )
  
  # 2. Merge samples with same phenotype but different antibiotics tested-------
  uncomplete_duplicates_to_remove = exact_duplicates %>%
    filter(uniqueness == 2) %>%
    select(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur, ligne) %>%
    distinct()
  
  uncomplete_duplicates_merged = exact_duplicates %>%
    filter(uniqueness == 2) %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur) %>%
    mutate(ligne = min(ligne)) %>%
    ungroup() %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, molecule, molecule_class,
             ligne, secteur, Date_year) %>%
    summarise(Resultat = unique(Resultat), .groups = "drop") %>%
    distinct()
  
  res = res %>%
    anti_join(., uncomplete_duplicates_to_remove,
              by = c("code", "site", "prelevement", "Num_Patient", "age_cat", "bacterie", "secteur", "ligne")) %>%
    bind_rows(., uncomplete_duplicates_merged)
  
  out = paste0(out,
               "\nNumber of duplicated samples with same phenotypes but different combination of antibiotics: ",
               nrow(uncomplete_duplicates_to_remove),
               "\nNumber of samples with same phenotypes but different combination of antibiotics tested that were removed: ",
               nrow(uncomplete_duplicates_to_remove) -
                 uncomplete_duplicates_merged %>%
                 select(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur, ligne) %>%
                 distinct() %>%
                 nrow(.)
  )
  
  # 3. Merge samples isolated the same day that have different phenotypes-------
  different_phenotypes = exact_duplicates %>%
    filter(uniqueness == 3) %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur) %>%
    mutate(ligne = min(ligne)) %>%
    ungroup() %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, molecule, molecule_class, ligne, secteur, Date_year) %>%
    mutate( n = length(unique(Resultat))) %>%
    filter(n > 1, molecule == "BLSE" & bacterie %in% bacteria_of_interest[1:3] | molecule %in% c("Imipénème", "Méropénème") & bacterie %in% bacteria_of_interest[5:6] | molecule == "Vancomycine" & bacterie %in% bacteria_of_interest[7:8])
  
  different_duplicates_to_remove = exact_duplicates %>%
    filter(uniqueness == 3) %>%
    select(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur, ligne) %>%
    distinct()
  
  different_duplicates_merged = exact_duplicates %>%
    filter(uniqueness == 3) %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur) %>%
    mutate(ligne = min(ligne)) %>%
    ungroup() %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, molecule, molecule_class,
             ligne, secteur, Date_year) %>%
    summarise(Resultat = case_when(
      any(Resultat %in% "O") ~ "O",
      any(Resultat %in% "R") ~ "R",
      all(Resultat %in% "N") ~ "N",
      all(Resultat %in% c("I", "S")) ~ "S",
      all(Resultat %in% "S") ~ "S"
    ),
    .groups = "drop") %>%
    distinct()
  
  res = res %>%
    anti_join(., different_duplicates_to_remove,
              by = c("code", "site", "prelevement", "Num_Patient", "age_cat", "bacterie", "secteur", "ligne")) %>%
    bind_rows(., different_duplicates_merged)
  
  out = paste0(out,
               "\nNumber of duplicated samples with same antibiotics tested but different phenotypes: ",
               nrow(different_duplicates_to_remove),
               "\nNumber of samples with same antibiotics tested but different phenotypes that were removed: ",
               nrow(different_duplicates_to_remove) -
                 different_duplicates_merged %>%
                 select(code, site, prelevement, Num_Patient, age_cat, bacterie, secteur, ligne) %>%
                 distinct() %>%
                 nrow(.),
               "\nNumber of different phenotypes for same antibiotic in duplicates: ",
               different_phenotypes %>%
                 nrow(.),
               "\nNumber of phenotypes with same antibiotic tested that were removed: ",
               different_phenotypes %>%
                 anti_join(., different_duplicates_merged, by = c("code", "site", "prelevement", "ligne", "Resultat", "Num_Patient", "age_cat", "bacterie", "molecule", "molecule_class", "secteur", "Date_year")) %>%
                 nrow(.)
  )
  
  out = paste0(out, "\n[ADDED] BLSE-positive isolates after duplicate steps 1-3 (exact / incomplete / different phenotypes): ", count_blse_pos(res))
  
  # 4. Remove samples isolated in Chirurgie when already isolated in Médecine
  # on day d, at site s, in patient p, for bacteria b---------------------------
  sector_duplicates = res %>%
    select(code, site, prelevement, Num_Patient, age_cat, bacterie, Date_year, secteur) %>%
    distinct() %>%
    group_by(code, site, prelevement, Num_Patient, age_cat, bacterie, Date_year) %>%
    summarise(n = n(),
              n_chirurgie = sum(secteur %in% "Surgery"),
              n_medecine = sum(secteur %in% "Medicine"),
              n_reanimation = sum(secteur %in% "ICU"),
              n_gynecologie = sum(secteur %in% "Obstetrics and gynaecology"),
              n_ssr = sum(secteur %in% "Rehabilitation care"),
              .groups = "drop") %>%
    filter(n > 1)
  
  if (any(sector_duplicates$n_chirurgie > 1) |
      any(sector_duplicates$n_medecine > 1) |
      any(sector_duplicates$n_reanimation > 1) |
      any(sector_duplicates$n_gynecologie > 1) |
      any(sector_duplicates$n_ssr > 1))
    stop("Deduplication steps did not work properly")
  
  out = paste0(out,
               "\nNumber of isolates on the same day in two different sectors: ",
               sum(sector_duplicates$n)
  )
  
  res = anti_join(
    res,
    sector_duplicates %>%
      mutate(secteur = case_when(
        n_reanimation > 0                                         ~ "ICU",
        
        n_chirurgie > 0 & n_reanimation == 0                      ~ "Surgery",
        
        n_gynecologie > 0 & n_reanimation == 0 & n_chirurgie == 0 ~ "Obstetrics and gynaecology",
        
        .default = "Medicine"
      )) %>%
      select(-c(n, n_chirurgie, n_medecine, n_reanimation, n_gynecologie)),
    by = c("code", "site", "prelevement", "Num_Patient", "age_cat", "bacterie", "Date_year", "secteur")
  )
  
  out = paste0(out, "\n[ADDED] BLSE-positive isolates after sector-duplicate step: ", count_blse_pos(res))
  
  # 5. Remove the most recent samples when there are less than 2 phenotypic
  # variations and carried out in the same facility
  out = paste0(out,
               "\nNumber of isolates that are removed due to similar phenotypic profile: ",
               nrow(redundant_profiles)
  )
  
  res = res %>%
    mutate(prelevement = as.Date(prelevement, "%d/%m/%Y")) %>%
    anti_join(., redundant_profiles, by = c("code", "site", "Num_Patient", "age_cat", "bacterie",
                                            "secteur", "ligne", "prelevement"))
  
  out = paste0(out, "\n[ADDED] BLSE-positive isolates after 30-day redundant-profile step: ", count_blse_pos(res))
  
  # 6. Remove samples from the first week of 2019 and the last week from 2021---
  res = res %>%
    mutate(prelevement = as.Date(prelevement, "%d/%m/%Y")) %>%
    mutate(
      Date_week = as.Date(cut(prelevement, "week")),
      Date_month = as.Date(cut(prelevement, "month"))
    )
  
  out = paste0(out,
               "\nNumber of isolates in the 1st week and last week of the study period: ",
               res %>%
                 filter(Date_week %in% c("2018-12-31", "2022-12-26")) %>%
                 select(code, site, prelevement, ligne, Num_Patient, age_cat, bacterie, secteur) %>%
                 distinct() %>%
                 nrow()
  )
  
  res = res %>%
    filter(!Date_week %in% c("2018-12-31", "2022-12-26"))
  
  out = paste0(out, "\n[ADDED] BLSE-positive isolates after first/last-week removal: ", count_blse_pos(res))
  
  ##############################################################################
  # Save data-------------------------------------------------------------------
  ##############################################################################
  # Save data-------------------------------------------------------------------
  res = res %>%
    rename(Date_day = prelevement, atb_class = molecule_class) %>%
    filter(
      (bacterie %in% bacteria_of_interest[1:3] & molecule %in% "BLSE") |
        (bacterie == bacteria_of_interest[4] & molecule %in% "Oxacilline") |
        (bacterie == bacteria_of_interest[5] & molecule %in% c("Imipénème", "Méropénème")) |
        (bacterie == bacteria_of_interest[6] & molecule %in% c("Imipénème", "Méropénème")) |
        (bacterie %in% bacteria_of_interest[7:8] & molecule %in% "Vancomycine")
    )
  
  res %>%
    write.table(., paste0("Datasets/SPARES/combined/resistance_cohort2022_", y, ".txt"),
                sep = "\t", quote = F, row.names = F)
  
  # Save final number of samples------------------------------------------------
  numbers_tested = left_join(
    numbers_tested,
    res %>%
      distinct(code, site, Date_day, ligne, Num_Patient, age_cat, bacterie, Date_year) %>%
      count(Date_year, bacterie) %>%
      rename(final_samples = n),
    by = c("Date_year", "bacterie")
  )
  write.table(numbers_tested, paste0("Datasets/SPARES/combined/numbers_samples_", y, ".txt"),
              sep = "\t", quote = F, row.names = F)
  
  # Get updated basic information-----------------------------------------------
  new_nlines = res %>% select(code, site, secteur, Date_day, ligne, Num_Patient) %>% distinct() %>% nrow(.)
  new_npatients = res %>% select(code, Num_Patient) %>% distinct() %>% nrow(.)
  out = paste0(out,
          "\nFinal number of isolates: ",
          res %>%
            select(-c(molecule, atb_class, Resultat)) %>%
            distinct() %>%
            nrow(.),
          "\nFinal number of isolates in ICUs : ",
          res %>%
            select(-c(molecule, atb_class, Resultat)) %>%
            filter(secteur == "Réanimation") %>%
            distinct() %>%
            nrow(.)
          )
  writeLines(out, paste0("Datasets/SPARES/combined/sample_selection_", y, ".txt"))

}

# ADDED: print the log and the sample counts for 2022
cat(readLines("Datasets/SPARES/combined/sample_selection_2022.txt"), sep = "\n")
print(read.table("Datasets/SPARES/combined/numbers_samples_2022.txt", header = TRUE, sep = "\t"))


##################################################
## SUBSET ESBL-POSITIVE ISOLATES FROM THE 2022
## CLEANED SAMPLE SELECTION AND EXPORT
##################################################
library(tidyverse)

# ---- Paths: edit if yours differ -------------------------------------------------
path_selected = "./Datasets/SPARES/combined/resistance_cohort2022_2022.txt"
path_out      = "./Datasets/SPARES/esbl_positives_2022.csv"

res = read.table(path_selected, header = TRUE, sep = "\t")

# ESBL-positive = BLSE test, result "O" (Oui). Since resistance_cohort2022_2022.txt
# is already restricted to bacteria_of_interest with their one relevant molecule each,
# molecule == "BLSE" only exists for E. coli, K. pneumoniae and E. cloacae complex -
# no extra species filter needed.
esbl_positive = res %>%
  filter(molecule == "BLSE", Resultat == "O")

cat("ESBL-positive isolates:", nrow(esbl_positive), "of", sum(res$molecule == "BLSE"), "BLSE-tested\n")
print(table(esbl_positive$bacterie))

dir.create(dirname(path_out), showWarnings = FALSE, recursive = TRUE)
write.csv(esbl_positive, path_out, row.names = FALSE)
saveRDS(esbl_positive, "./Datasets/SPARES/esbl_positives_2022.RDS")


