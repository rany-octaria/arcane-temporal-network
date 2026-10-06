##################################################
## SELECTION OF BACTERIAL SAMPLES FOR THE
## FIVE SPECIES OF INTEREST  --  2022 ONLY
## Simplified, sequential version
##################################################
# CHANGES vs the previous version of this script:
#  1. NO sector restriction - every secteur value in your data is now in
#     scope (Chirurgie, Gynécologie-Obstétrique, Médecine, Réanimation,
#     SSR, Pédiatrie, Psychiatrie, SLD, or anything else present).
#  2. NO age restriction - idtrancheage >= 3 (15y+) is removed. IdSite != 13
#     (newborn-site exclusion) is ALSO removed, since it's an age-based
#     exclusion in effect - flag if you want it kept for a different reason.
#  3. NO loop - this runs 2022 directly, top to bottom. foreach, doParallel
#     and gt are no longer loaded; they weren't doing anything once the
#     loop was down to one year.
#  4. selection_30days_sequence() is GONE - it kept erroring as "object
#     not found" because it isn't in your helper_functions.R, so this
#     script no longer sources that file or needs it at all. The 30-day
#     rule (drop a sample if it falls within 30 days of the PRECEDING
#     sample from the same patient/organism/sector, i.e. keep the
#     earliest of each run) is now a few vectorised dplyr steps instead of
#     a function called once per group via nest()/map() - same rule,
#     should also run much faster at your scale.
#  5. The sector-duplicate step (same isolate reported under two sectors
#     the same day) now uses an explicit priority RANKING across every
#     sector instead of three hardcoded special cases with "Medicine" as
#     a catch-all - that catch-all could fail to match now that
#     Pédiatrie/Psychiatrie/SLD are in scope too. Priority, highest first:
#     Réanimation, Chirurgie, Gynécologie-Obstétrique, Médecine, Pédiatrie,
#     SSR, Psychiatrie, SLD. Edit PRIORITY below if you want a different
#     order, or if another secteur value exists in your data that isn't
#     in this list (it will rank last, after SLD, by default).
#  6. Simplified outputs - only the final selected-sample file, the
#     tested-vs-included counts, and the summary log are written. The
#     detailed not_tested / enterobacter_c3g / phenotype_int tables from
#     the original are dropped. Ask if you want any of them back.
#
# Needs: ./Code/SPARES Reconstruct/dictionaries.R (enzymes,
# bacteria_of_interest) only - helper_functions.R is no longer used.
##################################################

#rm(list = ls())
library(tidyverse)

# ---- Paths: edit to where YOUR files are -----------------------------------------
path_cohort = "./Datasets/SPARES/cohort2022.rda"
path_dicts  = "./Code/SPARES Reconstruct/dictionaries.R"
path_res22  = "./Datasets/SPARES/BMR_Covid_souches_2022.RDS"  # .csv ("|" separated), .RDS or .xlsx
dir.create("./Datasets/SPARES/combined", showWarnings = FALSE, recursive = TRUE)

# ---- Helpers: clear error messages for missing files + flexible readers ----------
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

read_res22 = function(path) {
  ext = tolower(tools::file_ext(path))
  if (ext %in% c("csv", "txt")) {
    d = read.table(path, sep = "|", header = TRUE, encoding = "latin1")
  } else {
    d = if (ext == "rds") readRDS(path)
    else if (ext %in% c("xlsx", "xls")) readxl::read_excel(path)
    else stop("Unsupported file type '", ext, "' for ", path, " (use .csv, .RDS or .xlsx)", call. = FALSE)
    d = as.data.frame(d)
    d[] = lapply(d, function(x) {
      if (is.character(x)) { bad = !validUTF8(x); x[bad] = iconv(x[bad], "latin1", "UTF-8") }
      x
    })
  }
  d = as.data.frame(d)
  if ("IdEtablissement" %in% names(d)) names(d)[names(d) == "IdEtablissement"] = "code"
  if ("prelevement" %in% names(d)) {
    p = d$prelevement
    if (inherits(p, c("Date", "POSIXt"))) {
      d$prelevement = format(as.Date(p), "%d/%m/%Y")
    } else if (is.numeric(p)) {
      d$prelevement = format(as.Date(p, origin = "1899-12-30"), "%d/%m/%Y")
    } else if (is.character(p) && any(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", p))) {
      d$prelevement = format(as.Date(substr(p, 1, 10)), "%d/%m/%Y")
    }
    ok = sum(!is.na(as.Date(d$prelevement, "%d/%m/%Y")))
    message("[ADDED] `prelevement` dates readable as dd/mm/yyyy: ", ok, " of ", nrow(d))
    if (ok == 0) stop("None of the `prelevement` values can be read as dd/mm/yyyy dates. First values: ",
                      paste(head(unique(d$prelevement), 5), collapse = ", "), call. = FALSE)
  }
  d
}

check_inputs(c(cohort = path_cohort, dictionaries = path_dicts, resistance_2022 = path_res22),
             hints = c(cohort = "cohort", dictionaries = "dictionar", resistance_2022 = "souches"))

loaded_objects = load(path_cohort)
if (!"cohort2022" %in% loaded_objects) {
  stop("The cohort file does not contain an object called `cohort2022` (it contains: ",
       paste(loaded_objects, collapse = ", "), "). Rerun 0_hospital_sample_selection_2022.R.", call. = FALSE)
}
if (length(cohort2022) == 0) {
  stop("`cohort2022` is empty. Rerun 0_hospital_sample_selection_2022.R and check its ",
       "'excluded by ...' counts.", call. = FALSE)
}
message("[ADDED] cohort2022: ", length(cohort2022), " hospitals")

source(path_dicts)
needed_objects = c("enzymes", "bacteria_of_interest")
if (!all(sapply(needed_objects, exists))) {
  stop("dictionaries.R does not define: ", paste(needed_objects[!sapply(needed_objects, exists)], collapse = ", "),
       call. = FALSE)
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

##################################################
# Process resistance data - 2022 only, sequential (no loop)
##################################################
out = paste0("###############################################################\n",
             "Resistance data - 2022\n",
             "###############################################################\n")

res = read_res22(path_res22)
message("[ADDED] rows in the resistance file: ", nrow(res))

n_isolates = res %>%
  select(-c(IdMolecule, molecule, nosocomial, Resultat, IdArchiveLaboratoire)) %>%
  distinct() %>%
  nrow()
out = paste0(out, "Number of isolates: ", n_isolates)

##################################################
# Filtering: hospital cohort + result-coding ONLY
# (no sector, newborn-site, or age restriction - see header)
##################################################

enzymes = "BLSE"
res = res %>%
  filter(code %in% cohort2022) %>%  #only keep the hospitals we didnt exclude
  filter(
    !(Resultat %in% c("O", "N") & !molecule %in% enzymes) |
      (Resultat %in% c("S", "I", "R") & molecule %in% enzymes)
  ) %>%
  select(-c(IdArchiveLaboratoire, IdPeriode, IdBacterie, IdMolecule, nosocomial, suppr_dedoublonnage2,
            iduf, idactivite, code_de, code_ta, IdetablissementLaboratoire)) %>%
  mutate(
    # age_cat, site, molecule_class: dict_id_age / dict_id_site /
    # dict_molecule_class are not available - identity pass-throughs of
    # idtrancheage / IdSite / molecule instead of recoded labels
    age_cat = idtrancheage,
    molecule_class = molecule,
    site = IdSite,
    Date_year = 2022,
    bacterie = case_when(
      bacterie %in% c("Enterobacter cloacae co", "Enterobacter cloacae", "Enterobacter asburiae",
                      "Enterobacter hormaechei", "Enterobacter kobei", "Enterobacter ludwigii",
                      "Enterobacter nimipressuralis") ~ "Enterobacter cloacae complex",
      .default = bacterie
    )
  ) %>%
  select(code, site, prelevement, ligne, Resultat, Num_Patient,
         age_cat, bacterie, molecule, molecule_class, secteur, Date_year) %>%
  distinct()

message("[ADDED] rows remaining after cohort + result-coding filters: ", nrow(res))
if (nrow(res) == 0) stop("No rows left after the hospital cohort / result-coding filters.", call. = FALSE)

res = res %>% filter(bacterie %in% bacteria_of_interest)
message("[ADDED] rows remaining after keeping bacteria_of_interest: ", nrow(res))
if (nrow(res) == 0) {
  stop("No rows left after keeping `bacteria_of_interest`.\n  bacteria_of_interest: ",
       paste(bacteria_of_interest, collapse = " | "), call. = FALSE)
}

out = paste0(out, "\n[ADDED] BLSE-positive isolates BEFORE any deduplication: ", count_blse_pos(res))

##################################################
# Samples tested vs. included, by species (kept, unchanged logic)
##################################################
numbers_tested = left_join(
  res %>%
    distinct(code, site, prelevement, ligne, Num_Patient, age_cat, bacterie, Date_year) %>%
    count(Date_year, bacterie) %>%
    rename(all_samples = n),
  res %>%
    filter(
      (bacterie %in% bacteria_of_interest[1:3] & molecule %in% "BLSE")
    ) %>%
    distinct(code, site, prelevement, ligne, Num_Patient, age_cat, bacterie, Date_year) %>%
    count(Date_year, bacterie) %>%
    rename(tested_samples = n),
  by = c("Date_year", "bacterie")
)

##################################################
# Deduplication step 1: exact duplicates / incomplete-panel duplicates /
# conflicting phenotypes (same logic as before, unchanged)
##################################################
dup_key = c("code", "site", "prelevement", "Num_Patient", "age_cat", "bacterie", "secteur")

ids_duplicates = res %>%
  select(all_of(dup_key), ligne) %>%
  distinct() %>%
  group_by(across(all_of(dup_key))) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  ungroup() %>%
  select(-n)

out = paste0(out, "\nNumber of potential duplicates: ",
             ids_duplicates %>% select(-ligne) %>% distinct() %>% nrow())

exact_duplicates = res %>%
  inner_join(ids_duplicates, by = c(dup_key, "ligne")) %>%
  group_by(across(all_of(dup_key))) %>%
  mutate(n_samples = length(unique(ligne))) %>%
  ungroup() %>%
  group_by(across(all_of(dup_key)), molecule) %>%
  mutate(n = n(), n_diff = length(unique(Resultat))) %>%
  ungroup() %>%
  group_by(across(all_of(dup_key))) %>%
  mutate(uniqueness = case_when(
    all(n > 1) & length(unique(n)) == 1 & all(n_diff == 1) ~ 1,  # exact duplicates
    any(n < n_samples) & all(n_diff == 1)                  ~ 2,  # more antibiotics tested, same phenotype
    .default = 3                                                  # at least one different phenotype
  )) %>%
  ungroup()

rm(ids_duplicates)

# 1a. Exact duplicates -> keep the lowest ligne
exact_duplicates_to_remove = exact_duplicates %>%
  filter(uniqueness == 1) %>%
  group_by(across(all_of(dup_key))) %>%
  mutate(m = min(ligne)) %>%
  filter(ligne != m) %>%
  ungroup() %>%
  select(all_of(dup_key), ligne) %>%
  distinct()

res = res %>% anti_join(exact_duplicates_to_remove, by = c(dup_key, "ligne"))

out = paste0(out,
             "\nNumber of exact duplicates: ",
             exact_duplicates %>% filter(uniqueness == 1) %>%
               select(all_of(dup_key), ligne) %>% distinct() %>% nrow(),
             "\nNumber of samples from exact duplicates that were removed: ",
             nrow(exact_duplicates_to_remove)
)

# 1b. Different antibiotics tested, same phenotype -> merge into one row
uncomplete_duplicates_to_remove = exact_duplicates %>%
  filter(uniqueness == 2) %>%
  select(all_of(dup_key), ligne) %>%
  distinct()

uncomplete_duplicates_merged = exact_duplicates %>%
  filter(uniqueness == 2) %>%
  group_by(across(all_of(dup_key))) %>%
  mutate(ligne = min(ligne)) %>%
  ungroup() %>%
  group_by(across(all_of(dup_key)), molecule, molecule_class, ligne, Date_year) %>%
  summarise(Resultat = unique(Resultat), .groups = "drop") %>%
  distinct()

res = res %>%
  anti_join(uncomplete_duplicates_to_remove, by = c(dup_key, "ligne")) %>%
  bind_rows(uncomplete_duplicates_merged)

# 1c. Same antibiotics tested, different phenotype -> resolve (resistant/positive wins)
different_duplicates_to_remove = exact_duplicates %>%
  filter(uniqueness == 3) %>%
  select(all_of(dup_key), ligne) %>%
  distinct()

different_duplicates_merged = exact_duplicates %>%
  filter(uniqueness == 3) %>%
  group_by(across(all_of(dup_key))) %>%
  mutate(ligne = min(ligne)) %>%
  ungroup() %>%
  group_by(across(all_of(dup_key)), molecule, molecule_class, ligne, Date_year) %>%
  summarise(Resultat = case_when(
    any(Resultat == "O") ~ "O",
    any(Resultat == "R") ~ "R",
    all(Resultat == "N") ~ "N",
    .default = "S"
  ), .groups = "drop") %>%
  distinct()

res = res %>%
  anti_join(different_duplicates_to_remove, by = c(dup_key, "ligne")) %>%
  bind_rows(different_duplicates_merged)

rm(exact_duplicates)

out = paste0(out, "\n[ADDED] BLSE-positive isolates after duplicate steps 1-3: ", count_blse_pos(res))

##################################################
# Deduplication step 2: sector duplicates - same isolate reported under
# more than one sector the same day -> keep the highest-priority sector
# only (see header note on PRIORITY)
##################################################
sector_key = c("code", "site", "prelevement", "Num_Patient", "age_cat", "bacterie", "Date_year")

PRIORITY = c("Réanimation" = 1, "Chirurgie" = 2, "Gynécologie-Obstétrique" = 3, "Médecine" = 4,
             "Pédiatrie" = 5, "SSR" = 6, "Psychiatrie" = 7, "SLD" = 8)

sector_winner = res %>%
  distinct(across(all_of(sector_key)), secteur) %>%
  mutate(.rank = ifelse(secteur %in% names(PRIORITY), PRIORITY[secteur], 99)) %>%
  group_by(across(all_of(sector_key))) %>%
  summarise(n_sectors = n_distinct(secteur), .winner = secteur[which.min(.rank)], .groups = "drop")

out = paste0(out, "\nNumber of isolates reported in two sectors the same day: ",
             sum(sector_winner$n_sectors > 1))

res = res %>%
  inner_join(sector_winner %>% select(all_of(sector_key), .winner), by = sector_key) %>%
  filter(secteur == .winner) %>%
  select(-.winner)

out = paste0(out, "\n[ADDED] BLSE-positive isolates after sector-duplicate step: ", count_blse_pos(res))

##################################################
# Deduplication step 3: 30-day sequences - repeat samples from the same
# patient/organism/sector within 30 days of a PRECEDING sample -> keep the
# earliest of each run (see header note on selection_30days_sequence())
##################################################
episode_key = c("code", "site", "Num_Patient", "age_cat", "bacterie", "secteur")

samples_30days = res %>%
  distinct(across(all_of(episode_key)), ligne, prelevement) %>%
  mutate(prelevement = as.Date(prelevement, "%d/%m/%Y")) %>%
  arrange(across(all_of(episode_key)), prelevement) %>%
  group_by(across(all_of(episode_key))) %>%
  mutate(time_diff = as.numeric(prelevement - lag(prelevement))) %>%
  ungroup()

redundant_profiles = samples_30days %>%
  filter(!is.na(time_diff), time_diff <= 30) %>%
  select(all_of(episode_key), ligne, prelevement)

out = paste0(out, "\nNumber of samples within 30 days of a prior sample: ", nrow(redundant_profiles))

res = res %>%
  mutate(prelevement = as.Date(prelevement, "%d/%m/%Y")) %>%
  anti_join(redundant_profiles, by = c(episode_key, "ligne", "prelevement"))

out = paste0(out, "\n[ADDED] BLSE-positive isolates after 30-day redundant-profile step: ", count_blse_pos(res))

##################################################
# Drop the final partial week of 2022
##################################################
res = res %>%
  mutate(Date_week = as.Date(cut(prelevement, "week")))

out = paste0(out, "\nNumber of isolates in the last (partial) week of 2022: ",
             res %>% filter(Date_week == as.Date("2022-12-26")) %>%
               select(all_of(episode_key), ligne) %>% distinct() %>% nrow())

res = res %>% filter(Date_week != as.Date("2022-12-26"))

out = paste0(out, "\n[ADDED] BLSE-positive isolates after first/last-week removal: ", count_blse_pos(res))

##################################################
# Save
##################################################
# ADDED (per instruction): keep ONLY ESBL-positive specimens in the final
# output - E. coli / K. pneumoniae / E. cloacae complex, BLSE-tested,
# Resultat == "O" (positive). This REPLACES the broader "tested for the
# relevant molecule, positive or negative, across all 5 resistance
# mechanisms" filter the original script used - MRSA/CRPA/CRAB/VRE rows
# and ESBL-negative rows are now dropped here, not just ESBL-negative ones.
# numbers_tested above still reflects all 5 mechanisms (tested vs.
# available), since that table is computed before this filter runs.
res = res %>%
  rename(Date_day = prelevement, atb_class = molecule_class) %>%
  filter(
    bacterie %in% c("Escherichia coli", "Klebsiella pneumoniae", "Enterobacter cloacae complex"),
    molecule == "BLSE",
    Resultat == "O"
  )

res %>%
  write.csv("./Datasets/SPARES/combined/resistance_cohort2022.csv",
               row.names = FALSE)
res  %>% 
  saveRDS("./Datasets/SPARES/combined/resistance_cohort2022.RDS")
    
numbers_tested = left_join(
  numbers_tested,
  res %>%
    distinct(code, site, Date_day, ligne, Num_Patient, age_cat, bacterie, Date_year) %>%
    count(Date_year, bacterie) %>%
    rename(final_samples = n),
  by = c("Date_year", "bacterie")
)
write.table(numbers_tested, "./Datasets/SPARES/combined/numbers_samples_2022.txt",
            sep = "\t", quote = FALSE, row.names = FALSE)

out = paste0(out,
             "\nFinal number of isolates: ",
             res %>% select(-c(molecule, atb_class, Resultat)) %>% distinct() %>% nrow(),
             "\nFinal number of isolates in ICUs: ",
             res %>% select(-c(molecule, atb_class, Resultat)) %>%
               filter(secteur == "Réanimation") %>% distinct() %>% nrow()
)

writeLines(out, "./Datasets/SPARES/combined/sample_selection_2022.txt")

cat(out, "\n")
print(numbers_tested)
