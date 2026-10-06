##############################################
# First selection step to constitute the
# final cohort of hospitals  --  2022 ONLY
# Adapted from 0_hospital_sample_selection.R
##############################################
# CHANGES vs the original (everything not listed here is the authors' code, unchanged):
#  0. Paths are in one block at the top; missing files are reported clearly BEFORE anything runs; the
#     antibiotic and ADM tables can be .xlsx or .RDS.
#  1. 2022 only: the 2019-2021 files are not read.
#  2. NO continuity requirement. The original dropped (a) hospitals that did not report BOTH
#     antibiotic and resistance data in ALL four years (missing_year_or_database) and (b) kept
#     only hospitals present in all four years (n == 4). Both are removed. As in the original,
#     the starting list is the hospitals in the ANTIBIOTIC-consumption file (2022).
#  2b. The etalab / data.gouv.fr FINESS registry is NOT needed (not available). It was only used to
#     identify overseas, Corsica and "non-official" FINESS hospitals; these are now derived from the
#     FINESS number's department prefix + the SPARES region, and a FINESS format check. See the
#     REPLACED blocks under "Hospitals to exclude" for the (small) difference this can make.
#  3. Output is data/cohort2022.rda (object `cohort2022`), so the authors' own
#     data/cohort19202122.rda is never overwritten.
#  4. Removed because they are not part of choosing hospitals/samples (multi-year, ICU-only, or
#     PMSI/SAS export): ICU cohort, bed/bed-day metadata, GPS + metadata_admin, finess.txt export,
#     exploratory plots.
#  5. Switch `keep_psy_clcc` (default FALSE = exactly as the authors: PSY and CLCC excluded).
#     Set TRUE if you also want psychiatric hospitals and cancer centres kept.
# Needs (as in the original): R/helper/dictionaries.R  (dict_hospital_type only; chu_france is now built here)
##############################################
rm(list = ls())
library(tidyverse)
library(readxl)

# ---- Paths: edit to where YOUR files are (relative to R's working directory = the project root) ----
path_dicts  = "./Code/SPARES Reconstruct/dictionaries.R"
path_atb22  = "./Datasets/SPARES/ATB2022.xlsx"                # sheet "ATB2022"  (.xlsx or .RDS)
path_bmr22  = "./Datasets/SPARES/BMR_Covid_2022_admin.RDS"         # sheet "ADM"      (.xlsx or .RDS)
path_res22  = "./Datasets/SPARES/BMR_Covid_souches_2022.RDS"  # optional: only used for a diagnostic count
path_cohort = "./Datasets/SPARES/cohort2022.rda"                 # output (script 2 loads it from here)

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
  d
}

# Reads a sheet from .xlsx, or a table from .RDS
read_any = function(path, sheet = NULL) {
  ext = tolower(tools::file_ext(path))
  if (ext == "rds") return(as.data.frame(readRDS(path)))
  if (ext %in% c("xlsx", "xls")) return(as.data.frame(readxl::read_excel(path, sheet = sheet)))
  stop("Unsupported file type '", ext, "' for ", path, " (use .xlsx or .RDS)", call. = FALSE)
}

check_inputs(c(dictionaries = path_dicts, antibiotic_2022 = path_atb22, bmr_admin_2022 = path_bmr22),
             hints = c(dictionaries = "dictionar", antibiotic_2022 = "ATB", bmr_admin_2022 = "BMR_Covid"))
source(path_dicts)

keep_psy_clcc = TRUE

##############################################
# Antibiotic consumption data
##############################################
antibiotic22 = read_any(path_atb22, "ATB2022")

# List of hospitals reporting their antibiotic data
hosp = antibiotic22 %>% dplyr::select(code) %>% distinct() %>% mutate(year = 2022)

##############################################
# Antibiotic resistance data
##############################################
# (only used for the diagnostics; the cohort itself does not depend on it)
if (file.exists(path_res22)) {
  res22 = read_res22(path_res22)
  res_hosp = res22 %>% dplyr::select(code) %>% distinct() %>% mutate(year = 2022)
} else {
  warning("Resistance file not found (", path_res22, "): diagnostics on the resistance file are skipped.")
  res_hosp = data.frame(code = numeric(0), year = numeric(0))
}

##############################################
# All finess numbers  (2022 ADM sheet; corrections are the authors', verbatim)
##############################################
all_finess = read_any(path_bmr22, "ADM") %>%
  rename(code = idetablissement, name = etablissement, city = ville, type = groupe, region = Nouvelle_Region) %>%
  mutate(
    finess = ifelse(nchar(finess) < 9, paste0("0", finess), finess),
    finess_juridique = ifelse(nchar(finess_juridique) < 9, paste0("0", finess_juridique), finess_juridique),
    name = gsub("  ", " ", name),
    region = case_when(
      region == "Auvergne - Rhône Alpes" ~ "Auvergne-Rhône-Alpes",
      region == "Bourgogne - Franche Comté" ~ "Bourgogne-Franche-Comté",
      region == "Grand Est" ~ "Grand-Est",                 
      region == "Hauts de France" ~ "Hauts-de-France",
      region == "Ile de France" ~ "Île-de-France",
      region == "Nouvelle Aquitaine" ~ "Nouvelle-Aquitaine",
      region == "Pays de Loire" ~ "Pays de la Loire",
      region == "Provence Alpes Côte d'Azur" ~ "Provence-Alpes-Côte d'Azur",
      region == "Reunion - Mayotte" ~ "La Réunion-Mayotte",
      .default = region  
    ),
    type = case_when(
      code == 11014 ~ "CLCC", # physical Finess code: 840000350
      code == 1913 ~ "MCO", # physical Finess code: 210011847
      code == 11180 ~ "MCO", # physical Finess code: 420000192
      .default = type)
  ) %>%
  mutate(
    # Manual corrections of duplicates
    name = case_when(
      grepl(" \\(fermé\\)", name) ~ gsub(" \\(fermé\\)", "", name),
      name == "CHU GRENOBLE" ~ "CHU GRENOBLE-HOPITAL NORD",
      name == "CENTRE DE REEDUCATION LA LANDE" ~ "SSR LA LANDE",
      name == "CENTRE DE READAPTATION DU CONFLUENT" ~ "CENTRE DE SSR DU CONFLUENT",
      name == "CLINIQUE J.VERNE- POLE HOSP MUTUALISTE" ~ "CLINIQUE MUTUALISTE JULES VERNE",
      name == "CTRE READAPTATION VILLA NOTRE DAME" ~ "SSR VILLA NOTRE DAME",
      name == "CENTRE HOSPITALIER DE MONTLUCON NERIS LES BAINS" ~ "CENTRE HOSPITALIER DE MONTLUCON",
      name == "CLINIQUE CHANTECLER (hacking 2020 : abs conso ATB)" ~ "CLINIQUE CHANTECLER",
      name == "CLINEA CRF DU BESSILLON" ~ "CRF DU BESSILLON", 
      name == "CLINIQUE MALARTIC" ~ "POLYCLINIQUE MALARTIC",
      name == "C.H.I.C. COTE BASQUE - BAYONNE" ~ "CH DE LA COTE BASQUE - BAYONNE",
      name == "EPSYLAN" ~ "CHS BLAIN",
      name == "HOPITAUX DE GRAND COGNAC" ~ "CH INTERCOMMUNAL DU PAYS DE COGNAC",
      name == "CLINIQUE FSEF RENNES BEAULIEU" ~ "CENTRE MEDICAL ET PEDAGOGIQUE BEAULIEU",
      code == 9960 ~ "INSTITUT DE READAPTATION D'ACHERES",
      code == 11330 ~ "CENTRE MEDICAL SANCELLEMOZ",
      code == 2727 ~ "CH DE PRIVAS ARDECHE",
      code == 10650 ~ "CLINIQUE SAINT JEAN SUD DE FRANCE",
      code == 10839 ~ "HOPITAL DU PAYS SALONAIS",
      code == 2406 ~ "HOPITAL DES COLLINES VENDEENNES",
      code == 2009 ~ "HOPITAL ROBERT SCHUMAN DE VANTOUX",
      code == 7115 ~ "CHU G. MONTPIED",
      code == 9709 ~ "CLINIQUE BLAGNAC",
      code == 10406 ~ "CLINIQUE LES HAUTS DE CENON",
      .default = name
    ),
    finess = case_when(
      code == 2406 ~ "850000647",
      code == 12672 ~ "940110042",
      code == 2453 ~ "440059319",
      code == 2009 ~ "570026252",
      code == 7115 ~ "630000404",
      code == 9657 ~ "690781810", 
      code == 9709 ~ "310025010",
      .default = finess
    ),
    finess_juridique = case_when(
      code == 2478 ~ "440041895", 
      code == 10597 ~ "920029527",
      code == 8984 ~ "420784878",
      code == 8836 ~ "350001137",
      code == 9984 ~ "950042994",
      code == 12672 ~ "940110042",
      code == 2453 ~ "440059301",
      code == 2009 ~ "570023630",
      code == 9709 ~ "310025010",
      .default = as.character(finess_juridique)
    ),
    city = case_when(
      code == 2420 ~ "LE LOUROUX BECONNAIS",
      code == 2839 ~ "LA TESTE DE BUCH",
      code == 9960 ~ "ACHERES",
      code == 10806 ~ "CANNES",
      code == 11330 ~ "PASSY",
      code == 12420 ~ "NEVILLE",
      code == 10650 ~ "SAINT JEAN DE VEDAS",
      code == 9984 ~ "ENNERY",
      code == 2009 ~ "VANTOUX",
      code == 9709 ~ "BLAGNAC",
      .default = city
    ),
    type = recode(type, !!!dict_hospital_type)
  ) %>%
  distinct()
# [REMOVED] Data.gouv.fr (etalab) FINESS registry: not available. It was only used for the three
# exclusions below (overseas, Corsica, non_official_finess), which are now derived from the FINESS
# number itself and from the region (see the REPLACED blocks under "Hospitals to exclude").

##############################################
# Hospitals to exclude
##############################################
# Hospitals in the antibiotic file for 2022
nrow(hosp)

# Verify that SPARES provides the administrative data for all facilities
# that are present in both datasets in 2022
verify_spares_admin = rbind(
  res_hosp %>% mutate(data = "resistance"),
  hosp %>% mutate(data = "antibiotic")
) %>%
  group_by(code, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n == 2) %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code

length(verify_spares_admin)
sum(!verify_spares_admin %in% all_finess$code)

# [REMOVED] missing_year_or_database (multi-year reporting requirement)

# REPLACED (etalab registry not available). The authors looked up each hospital's department in
# the national registry. The first two characters of a FINESS number ARE the department code, so the
# same information is taken from the FINESS number, plus the SPARES region as a second check.
# Hospitals in overseas territories (departments 97x / 98x)
overseas = all_finess %>%
  filter(substr(finess, 1, 2) %in% c("97", "98") |
           region %in% c("Guadeloupe", "Martinique", "Guyane", "La Réunion-Mayotte", "Nouvelle Calédonie")) %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code
length(overseas)

# Hospitals in Corsica (departments 2A / 2B; "20" is the pre-1976 code)
corsica = all_finess %>%
  filter(substr(finess, 1, 2) %in% c("2A", "2B", "20") | region == "Corse") %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code
length(corsica)

# REPLACED. The authors excluded hospitals whose finess AND finess_juridique are both absent from the
# national registry. Without the registry, a FINESS is checked only for a valid FORMAT (9 characters:
# 2-character department + 7 digits). Same AND logic: excluded only if both numbers are invalid.
valid_finess = function(x) grepl("^([0-9]{2}|2A|2B)[0-9]{7}$", x)
non_official_finess = all_finess %>%
  filter(!valid_finess(finess) & !valid_finess(finess_juridique)) %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code
length(non_official_finess)

# REPLACED (data-raw/spares/finess_issues/all_chu_finess.xlsx is not available). The authors keep a
# hospital that reports under its LEGAL FINESS (finess == finess_juridique) only if it is a teaching hospital,
# using a list of all CHU legal FINESS numbers. Same rule here, but the list is built from SPARES itself:
# the legal FINESS of every hospital typed as a university hospital in the ADM sheet (so any hospital that
# shares a CHU's legal FINESS is kept too). This overrides any `chu_france` defined in dictionaries.R.
chu_france = data.frame(
  finess_jur = unique(all_finess$finess_juridique[all_finess$type %in% c("University hospital", "CHU")])
)
cat("Legal FINESS numbers of university hospitals (teaching-hospital exception):", nrow(chu_france), "\n")

# Hospitals that are not geographic entities except for teaching hospitals
geographic_entities = all_finess %>%
  filter(finess == finess_juridique, !finess_juridique %in% chu_france$finess_jur) %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code
length(geographic_entities)

# Psychiatric hospitals
psy = all_finess %>%
  filter(type == "PSY") %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code
length(psy)

# CLCC hospitals
clcc = all_finess %>%
  filter(type == "CLCC") %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code
length(clcc)

# ADDED: keep psychiatric hospitals and cancer centres if requested
if (keep_psy_clcc) { psy = integer(0); clcc = integer(0) }

##############################################
# Selected hospitals
##############################################
excluded_by = list(
  non_official_finess = non_official_finess, overseas = overseas, corsica = corsica,
  geographic_entities = geographic_entities, psy = psy, clcc = clcc
)

# Hospitals in the 2022 antibiotic file that are excluded, by criterion (a hospital can be in several)
cat("\nHospitals in the 2022 antibiotic file:", nrow(hosp), "\n")
for (nm in names(excluded_by)) {
  cat(sprintf("  excluded by %-20s: %d\n", nm, sum(hosp$code %in% excluded_by[[nm]])))
}

# ADDED (diagnostic): the same six criteria, counted on the hospitals of the RESISTANCE file and on the ADM
# sheet, plus what the FINESS columns look like. If a criterion removes (almost) every hospital of the
# resistance file, it shows up here.
cat("\nHospitals in the 2022 RESISTANCE file:", nrow(res_hosp),
    "| also in the ADM sheet:", sum(res_hosp$code %in% all_finess$code),
    "| also in the antibiotic file:", sum(res_hosp$code %in% hosp$code), "\n")
for (nm in names(excluded_by)) {
  cat(sprintf("  excluded by %-20s: %d of the resistance-file hospitals, %d of the ADM sheet\n", nm,
              sum(res_hosp$code %in% excluded_by[[nm]]), sum(all_finess$code %in% excluded_by[[nm]])))
}
cat("Valid-format FINESS in the ADM sheet (", nrow(all_finess), " hospitals): finess ",
    sum(valid_finess(all_finess$finess)), " | finess_juridique ", sum(valid_finess(all_finess$finess_juridique)),
    " | finess == finess_juridique: ", sum(all_finess$finess == all_finess$finess_juridique, na.rm = TRUE), "\n", sep = "")
cat("First hospitals of the ADM sheet:\n")
print(as.data.frame(head(all_finess[, c("code", "type", "region", "finess", "finess_juridique")], 6)), row.names = FALSE)
cat("\n")

# Hospitals that are in the 2022 cohort  (original also required n == 4 years and
# reporting in both databases every year: removed)
cohort2022 = hosp %>%
  filter(!code %in% unlist(excluded_by)) %>%
  distinct(code) %>%
  .$code
cat("Hospitals in the 2022 cohort:", length(cohort2022), "\n")
cat("  of which also present in the 2022 resistance file:", sum(cohort2022 %in% res_hosp$code), "\n")

# ADDED: a cohort that contains none of the hospitals of the resistance file would make script 2 return 0 rows,
# so it is not saved.
if (nrow(res_hosp) > 0 && !any(res_hosp$code %in% cohort2022)) {
  stop("The cohort contains NONE of the ", nrow(res_hosp), " hospitals of the resistance file, so it was NOT saved. ",
       "See the 'excluded by ...' counts for the resistance-file hospitals above to find the criterion that removes them.",
       call. = FALSE)
}

dir.create(dirname(path_cohort), showWarnings = FALSE, recursive = TRUE)
save(cohort2022, file = path_cohort)