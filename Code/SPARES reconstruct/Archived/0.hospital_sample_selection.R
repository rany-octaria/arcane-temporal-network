##############################################
# First selection step to constitute the 
# final cohort of hospitals and ICUs
##############################################
rm(list = ls())
library(tidyverse)
library(ggpubr)
library(readxl)
library(sf)

source("R/helper/dictionaries.R")

##############################################
# Antibiotic consumption data
##############################################
# Load antibiotic consumption data
antibiotic19 = read_excel("data-raw/spares/2019/ATB2019_NAT.xlsx", sheet = "ATB2019")
antibiotic20 = read_excel("data-raw/spares/2020/atb2020_2021.xlsx", sheet = "ATB2020")
antibiotic21 = read_excel("data-raw/spares/2020/atb2020_2021.xlsx", sheet = "ATB2021")
antibiotic22 = read_excel("data-raw/spares/2022/ATB2022.xlsx", sheet = "ATB2022")

# Change column names in antibiotic21 and antibiotic22
colnames(antibiotic22) = colnames(antibiotic21) = colnames(antibiotic20)

# List of hospitals reporting their antibiotic data 
hosp = rbind(
  antibiotic19 %>% dplyr::select(code) %>% distinct() %>% mutate(year = 2019), 
  antibiotic20 %>% dplyr::select(code) %>% distinct() %>% mutate(year = 2020),
  antibiotic21 %>% dplyr::select(code) %>% distinct() %>% mutate(year = 2021),
  antibiotic22 %>% dplyr::select(code) %>% distinct() %>% mutate(year = 2022)
)

# Get location and type of hospital
region_type = rbind(
  antibiotic19 %>% 
    dplyr::select(code) %>% 
    distinct() %>% 
    left_join(., read_excel("data-raw/spares/2019/SPARES_2019_BMRCovid.xlsx", sheet = "ADM"), 
              by = c("code" = "IdEtablissement")) %>%
    rename(type = groupe, region = `Nouvelle-Region`) %>%
    dplyr::select(code, type, region), 
  antibiotic20 %>% dplyr::select(code, region, type) %>% distinct(),
  antibiotic21 %>% dplyr::select(code, region, type) %>% distinct(),
  antibiotic22 %>% dplyr::select(code, region, type) %>% distinct()
) %>%
  mutate(
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
    type = case_when(code == 11014 ~ "CLCC",
                     code == 2013 ~ "MCO",
                     .default = type)
  ) %>%
  filter(!is.na(region)) %>%
  mutate(type = recode(type, !!!dict_hospital_type)) %>%
  distinct()

##############################################
# Antibiotic resistance data
##############################################
# Load antibiotic resistance data
res19 = read.csv("data-raw/spares/2019/Souches_BMRCovid_2019_d1 copie.csv", 
                 sep = "|", encoding="latin1")
res20 = read.csv("data-raw/spares/2020/BMR_Covid_souches_2020.csv",
                 sep = "|", encoding = "latin1")
res21 = read.csv("data-raw/spares/2021/BMR_Covid_souches_2021.csv", 
                 sep = "|", encoding = "latin1")
res22 = read.csv("data-raw/spares/2022/BMR_Covid_souches_2022.csv", 
                 sep = "|", encoding = "latin1")

# Verify that the 4 dataframes have the same column names
identical(colnames(res21), colnames(res20)) 
identical(colnames(res21), colnames(res19)) 
identical(colnames(res21), colnames(res22)) 

# Compare hospitals
res_hosp = rbind(
  res19 %>% dplyr::select(IdEtablissement) %>% distinct() %>% mutate(year = 2019), 
  res20 %>% dplyr::select(IdEtablissement) %>% distinct() %>% mutate(year = 2020),
  res21 %>% dplyr::select(IdEtablissement) %>% distinct() %>% mutate(year = 2021),
  res22 %>% dplyr::select(IdEtablissement) %>% distinct() %>% mutate(year = 2022)
) %>%
  rename(code = IdEtablissement)

##############################################
# Metadata on the number of beds
##############################################
# Load data per sector
nbed19 = read_excel("data-raw/spares/2019/SPARES_2019_BMRCovid.xlsx", sheet = "JH_act") %>%
  mutate(year = 2019, secteur = recode(secteur, !!!dict_secteur_spares)) %>%
  rename(code = idetablissement, nbeds = nb_lits, nbjh = nbJH)

nbed20 = read_excel("data-raw/spares/2020/BMR_Covid_2020.xlsx", sheet = "JH_act") %>%
  mutate(year = 2020, secteur = recode(secteur, !!!dict_secteur_spares)) %>%
  rename(code = idetablissement, nbeds = nb_lits)

nbed21 = read_excel("data-raw/spares/2021/BMR_Covid_2021.xlsx", sheet = "JH_act") %>%
  mutate(year = 2021, secteur = recode(secteur, !!!dict_secteur_spares)) %>%
  rename(code = idetablissement, nbeds = nb_lits)

nbed22 = read_excel("data-raw/spares/2022/BMR_Covid_2022.xlsx", sheet = "JH_act") %>%
  mutate(year = 2022, secteur = recode(secteur, !!!dict_secteur_spares)) %>%
  rename(code = idetablissement, nbeds = nb_lits)

# Create data at the hospital level
nbed_tot = rbind(nbed19, nbed20, nbed21, nbed22) %>%
  filter(!secteur %in% c("Pediatry", "Psychiatry", "LTC")) %>%
  group_by(code, year) %>%
  summarise(nbjh = sum(nbjh), nbeds = sum(nbeds), .groups = "drop") %>%
  mutate(secteur = "Total")

##############################################
# Total number of hospitals reporting in 
# SPARES
##############################################
# All hospitals IDs
all_ids = c(
  res19$IdEtablissement,
  res20$IdEtablissement,
  res21$IdEtablissement,
  res22$IdEtablissement,
  antibiotic19$code, 
  antibiotic20$code,
  antibiotic21$code,
  antibiotic22$code
)
length(unique(all_ids))

# All ICU IDs
all_icu_ids = c(
  res19$IdEtablissement[res19$secteur == "Réanimation"],
  res20$IdEtablissement[res20$secteur == "Réanimation"],
  res21$IdEtablissement[res21$secteur == "Réanimation"],
  res22$IdEtablissement[res22$secteur == "Réanimation"],
  antibiotic19$code[antibiotic19$secteur == "Réanimation"], 
  antibiotic20$code[antibiotic20$secteur == "Réanimation"],
  antibiotic21$code[antibiotic21$secteur == "Réanimation"],
  antibiotic22$code[antibiotic22$secteur == "Réanimation"]
)
length(unique(all_icu_ids))

##############################################
# All finess numbers
##############################################
# SPARES hospital types
all_types = rbind(
  read_excel("data-raw/spares/2019/SPARES_2019_BMRCovid.xlsx", sheet = "ADM") %>% dplyr::select(IdEtablissement, groupe)%>% rename(idetablissement=IdEtablissement),
  read_excel("data-raw/spares/2020/BMR_Covid_2020.xlsx", sheet = "ADM") %>% dplyr::select(idetablissement, groupe),
  read_excel("data-raw/spares/2021/BMR_Covid_2021.xlsx", sheet = "ADM") %>% dplyr::select(idetablissement, groupe),
  read_excel("data-raw/spares/2022/BMR_Covid_2022.xlsx", sheet = "ADM") %>% dplyr::select(idetablissement, groupe)
) %>%
  rename(code = idetablissement, type = groupe) %>%
  # Manual corrections using the national database https://finess.esante.gouv.fr/fininter/jsp/recherche.jsp?mode=simple
  mutate(type = case_when(code == 11014 ~ "CLCC", # physical Finess code: 840000350
                          code == 1913 ~ "MCO", # physical Finess code: 210011847
                          code == 11180 ~ "MCO", # physical Finess code: 420000192
                          .default = type)) %>%
  mutate(type = recode(type, !!!dict_hospital_type)) %>%
  distinct()

# Verify duplicates that do not have the same metadata
# SPARES code 9263 - Grenoble university hospital 
# --> It corresponds to Grenoble Nord and not to the aggregation of all the CHU centers in Grenoble
rbind(nbed19, nbed20, nbed21, nbed22) %>% 
  filter(code == 9263) %>% 
  ggplot(., aes(x = secteur, y = nbjh, fill = factor(year))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(fill = "Year", y = "Number of hospitalization days", x = "", title = "Grenoble Nord")

rbind(nbed19, nbed20, nbed21, nbed22) %>% 
  filter(code == 9263) %>% 
  ggplot(., aes(x = secteur, y = nbeds, fill = factor(year))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(fill = "Year", y = "Number of beds", x = "", title = "Grenoble Nord")

# Verify code 7115 that corresponds to Clermont Ferrand 
# It corresponds to Gabriel Montpied and not to the 
# aggregation of all the CHU centers in Clermont Ferrand
rbind(nbed19, nbed20, nbed21, nbed22) %>% 
  filter(code == 7115) %>% 
  ggplot(., aes(x = secteur, y = nbjh, fill = factor(year))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(fill = "Year", y = "Number of hospitalization days", x = "", title = "Clermont Ferrand - Gabriel Montpied")

rbind(nbed19, nbed20, nbed21, nbed22) %>% 
  filter(code == 7115) %>% 
  ggplot(., aes(x = secteur, y = nbeds, fill = factor(year))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(fill = "Year", y = "Number of beds", x = "", title = "Clermont Ferrand - Gabriel Montpied")

# SPARES finess numbers
all_finess = rbind(
  read_excel("data-raw/spares/2019/SPARES_2019_BMRCovid.xlsx", sheet = "ADM") %>% 
    rename(code = IdEtablissement, name = etablissement, city = ville, type = groupe, region = `Nouvelle-Region`),
  read_excel("data-raw/spares/2020/BMR_Covid_2020.xlsx", sheet = "ADM") %>% 
    rename(code = idetablissement, name = etablissement, city = ville, type = groupe, region = Nouvelle_Region),
  read_excel("data-raw/spares/2021/BMR_Covid_2021.xlsx", sheet = "ADM") %>% 
    rename(code = idetablissement, name = etablissement, city = ville, type = groupe, region = Nouvelle_Region),
  read_excel("data-raw/spares/2022/BMR_Covid_2022.xlsx", sheet = "ADM") %>% 
    rename(code = idetablissement, name = etablissement, city = ville, type = groupe, region = Nouvelle_Region)
) %>% 
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

# Data.gouv.fr finess numbers
# Location data
data_gouv = readLines("data-raw/spares/hospital_location/etalab-cs1100507-stock-20230502-0337.csv")
gps_coor = data_gouv[sapply(data_gouv, grepl, pattern = "geolocalisation")]
gps_coor = data.frame(X = gps_coor) %>%
  tidyr::separate(X, c("section", "finess", "coordX", "coordY", "sourcecoordet", "datemaj"), ";") %>%
  dplyr::select(-c(section, sourcecoordet, datemaj))

# Admin data
admin = data_gouv[sapply(data_gouv, grepl, pattern = "^structureet")]
admin = data.frame(X = admin) %>%
  tidyr::separate(X, c("section", "finess", "finess_jur", "rs", "rslong", "complrs",
                       "compldistrib", "numvoie", "typvoie", "voie", "compvoie", "lieuditbp",
                       "commune", "departement", "libdepartement", "ligneacheminent", "telephone",
                       "telecopie", "categetab", "libcategetab", "categagretab", "libcategagretab",
                       "siret", "codeape", "codemft", "libmft", "codesph", "libsph", "dateouv",
                       "dateautor", "datemaj", "numuai"), ";") %>%
  dplyr::select(finess, finess_jur, rs, rslong, commune, departement, libcategetab) %>%
  rename(finess_juridique = finess_jur)

rm(data_gouv)

##############################################
# Hospitals to exclude 
##############################################
# Total number of unique facilities
rbind(res_hosp, hosp) %>%
  dplyr::select(code) %>%
  distinct() %>%
  nrow(.)

# Total number of unique facilities by year and database
rbind(
  res_hosp %>% mutate(data = "resistance"), 
  hosp %>% mutate(data = "antibiotic")
) %>%
  distinct() %>%
  group_by(year, data) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(data, year)

# Verify that SPARES provides the administrative data for all facilities
# that are present in both datasets for at least one year
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

# Hospitals that did not report their antibiotic consumption or their 
# resistance data for a given year
missing_year_or_database = rbind(
  res_hosp %>% mutate(data = "resistance"), 
  hosp %>% mutate(data = "antibiotic")
) %>%
  distinct() %>%
  group_by(code) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n < 8) %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code
length(missing_year_or_database)

# Hospitals in overseas territories
admin_overseas = admin %>%
  filter(departement %in% c("9A", "9B", "9C", "9D", "9E", "9F")) %>%
  dplyr::select(finess, finess_juridique) %>%
  distinct()

overseas = all_finess %>%
  filter(finess %in% admin_overseas$finess) %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code
length(overseas)

# Hospitals in Corsica
admin_corsica = admin %>%
  filter(departement %in% c("2A", "2B")) %>%
  dplyr::select(finess, finess_juridique) %>%
  distinct()

corsica = all_finess %>%
  filter(finess %in% admin_corsica$finess) %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code
length(corsica)

# Hospitals with finess not in data.gouv.fr 
non_official_finess = all_finess %>%
  filter(!finess %in% admin$finess & !finess %in% admin$finess_juridique) %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code
length(non_official_finess)

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

##############################################
# Selected hospitals
##############################################
# Selected hospitals by year
hosp %>%
  filter(!code %in% c(non_official_finess, overseas, corsica, missing_year_or_database, geographic_entities, psy, clcc)) %>%
  dplyr::select(code, year) %>%
  distinct() %>%
  count(year)

res_hosp %>%
  filter(!code %in% c(non_official_finess, overseas, corsica, missing_year_or_database, geographic_entities, psy, clcc)) %>%
  dplyr::select(code, year) %>%
  distinct() %>%
  count(year)

# Hospitals that are in the 2019-2020-2021-2022 cohort
cohort19202122 = hosp %>%
  filter(!code %in% c(non_official_finess, overseas, corsica, missing_year_or_database, geographic_entities, psy, clcc)) %>% 
  group_by(code) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n == 4) %>%
  .$code
length(cohort19202122)
save(cohort19202122, file="data/cohort19202122.rda")

# Number of university hospitals located in hexagonal France
all_chu_cities = c("AMIENS", "ANGERS", "BESANCON", "BORDEAUX", "BREST", "CAEN", 
                   "CLERMONT FERRAND", "DIJON", "GRENOBLE", "LILLE", "LIMOGES", 
                   "LYON", "MARSEILLE", "METZ", "MONTPELLIER", "NANCY", "NANTES", 
                   "NICE", "NIMES", "ORLEANS", "PARIS", "POITIERS", "REIMS", 
                   "RENNES", "ROUEN", "ST ETIENNE", "STRASBOURG", "TOULOUSE", "TOURS")

cohort_chu_cities = all_finess %>%
  filter(code %in% cohort19202122, type == "University hospital") %>%
  dplyr::select(city) %>%
  distinct() %>%
  arrange(city) %>%
  .$city
length(chu_finess_juridique$city)
sum(chu_finess_juridique$city %in% cohort_chu_cities)
sum(!cohort_chu_cities %in% chu_finess_juridique$city)

# University hospitals that reported under their legal FINESS code 
chu_legal_entity_reporting = all_finess %>% 
  filter(code %in% cohort19202122, type == "University hospital", finess == finess_juridique) %>%
  .$code
chu_legal_entity_reporting = unique(chu_legal_entity_reporting)
save(chu_legal_entity_reporting, file = "data/chu_legal_entity_reporting.rda")

##############################################
# Selected ICUs
##############################################
# Codes of selected ICUs
icu_cohort19202122 = rbind(nbed19, nbed20, nbed21, nbed22) %>%
  filter(secteur == "ICU", code %in% cohort19202122) %>%
  dplyr::select(code) %>%
  distinct() %>%
  .$code

# Verify that the selected ICUs report in the two databases over the three years
res_icus = bind_rows(
  res19 %>% mutate(Date_year = 2019), 
  res20 %>% mutate(Date_year = 2020), 
  res21 %>% mutate(Date_year = 2021),
  res22 %>% mutate(Date_year = 2022)
) %>%
  filter(secteur == "Réanimation") %>%
  dplyr::select(IdEtablissement, secteur, Date_year) %>%
  rename(code = IdEtablissement) %>%
  distinct() %>%
  mutate(data = "resistance")

atb_icus = bind_rows(
  antibiotic19 %>% mutate(Date_year = 2019), 
  antibiotic20 %>% mutate(Date_year = 2020), 
  antibiotic21 %>% mutate(Date_year = 2021),
  antibiotic22 %>% mutate(Date_year = 2022)
) %>%
  filter(secteur == "Réanimation") %>%
  dplyr::select(code, secteur, Date_year) %>%
  distinct() %>%
  mutate(data = "antibiotic")

icu_missing_year_or_database = rbind(res_icus, atb_icus) %>%
  filter(code %in% icu_cohort19202122) %>%
  group_by(code) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n < 8) %>%
  .$code

# Save ICU ids
length(icu_cohort19202122)
length(icu_missing_year_or_database)
icu_cohort19202122 = icu_cohort19202122[!icu_cohort19202122 %in% icu_missing_year_or_database]
length(icu_cohort19202122)
save(icu_cohort19202122, file = "data/icu_cohort19202122.rda")

##############################################
# Metadata 
##############################################
# Save metadata on beds and bed-days
metadata_beds = rbind(nbed19, nbed20, nbed21, nbed22) %>%
  filter(!secteur %in% c("Pediatry", "Psychiatry", "LTC")) %>%
  bind_rows(., nbed_tot) %>%
  filter(code %in% cohort19202122)
save(metadata_beds, file = "data/metadata_beds.rda")

# Save metadata
metadata_admin_unique = all_finess %>%
  filter(code %in% cohort19202122, finess != finess_juridique) %>%
  left_join(., gps_coor, by = "finess") %>%
  left_join(., admin %>% dplyr::select(finess, departement), by = "finess") %>%
  rename(department = departement) %>%
  mutate(department = recode(department, !!!dict_departments))

# Specific case of university hospitals for which the finess == finess_juridique
metadata_admin_jur = read_excel("data-raw/spares/finess_issues/all_chu_finess_annotated.xlsx", 
                                sheet = "Sheet1") %>%
  filter(spares_inclusion == 1) %>%
  left_join(., all_finess %>% filter(code %in% cohort19202122, finess == finess_juridique), by = c("finess_jur" = "finess_juridique")) %>%
  filter(!is.na(code)) %>%
  dplyr::select(code, finess.x, finess_jur, type, name, city, region, departement) %>%
  rename(finess = finess.x, finess_juridique = finess_jur, department = departement) %>%
  mutate(department = recode(department, !!!dict_departments)) %>%
  left_join(., gps_coor, by = "finess")

# Save metadata
metadata_admin = bind_rows(metadata_admin_unique, metadata_admin_jur) %>%
  mutate(icu = ifelse(code %in% icu_cohort19202122, 1, 0))
metadata_admin %>% group_by(finess) %>% mutate(n=n()) %>% filter(n>1)
save(metadata_admin, file = "data/metadata_admin.rda")

# Write file with all finess geographique 
# to paste in SAS code for PMSI
towrite = unique(metadata_admin$finess)
writeLines(text = paste0('"', paste0(towrite, collapse = '","'), '"'), 
           con = "data-raw/atih/finess.txt")

