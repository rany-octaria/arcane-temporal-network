# Modeling Code
# Tinkering with the Modeling of the network over many days
# Rany Octaria
# Le CNAM

#The facility is grouped by finessGeo

# FIRST: MAKE SURE YOU OPEN THIS CODE FROM THE ARCANE-TEMPORAL-NETWORK PROJECT
# SECOND: make sure you download  the transfer datasets here #https://ehespfr-my.sharepoint.com/personal/shrichand_bhuria_ehesp_fr/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fshrichand%5Fbhuria%5Fehesp%5Ffr%2FDocuments%2FHBN%5F2024%5Fdata%5Fsharing&ga=1 

# Download it into the Datasets folder. I can't sync it into GitHub because the files were too large

library(here)
library(purrr)     # map_dfr
library(igraph)
library(patchwork)   # side-by-side layout
library(dplyr)
library(igraph)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(visNetwork)
library(ggiraph)
library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(tibble)
library(scales)
library(purrr)
library(readr)
library(stringr)
# install.packages(c("rnaturalearth", "rnaturalearthdata", "remotes"))
# remotes::install_github("ropensci/rnaturalearthhires")

options(scipen = 999)
# Import the CSV datasets
# Importing from WINDOWS
#monthly = read_csv(here("Datasets", "hbn_direct_transfer_2024","HBN_monthly_sliding_edgelist_2024.csv" ))
weekly = read_csv(here("Datasets", "hbn_direct_transfer_2024","HBN_weekly_sliding_edgelist_2024.csv" ))

#Importing facility databaseIUM_hospital_info_2024 <- read_delim("Datasets/IUM_hospital_info_2024.csv", 
IUM_hospital_info_2024 <- read_delim(here("Datasets","IUM_hospital_info_2024.csv"),
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                     locale = locale(encoding = "Latin1"))
head(IUM_hospital_info_2024)

#Getting the CAPACT dataset for regions and names
capact24 = read_delim(here("Datasets","capact24.csv"),
                      delim = ",", escape_double = FALSE, trim_ws = TRUE,
                      locale = locale(encoding = "Latin1"))

#Maybe I need this too
nblit_ium = IUM_hospital_info_2024 %>% 
  group_by(finessGeo) %>% 
  summarise(nblit = sum(nblit)) %>% 
  rename(finess_geo = finessGeo)

#Importing admission data

daily_admission<- read_delim(here("Datasets" , "hbn_direct_transfer_2024", 
                                  "NO_ADMISSION_DAILY_2024.csv"), 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
head(daily_admission)

#Importing Data No beds
beds_data = read.csv(here("Datasets", "hbn_direct_transfer_2024",
                          "Data_No_Beds_2024_FinessGeo.csv"))
head(beds_data)

#geographical coord
coords = read.csv(here("Datasets", "finessGeo_data.csv"))
head(coords)

# Data cleaning of the datasets imported because it's not aggregated by finessgeo

hosp_info <- IUM_hospital_info_2024 %>% 
  mutate(
    mode_hospit = stringr::str_trim(str_to_upper(mode_hospit))
  ) %>% 
  group_by(finessGeo) %>% 
  summarise(
    nblit  = sum(nblit, na.rm = TRUE),
    mode_c = any(mode_hospit == "C", na.rm = TRUE),
    mode_m = any(mode_hospit == "M", na.rm = TRUE),
    mode_p = any(mode_hospit == "P", na.rm = TRUE)
  )

# Getting the coords of each hospital into LAT LONG for mapping, 
# and get the geo location L City/ Commune, Dept, and Region

library(dplyr)
library(sf)
library(giscoR)

# convert Lambert-93 to lon/lat
coords_sf <- coords %>%
  st_as_sf(
    coords = c("coordxet", "coordyet"),
    crs = 2154,
    remove = FALSE
  ) %>%
  st_transform(4326) %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude  = st_coordinates(.)[, 2]
  )

# get France boundaries
communes_fr <- gisco_get_communes(country = "FR", epsg = "4326")
regions_fr <- gisco_get_nuts(country = "FR", nuts_level = 2, epsg = "4326")
departments_fr <- gisco_get_nuts(country = "FR", nuts_level = 3, epsg = "4326")

# inspect names once, because column names can differ by version
# names(communes_fr)
# names(regions_fr)
# names(departments_fr)

coords_enriched <- coords_sf %>%
  st_join(
    communes_fr %>% select(city = COMM_NAME),
    join = st_within,
    left = TRUE
  ) %>%
  st_join(
    departments_fr %>% select(department = NAME_LATN),
    join = st_within,
    left = TRUE
  ) %>%
  st_join(
    regions_fr %>% select(region = NAME_LATN),
    join = st_within,
    left = TRUE
  ) %>%
  st_drop_geometry()

head(coords_enriched)

library(dplyr)
library(stringr)
library(janitor)

# 1. Clean column names: lowercase + consistent style
coords_enriched <- coords_enriched %>%
  clean_names()

daily_admission <- daily_admission %>%
  clean_names()

beds_data <- beds_data %>%
  clean_names()


coords_enriched <- coords_enriched %>%
  rename_with(~"finess_geo", .cols = any_of(c("finessgeo", "finess_geo")))

daily_admission <- daily_admission %>%
  rename_with(~"finess_geo", .cols = any_of(c("finessgeo", "finess_geo")))

beds_data <- beds_data %>%
  rename_with(~"finess_geo", .cols = any_of(c("finessgeo", "finess_geo")))

# 3. Check whether each dataset is 1 row per finess_geo
# coords_dup <- coords_enriched %>%
#   count(finess_geo) %>%
#   filter(n > 1)
# 
# 
# beds_dup <- beds_data %>%
#   count(finess_geo) %>%
#   filter(n > 1)
# 
# coords_dup
# beds_dup

#Merge Coords 
coords_beds = full_join(coords_enriched, beds_data, by = "finess_geo")
#Many doesnt have beds, lets just include the hospitals with any active admissions this year
active_hosp = daily_admission %>% 
  group_by(finess_geo) %>% 
  summarise(admit_yr = sum(no_admissions))

coords_beds_active = left_join(active_hosp, coords_beds, by ="finess_geo")


#Only keep the files we want to use for later in the environment
rm(list = setdiff(ls(), c("coords_beds_active", "weekly", "daily_admission")))
