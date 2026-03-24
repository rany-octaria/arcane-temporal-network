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
monthly = read_csv(here("Datasets", "hbn_direct_transfer_2024","HBN_monthly_sliding_edgelist_2024.csv" ))
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


# If one dataset already has finess_geo, rename() will fail.
# In that case, use this safer version instead:

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


# Start the Modeling Now!

#Setup
#1. Model assumption 1 : Full Occupancy. the number of discharges each day is the 
# same as the number of admissions

#Length of stay : Because it is unknown, we will take from a distribution, an average
#los based on a distribution of 0-21 days (covering short term and long term hospital reasonably)


# -------------------------------
# Clean hospital bed dataset
# -------------------------------
hospitals <- coords_beds_active %>%
  clean_names() %>%
  mutate(
    finess_geo = as.character(finess_geo),
    no_beds = as.integer(no_beds)
  ) %>%
  select(finess_geo, no_beds) %>%
  distinct() %>% 
  mutate(
    mean_beds = round(mean(no_beds, na.rm = TRUE)),
    no_beds = if_else(is.na(no_beds), mean_beds, no_beds),
    no_beds = as.integer(round(no_beds))
  ) %>%
  select(-mean_beds)

# -------------------------------
# Clean daily admissions dataset
# -------------------------------
adm_daily <- daily_admission %>%
  clean_names() %>%
  mutate(
    date = as.Date(date_entree),
    daily_admissions = as.integer(no_admissions)
  ) %>%
  select(finess_geo, date, daily_admissions)

# -------------------------------
# Clean weekly transfer dataset
# -------------------------------
#Getting the date start as the last date
transfers <- weekly %>%
  clean_names() %>%
  mutate(
    finess_geo_origin = as.character(finess_geo_origin),
    finess_geo_target = as.character(finess_geo_target),
    transfer_date = as.Date(window_end),
    weight = as.numeric(weight)
  ) 
# --------------------------------------------------------
# Draw integer LOS from a bounded normal-like distribution
# --------------------------------------------------------
sample_los <- function(n, mean_los = 7, sd_los = 2, min_los = 1, max_los = 15) {
  x <- round(rnorm(n, mean = mean_los, sd = sd_los))
  x <- pmax(min_los, pmin(max_los, x))
  as.integer(x)
}
#this will change according to the availability of the data


## INITIALIZING PATIENT POPULATION

# -------------------------------------------------------------------
# Create one occupied bed = one patient at time 0
# Seed infection into one hospital
# -------------------------------------------------------------------
initialize_patients <- function(hospitals,
                                seed_hospital,
                                n_seed_infected = 1,
                                mean_los = 7,  #all of this is assumptions
                                sd_los = 2,
                                min_los = 1,
                                max_los = 15) {
  
  patients <- hospitals %>%
    mutate(bed_id = map(no_beds, seq_len)) %>%
    unnest(bed_id) %>%
    mutate(
      patient_id = row_number(),
      infected = FALSE,
      los_remaining = sample_los(
        n = n(),
        mean_los = mean_los,
        sd_los = sd_los,
        min_los = min_los,
        max_los = max_los
      )
    ) %>%
    select(patient_id, finess_geo, infected, los_remaining)
  
  # Seed the initial infected patients in one hospital
  seed_ids <- patients %>%
    dplyr::filter(finess_geo == seed_hospital) %>%
    slice_sample(n = min(n_seed_infected, n())) %>%
    pull(patient_id)
  
  patients <- patients %>%
    mutate(
      infected = patient_id %in% seed_ids
    )
  
  patients
}

# -------------------------------------------------------------------
# Get the transfer records that occur on a given date
# -------------------------------------------------------------------
# get_daily_transfers <- function(transfers, current_date) {
#   
#   transfers %>%
#     filter(transfer_date == current_date) %>%
#     filter(weight > 0) %>%
#     mutate(
#       n_transfers = as.integer(round(weight))
#     ) %>%
#     filter(n_transfers > 0) %>%
#     select(finess_geo_origin, finess_geo_target, n_transfers)
# }

library(tidyverse)
library(lubridate)
library(janitor)

# -------------------------------------------------------------------
# Assign one fixed LOS to each hospital for the full simulation
# -------------------------------------------------------------------
assign_hospital_los <- function(hospitals,
                                mean_los = 7,
                                sd_los = 2,
                                min_los = 1,
                                max_los = 15,
                                seed = 123) {
  
  set.seed(seed)
  
  hospitals %>%
    mutate(
      hospital_los = sample_los(
        n = n(),
        mean_los = mean_los,
        sd_los = sd_los,
        min_los = min_los,
        max_los = max_los
      )
    )
}

# -------------------------------------------------------------------
# Initialize one patient per occupied bed
# All patients in a hospital get the same LOS = hospital_los
# -------------------------------------------------------------------
initialize_patients <- function(hospitals,
                                seed_hospital,
                                n_seed_infected = 1) {
  
  patients <- hospitals %>%
    mutate(bed_id = map(no_beds, seq_len)) %>%
    unnest(bed_id) %>%
    mutate(
      patient_id = row_number(),
      infected = FALSE,
      los_remaining = hospital_los
    ) %>%
    select(patient_id, finess_geo, infected, los_remaining)
  
  seed_pool <- patients %>%
    filter(finess_geo == seed_hospital)
  
  seed_ids <- seed_pool %>%
    slice_sample(n = min(n_seed_infected, nrow(seed_pool))) %>%
    pull(patient_id)
  
  patients %>%
    mutate(
      infected = patient_id %in% seed_ids
    )
}

# -------------------------------------------------------------------
# Simulate one day of SIS transmission + turnover + transfers
# -------------------------------------------------------------------
simulate_one_day <- function(patients,
                             hospitals,
                             adm_daily,
                             transfers,
                             current_date,
                             beta_within = 0.05,
                             gamma_clear = 1 / 387,
                             admission_prev = 0) {
  
  # ===============================================================
  # A. Within-hospital transmission
  # ===============================================================
  hosp_prev <- patients %>%
    group_by(finess_geo) %>%
    summarise(
      n_patients = n(),
      n_infected = sum(infected),
      prevalence = n_infected / n_patients,
      .groups = "drop"
    )
  
  patients <- patients %>%
    left_join(
      hosp_prev %>% select(finess_geo, prevalence),
      by = "finess_geo"
    ) %>%
    mutate(
      p_infection = 1 - exp(-beta_within * prevalence),
      new_infection = if_else(
        infected,
        FALSE,
        rbinom(n(), size = 1, prob = p_infection) == 1
      ),
      infected = infected | new_infection
    ) %>%
    select(-prevalence, -p_infection, -new_infection)
  
  # ===============================================================
  # B. SIS clearance
  # ===============================================================
  patients <- patients %>%
    mutate(
      infected = if_else(
        infected,
        rbinom(n(), size = 1, prob = gamma_clear) == 0,
        FALSE
      )
    )
  
  # ===============================================================
  # C. Advance LOS by one day
  # ===============================================================
  patients <- patients %>%
    mutate(
      los_remaining = los_remaining - 1
    )
  
  # ===============================================================
  # D. Get today's admission targets
  # ===============================================================
  adm_today <- hospitals %>%
    select(finess_geo) %>%
    left_join(
      adm_daily %>% filter(date == current_date),
      by = "finess_geo"
    ) %>%
    mutate(
      daily_admissions = replace_na(daily_admissions, 0L)
    )
  
  # ===============================================================
  # E. Candidate departures:
  # first use LOS-ready patients, then force extras if needed
  # so that departures = daily_admissions
  # ===============================================================
  departing_ready <- patients %>%
    filter(los_remaining <= 0)
  
  staying_patients <- patients %>%
    filter(los_remaining > 0)
  
  dep_ready_counts <- departing_ready %>%
    count(finess_geo, name = "n_dep_ready")
  
  adm_today <- adm_today %>%
    left_join(dep_ready_counts, by = "finess_geo") %>%
    mutate(
      n_dep_ready = replace_na(n_dep_ready, 0L),
      extra_needed = pmax(0L, daily_admissions - n_dep_ready)
    )
  
  # If LOS-ready departures are not enough, sample extra departures
  # from patients who otherwise would have stayed
  forced_departures <- purrr::pmap_dfr(
    list(adm_today$finess_geo, adm_today$extra_needed),
    function(hosp_id, n_force) {
      
      eligible <- staying_patients %>%
        filter(finess_geo == hosp_id)
      
      if (is.na(n_force) || n_force <= 0 || nrow(eligible) == 0) {
        return(tibble(
          patient_id = integer(),
          finess_geo = character(),
          infected = logical(),
          los_remaining = integer()
        ))
      }
      
      eligible %>%
        slice_sample(n = min(n_force, nrow(eligible)))
    }
  )
  
  # Candidate departures = LOS-ready + forced departures
  candidate_departures <- bind_rows(
    departing_ready,
    forced_departures
  ) %>%
    distinct(patient_id, .keep_all = TRUE)
  
  # Final departures: exactly daily_admissions per hospital
  departures_today <- purrr::pmap_dfr(
    list(adm_today$finess_geo, adm_today$daily_admissions),
    function(hosp_id, n_leave) {
      
      cand <- candidate_departures %>%
        filter(finess_geo == hosp_id)
      
      if (is.na(n_leave) || n_leave <= 0 || nrow(cand) == 0) {
        return(tibble(
          patient_id = integer(),
          finess_geo = character(),
          infected = logical(),
          los_remaining = integer()
        ))
      }
      
      cand %>%
        slice_sample(n = min(n_leave, nrow(cand)))
    }
  )
  
  # Everyone not selected for departure stays
  staying_patients <- patients %>%
    anti_join(
      departures_today %>% select(patient_id),
      by = "patient_id"
    )
  # ===============================================================
  # F. Transfer assignments
  # Transfers are chosen from today's departures
  # ===============================================================
  transfer_plan <- get_daily_transfers(
    transfers = transfers,
    current_date = current_date
  )
  
  transfer_assignments <- tibble(
    patient_id = integer(),
    origin = character(),
    destination = character()
  )
  
  if (nrow(transfer_plan) > 0) {
    transfer_assignments <- transfer_plan %>%
      group_by(finess_geo_origin) %>%
      group_modify(~{
        origin_id <- .y$finess_geo_origin[[1]]
        
        dep_origin <- departures_today %>%
          filter(finess_geo == origin_id)
        
        if (nrow(dep_origin) == 0) {
          return(tibble(
            patient_id = integer(),
            origin = character(),
            destination = character()
          ))
        }
        
        plan_origin <- .x %>%
          rename(
            destination = finess_geo_target,
            n_transfer = n_transfers
          )
        
        total_requested <- sum(plan_origin$n_transfer)
        total_available <- nrow(dep_origin)
        total_to_transfer <- min(total_requested, total_available)
        
        if (total_to_transfer == 0) {
          return(tibble(
            patient_id = integer(),
            origin = character(),
            destination = character()
          ))
        }
        
        selected <- dep_origin %>%
          slice_sample(n = total_to_transfer)
        
        dest_vec <- rep(plan_origin$destination, times = plan_origin$n_transfer)
        
        if (length(dest_vec) > total_to_transfer) {
          dest_vec <- sample(dest_vec, size = total_to_transfer, replace = FALSE)
        }
        
        tibble(
          patient_id = selected$patient_id,
          origin = origin_id,
          destination = dest_vec
        )
      }) %>%
      ungroup()
  }
  
  # ===============================================================
  # G. Split departures
  # ===============================================================
  transfer_out_patients <- departures_today %>%
    inner_join(
      transfer_assignments %>% select(patient_id, destination),
      by = "patient_id"
    )
  
  non_transfer_discharges <- departures_today %>%
    anti_join(
      transfer_assignments %>% select(patient_id),
      by = "patient_id"
    )
  
  # ===============================================================
  # H. Build transferred-in patients
  # New LOS = hospital_los at destination
  # ===============================================================
  transferred_in <- transfer_out_patients %>%
    left_join(
      hospitals %>% select(finess_geo, hospital_los),
      by = c("destination" = "finess_geo")
    ) %>%
    transmute(
      patient_id = NA_integer_,
      finess_geo = destination,
      infected = infected,
      los_remaining = hospital_los
    )
  
  # ===============================================================
  # I. Remaining community admissions
  # daily_admissions = transfer-ins + community admissions
  # ===============================================================
  transfer_in_counts <- transferred_in %>%
    count(finess_geo, name = "n_transfer_in")
  
  adm_today <- adm_today %>%
    left_join(transfer_in_counts, by = "finess_geo") %>%
    mutate(
      n_transfer_in = replace_na(n_transfer_in, 0L),
      n_community_adm = pmax(0L, daily_admissions - n_transfer_in)
    )
  
  community_admissions <- adm_today %>%
    mutate(adm_id = map(n_community_adm, seq_len)) %>%
    unnest(adm_id, keep_empty = FALSE) %>%
    left_join(
      hospitals %>% select(finess_geo, hospital_los),
      by = "finess_geo"
    ) %>%
    transmute(
      patient_id = NA_integer_,
      finess_geo,
      infected = rbinom(n(), size = 1, prob = admission_prev) == 1,
      los_remaining = hospital_los
    )
  
  # ===============================================================
  # J. Rebuild patient table
  # ===============================================================
  patients_next <- bind_rows(
    staying_patients %>% select(patient_id, finess_geo, infected, los_remaining),
    transferred_in,
    community_admissions
  ) %>%
    mutate(
      patient_id = row_number()
    )
  
  # ===============================================================
  # K. Enforce exact occupancy = no_beds
  # ===============================================================
  current_counts <- patients_next %>%
    count(finess_geo, name = "n_current") %>%
    right_join(hospitals, by = "finess_geo") %>%
    mutate(
      n_current = replace_na(n_current, 0L),
      diff = no_beds - n_current
    )
  
  fill_ins <- current_counts %>%
    filter(diff > 0) %>%
    mutate(fill_id = map(diff, seq_len)) %>%
    unnest(fill_id, keep_empty = FALSE) %>%
    transmute(
      patient_id = NA_integer_,
      finess_geo,
      infected = FALSE,
      los_remaining = hospital_los
    )
  
  patients_next <- bind_rows(patients_next, fill_ins) %>%
    mutate(patient_id = row_number())
  
  patients_next <- patients_next %>%
    group_by(finess_geo) %>%
    group_modify(~{
      hosp_id <- .y$finess_geo[[1]]
      target_n <- hospitals %>%
        filter(finess_geo == hosp_id) %>%
        pull(no_beds)
      
      if (nrow(.x) <= target_n) {
        .x
      } else {
        .x %>% slice_sample(n = target_n)
      }
    }) %>%
    ungroup()
  
  # ===============================================================
  # L. Daily summaries
  # ===============================================================
  daily_summary <- patients_next %>%
    group_by(finess_geo) %>%
    summarise(
      n_patients = n(),
      n_infected = sum(infected),
      prevalence = n_infected / n_patients,
      .groups = "drop"
    ) %>%
    mutate(date = current_date)
  
  overall_summary <- daily_summary %>%
    summarise(
      date = current_date,
      total_patients = sum(n_patients),
      total_infected = sum(n_infected),
      overall_prevalence = total_infected / total_patients
    )
  
  list(
    patients = patients_next,
    daily_summary = daily_summary,
    overall_summary = overall_summary,
    departures_today = departures_today,
    transfer_plan = transfer_plan,
    transfer_assignments = transfer_assignments,
    transferred_in = transferred_in,
    non_transfer_discharges = non_transfer_discharges
  )
}

# -------------------------------------------------------------------
# Run simulation over a date range
# -------------------------------------------------------------------
run_sis_simulation <- function(hospitals,
                               adm_daily,
                               transfers,
                               start_date,
                               end_date,
                               seed_hospital,
                               n_seed_infected = 1,
                               beta_within = 0.05,
                               gamma_clear = 1 / 387,
                               admission_prev = 0,
                               mean_los = 7,
                               sd_los = 2,
                               min_los = 1,
                               max_los = 15,
                               seed = 123) {
  
  # assign one fixed LOS per hospital
  hospitals <- assign_hospital_los(
    hospitals = hospitals,
    mean_los = mean_los,
    sd_los = sd_los,
    min_los = min_los,
    max_los = max_los,
    seed = seed
  )
  
  # initialize patient population
  set.seed(seed)
  patients <- initialize_patients(
    hospitals = hospitals,
    seed_hospital = seed_hospital,
    n_seed_infected = n_seed_infected
  )
  
  sim_dates <- seq.Date(
    from = as.Date(start_date),
    to   = as.Date(end_date),
    by   = "day"
  )
  
  daily_results <- vector("list", length(sim_dates))
  overall_results <- vector("list", length(sim_dates))
  
  for (i in seq_along(sim_dates)) {
    current_date <- sim_dates[i]
    
    out <- simulate_one_day(
      patients = patients,
      hospitals = hospitals,
      adm_daily = adm_daily,
      transfers = transfers,
      current_date = current_date,
      beta_within = beta_within,
      gamma_clear = gamma_clear,
      admission_prev = admission_prev
    )
    
    patients <- out$patients
    daily_results[[i]] <- out$daily_summary
    overall_results[[i]] <- out$overall_summary
  }
  
  list(
    hospitals = hospitals,
    patients_final = patients,
    daily_results = bind_rows(daily_results),
    overall_results = bind_rows(overall_results)
  )
}

#Seeding at a random hospital 
seed_hospital <- "010000024"

sim_out <- run_sis_simulation(
  hospitals = hospitals,
  adm_daily = adm_daily,
  transfers = transfers,
  start_date = "2024-01-07",
  end_date   = "2024-03-31",
  seed_hospital = seed_hospital,
  n_seed_infected = 5,
  beta_within = 0.20,
  gamma_clear = 1 / 387,
  admission_prev = 0,
  mean_los = 7,
  sd_los = 2,
  min_los = 1,
  max_los = 15,
  seed = 101
)

head(sim_out$hospitals)
head(sim_out$daily_results)
head(sim_out$overall_results)


#Diagnosis
hospitals %>%
  filter(is.na(no_beds) | no_beds < 0) %>%
  arrange(no_beds)

#With this simplification, all patients in the same hospital turn over on the 
# same LOS clock unless they entered on different days. 
# That is acceptable for a first version, but it may 
# create somewhat artificial departure patterns compared with individual-level LOS draws.
# 
# The next thing I would refine is the departure mechanism,
# because right now it still forces departures to equal dail
# even when LOS-ready patients are fewer.