
#Gettiing the LOS data 
#1. By Hospitals, by Hospital Types and by Hospital Type/Region
library(here)
library(tidyverse)
library(lubridate)
# Getting the MCO and SSR Stays Data
stays <- read_delim(here("Datasets", "MCO_SSR_HBN_2024", "MCO_SSR_HBN_IP_Direct_2024" ,
"WORK_QUERY_FOR_BEFORE_DIRECT_SSR_MCO_HBN.csv"),   
delim = ";", escape_double = FALSE, trim_ws = TRUE)



#Getting the facility data so we have the region of the hospitals too 

nodes <- read_csv(here("Datasets", "Output Data", "Enriched Facility Data",
                       "node_attributes_enriched.csv"))

#Getting the Length of Stay Averages, total patient-days, by year. 
str(stays)

los = stays %>% 
  group_by(FinessGeo) %>% 
  summarise(
    los_mean = mean(LOS_Days),
    los_median = median(LOS_Days),
    los_q1 = quantile(LOS_Days, prob = .25),
    los_q3 = quantile(LOS_Days, prob = .75),
    los_ci_low = quantile(LOS_Days, prob = .05),
    los_ci_hi = quantile(LOS_Days, prob = .95),
    los_sd = sd(LOS_Days),
    pt_days_total = sum(LOS_Days),
    patient_total = sum(n())  #Getting the total number of stays which may be important to get the LOS by region and type
  )

# Getting the Facility-Level Census Data

# Now parse
stays$date_entree <- as.Date(stays$date_entree, format = "%d/%m/%Y")
stays$date_sortie <- as.Date(stays$date_sortie, format = "%d/%m/%Y")

# Define your date range 
# Including only February until three weeks of December to remove outliers

date_seq <- seq(as.Date("2024-02-01"), as.Date("2024-12-23"), by = "day")

# Count census for one day across all hospitals
count_day <- function(d) {
  stays %>%
    filter(date_entree <= d & date_sortie >= d) %>%
    group_by(FinessGeo) %>%
    summarise(n_patients = n(), .groups = "drop") %>%
    mutate(day = d)
}

# Apply over all days
daily_census <- map_dfr(date_seq, count_day)

# Fill in 0s for hospital-days with no patients
daily_census <- daily_census %>%
  complete(FinessGeo, day = date_seq, fill = list(n_patients = 0))


saveRDS(daily_census, file = 
          here("Datasets", "Output Data", "Enriched Facility Data",
               "daily_census.RDS"))

write.csv(daily_census, file = 
            here("Datasets", "Output Data", "Enriched Facility Data",
                 "daily_census.csv"))



hospital_census <- daily_census %>%
  group_by(FinessGeo) %>%
  summarise(
    min      = min(n_patients),
    max      = max(n_patients),
    mean     = round(mean(n_patients), 2),
    median   = median(n_patients),
    ci95_low  =  quantile(n_patients, probs = 0.05, na.rm = TRUE),
    ci95_high =  quantile(n_patients, probs = 0.95, na.rm = TRUE),
    .groups = "drop"
  )%>% 
  rename(finess_geo =FinessGeo) 

#Now merge it with the nodes attribute enriched data 
census_vs_beds = full_join(hospital_census, nodes,
                          by ="finess_geo") %>%
  rename (
    pmsi_total_beds_mco  =  total_beds_mco ,
    capact_total_beds = no_beds, 
    capact_beds_mco = lit_MCO,
    capact_beds_ssr = lit_SSR,
    census_min = min,
    census_max = max, 
    census_mean = mean, 
    census_median = median, 
    census_95ci_low = ci95_low,
    census_95ci_high = ci95_high)  

# Getting the Region and Type of the Facility

hospital_stats = full_join(los, census_vs_beds, by =c( "FinessGeo"="finess_geo") ) 



#And exporting
write.csv(hospital_stats, file = 
            here("Datasets", "Output Data", "Enriched Facility Data",
                 "hospital_stats_los_census.csv"))
saveRDS(hospital_stats, file = 
          here("Datasets", "Output Data", "Enriched Facility Data",
               "hospital_stats_los_census.RDS"))

#Exporting stats by region and type
write.csv(region_type_stats, file = 
            here("Datasets", "Output Data", "Enriched Facility Data",
                 "reg_type_stats_los.csv"))
saveRDS(region_type_stats, file = 
          here("Datasets", "Output Data", "Enriched Facility Data",
               "reg_type_stats_los.RDS"))

hospital_stats = hospital_stats %>% 
  mutate(
    type_spares = dplyr::case_when(
      # 1) ACTIVITY axis first — an SSR-only facility is a rehab establishment
      #    regardless of who owns it (this is how SPARES defines "Rehabilitation hospital").
      #    MCO and MCO/SSR facilities keep their acute identity and fall through to ownership.
      hospital_type == "SSR" | facility_type_pmsi == "SSR" ~ "Rehabilitation hospital",
      
      # 2) OWNERSHIP axis for everything acute (MCO / MCO-SSR)
      facility_type_pmsi == "General public hospital (CH)"         ~ "General public hospital",
      facility_type_pmsi == "Regional/University hospital (CHR/U)" ~ "University hospital",
      facility_type_pmsi == "Private"                              ~ "Private for profit hospital",
      facility_type_pmsi == "Non-profit facility (PSPH/EBNL)"      ~ "Private not-for-profit hospital",
      
      # CLCC = Centre de Lutte Contre le Cancer = ESPIC (non-profit), so folded in here.
      # <- change THIS line if you'd rather keep cancer centres separate or set them NA.
      facility_type_pmsi == "Cancer centre (CLCC)"                 ~ "Cancer centre (CLCC)",
      
      TRUE ~ NA_character_   # the 2 facilities missing hospital_type / facility_type_pmsi
    )
  )
#Getting the Average LOS by Hospital Type and Region

region_type_stats = hospital_stats %>% 
  group_by(region, type_spares) %>% 
  summarise(reg_type_pt_days = sum(pt_days_total),
            reg_type_n_patients = sum(patient_total),
            .groups = "drop_last") %>% 
  mutate(reg_type_los_avg = reg_type_pt_days /reg_type_n_patients)



hospital_stats = readRDS(here("Datasets", "Output Data", "Enriched Facility Data",
                             "hospital_stats_los_census.RDS"))
