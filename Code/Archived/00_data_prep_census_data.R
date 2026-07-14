
library(tidyverse)
library(readxl)
library(httr)
library(here)



# =============================================================================
# PART 7: COMPARING BED COUNTS TO CENSUS NUMBERS
# =============================================================================

#getting all the stays data
stays = read.csv(  here("Datasets", "Facility Data", 
                        "WORK_QUERY_FOR_BEFORE_DIRECT_HBN.csv"), sep = ";")


node_attributes_enriched = read.csv (here("Datasets", "Output Data", 
                                                "Enriched Facility Data","node_attributes_enriched.csv"))
str(stays)
# Now parse
stays$date_entree <- as.Date(stays$date_entree, format = "%d/%m/%Y")
stays$date_sortie <- as.Date(stays$date_sortie, format = "%d/%m/%Y")

str(stays)

#Removing same-day stays
stays = filter(stays, LOS_days >0)

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

hospital_stats <- daily_census %>%
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

print(hospital_stats)


#Now merge it with the nodes attribute enriched data 
stats_vs_beds = left_join(node_attributes_enriched, hospital_stats, 
                          by ="finess_geo")
#Show only the variables we need to see

comparisons_mco = stats_vs_beds %>% 
  select(finess_geo, hospital_type, 
         facility_name, facility_type_capact, total_beds_mco,  no_beds, 
         lit_MCO, lit_SSR, min, max, mean, median, ci95_low, ci95_high)  %>% 
  filter(hospital_type != "SSR") %>% 
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

comparison_mco = comparisons_mco %>% 
  select(
    finess_geo, facility_name, hospital_type,facility_type_capact, 
    pmsi_total_beds_mco, capact_total_beds, capact_beds_mco, capact_beds_ssr,
    census_max, census_95ci_high, census_median, everything())


#Adding differences between census mean and p95

comparison_mco = comparison_mco %>% 
  mutate(
    diff_max_mean_prop = (census_max - census_mean)/census_max,
    diff_max_95cihi_prop = (census_max - census_95ci_high)/census_max
  )

#Create the histogram of proportion of differences 

hist(comparison_mco$diff_max_mean_prop)
hist(comparison_mco$diff_max_95cihi_prop)

library(ggplot2)
library(scales)

# Histogram 1: difference in max mean proportion
ggplot(comparison_mco, aes(x = diff_max_mean_prop)) +
  geom_histogram(bins = 30, fill = "#4C9F8F", color = "white", alpha = 0.9) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Difference between Census Maximum and Mean",
    subtitle = "Distribution across facilities",
    x = "Difference (%)",
    y = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid.minor = element_blank()
  )

# Histogram 2: difference in max 95% CI upper bound proportion
ggplot(comparison_mco, aes(x = diff_max_95cihi_prop)) +
  geom_histogram(bins = 30, fill = "#E07A5F", color = "white", alpha = 0.9) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Difference between  Census Maximum vs 95% CI Upper Bound",
    subtitle = "Distribution across facilities",
    x = "Difference (%)",
    y = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid.minor = element_blank()
  )

head(comparison_mco)
write.csv(comparison_final, 
          file =here("Datasets", "Output Data", "Enriched Facility Data",
                     "beds_comparisons.csv"))
