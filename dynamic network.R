# Exploration of Dynamic Temporal Networks of France Patient Transfer
# Rany Octaria
# 16 Fevrier, 2026

# Loading libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
options(scipen = 999)

# Import the CSV datasets
# Importing from WINDOWS

monthly = read.csv(".\\Datasets\\hbn_direct_transfer_2024\\HBN_monthly_sliding_edgelist_2024.csv")
weekly = read.csv(".\\Datasets\\hbn_direct_transfer_2024\\HBN_weekly_sliding_edgelist_2024.csv")

head(weekly)
tail(weekly)

df = weekly

# Assigning Seasons based on the window start date.



df <- df %>%
  mutate(
    window_start = as.Date(window_start),
    month = month(window_start),
    season = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    )
  )

#Note : I only found the data spanning from 01-01-2024 to 07-20-2024

#Doing the initial basic statistics of how many nodes, edges, 
#density, transfer volume, and edgeweight

summary_stats <- df %>%
  group_by(weekly_window_number,window_start,  month, season) %>%
  summarise(
    n_edges = n(),
    n_nodes = n_distinct(c(finessGeo_origin, finessGeo_target)),
    total_transfers = sum(weight),
    mean_weight = mean(weight),
    median_weight = median(weight),
    .groups = "drop"
  )

#Calculate density with and remove non-full transfer window
summary_stats <- summary_stats %>%
  mutate(
    density = n_edges / (n_nodes * (n_nodes - 1))
  ) %>% 
  filter(as.Date("2024-12-31") - window_start >= 7)

tail(summary_stats)
 

summary_overall <- summary_stats %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

summary_overall = summary_overall %>% 
  pivot_longer(
    cols = everything(),
    names_to = "metric", 
    values_to = "value"
  ) 
summary_overall = summary_overall[-c(1:4), ]


# Plotting the Distribution of Transfers

season_colors <- c(
  "Winter" = "#1f78b4",
  "Spring" = "#33a02c",
  "Summer" = "#ff7f00",
  "Autumn" = "#e31a1c"
)

metrics <- summary_stats %>%
  select(-weekly_window_number, -season) %>%
  colnames()


# Make sure the folder exists
if(!dir.exists("./Plots/")) dir.create("./Plots/")

# Loop over metrics to create plots and save
lapply(metrics, function(metric) {
  
  p <- ggplot(summary_stats, 
              aes(x = window_start,  # assuming this is a Date object
                  y = .data[[metric]],
                  color = season,
                  group = 1)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1) +
    scale_color_manual(values = season_colors) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + # monthly x-axis
    labs(
      title = paste("Weekly Trend of", metric),
      subtitle = "French Direct Transfer Data, 2024, Weekly Temporal Window", 
      x = "Month",
      y = metric,
      color = "Season"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          element_title(hjust=.5))
  
  # Save as high-res JPEG (4 x 6 inches)
  ggsave(
    filename = paste0("./Plots/", metric, ".jpeg"),
    plot = p,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300
  )
  
  return(p)
})
