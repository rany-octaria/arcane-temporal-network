# Exploration of Dynamic Temporal Networks of France Patient Transfer
# Rany Octaria
# 16 Fevrier, 2026

# Loading libraries
library(tidyverse)
options(scipen = 999)

# Import the CSV datasets
# Importing from WINDOWS

monthly = read.csv("G:\\My Drive\\CNAM\\Datasets\\HBN_monthly_sliding_edgelist_2024(in).csv")
weekly = read.csv("G:\\My Drive\\CNAM\\Datasets\\HBN_weekly_sliding_edgelist_2024(in).csv")

head(weekly)


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

#Doing the initial basic statistics of how many nodes, edges, 
#density, transfer volume, and edgeweight

summary_stats <- df %>%
  group_by(weekly_window_number, season) %>%
  summarise(
    n_edges = n(),
    n_nodes = n_distinct(c(finessGeo_origin, finessGeo_target)),
    total_transfers = sum(weight),
    mean_weight = mean(weight),
    median_weight = median(weight),
    .groups = "drop"
  )


summary_stats <- summary_stats %>%
  mutate(
    density = n_edges / (n_nodes * (n_nodes - 1))
  )



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

plots <- lapply(metrics, function(metric) {
  ggplot(summary_stats,#filter(summary_stats, weekly_window_number < 91),
         aes(x = weekly_window_number,
             y = .data[[metric]],
             color = season,
             group = 1)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = season_colors) +
    labs(
      title = paste("Weekly Trend of", metric),
      x = "Weekly Window Number",
      y = metric,
      color = "Season"
    ) +
    theme_minimal()
})

for (p in plots) print(p)
