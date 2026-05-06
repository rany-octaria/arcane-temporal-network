# ============================================================
# ARCANE — HOSPITAL NETWORK EPIDEMIC ANIMATION
# Author: Rany Octaria
# Description:
#   Loads per-hospital simulation data produced by
#   arcane_single_sim_export.R on the cluster and generates:
#     (1) Static PNG series at key timepoints
#     (2) Animated GIF — full daily progression over France map
#
# Prerequisites:
#   1. Run arcane_single_sim_export.R on the cluster
#   2. scp the three output RDS files to your local Outputs folder
#   3. Set run_date below to match the cluster run date
#
# install.packages(c("sf","rnaturalearth","rnaturalearthdata",
#                    "gganimate","gifski","tidyverse","scales","here"))
# ============================================================

library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(gganimate)
library(gifski)

# ============================================================
# 0. CONFIG
# ============================================================

# Match this to the date you ran arcane_single_sim_export.R on cluster
run_date <- "2026-05-05"
BETA_SIM <- 0.02
SIM_SEED <- 42L

job_dir    <- here::here("cluster_jobs")
sim_dir    <- file.path(job_dir, "Outputs", run_date)
output_dir <- file.path(job_dir, "Outputs", "Animations")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

SNAPSHOT_DATES <- as.Date(c(
  "2024-01-01",   # Day 0 — seed only
  "2024-04-01",   # 3 months
  "2024-07-01",   # 6 months
  "2025-01-01",   # 1 year
  "2025-07-01",   # 18 months
  "2026-01-01",   # 2 years
  "2026-07-01",   # 2.5 years
  "2026-12-31"    # 3 years — final
))

# ============================================================
# 1. LOAD SERVER OUTPUTS
# ============================================================

message("Loading server simulation data...")

traj_file  <- file.path(sim_dir,
  sprintf("%s_hospital_trajectory_beta%.3f_seed%d.rds",
          run_date, BETA_SIM, SIM_SEED))
edges_file <- file.path(sim_dir, sprintf("%s_top_edges.rds", run_date))
hosp_file  <- file.path(sim_dir, sprintf("%s_hospitals_coords.rds", run_date))

if (!file.exists(traj_file))
  stop(
    "Trajectory file not found:\n  ", traj_file,
    "\n\nDo this first:\n",
    "  1. scp arcane_single_sim_export.R + .sh to cluster\n",
    "  2. qsub cluster_jobs/arcane_single_sim_export.sh\n",
    "  3. scp the 3 output RDS files back to:\n     ", sim_dir
  )

sim_trajectory <- readRDS(traj_file)
top_edges      <- readRDS(edges_file)
hospitals      <- readRDS(hosp_file)

message("Trajectory rows   : ", nrow(sim_trajectory))
message("Hospitals         : ", n_distinct(sim_trajectory$finess_geo))
message("Date range        : ", min(sim_trajectory$date),
        " to ", max(sim_trajectory$date))

# Clip to mainland France
sim_trajectory <- sim_trajectory %>%
  filter(!is.na(lon), !is.na(lat),
         lon > -5.5, lon < 9.6,
         lat >  41,  lat < 51.2)

top_edges <- top_edges %>%
  filter(lon_o > -5.5, lon_o < 9.6, lat_o > 41, lat_o < 51.2,
         lon_t > -5.5, lon_t < 9.6, lat_t > 41, lat_t < 51.2)

# ============================================================
# 2. MAP + THEME
# ============================================================

france <- ne_countries(country = "france", scale = "medium",
                       returnclass = "sf") %>%
  st_crop(xmin = -5.5, xmax = 9.6, ymin = 41.0, ymax = 51.5)

map_theme <- theme_void(base_family = "sans") +
  theme(
    plot.background  = element_rect(fill = "#0D1B2A", color = NA),
    panel.background = element_rect(fill = "#0D1B2A", color = NA),
    legend.position  = c(0.92, 0.25),
    legend.title     = element_text(color = "white",  size = 9, face = "bold"),
    legend.text      = element_text(color = "grey70", size = 8),
    plot.title       = element_text(color = "white",  size = 16, face = "bold",
                                    hjust = 0.5, margin = margin(t = 12, b = 4)),
    plot.subtitle    = element_text(color = "#56B4E9", size = 11,
                                    hjust = 0.5, margin = margin(b = 6)),
    plot.caption     = element_text(color = "grey50", size = 7,
                                    hjust = 0.5, margin = margin(t = 6, b = 8)),
    plot.margin      = margin(10, 10, 10, 10)
  )

# ============================================================
# 3. FRAME BUILDER
# ============================================================

make_frame <- function(day_data, date_val) {

  infected <- day_data %>% filter(n_infected > 0)
  clean    <- day_data %>% filter(n_infected == 0)
  n_cases  <- nrow(infected)
  n_total  <- nrow(day_data)
  net_prev <- sum(day_data$n_infected) / sum(day_data$no_beds)

  ggplot() +
    geom_sf(data = france,
            fill = "#1A2D3E", color = "#2E4A63", linewidth = 0.3) +
    geom_segment(
      data = top_edges,
      aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t),
      color = "#1E4060", linewidth = 0.15, alpha = 0.35,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = clean,
      aes(x = lon, y = lat),
      color = "#4A6280", size = 0.7, alpha = 0.45, shape = 16
    ) +
    geom_point(
      data = infected,
      aes(x = lon, y = lat, color = prevalence, size = n_infected),
      alpha = 0.88, shape = 16
    ) +
    geom_point(
      data = infected %>% filter(prevalence > 0.30),
      aes(x = lon, y = lat, size = n_infected * 2.2),
      color = "#FF2200", alpha = 0.18, shape = 16
    ) +
    scale_color_gradientn(
      name   = "Bed occupancy\n(proportion infected)",
      colors = c("#FFD700", "#FF8C00", "#FF3300", "#CC0000", "#7B0000"),
      limits = c(0, 1),
      labels = percent_format(accuracy = 1),
      guide  = guide_colorbar(barwidth = 0.8, barheight = 5,
                              title.position = "top")
    ) +
    scale_size_continuous(range = c(0.8, 7), guide = "none") +
    annotate("text", x = -5.1, y = 51.0,
             label = format(as.Date(date_val), "%d %b %Y"),
             color = "white", size = 5.8, fontface = "bold", hjust = 0) +
    annotate("text", x = -5.1, y = 41.6,
             label = sprintf(
               "%d hospitals with cases (%.1f%% of network)\nNetwork prevalence: %.2f%%",
               n_cases, 100 * n_cases / n_total, 100 * net_prev),
             color = "#B0C4DE", size = 3.2, hjust = 0, lineheight = 1.5) +
    coord_sf(xlim = c(-5.5, 9.6), ylim = c(41.0, 51.5), expand = FALSE) +
    map_theme +
    labs(
      title   = "ARB Spread Across the French Hospital Network",
      subtitle = sprintf(
        "SIS Metapopulation Model  |  β = %.3f  |  Seed: random hospital",
        BETA_SIM),
      caption = "Grey = unaffected  |  Colour = proportion of beds infected  |  Size ∝ beds infected"
    )
}

# ============================================================
# 4. STATIC SNAPSHOTS
# ============================================================

message("\nGenerating static snapshots...")

available_dates <- sort(unique(sim_trajectory$date))
snap <- function(d) available_dates[which.min(abs(available_dates - d))]
static_dates <- sapply(SNAPSHOT_DATES, snap) %>%
  as.Date(origin = "1970-01-01") %>% unique()

for (i in seq_along(static_dates)) {
  d        <- static_dates[i]
  day_data <- sim_trajectory %>% filter(date == d)
  p        <- make_frame(day_data, d)
  fname    <- file.path(output_dir,
    sprintf("network_snapshot_%02d_%s.png", i, format(d, "%Y-%m-%d")))
  ggsave(fname, p, width = 14, height = 10, dpi = 200, bg = "#0D1B2A")
  message("  Saved: ", basename(fname))
}

message("Static snapshots done.")

# ============================================================
# 5. ANIMATED GIF
# ============================================================

message("\nPreparing animation (every 7th day)...")

anim_dates    <- available_dates[seq(1, length(available_dates), by = 7)]
anim_data     <- sim_trajectory %>% filter(date %in% anim_dates)
infected_anim <- anim_data %>% filter(n_infected > 0)
clean_anim    <- anim_data %>% filter(n_infected == 0)

anim_plot <- ggplot() +
  geom_sf(data = france,
          fill = "#1A2D3E", color = "#2E4A63", linewidth = 0.3) +
  geom_segment(
    data = top_edges,
    aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t),
    color = "#1E4060", linewidth = 0.15, alpha = 0.35,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = clean_anim,
    aes(x = lon, y = lat, group = finess_geo),
    color = "#4A6280", size = 0.7, alpha = 0.45, shape = 16
  ) +
  geom_point(
    data = infected_anim,
    aes(x = lon, y = lat, color = prevalence,
        size = n_infected, group = finess_geo),
    alpha = 0.88, shape = 16
  ) +
  geom_point(
    data = infected_anim %>% filter(prevalence > 0.30),
    aes(x = lon, y = lat, size = n_infected * 2.2, group = finess_geo),
    color = "#FF2200", alpha = 0.18, shape = 16
  ) +
  scale_color_gradientn(
    name   = "Bed occupancy\n(proportion infected)",
    colors = c("#FFD700", "#FF8C00", "#FF3300", "#CC0000", "#7B0000"),
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    guide  = guide_colorbar(barwidth = 0.8, barheight = 5,
                            title.position = "top")
  ) +
  scale_size_continuous(range = c(0.8, 7), guide = "none") +
  coord_sf(xlim = c(-5.5, 9.6), ylim = c(41.0, 51.5), expand = FALSE) +
  map_theme +
  labs(
    title   = "ARB Spread Across the French Hospital Network",
    caption = "Grey = unaffected  |  Colour = proportion beds infected  |  Size ∝ beds infected"
  ) +
  transition_time(date) +
  labs(subtitle = sprintf(
    "SIS Metapopulation Model  |  β = %.3f  |  {format(frame_time, '%%d %%b %%Y')}",
    BETA_SIM)) +
  ease_aes("linear")

message("Rendering GIF (", length(anim_dates), " frames at 12fps)...")
message("Estimated time: ~5–10 minutes")

animate(
  anim_plot,
  nframes  = length(anim_dates),
  fps      = 12,
  width    = 1400,
  height   = 1000,
  renderer = gifski_renderer(
    file.path(output_dir, "network_animation.gif")),
  bg       = "#0D1B2A"
)

message("GIF saved.")

# ============================================================
# 6. HIGH-RES FINAL STATE PNG
# ============================================================

final_state <- sim_trajectory %>% filter(date == max(date))
p_final     <- make_frame(final_state, max(sim_trajectory$date))
ggsave(file.path(output_dir, "network_final_state.png"),
       p_final, width = 16, height = 11, dpi = 300, bg = "#0D1B2A")

message("Final state PNG saved.")
message("\n✓ All done. Outputs in:\n  ", output_dir)
