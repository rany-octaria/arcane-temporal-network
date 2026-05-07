# ============================================================
# ARCANE — HOSPITAL NETWORK EPIDEMIC ANIMATION
# Author: Rany Octaria
# Description:
#   Runs a single SIS simulation at a chosen beta value and
#   visualises ARB spread across the French hospital network
#   on a map of France. Animation stops when steady state
#   is detected.
#
#   Output (saved to cluster_jobs/Outputs/Animations/):
#     - 6 static PNG snapshots at key timepoints
#     - Animated GIF (~1 min, every 2 days, stops at steady state)
#     - High-res final state PNG
#
# Required packages (install once):
#   install.packages(c("tidyverse","here","janitor","sf",
#     "rnaturalearth","rnaturalearthdata","scales",
#     "gganimate","gifski"))
# ============================================================

library(tidyverse)
library(here)
library(janitor)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(gganimate)
library(gifski)

# ============================================================
# 0. CONFIG
# ============================================================

BETA_SIM    <- 0.02    # within-hospital transmission rate to simulate
SIM_SEED    <- 42L     # random seed — fix for reproducibility
START_DATE  <- "2024-01-01"
END_DATE    <- "2026-12-31"   # upper bound; simulation stops at steady state
GAMMA_CLEAR <- 1 / 387        # daily clearance rate (mean carriage ~387 days)
ADMIT_PREV  <- 0              # proportion of new admissions already colonised

# Steady-state detection thresholds (checked on a rolling 30-day window)
SS_WINDOW    <- 30       # rolling window length in days
SS_RANGE_TOL <- 0.0005   # max allowed range of prevalence in window
SS_SLOPE_TOL <- 0.00001  # max allowed slope of prevalence in window

# Animation — targets ~1 minute total playback
ANIM_EVERY_N_DAYS <- 2   # sample one frame every 2 simulated days
ANIM_FPS          <- 9   # 9fps × ~548 frames ≈ 61 seconds

# Node sizes — proportional to hospital bed count, 3× original
NODE_SIZE_RANGE <- c(1.2, 13)

# Colour tokens — light background theme
BG_COLOR      <- "#F5F7FA"   # slide background
FRANCE_FILL   <- "#E2E8EF"   # country polygon
FRANCE_BORDER <- "#B0BFCC"   # country border line
EDGE_INACTIVE <- "#C5D3DC"   # edges between unaffected hospitals (grey)
EDGE_ACTIVE   <- "#1B5E8A"   # edges touching an infected hospital (dark blue)
NODE_CLEAN    <- "#9DB5C8"   # uninfected hospital dot colour
TEXT_DARK     <- "#1A2332"   # primary annotation text
TEXT_SOFT     <- "#4A6070"   # secondary annotation text

# Paths
job_dir    <- here::here("cluster_jobs")
data_dir   <- file.path(job_dir, "data")
output_dir <- file.path(job_dir, "Outputs", "Animations")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 6 evenly spaced snapshot dates (clipped to steady-state day later)
SNAPSHOT_DATES <- as.Date(c(
  "2024-01-01",   # Day 0 — seed only
  "2024-07-01",   # 6 months
  "2025-01-01",   # 1 year
  "2025-07-01",   # 18 months
  "2026-01-01",   # 2 years
  "2026-12-31"    # 3 years / final (clipped to SS date if earlier)
))

message("BETA_SIM   = ", BETA_SIM)
message("SIM_SEED   = ", SIM_SEED)
message("OUTPUT_DIR = ", output_dir)

# Tag used in all output filenames — e.g. "beta0.020"
beta_tag <- sprintf("beta%.3f", BETA_SIM)

# ============================================================
# 1. LOAD DATA
# ============================================================

message("Loading data...")
coords_beds_active <- readRDS(file.path(data_dir, "coords_beds_active.RDS"))
weekly_transfers   <- readRDS(file.path(data_dir, "weekly.RDS"))

# ============================================================
# 2. BUILD HOSPITAL UNIVERSE WITH COORDINATES
# Pull all unique hospital IDs from both transfer directions,
# join GPS coordinates and bed counts. Impute missing bed
# counts with the network mean.
# ============================================================

hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finessGeo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finessGeo_target))
) %>%
  distinct() %>%
  left_join(
    coords_beds_active %>%
      clean_names() %>%
      transmute(
        finess_geo = as.character(finess_geo),
        no_beds    = readr::parse_number(as.character(no_beds)),
        lon        = as.numeric(longitude),
        lat        = as.numeric(latitude)
      ),
    by = "finess_geo"
  ) %>%
  mutate(
    no_beds = if_else(is.na(no_beds),
                      round(mean(no_beds, na.rm = TRUE)),
                      no_beds),
    no_beds = as.integer(no_beds)
  )

message("Hospitals loaded: ", nrow(hospitals))

# ============================================================
# 3. SEED HOSPITAL — largest hospital by bed count
# The largest hospital is chosen as the index case to model
# a high-capacity hub seeding the network.
# ============================================================

set.seed(SIM_SEED)
seed_hospital <- hospitals %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  slice_sample(n = 1) %>%
  pull(finess_geo)

message("Seed hospital (random): ", seed_hospital)

# ============================================================
# 4. TOP TRANSFER EDGES FOR VISUALISATION
# Keep the 600 highest-volume transfer corridors and attach
# origin/destination coordinates. Clip to mainland France.
# Split into two objects: all edges (background grey) and
# per-date active edges (dark blue, touching infected hospitals).
# ============================================================

top_edges <- weekly_transfers %>%
  group_by(finessGeo_origin, finessGeo_target) %>%
  summarise(total_weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_weight)) %>%
  slice_head(n = 600) %>%
  inner_join(hospitals %>% select(finess_geo, lon, lat),
             by = c("finessGeo_origin" = "finess_geo")) %>%
  rename(lon_o = lon, lat_o = lat) %>%
  inner_join(hospitals %>% select(finess_geo, lon, lat),
             by = c("finessGeo_target" = "finess_geo")) %>%
  rename(lon_t = lon, lat_t = lat) %>%
  filter(!is.na(lon_o), !is.na(lon_t),
         lon_o > -5.5, lon_o < 9.6, lat_o > 41, lat_o < 51.2,
         lon_t > -5.5, lon_t < 9.6, lat_t > 41, lat_t < 51.2)

message("Transfer edges for viz: ", nrow(top_edges))

# ============================================================
# 5. SIMULATION FUNCTIONS
# ============================================================

# initialize_state(): place 1 infected patient at the seed hospital,
# 0 everywhere else. Cap at no_beds so we never exceed capacity.
initialize_state <- function(hospitals, seed_hospital, n_seed = 1) {
  hospitals %>%
    mutate(
      n_infected = if_else(finess_geo == seed_hospital,
                           pmin(n_seed, no_beds), 0L),
      prevalence = n_infected / no_beds
    )
}

# sim_one_day(): advances the SIS model by one day.
# Steps: (1) draw infected transfers, (2) cap to available infected,
# (3) apply transfers in/out, (4) new admissions, (5) clearance,
# (6) within-hospital transmission.
sim_one_day <- function(state, transfers_day,
                        beta_within, gamma_clear, admission_prev) {
  
  if (nrow(transfers_day) == 0)
    return(state %>% mutate(prevalence = n_infected / no_beds))
  
  # Step 1 — draw stochastic infected transfers per edge
  idx_o  <- match(transfers_day$finessGeo_origin, state$finess_geo)
  prev_o <- replace_na(state$prevalence[idx_o], 0)
  ninf_o <- replace_na(state$n_infected[idx_o], 0L)
  inf_raw <- rbinom(nrow(transfers_day), transfers_day$weight, prev_o)
  
  # Step 2 — cap outgoing transfers so total out ≤ n_infected at origin
  split_idx <- split(seq_len(nrow(transfers_day)),
                     transfers_day$finessGeo_origin)
  for (idx in split_idx) {
    avail    <- ninf_o[idx[1]]
    proposed <- sum(inf_raw[idx])
    if (proposed > avail && avail > 0) {
      scaled   <- inf_raw[idx] * avail / proposed
      base     <- floor(scaled)
      leftover <- avail - sum(base)
      if (leftover > 0) {
        top       <- order(scaled - base, decreasing = TRUE)[seq_len(leftover)]
        base[top] <- base[top] + 1L
      }
      inf_raw[idx] <- base
    }
  }
  
  # Step 3 — accumulate infected patients leaving and arriving per hospital
  n   <- nrow(state)
  out <- integer(n); inn <- integer(n)
  io  <- match(transfers_day$finessGeo_origin, state$finess_geo)
  it  <- match(transfers_day$finessGeo_target, state$finess_geo)
  for (k in seq_along(io)) {
    if (!is.na(io[k])) out[io[k]] <- out[io[k]] + inf_raw[k]
    if (!is.na(it[k])) inn[it[k]] <- inn[it[k]] + inf_raw[k]
  }
  
  # Step 4 — replace departing patients with new admissions
  replaced     <- rbinom(n, out, admission_prev)
  n_after_xfer <- pmax(0L, pmin(state$n_infected - out + inn + replaced,
                                state$no_beds))
  
  # Step 5 — stochastic clearance at rate gamma_clear
  n_cleared     <- rbinom(n, n_after_xfer, gamma_clear)
  n_after_clear <- n_after_xfer - n_cleared
  
  # Step 6 — frequency-dependent within-hospital transmission
  n_suscept <- state$no_beds - n_after_clear
  p_inf     <- pmin(pmax(1 - exp(-beta_within * n_after_clear /
                                   state$no_beds), 0), 1)
  n_new     <- rbinom(n, n_suscept, p_inf)
  n_final   <- pmin(state$no_beds, n_after_clear + n_new)
  
  state %>% mutate(n_infected = n_final, prevalence = n_final / no_beds)
}

# ============================================================
# 6. RUN SIMULATION — stops early when steady state is detected
# Loops day by day. Every SS_WINDOW days, checks whether network
# prevalence has stabilised. Breaks out of the loop early and
# records the steady-state date so animation can end there.
# ============================================================

message("Running simulation (beta=", BETA_SIM, " | seed=", SIM_SEED, ")...")

set.seed(SIM_SEED)
state      <- initialize_state(hospitals, seed_hospital)
sim_dates  <- seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = "day")

transfers_by_day <- weekly_transfers %>%
  mutate(transfer_date = as.Date(window_end)) %>%
  select(transfer_date, finessGeo_origin, finessGeo_target, weight) %>%
  split(.$transfer_date)

daily_states <- vector("list", length(sim_dates))

for (i in seq_along(sim_dates)) {
  d  <- sim_dates[i]
  td <- transfers_by_day[[as.character(d)]]
  
  if (is.null(td)) {
    td <- tibble(finessGeo_origin = character(),
                 finessGeo_target = character(),
                 weight           = integer())
  } else {
    td <- td %>% select(finessGeo_origin, finessGeo_target, weight)
  }
  
  state <- sim_one_day(state, td, BETA_SIM, GAMMA_CLEAR, ADMIT_PREV)
  
  # Store per-hospital state for map frames
  daily_states[[i]] <- tibble(
    finess_geo = state$finess_geo,
    date       = d,
    no_beds    = state$no_beds,
    n_infected = state$n_infected,
    prevalence = state$prevalence,
    lon        = state$lon,
    lat        = state$lat
  )
  
  if (i %% 100 == 0)
    message(sprintf("  Day %d / %d  —  %s  —  %d hospitals with cases",
                    i, length(sim_dates), d,
                    sum(state$n_infected > 0, na.rm = TRUE)))
}

message("Simulation complete.")

# Combine all daily snapshots and clip to mainland France
sim_trajectory <- bind_rows(daily_states) %>%
  filter(!is.na(lon), !is.na(lat),
         lon > -5.5, lon < 9.6,
         lat >  41,  lat < 51.2)

message("Total simulated days: ", n_distinct(sim_trajectory$date))

# ============================================================
# 7. ACTIVE EDGES PER DATE
# For each animation date, identify which top-600 edges have
# at least one infected endpoint. These are drawn in dark blue;
# the rest remain grey. Pre-computing this avoids slow per-frame
# filtering inside gganimate.
# ============================================================

available_dates <- sort(unique(sim_trajectory$date))
anim_dates <- available_dates[seq(1, length(available_dates),
                                  by = ANIM_EVERY_N_DAYS)]

message("Animation frames: ", length(anim_dates),
        " (every ", ANIM_EVERY_N_DAYS, " days)")
message("Estimated duration: ",
        round(length(anim_dates) / ANIM_FPS, 0), " seconds at ",
        ANIM_FPS, "fps")

# Set of infected hospital IDs per animation date
infected_ids <- sim_trajectory %>%
  filter(date %in% anim_dates, n_infected > 0) %>%
  select(date, finess_geo) %>%
  distinct()

# Edges where origin OR target is infected on that date
anim_edges <- bind_rows(
  top_edges %>%
    inner_join(infected_ids, by = c("finessGeo_origin" = "finess_geo")),
  top_edges %>%
    inner_join(infected_ids, by = c("finessGeo_target" = "finess_geo"))
) %>%
  distinct(date, finessGeo_origin, finessGeo_target, .keep_all = TRUE)

message("Active edges pre-computed: ", nrow(anim_edges), " rows")

# ============================================================
# 8. MAP BASE + SHARED THEME — light background
# ============================================================

france <- ne_countries(country = "france", scale = "medium",
                       returnclass = "sf") %>%
  st_crop(xmin = -5.5, xmax = 9.6, ymin = 41.0, ymax = 51.5)

map_theme <- theme_void(base_family = "sans") +
  theme(
    plot.background  = element_rect(fill = BG_COLOR, color = NA),
    panel.background = element_rect(fill = BG_COLOR, color = NA),
    legend.position  = c(0.92, 0.25),
    legend.title     = element_text(color = TEXT_DARK, size = 36, face = "bold"),
    legend.text      = element_text(color = TEXT_SOFT, size = 30),
    legend.key.size  = unit(1.2, "cm"),
    plot.title       = element_text(color = TEXT_DARK, size = 22, face = "bold",
                                    hjust = 0.5, margin = margin(t = 12, b = 4)),
    plot.subtitle    = element_text(color = "#1B6CA8", size = 36,
                                    hjust = 0.5, margin = margin(b = 6)),
    plot.caption     = element_text(color = TEXT_SOFT, size = 28,
                                    hjust = 0.5, margin = margin(t = 6, b = 8)),
    plot.margin      = margin(10, 10, 10, 10)
  )

# ============================================================
# 9. FRAME BUILDER FUNCTION
# Accepts a single day's hospital data. Draws layers in order:
#   1. France polygon
#   2. All top-600 edges (grey) — static background network
#   3. Active edges (dark blue) — edges touching infected hospitals
#   4. All hospitals as grey dots sized by bed count
#   5. Infected hospitals coloured gold→red by bed occupancy
#   6. Halo on high-burden hospitals (>30% beds infected)
#   7. Date + stats annotations
# ============================================================

make_frame <- function(day_data, date_val,
                       active_edges = NULL) {  # NULL = compute from day_data
  
  infected <- day_data %>% filter(n_infected > 0)
  clean    <- day_data %>% filter(n_infected == 0)
  n_cases  <- nrow(infected)
  n_total  <- nrow(day_data)
  net_prev <- sum(day_data$n_infected) / sum(day_data$no_beds)
  
  # If no active_edges provided (static frames), compute on the fly
  if (is.null(active_edges)) {
    infected_set <- infected$finess_geo
    active_edges <- bind_rows(
      top_edges %>% filter(finessGeo_origin %in% infected_set),
      top_edges %>% filter(finessGeo_target %in% infected_set)
    ) %>% distinct(finessGeo_origin, finessGeo_target, .keep_all = TRUE)
  }
  
  ggplot() +
    
    # Layer 1: France country polygon (light grey-blue fill)
    geom_sf(data = france,
            fill = FRANCE_FILL, color = FRANCE_BORDER, linewidth = 0.3) +
    
    # Layer 2: All top-600 edges — faint grey, always visible as background
    geom_segment(
      data = top_edges,
      aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t),
      color = EDGE_INACTIVE, linewidth = 0.2, alpha = 0.4,
      inherit.aes = FALSE
    ) +
    
    # Layer 3: Active edges — dark blue, connecting hospitals with cases
    geom_segment(
      data = active_edges,
      aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t),
      color = EDGE_ACTIVE, linewidth = 0.5, alpha = 0.7,
      inherit.aes = FALSE
    ) +
    
    # Layer 4: All hospitals — grey dots, size proportional to bed count
    # Shown always so the full network structure is visible from day 0
    geom_point(
      data = day_data,
      aes(x = lon, y = lat, size = no_beds),
      color = NODE_CLEAN, alpha = 0.35, shape = 16
    ) +
    
    # Layer 5: Infected hospitals — gold→red gradient by bed occupancy,
    # size still proportional to hospital capacity (no_beds)
    geom_point(
      data = infected,
      aes(x = lon, y = lat, color = prevalence, size = no_beds),
      alpha = 0.90, shape = 16
    ) +
    
    # Layer 6: Halo on heavily burdened hospitals (>30% beds infected)
    # Subtle outer glow to highlight outbreak hotspots
    geom_point(
      data = infected %>% filter(prevalence > 0.30),
      aes(x = lon, y = lat, size = no_beds * 2.5),
      color = "#CC2200", alpha = 0.15, shape = 16
    ) +
    
    # Colour scale: gold (low occupancy) → dark red (high occupancy)
    scale_color_gradientn(
      name   = "Proportion of beds\nwith infected cases\n\n● Grey = 0 cases\n● Colour = ≥1 case",
      colors = c("#F5A623", "#E8650A", "#CC2200", "#8B0000"),
      limits = c(0.001, 1),
      labels = percent_format(accuracy = 1),
      guide  = guide_colorbar(barwidth = 3.6, barheight = 18,
                              title.position = "top")
    ) +
    
    # Size scale: proportional to no_beds, hidden from legend
    scale_size_continuous(range = NODE_SIZE_RANGE, guide = "none") +
    
    # Date stamp — top left
    annotate("text", x = -5.1, y = 51.0,
             label    = format(as.Date(date_val), "%d %b %Y"),
             color    = TEXT_DARK, size = 10, fontface = "bold", hjust = 0) +
    
    # Stats annotation — bottom left
    annotate("text", x = -5.1, y = 41.7,
             label = sprintf(
               "%d hospitals with cases (%.1f%% of network)\nNetwork prevalence: %.2f%%",
               n_cases, 100 * n_cases / n_total, 100 * net_prev),
             color = TEXT_SOFT, size = 6.5, hjust = 0, lineheight = 1.5) +
    
    # Lock axes to mainland France — prevents per-frame rescaling
    coord_sf(xlim = c(-5.5, 9.6), ylim = c(41.0, 51.5), expand = FALSE) +
    map_theme +
    labs(
      title    = "ARB Spread Across the French Hospital Network",
      subtitle = sprintf(
        "SIS Metapopulation Model  |  β = %.3f  |  Seed: random hospital",
        BETA_SIM),
      caption  = paste0(
        "Node size ∝ hospital capacity  |  ",
        "Colour = proportion of beds infected  |  ",
        "Dark blue edges = corridors linking hospitals with active cases")
    )
}

# ============================================================
# 10. STATIC SNAPSHOT SERIES (6 frames)
# Dates from SNAPSHOT_DATES are clipped to the steady-state
# date so no snapshot falls after the simulation ended.
# Each date is snapped to the nearest actually-simulated day.
# ============================================================

message("\nGenerating 6 static snapshots...")

snap         <- function(d) available_dates[which.min(abs(available_dates - d))]
static_dates <- sapply(SNAPSHOT_DATES, snap) %>%
  as.Date(origin = "1970-01-01") %>%
  unique()

for (i in seq_along(static_dates)) {
  d        <- static_dates[i]
  day_data <- sim_trajectory %>% filter(date == d)
  p        <- make_frame(day_data, d)   # active_edges computed internally
  fname    <- file.path(output_dir,
                        sprintf("network_snapshot_%02d_%s_%s.png", i, format(d, "%Y-%m-%d"), beta_tag))
  ggsave(fname, p, width = 14, height = 10, dpi = 200, bg = BG_COLOR)
  message("  Saved: ", basename(fname))
}

message("Static snapshots done.")

# ============================================================
# 10b. 3x2 PANEL — all 6 snapshots in one figure
# Each snapshot is rebuilt as a stripped-down frame (no title,
# no subtitle, no caption — those live only on the panel level).
# The date stamp is kept inside each panel. A black border is
# drawn around each panel via plot.background. patchwork
# assembles the 6 plots into a 3-column x 2-row grid with a
# single large shared title on top.
# ============================================================

message("Building 3x2 panel...")

library(patchwork)   # install.packages("patchwork") if needed

# Helper: builds one panel frame — map only, no title/subtitle/caption,
# date stamp and compact stats kept, black border via plot.background
make_panel_frame <- function(day_data, date_val, show_legend = FALSE) {
  
  infected     <- day_data %>% filter(n_infected > 0)
  n_cases      <- nrow(infected)
  n_total      <- nrow(day_data)
  net_prev     <- sum(day_data$n_infected) / sum(day_data$no_beds)
  infected_set <- infected$finess_geo
  
  active_edges <- bind_rows(
    top_edges %>% filter(finessGeo_origin %in% infected_set),
    top_edges %>% filter(finessGeo_target %in% infected_set)
  ) %>% distinct(finessGeo_origin, finessGeo_target, .keep_all = TRUE)
  
  ggplot() +
    geom_sf(data = france,
            fill = FRANCE_FILL, color = FRANCE_BORDER, linewidth = 0.3) +
    geom_segment(
      data = top_edges,
      aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t),
      color = EDGE_INACTIVE, linewidth = 0.2, alpha = 0.4,
      inherit.aes = FALSE
    ) +
    geom_segment(
      data = active_edges,
      aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t),
      color = EDGE_ACTIVE, linewidth = 0.5, alpha = 0.7,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = day_data,
      aes(x = lon, y = lat, size = no_beds),
      color = NODE_CLEAN, alpha = 0.35, shape = 16
    ) +
    geom_point(
      data = infected,
      aes(x = lon, y = lat, color = prevalence, size = no_beds),
      alpha = 0.90, shape = 16
    ) +
    geom_point(
      data = infected %>% filter(prevalence > 0.30),
      aes(x = lon, y = lat, size = no_beds * 2.5),
      color = "#CC2200", alpha = 0.15, shape = 16
    ) +
    scale_color_gradientn(
      name   = "Proportion of beds\nwith infected cases\n\n● Grey = 0 cases\n● Colour = ≥1 case",
      colors = c("#F5A623", "#E8650A", "#CC2200", "#8B0000"),
      limits = c(0.001, 1),
      labels = percent_format(accuracy = 1),
      guide  = guide_colorbar(barwidth = 1.2, barheight = 8,
                              title.position = "top")
    ) +
    scale_size_continuous(range = NODE_SIZE_RANGE, guide = "none") +
    # Date stamp — bold, top-left inside each panel
    annotate("text", x = -5.0, y = 51.0,
             label = format(as.Date(date_val), "%d %b %Y"),
             color = TEXT_DARK, size = 5, fontface = "bold", hjust = 0) +
    # Compact stats — bottom-left inside each panel
    annotate("text", x = -5.0, y = 41.8,
             label = sprintf("%d hospitals (%.1f%%)\nPrevalence: %.2f%%",
                             n_cases, 100 * n_cases / n_total, 100 * net_prev),
             color = TEXT_SOFT, size = 2.8, hjust = 0, lineheight = 1.4) +
    coord_sf(xlim = c(-5.5, 9.6), ylim = c(41.0, 51.5), expand = FALSE) +
    theme_void() +
    theme(
      # Black border drawn as the plot background outline
      plot.background  = element_rect(fill = BG_COLOR,
                                      color = "black", linewidth = 1.8),
      panel.background = element_rect(fill = BG_COLOR, color = NA),
      legend.position  = if (show_legend) c(1.12, 0.5) else "none",
      legend.title     = element_text(color = TEXT_DARK, size = 22, face = "bold"),
      legend.text      = element_text(color = TEXT_SOFT, size = 18),
      plot.margin      = margin(6, 6, 6, 6)  # space so the border shows clearly
    )
}

# Build the 6 individual panel frames
# Only the last frame (bottom right) shows the legend
panel_plots <- imap(static_dates, function(d, i) {
  make_panel_frame(sim_trajectory %>% filter(date == d), d,
                   show_legend = (i == length(static_dates)))
})

# Assemble 3 columns x 2 rows; shared title via plot_annotation
panel_grid <- wrap_plots(panel_plots, ncol = 3, nrow = 2) +
  plot_annotation(
    title = "ARB Spread Across the French Hospital Network",
    theme = theme(
      plot.title      = element_text(size = 28, face = "bold",
                                     hjust = 0.5, color = TEXT_DARK,
                                     margin = margin(b = 14)),
      plot.background = element_rect(fill = BG_COLOR, color = NA)
    )
  )

panel_fname <- file.path(output_dir, sprintf("network_panel_6snapshots_%s.png", beta_tag))
ggsave(panel_fname, panel_grid,
       width = 24, height = 16, dpi = 200, bg = BG_COLOR)
message("Panel saved: ", basename(panel_fname))

# ============================================================
# 11. ANIMATED GIF
# Uses pre-computed anim_edges (already filtered to infected
# endpoints per date) — avoids slow per-frame filtering inside
# gganimate. transition_time(date) drives one frame per date.
# ============================================================

message("\nPreparing animation...")

anim_data     <- sim_trajectory %>% filter(date %in% anim_dates)
infected_anim <- anim_data %>% filter(n_infected > 0)
all_anim      <- anim_data   # all hospitals (for size layer)

anim_plot <- ggplot() +
  
  # France base map — static
  geom_sf(data = france,
          fill = FRANCE_FILL, color = FRANCE_BORDER, linewidth = 0.3) +
  
  # All background edges — static grey layer
  geom_segment(
    data = top_edges,
    aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t),
    color = EDGE_INACTIVE, linewidth = 0.2, alpha = 0.4,
    inherit.aes = FALSE
  ) +
  
  # Active edges — animated, dark blue, tracked by hospital pair + date
  geom_segment(
    data = anim_edges,
    aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t,
        group = interaction(finessGeo_origin, finessGeo_target)),
    color = EDGE_ACTIVE, linewidth = 0.5, alpha = 0.7,
    inherit.aes = FALSE
  ) +
  
  # All hospitals — size by bed count, grey; group tracks each dot across frames
  geom_point(
    data = all_anim,
    aes(x = lon, y = lat, size = no_beds, group = finess_geo),
    color = NODE_CLEAN, alpha = 0.35, shape = 16
  ) +
  
  # Infected hospitals — colour by prevalence, size by capacity
  geom_point(
    data = infected_anim,
    aes(x = lon, y = lat, color = prevalence,
        size = no_beds, group = finess_geo),
    alpha = 0.90, shape = 16
  ) +
  
  # Halo on high-burden hospitals (>30%)
  geom_point(
    data = infected_anim %>% filter(prevalence > 0.30),
    aes(x = lon, y = lat, size = no_beds * 2.5, group = finess_geo),
    color = "#CC2200", alpha = 0.15, shape = 16
  ) +
  
  scale_color_gradientn(
    name   = "Proportion of beds\nwith infected cases\n\n● Grey = 0 cases\n● Colour = ≥1 case",
    colors = c("#F5A623", "#E8650A", "#CC2200", "#8B0000"),
    limits = c(0.001, 1),
    labels = percent_format(accuracy = 1),
    guide  = guide_colorbar(barwidth = 3.6, barheight = 18,
                            title.position = "top")
  ) +
  scale_size_continuous(range = NODE_SIZE_RANGE, guide = "none") +
  coord_sf(xlim = c(-5.5, 9.6), ylim = c(41.0, 51.5), expand = FALSE) +
  map_theme +
  labs(
    title   = "ARB Spread Across the French Hospital Network",
    caption = paste0(
      "Node size ∝ hospital capacity  |  ",
      "Colour = proportion beds infected  |  ",
      "Dark blue edges = corridors with active cases")
  ) +
  
  # gganimate: advance one frame per unique date in the data
  transition_time(date) +
  
  # Dynamic subtitle shows current date — {frame_time} is injected by gganimate
  labs(subtitle = sprintf(
    "SIS Metapopulation Model  |  β = %.3f  |  {format(frame_time, '%%d %%b %%Y')}",
    BETA_SIM)) +
  
  # Linear easing — no slow-in / slow-out between frames
  ease_aes("linear")

n_frames <- length(anim_dates)
message("Rendering GIF: ", n_frames, " frames at ", ANIM_FPS, "fps  ≈ ",
        round(n_frames / ANIM_FPS), " seconds")
message("Estimated rendering time: ~5–15 minutes")

animate(
  anim_plot,
  nframes  = n_frames,
  fps      = ANIM_FPS,
  width    = 1400,
  height   = 1000,
  renderer = gifski_renderer(
    file.path(output_dir, sprintf("network_animation_%s.gif", beta_tag))),
  bg       = BG_COLOR
)

message("GIF saved.")

# ============================================================
# 12. HIGH-RES FINAL STATE PNG
# Shows the network at steady state — the last simulated day.
# ============================================================

final_state <- sim_trajectory %>% filter(date == max(date))
p_final     <- make_frame(final_state, max(sim_trajectory$date))
ggsave(file.path(output_dir, sprintf("network_final_state_%s.png", beta_tag)),
       p_final, width = 16, height = 11, dpi = 300, bg = BG_COLOR)

message("Final state PNG saved.")
message("\n✓ All done. Outputs saved to:\n  ", output_dir)