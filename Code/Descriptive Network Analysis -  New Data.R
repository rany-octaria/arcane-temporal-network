# ============================================================
# ARCANE — French Hospital Transfer Network
#          Descriptive & Exploratory Analysis (2024)
# Author : Rany Octaria — MESURS Lab, CNAM
# Updated: 2026
#
# Prerequisites:
#   • Open from the ARCANE-Temporal-Network RStudio project
#   • Download transfer datasets from the shared SharePoint
#     and place in Datasets/MCO_SSR_HBN_2024/
#
# All outputs (plots, tables, RDS cache) are saved to:
#   Outputs/Descriptive Analysis/
#   (folder is created automatically if it does not exist)
#
# install.packages(c("tidyverse","lubridate","here","rlang",
#   "scales","igraph","sf","rnaturalearth","rnaturalearthdata"))
# ============================================================



# ============================================================
# 0. LIBRARIES AND CONFIGURATION
# ============================================================
# ── Core data wrangling ───────────────────────────────────────
library(tidyverse)     # dplyr, ggplot2, tidyr, purrr, readr, stringr
library(lubridate)     # date handling
library(here)          # project-relative paths
library(rlang)         # tidy evaluation (enquo)
library(scales)        # axis formatting (comma, percent)

# ── Network analysis ─────────────────────────────────────────
library(igraph)        # graph construction + centrality metrics

# ── Spatial / mapping ────────────────────────────────────────
library(sf)                  # spatial features + coordinate transformation
library(rnaturalearth)       # France country shapefile
library(rnaturalearthdata)   # data backend for rnaturalearth

options(scipen = 999)        # suppress scientific notation

# ── PPT-friendly plot theme ──────────────────────────────────
# Apply this theme to every plot for consistent, readable slides.
# Adjust BASE_SIZE once here to resize all plot text globally.
BASE_SIZE <- 14

theme_ppt <- function(base_size = BASE_SIZE) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size * 1.3,
                                      hjust = 0.5, margin = margin(b = 8)),
      plot.subtitle    = element_text(size = base_size * 1.0, hjust = 0.5,
                                      color = "grey40", margin = margin(b = 6)),
      plot.caption     = element_text(size = base_size * 0.75, color = "grey60"),
      axis.title       = element_text(size = base_size * 1.0, face = "bold"),
      axis.text        = element_text(size = base_size * 0.85),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      legend.title     = element_text(size = base_size * 0.95, face = "bold"),
      legend.text      = element_text(size = base_size * 0.85),
      strip.text       = element_text(size = base_size * 0.90, face = "bold"),
      strip.background = element_rect(fill = "#f0f0f0", color = NA),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

# ── Output directory ─────────────────────────────────────────
# All plots and files saved here; created automatically if it does not exist
OUT_DIR <- here("Outputs", "Descriptive Analysis")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
message("Output directory: ", OUT_DIR)

# ── Shared colour palettes ────────────────────────────────────
season_colors <- c(
  Winter = "#1f78b4", Spring = "#33a02c",
  Summer = "#ff7f00", Autumn = "#e31a1c"
)

# Colour palette for hospital types (Okabe-Ito colorblind-safe)
# Hospital type colours — high-contrast, colorblind-safe (Okabe-Ito extended)
# Hues spread across the full wheel; tested against deuteranopia & protanopia.
# MCO      = cobalt blue    — bright, saturated, anchors the scale
# SSR      = vermilion      — CB-safe warm red (not green-confused)
# MCO/SSR  = deep magenta   — clearly halfway between blue and red
# PSY      = bright amber   — high luminance, distinct from all above
# HAD      = cyan-teal      — cool, maximally distinct from red/orange
# Other    = dark grey      — neutral
HOSP_COLORS <- c(
  "MCO"      = "#0057B8",   # cobalt blue
  "SSR"      = "#C1392B",   # vermilion red (CB-safe)
  "MCO/SSR"  = "#9B1D8A",   # deep magenta (midpoint blue–red)
  "PSY"      = "#F5A623",   # bright amber
  "HAD"      = "#00B4D8",   # vivid cyan-teal
  "Other"    = "#555555"    # dark charcoal grey
)


# ============================================================
# 1. LOAD DATA
# ============================================================
# ── Weekly sliding edge list ──────────────────────────────────
# Each row = one directed hospital pair in one 7-day rolling window.
# Columns: finessGeo_origin, finessGeo_target, weight,
#          window_start, window_end, weekly_window_number
weekly <- read_csv(
  here("Datasets", "MCO_SSR_HBN_2024", "MCO_SSR_HBN_Direct_2024",
       "HBN_weekly_sliding_edgelist_2024.csv"),
  show_col_types = FALSE
) %>%
  # Convert rolling 7-day sum to daily average (divide by 7, keep as integer)
  # pmax(1L, ...) ensures no edge weight drops to 0 after rounding
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

# ── Monthly sliding edge list ─────────────────────────────────
monthly <- read_csv(
  here("Datasets", "MCO_SSR_HBN_2024", "MCO_SSR_HBN_Direct_2024",
       "HBN_monthly_sliding_edgelist_2024.csv"),
  show_col_types = FALSE
) %>%
  # Convert rolling 7-day sum to daily average
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

# ── Hospital metadata (Finess Géographique) ───────────────────
# Columns: finessgeo, n_medical_units, n_ward_types, total_beds_mco,
#          coordxet (Lambert-93 x), coordyet (Lambert-93 y), hospital_type
# Note: coordinates are Lambert-93 (EPSG:2154) — converted to WGS84 below
finess_geo <- read_csv(
  here("Datasets", "MCO_SSR_HBN_2024", "finessgeo_metadata_2024.csv"),
  show_col_types = FALSE
) %>%
  rename_with(tolower) %>%              # ensure lowercase column names
  rename(finess_geo = finessgeo)        # standardise to match transfer data

# ── Convert Lambert-93 coordinates to WGS84 lon/lat ──────────
# coordxet and coordyet are projected (EPSG:2154) — need lon/lat for maps.
finess_geo_sf <- finess_geo %>%
  filter(!is.na(coordxet), !is.na(coordyet)) %>%
  st_as_sf(coords = c("coordxet", "coordyet"), crs = 2154) %>%
  st_transform(crs = 4326)

finess_geo <- finess_geo %>%
  left_join(
    finess_geo_sf %>%
      st_drop_geometry() %>%
      bind_cols(
        as_tibble(st_coordinates(finess_geo_sf)) %>%
          rename(lon = X, lat = Y)
      ) %>%
      select(finess_geo, lon, lat),
    by = "finess_geo"
  ) %>%
  mutate(
    # Standardise hospital_type — fill any blanks as "Other"
    hospital_type = if_else(
      is.na(hospital_type) | hospital_type == "",
      "Other", hospital_type
    )
  )

# ── Quick data check ──────────────────────────────────────────
message("Weekly rows: ",  nrow(weekly),
        " | Windows: ",   n_distinct(weekly$weekly_window_number))
message("Monthly rows: ", nrow(monthly),
        " | Windows: ",   n_distinct(monthly$monthly_window_number))
message("Hospitals in metadata: ", nrow(finess_geo),
        " | Types: ", paste(unique(finess_geo$hospital_type), collapse = ", "))


# ============================================================
# 2. NETWORK SUMMARY STATISTICS
# 2.1 Summary function
# ============================================================
# Computes per-window summary statistics (edges, nodes, transfers).
# unique_edges = TRUE counts each directed pair once per window.
summarize_transfer_network <- function(data, unique_edges = TRUE) {
  data %>%
    mutate(
      window_start = as.Date(window_start),
      month_num = month(window_start),
      season = case_when(
        month_num %in% c(12, 1, 2)  ~ "Winter",
        month_num %in% c(3, 4, 5)   ~ "Spring",
        month_num %in% c(6, 7, 8)   ~ "Summer",
        month_num %in% c(9, 10, 11) ~ "Autumn"
      ),
      season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))
    ) %>%
    group_by(window_start, month_num, season) %>%
    summarise(
      n_edges          = if (unique_edges)
        n_distinct(paste(finessGeo_origin, finessGeo_target))
      else n(),
      n_senders        = n_distinct(finessGeo_origin),
      n_receivers      = n_distinct(finessGeo_target),
      n_nodes          = n_distinct(c(finessGeo_origin, finessGeo_target)),
      total_transfers  = sum(weight, na.rm = TRUE),
      mean_transfers   = mean(weight, na.rm = TRUE),
      median_transfers = median(weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(density = n_edges / (n_nodes * (n_nodes - 1))) %>%
    arrange(window_start)
}

summary_weekly  <- summarize_transfer_network(weekly)
summary_monthly <- summarize_transfer_network(monthly)


# 2.2 Temporal trends — all hospitals
# Helper: plot any numeric metric over time, coloured by season
plot_metric_trend <- function(data, metric, period_label, date_col = "window_start") {
  
  label_map <- c(
    n_edges          = "Number of unique transfer corridors",
    n_nodes          = "Number of active hospitals",
    n_senders        = "Number of sending hospitals",
    n_receivers      = "Number of receiving hospitals",
    total_transfers  = "Total patient transfers",
    mean_transfers   = "Mean transfers per corridor",
    median_transfers = "Median transfers per corridor",
    density          = "Network density"
  )
  nice_label <- label_map[[metric]] %||% metric
  
  ggplot(data, aes(x = .data[[date_col]], y = .data[[metric]],
                   color = season, group = 1)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.5) +
    scale_color_manual(values = season_colors, name = "Season") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_y_continuous(labels = comma) +
    labs(
      title    = paste(period_label, "trend:", nice_label),
      subtitle = "French hospital direct transfer network, 2024",
      x = NULL, y = nice_label
    ) +
    theme_ppt()
}

# ── Weekly plots ──────────────────────────────────────────────
metrics_to_plot <- c("n_nodes", "n_edges", "total_transfers",
                     "mean_transfers", "density")

weekly_plots <- map(metrics_to_plot, function(m) {
  p <- plot_metric_trend(summary_weekly, m, "Weekly")
  ggsave(here("Outputs", "Descriptive Analysis", paste0("weekly_", m, ".png")),
         p, width = 13, height = 7, dpi = 200)
  p
})



# 2.3 Temporal trends — by hospital type
# Join hospital_type to each transfer edge (from the origin hospital).
# Both origin and target types are attached so we can stratify either way.
weekly_typed <- weekly %>%
  left_join(finess_geo %>% select(finess_geo, hospital_type),
            by = c("finessGeo_origin" = "finess_geo")) %>%
  rename(type_origin = hospital_type) %>%
  left_join(finess_geo %>% select(finess_geo, hospital_type),
            by = c("finessGeo_target" = "finess_geo")) %>%
  rename(type_target = hospital_type)

# Summarise by origin hospital type per window
summary_by_type <- weekly_typed %>%
  filter(!is.na(type_origin)) %>%
  mutate(
    window_start = as.Date(window_start),
    season = case_when(
      month(window_start) %in% c(12, 1, 2)  ~ "Winter",
      month(window_start) %in% c(3, 4, 5)   ~ "Spring",
      month(window_start) %in% c(6, 7, 8)   ~ "Summer",
      TRUE                                    ~ "Autumn"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  ) %>%
  group_by(window_start, season, hospital_type = type_origin) %>%
  summarise(
    n_edges         = n_distinct(paste(finessGeo_origin, finessGeo_target)),
    n_senders       = n_distinct(finessGeo_origin),
    total_transfers = sum(weight, na.rm = TRUE),
    mean_transfers  = mean(weight, na.rm = TRUE),
    .groups = "drop"
  )

# Plot: total transfers per window, faceted by hospital type
p_type_transfers <- ggplot(
  summary_by_type,
  aes(x = window_start, y = total_transfers,
      color = hospital_type, group = hospital_type)
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  facet_wrap(~ hospital_type, scales = "free_y", ncol = 2) +
  scale_color_manual(values = HOSP_COLORS, name = "Hospital type") +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Weekly total transfers by hospital type",
    subtitle = "French hospital network 2024 | Faceted by sending hospital type",
    x = NULL, y = "Total patient transfers"
  ) +
  theme_ppt() +
  theme(legend.position = "none")

print(p_type_transfers)
ggsave(here("Outputs", "Descriptive Analysis", "weekly_transfers_by_type.png"),
       p_type_transfers, width = 14, height = 10, dpi = 200)

# Plot: number of sending hospitals per window, by type
p_type_senders <- ggplot(
  summary_by_type,
  aes(x = window_start, y = n_senders,
      color = hospital_type, group = hospital_type)
) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ hospital_type, scales = "free_y", ncol = 2) +
  scale_color_manual(values = HOSP_COLORS, name = "Hospital type") +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Active sending hospitals per window by hospital type",
    subtitle = "French hospital network 2024",
    x = NULL, y = "Number of sending hospitals"
  ) +
  theme_ppt() +
  theme(legend.position = "none")

print(p_type_senders)
ggsave(here("Outputs", "Descriptive Analysis", "weekly_senders_by_type.png"),
       p_type_senders, width = 14, height = 10, dpi = 200)


# ============================================================
# 3. NETWORK METRICS (GRAPH THEORY)
# ============================================================
# Computes per-window graph metrics (density, degree, transitivity, etc.)
# NOTE: This is computationally expensive. Results are cached as RDS files.
# If running for the first time, uncomment the calculation block.
calculate_network_metrics <- function(df) {
  df %>%
    mutate(window_start = as.Date(window_start),
           weight       = as.numeric(weight)) %>%
    group_by(window_start) %>%
    group_split() %>%
    map_dfr(function(time_df) {
      
      g <- igraph::graph_from_data_frame(
        d        = time_df %>% select(finessGeo_origin, finessGeo_target, weight),
        directed = TRUE
      )
      
      # Weighted average shortest path (finite paths only)
      avg_path <- tryCatch({
        dmat <- igraph::distances(g, mode = "all", weights = E(g)$weight)
        dvec <- as.vector(dmat)
        dvec <- dvec[is.finite(dvec) & dvec > 0]
        if (length(dvec) == 0) NA_real_ else mean(dvec)
      }, error = function(e) NA_real_)
      
      deg_in  <- igraph::degree(g, mode = "in")
      deg_out <- igraph::degree(g, mode = "out")
      str_in  <- igraph::strength(g, mode = "in",  weights = E(g)$weight)
      str_out <- igraph::strength(g, mode = "out", weights = E(g)$weight)
      
      tibble(
        time_window = unique(time_df$window_start),
        # Size
        num_nodes              = vcount(g),
        num_edges              = ecount(g),
        # Connectivity
        density                = edge_density(g),
        reciprocity            = tryCatch(igraph::reciprocity(g, ignore.loops = TRUE),
                                          error = function(e) NA_real_),
        n_components_weak      = components(g, mode = "weak")$no,
        n_components_strong    = components(g, mode = "strong")$no,
        # Degree
        avg_degree_in          = mean(deg_in),
        avg_degree_out         = mean(deg_out),
        max_degree_in          = max(deg_in,  na.rm = TRUE),
        max_degree_out         = max(deg_out, na.rm = TRUE),
        # Weighted strength
        avg_strength_in        = mean(str_in,  na.rm = TRUE),
        avg_strength_out       = mean(str_out, na.rm = TRUE),
        max_strength_in        = max(str_in,   na.rm = TRUE),
        max_strength_out       = max(str_out,  na.rm = TRUE),
        # Paths
        avg_path_length        = avg_path,
        diameter_weighted      = tryCatch({
          max(igraph::distances(g, mode = "all",
                                weights = E(g)$weight)[is.finite(
                                  igraph::distances(g, mode = "all",
                                                    weights = E(g)$weight))],
              na.rm = TRUE)
        }, error = function(e) NA_real_),
        # Clustering
        transitivity_global    = tryCatch(igraph::transitivity(g, type = "global"),
                                          error = function(e) NA_real_),
        transitivity_local_mean = mean(
          tryCatch(igraph::transitivity(g, type = "local", isolates = "zero"),
                   error = function(e) rep(NA_real_, vcount(g))),
          na.rm = TRUE),
        # Centralization & assortativity
        centralization_in      = tryCatch(
          igraph::centr_degree(g, mode = "in",  normalized = TRUE)$centralization,
          error = function(e) NA_real_),
        centralization_out     = tryCatch(
          igraph::centr_degree(g, mode = "out", normalized = TRUE)$centralization,
          error = function(e) NA_real_),
        assortativity_degree   = tryCatch(
          igraph::assortativity_degree(g, directed = TRUE),
          error = function(e) NA_real_),
        constraint_mean        = tryCatch(
          mean(igraph::constraint(g), na.rm = TRUE),
          error = function(e) NA_real_)
      )
    }) %>%
    arrange(time_window)
}

# ── Load from cache or compute ────────────────────────────────
rds_weekly  <- here("Outputs", "Descriptive Analysis", "network_metrics_weekly.rds")
rds_monthly <- here("Outputs", "Descriptive Analysis", "network_metrics_monthly.rds")

if (file.exists(rds_weekly)) {
  network_metrics_weekly  <- readRDS(rds_weekly)
  network_metrics_monthly <- readRDS(rds_monthly)
  message("Loaded cached network metrics.")
} else {
  message("Computing network metrics (may take several minutes)...")
  network_metrics_weekly  <- calculate_network_metrics(weekly)
  network_metrics_monthly <- calculate_network_metrics(monthly)
  saveRDS(network_metrics_weekly,  rds_weekly)
  saveRDS(network_metrics_monthly, rds_monthly)
  message("Network metrics saved.")
}


# 3.2 Plot network metrics over time
metric_labels <- c(
  num_nodes              = "Active hospitals (nodes)",
  num_edges              = "Transfer corridors (edges)",
  density                = "Network density",
  reciprocity            = "Reciprocity",
  n_components_weak      = "Weakly connected components",
  n_components_strong    = "Strongly connected components",
  avg_degree_in          = "Mean in-degree",
  avg_degree_out         = "Mean out-degree",
  avg_strength_in        = "Mean in-strength (weighted)",
  avg_strength_out       = "Mean out-strength (weighted)",
  avg_path_length        = "Mean shortest path length (weighted)",
  transitivity_global    = "Global transitivity (clustering)",
  centralization_in      = "In-degree centralization",
  assortativity_degree   = "Degree assortativity"
)

plot_network_metric <- function(data, metric,
                                period_label = "Weekly",
                                date_col     = "time_window") {
  nice_label <- metric_labels[[metric]] %||% metric
  
  data %>%
    mutate(
      date   = as.Date(.data[[date_col]]),
      season = case_when(
        month(date) %in% c(12, 1, 2)  ~ "Winter",
        month(date) %in% c(3, 4, 5)   ~ "Spring",
        month(date) %in% c(6, 7, 8)   ~ "Summer",
        TRUE                            ~ "Autumn"
      ),
      season = factor(season, levels = c("Winter","Spring","Summer","Autumn"))
    ) %>%
    ggplot(aes(x = date, y = .data[[metric]],
               color = season, group = 1)) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 1.5, na.rm = TRUE) +
    scale_color_manual(values = season_colors, name = "Season") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_y_continuous(labels = comma) +
    labs(
      title    = paste(period_label, "trend:", nice_label),
      subtitle = "French hospital transfer network, 2024",
      x = NULL, y = nice_label
    ) +
    theme_ppt()
}

# Print and save key metrics
key_metrics <- c("num_nodes", "num_edges", "density", "reciprocity",
                 "avg_degree_in", "avg_strength_in",
                 "transitivity_global", "assortativity_degree")

walk(key_metrics, function(m) {
  p <- plot_network_metric(network_metrics_weekly, m, "Weekly")
  print(p)
  ggsave(here("Outputs", "Descriptive Analysis", paste0("network_weekly_", m, ".png")),
         p, width = 13, height = 7, dpi = 200)
})


# ============================================================
# 4. NETWORK VISUALISATION ON FRANCE MAP
# 4.1 Year-aggregate network — all hospitals
# ============================================================
# France polygon (mainland)
france <- ne_countries(country = "france", scale = "medium",
                       returnclass = "sf") %>%
  st_crop(xmin = -5.5, xmax = 9.6, ymin = 41.0, ymax = 51.5)

# ── Build year-aggregate edge list ───────────────────────────
edge_year <- weekly %>%
  transmute(
    sender   = as.character(finessGeo_origin),
    receiver = as.character(finessGeo_target),
    weight   = as.numeric(weight)
  ) %>%
  filter(!is.na(sender), !is.na(receiver)) %>%
  group_by(sender, receiver) %>%
  summarise(total_weight = sum(weight), .groups = "drop")

# ── Node table with WGS84 coordinates ────────────────────────
nodes_geo <- finess_geo %>%
  filter(!is.na(lon), !is.na(lat),
         lon > -5.5, lon < 9.6,
         lat >  41,  lat < 51.5) %>%
  select(finess_geo, lon, lat, hospital_type, total_beds_mco) %>%
  mutate(total_beds_mco = replace_na(total_beds_mco, 0))

# ── Join coordinates to edges ─────────────────────────────────
edges_geo <- edge_year %>%
  left_join(nodes_geo %>% transmute(sender   = finess_geo,
                                    lon_o = lon, lat_o = lat),
            by = "sender") %>%
  left_join(nodes_geo %>% transmute(receiver = finess_geo,
                                    lon_t = lon, lat_t = lat),
            by = "receiver") %>%
  filter(!is.na(lon_o), !is.na(lon_t))

# ── Keep only strongest edges to avoid overplotting ──────────
# Top 2,000 corridors by total annual transfers
N_EDGES_PLOT <- 2000

edges_plot <- edges_geo %>%
  slice_max(order_by = total_weight, n = N_EDGES_PLOT) %>%
  mutate(weight_scaled = scales::rescale(log1p(total_weight), to = c(0.2, 3.2)))

# ── Map ───────────────────────────────────────────────────────
p_network_overall <- ggplot() +
  geom_sf(data = france,
          fill = "#F0F4F8", color = "#B0BFCC", linewidth = 0.4) +
  # Edges (strongest corridors)
  geom_segment(
    data = edges_plot,
    aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t,
        linewidth = weight_scaled),
    color = "#1B4F72", alpha = 0.18
  ) +
  # All hospitals (grey background)
  geom_point(
    data = nodes_geo,
    aes(x = lon, y = lat),
    color = "#7F8C8D", size = 0.8, alpha = 0.4
  ) +
  scale_linewidth_continuous(
    range  = c(0.2, 3.2),
    name   = "log(transfers + 1)",
    labels = function(x) round(expm1(x / 1.5 * log1p(max(edges_geo$total_weight))), -2)
  ) +
  coord_sf(xlim = c(-5.5, 9.6), ylim = c(41.0, 51.5), expand = FALSE) +
  theme_void(base_size = BASE_SIZE) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = BASE_SIZE * 0.9),
    plot.title      = element_text(face = "bold", size = BASE_SIZE * 1.3,
                                   hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle   = element_text(size = BASE_SIZE, hjust = 0.5,
                                   color = "grey40")
  ) +
  labs(
    title    = "French Hospital Transfer Network — Annual Aggregate (2024)",
    subtitle = paste0("Top ", N_EDGES_PLOT,
                      " transfer corridors by annual volume | ",
                      nrow(nodes_geo), " hospitals shown")
  )

print(p_network_overall)
ggsave(here("Outputs", "Descriptive Analysis", "network_overall.png"),
       p_network_overall, width = 14, height = 10, dpi = 250)


# 4.2 Network coloured by hospital type
# Same map but nodes coloured and sized by hospital_type.
# Only hospitals present in the top edges are labelled by type.
# Edges are coloured by the sending hospital's type.

edges_plot_typed <- edges_plot %>%
  left_join(nodes_geo %>% select(finess_geo, type_o = hospital_type),
            by = c("sender" = "finess_geo")) %>%
  mutate(type_o = replace_na(type_o, "Other"))

p_network_by_type <- ggplot() +
  geom_sf(data = france,
          fill = "#F0F4F8", color = "#B0BFCC", linewidth = 0.4) +
  # Edges coloured by sending hospital type
  geom_segment(
    data = edges_plot_typed,
    aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t,
        color    = type_o,
        linewidth = weight_scaled),
    alpha = 0.20
  ) +
  # All hospitals — size proportional to bed count, colour by type
  geom_point(
    data = nodes_geo,
    aes(x = lon, y = lat,
        color = hospital_type,
        size  = pmax(total_beds_mco, 1)),
    alpha = 0.70
  ) +
  scale_color_manual(
    values = HOSP_COLORS,
    name   = "Hospital type",
    na.value = "#999999"
  ) +
  scale_size_continuous(
    range  = c(0.5, 10),
    name   = "MCO beds",
    labels = comma
  ) +
  scale_linewidth_continuous(range = c(0.2, 3.2), guide = "none") +
  coord_sf(xlim = c(-5.5, 9.6), ylim = c(41.0, 51.5), expand = FALSE) +
  guides(
    color = guide_legend(override.aes = list(size = 4, alpha = 1)),
    size  = guide_legend(override.aes = list(alpha = 0.8))
  ) +
  theme_void(base_size = BASE_SIZE) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = BASE_SIZE * 0.9),
    legend.text     = element_text(size = BASE_SIZE * 0.80),
    plot.title      = element_text(face = "bold", size = BASE_SIZE * 1.3,
                                   hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle   = element_text(size = BASE_SIZE, hjust = 0.5,
                                   color = "grey40"),
    plot.caption    = element_text(size = BASE_SIZE * 0.75,
                                   color = "grey60", hjust = 0.5)
  ) +
  labs(
    title    = "French Hospital Transfer Network by Hospital Type (2024)",
    subtitle = paste0("Top ", N_EDGES_PLOT,
                      " corridors | Colour = hospital type | Size = MCO beds"),
    caption  = "Edge colour = sending hospital type"
  )

print(p_network_by_type)
ggsave(here("Outputs", "Descriptive Analysis", "network_by_hospital_type.png"),
       p_network_by_type, width = 14, height = 10, dpi = 250)


# ============================================================
# 5. ANALYSIS BY HOSPITAL TYPE
# 5.1 Descriptive statistics per hospital type
# ============================================================
# Count hospitals, median beds, and transfer volumes per type
hosp_type_stats <- finess_geo %>%
  group_by(hospital_type) %>%
  summarise(
    n_hospitals   = n(),
    median_beds   = median(total_beds_mco, na.rm = TRUE),
    mean_beds     = mean(total_beds_mco,   na.rm = TRUE),
    n_with_coords = sum(!is.na(lon)),
    .groups = "drop"
  ) %>%
  arrange(desc(n_hospitals))

# Print summary table to console
print(hosp_type_stats)

# Bar chart: number of hospitals per type
p_type_counts <- ggplot(
  hosp_type_stats,
  aes(x = fct_reorder(hospital_type, n_hospitals),
      y = n_hospitals, fill = hospital_type)
) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = comma(n_hospitals)),
            hjust = -0.15, size = BASE_SIZE * 0.30) +
  scale_fill_manual(values = HOSP_COLORS) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title = "Number of hospitals by type",
    x = NULL, y = "Number of hospitals"
  ) +
  theme_ppt() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(p_type_counts)
ggsave(here("Outputs", "Descriptive Analysis", "hospital_counts_by_type.png"),
       p_type_counts, width = 11, height = 6, dpi = 200)


# 5.2 Transfer volume and degree by hospital type
# Compute annual out-degree and total out-strength per hospital,
# then summarise by hospital type.

annual_node_metrics <- weekly %>%
  group_by(finess_geo = as.character(finessGeo_origin)) %>%
  summarise(
    out_degree   = n_distinct(finessGeo_target),   # unique destinations
    out_strength = sum(weight, na.rm = TRUE),       # total outgoing patients
    .groups = "drop"
  ) %>%
  bind_rows(
    weekly %>%
      group_by(finess_geo = as.character(finessGeo_target)) %>%
      summarise(
        in_degree   = n_distinct(finessGeo_origin),
        in_strength = sum(weight, na.rm = TRUE),
        .groups = "drop"
      )
  ) %>%
  group_by(finess_geo) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  left_join(finess_geo %>% select(finess_geo, hospital_type, total_beds_mco),
            by = "finess_geo") %>%
  filter(!is.na(hospital_type))

# Box plot: out-strength (total sent patients) per type
p_strength_type <- ggplot(
  annual_node_metrics %>% filter(out_strength > 0),
  aes(x = fct_reorder(hospital_type, out_strength, median),
      y = out_strength, fill = hospital_type)
) +
  geom_violin(alpha = 0.45, linewidth = 0.5, scale = "width") +
  geom_boxplot(width = 0.18, outlier.size = 0.8, alpha = 0.85,
               color = "grey20", linewidth = 0.4) +
  scale_fill_manual(values = HOSP_COLORS, guide = "none") +
  scale_y_log10(labels = comma) +
  coord_flip() +
  labs(
    title    = "Annual outgoing patient volume by hospital type",
    subtitle = "Log scale | Box = median + IQR | Violin = full distribution",
    x = NULL, y = "Total outgoing transfers (log scale)"
  ) +
  theme_ppt() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(p_strength_type)
ggsave(here("Outputs", "Descriptive Analysis", "outstrength_by_type.png"),
       p_strength_type, width = 12, height = 7, dpi = 200)

# Box plot: out-degree (unique receiving hospitals)
p_degree_type <- ggplot(
  annual_node_metrics %>% filter(out_degree > 0),
  aes(x = fct_reorder(hospital_type, out_degree, median),
      y = out_degree, fill = hospital_type)
) +
  geom_violin(alpha = 0.45, linewidth = 0.5, scale = "width") +
  geom_boxplot(width = 0.18, outlier.size = 0.8, alpha = 0.85,
               color = "grey20", linewidth = 0.4) +
  scale_fill_manual(values = HOSP_COLORS, guide = "none") +
  coord_flip() +
  labs(
    title    = "Number of unique receiving hospitals by sender type",
    subtitle = "Out-degree = distinct destination hospitals per year",
    x = NULL, y = "Out-degree (unique destinations)"
  ) +
  theme_ppt() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(p_degree_type)
ggsave(here("Outputs", "Descriptive Analysis", "outdegree_by_type.png"),
       p_degree_type, width = 12, height = 7, dpi = 200)


# 5.3 Transfer flows between hospital types
# Count transfers between each pair of hospital types (origin → target)
type_flow <- weekly_typed %>%
  filter(!is.na(type_origin), !is.na(type_target)) %>%
  group_by(type_origin, type_target) %>%
  summarise(total = sum(weight, na.rm = TRUE), .groups = "drop")

# Heatmap of inter-type flows
p_type_flow <- ggplot(
  type_flow,
  aes(x = type_target, y = type_origin, fill = log1p(total))
) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = comma(total, accuracy = 1)),
            size = BASE_SIZE * 0.30, color = "white", fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#EBF5FB", "#2E86C1", "#1B4F72"),
    name   = "log(transfers + 1)"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Patient transfer flows between hospital types (2024)",
    subtitle = "Cell = total annual transfers | Row = sending type | Column = receiving type",
    x = "Receiving hospital type",
    y = "Sending hospital type"
  ) +
  theme_ppt() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5),
    panel.grid   = element_blank()
  )

print(p_type_flow)
ggsave(here("Outputs", "Descriptive Analysis", "flow_between_types_heatmap.png"),
       p_type_flow, width = 10, height = 8, dpi = 200)



# ============================================================
# 6. NETWORK MAP — FACILITIES COLOURED BY FRENCH REGION
#    Uses rnaturalearth ne_states() for region polygons.
#    Hospitals are spatially joined to their region via sf.
#    Edges show top transfer corridors coloured by region.
# ============================================================

# French administrative regions (13 metropolitan only, mainland)
france_regions <- ne_states(country = "france", returnclass = "sf") %>%
  filter(!name %in% c(
    "Guadeloupe", "Martinique", "Guyane", "La Reunion", "Mayotte",
    "Saint-Pierre-et-Miquelon", "Saint-Barthelemy", "Saint-Martin",
    "Wallis-et-Futuna", "Polynesie francaise",
    "Nouvelle-Caledonie", "Terres australes"
  )) %>%
  st_crop(xmin = -5.5, xmax = 9.6, ymin = 41.0, ymax = 51.5)

message("French regions loaded: ", nrow(france_regions))

# Convert hospital lon/lat to sf, spatially join to region polygons
nodes_sf <- nodes_geo %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

nodes_with_region <- nodes_sf %>%
  st_join(france_regions %>% select(region = name), join = st_within) %>%
  st_drop_geometry() %>%
  mutate(region = replace_na(region, "Unknown"))

message("Regions assigned: ", n_distinct(nodes_with_region$region))

# High-contrast qualitative palette for regions
n_regions <- n_distinct(nodes_with_region$region)
set.seed(42)
region_palette <- setNames(
  colorRampPalette(c(
    "#E63946", "#457B9D", "#2A9D8F", "#E9C46A", "#F4A261",
    "#264653", "#9B2335", "#A8DADC", "#6A0572", "#1D3557",
    "#F77F00", "#4CC9F0", "#7209B7", "#06A77D", "#D62828"
  ))(n_regions),
  sort(unique(nodes_with_region$region))
)

# Node and edge data with region attached
nodes_region_plot <- nodes_with_region %>%
  filter(lon > -5.5, lon < 9.6, lat > 41, lat < 51.5)

edges_region <- edges_plot %>%
  left_join(
    nodes_region_plot %>% select(finess_geo, region_o = region),
    by = c("sender" = "finess_geo")
  ) %>%
  mutate(region_o = replace_na(region_o, "Unknown"))

# Map
p_network_by_region <- ggplot() +
  geom_sf(data = france_regions,
          fill = "#F0F4F8", color = "#7B8FA1", linewidth = 0.6) +
  geom_segment(
    data = edges_region,
    aes(x = lon_o, y = lat_o, xend = lon_t, yend = lat_t,
        color = region_o, linewidth = weight_scaled),
    alpha = 0.22, show.legend = FALSE
  ) +
  geom_point(
    data = nodes_region_plot,
    aes(x = lon, y = lat, color = region,
        size = pmax(total_beds_mco, 1)),
    alpha = 0.80
  ) +
  geom_sf_text(
    data = france_regions,
    aes(label = name),
    size = 2.8, color = "#2C3E50", fontface = "bold",
    check_overlap = TRUE
  ) +
  scale_color_manual(values = region_palette, name = "French region",
                     na.value = "#CCCCCC") +
  scale_size_continuous(range = c(0.5, 10), name = "MCO beds",
                        labels = comma) +
  scale_linewidth_continuous(range = c(0.2, 3.2), guide = "none") +
  coord_sf(xlim = c(-5.5, 9.6), ylim = c(41.0, 51.5), expand = FALSE) +
  guides(
    color = guide_legend(override.aes = list(size = 4, alpha = 1), ncol = 2),
    size  = guide_legend(override.aes = list(alpha = 0.8))
  ) +
  theme_void(base_size = BASE_SIZE) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = BASE_SIZE * 0.9),
    legend.text     = element_text(size = BASE_SIZE * 0.75),
    plot.title      = element_text(face = "bold", size = BASE_SIZE * 1.3,
                                   hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle   = element_text(size = BASE_SIZE, hjust = 0.5,
                                   color = "grey40"),
    plot.caption    = element_text(size = BASE_SIZE * 0.75,
                                   color = "grey60", hjust = 0.5)
  ) +
  labs(
    title    = "French Hospital Transfer Network by Region (2024)",
    subtitle = paste0("Top ", N_EDGES_PLOT,
                      " corridors | Node colour = administrative region | Size = MCO beds"),
    caption  = "Edge colour = sending hospital region | Outlines = 13 metropolitan regions"
  )

print(p_network_by_region)
ggsave(here("Outputs", "Descriptive Analysis", "network_by_region.png"),
       p_network_by_region, width = 16, height = 11, dpi = 250)

message("Region map saved.")

# Summary table: hospitals and transfers per region
region_summary <- nodes_with_region %>%
  group_by(region) %>%
  summarise(
    n_hospitals = n(),
    median_beds = median(total_beds_mco, na.rm = TRUE),
    total_beds  = sum(total_beds_mco,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    edge_year %>%
      left_join(nodes_with_region %>% select(finess_geo, region),
                by = c("sender" = "finess_geo")) %>%
      group_by(region) %>%
      summarise(total_sent = sum(total_weight), .groups = "drop"),
    by = "region"
  ) %>%
  arrange(desc(n_hospitals))

message("\n--- Hospitals and transfers per region ---")
print(region_summary, n = Inf)

write_csv(region_summary,
          here("Outputs", "Descriptive Analysis", "hospitals_by_region.csv"))

message("\nAll done.")