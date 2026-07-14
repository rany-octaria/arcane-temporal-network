# ============================================================
# ARCANE — SEEDING SCENARIO SIMULATION
# Task 4.3: How does seeding location shape ARB spread?
# Author : Rany Octaria — MESURS Lab, CNAM
#
# What this script does
# ─────────────────────
#  1. Loads the new MCO/SSR weekly transfer data (daily average weights)
#     and the finess_geo metadata with hospital_type.
#  2. Selects seeding hospitals under 8 rules:
#       Network-based : highest in-degree, highest out-degree,
#                       highest betweenness, largest beds,
#                       largest outgoing transfers
#       Type-based    : random MCO, random SSR, random MCO/SSR
#     (random_hospital removed per analysis plan)
#  3. Draws beta values for each simulation replicate from a
#     truncated normal distribution centred on the calibrated median
#     with spread derived from the calibrated IQR (sd = IQR/1.35).
#     Falls back to hard-coded defaults if the calibration RDS is absent.
#  4. Builds a full simulation grid:
#       3 beta tiers  ×  up to 8 seed rules  ×  20 reps
#       = up to 480 simulation runs (de-duplicated where seed rules
#         resolve to the same hospital).
#  5. Runs the SIS metapopulation model on the 2024 transfer network.
#  6. Saves all results and generates trajectory comparison plots
#     faceted by beta tier and coloured by seed rule.
#
# Outputs (in Outputs/Seeding Scenarios/)
# ────────────────────────────────────────
#   all_simulations.rds          full result list (one row per run)
#   simulation_grid.csv          grid metadata
#   overall_trajectory_long.rds  network-level daily summary
#   plot_trajectories_by_tier.png
#   plot_hosps_with_cases_by_tier.png
#   plot_final_prevalence_violin.png
#
# Runtime estimate (20 reps, 8 seeds, 3 tiers, ~365 days)
#   Sequential  : ~30–60 min
#   Parallel (4 workers): ~10–20 min
#
# install.packages(c("tidyverse","here","janitor","igraph",
#   "truncnorm","furrr","future","scales","patchwork"))
# ============================================================

# ============================================================
# 0. LIBRARIES
# ============================================================

library(tidyverse)
library(here)
library(janitor)
library(igraph)
library(truncnorm)    # rtruncnorm() for bounded normal beta draws
library(furrr)        # parallel pmap
library(future)       # multisession backend
library(progressr)    # progress bar for future_pmap
library(scales)
library(patchwork)

options(scipen = 999)

# ── Output directory ─────────────────────────────────────────
OUT_DIR <- here("Outputs", "Seeding Scenarios")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
message("Output directory: ", OUT_DIR)

# ── PPT-friendly theme ────────────────────────────────────────
BASE_SIZE <- 13
theme_ppt <- function() {
  theme_minimal(base_size = BASE_SIZE) +
    theme(
      plot.title       = element_text(face = "bold", size = BASE_SIZE * 1.3,
                                      hjust = 0.5),
      plot.subtitle    = element_text(size = BASE_SIZE, hjust = 0.5,
                                      color = "grey40"),
      plot.caption     = element_text(size = BASE_SIZE * 0.75, color = "grey60"),
      axis.title       = element_text(size = BASE_SIZE, face = "bold"),
      axis.text        = element_text(size = BASE_SIZE * 0.85),
      legend.title     = element_text(size = BASE_SIZE * 0.95, face = "bold"),
      legend.text      = element_text(size = BASE_SIZE * 0.85),
      strip.text       = element_text(size = BASE_SIZE * 0.90, face = "bold"),
      strip.background = element_rect(fill = "#f0f0f0", color = NA),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

# ── Seed rule colours (for plots) ────────────────────────────
SEED_COLORS <- c(
  "highest_in_degree"        = "#0072B2",
  "highest_out_degree"       = "#56B4E9",
  "highest_betweenness"      = "#009E73",
  "largest_beds"             = "#CC79A7",
  "largest_outgoing"         = "#E69F00",
  "random_MCO"               = "#D55E00",
  "random_SSR"               = "#AA0000",
  "random_MCO_SSR"           = "#7B2D8B"
)

TIER_COLORS <- c(
  "low"  = "#009E73",
  "mid"  = "#E69F00",
  "high" = "#D55E00"
)

# ============================================================
# 1. FIXED SIMULATION PARAMETERS
# ============================================================

GAMMA_CLEAR    <- 1 / 387   # daily clearance rate (~387 day mean carriage)
ADMISSION_PREV <- 0          # proportion of new admissions already colonised
N_SEED_INF     <- 1          # one index patient at seed hospital on day 0
N_REPS         <- 20         # replicates per tier × seed combination
SIM_START      <- "2024-01-07"
SIM_END        <- "2025-12-31"   # 2 years — matrix duplicated below

# ============================================================
# 2. LOAD DATA
# ============================================================

message("Loading data...")

# Weekly sliding edge list — already /7 (daily average) from descriptive script
weekly <- read_csv(
  here("Datasets", "MCO_SSR_HBN_2024", "MCO_SSR_HBN_Direct_2024",
       "HBN_weekly_sliding_edgelist_2024.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%                               # standardise camelCase → snake_case
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

# Hospital metadata with hospital_type
finess_geo <- read_csv(
  here("Datasets", "MCO_SSR_HBN_2024", "finessgeo_metadata_2024.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  rename(finess_geo = finessgeo) %>%
  mutate(
    hospital_type = if_else(
      is.na(hospital_type) | hospital_type == "", "Other", hospital_type
    )
  )

message("Weekly rows: ",    nrow(weekly))
message("Hospital types: ", paste(unique(finess_geo$hospital_type), collapse = ", "))

# ── Prepare transfer table (match old code column names) ─────
# Build base transfer table (year 1: 2024)
transfers_2024 <- weekly %>%
  clean_names() %>%
  mutate(
    finess_geo_origin = as.character(finess_geo_origin),
    finess_geo_target = as.character(finess_geo_target),
    transfer_date     = as.Date(window_end),
    weight            = as.integer(weight)
  )

# Duplicate transfer matrix for year 2 (2025) by shifting all dates +365 days.
# This reuses the 2024 seasonal pattern to approximate a second year of transfers,
# allowing most simulations to approach steady state.
transfers_2025 <- transfers_2024 %>%
  mutate(transfer_date = transfer_date + 365L)

# Combine both years into one transfer table used by all simulations
transfers <- bind_rows(transfers_2024, transfers_2025)

message("Transfer rows (2 years): ", nrow(transfers),
        " | Date range: ", min(transfers$transfer_date),
        " to ", max(transfers$transfer_date))
rm(transfers_2024, transfers_2025)   # free memory

# ============================================================
# 3. BUILD HOSPITAL UNIVERSE
# ============================================================

hospitals <- bind_rows(
  transfers %>% transmute(finess_geo = finess_geo_origin),
  transfers %>% transmute(finess_geo = finess_geo_target)
) %>%
  distinct() %>%
  left_join(
    finess_geo %>%
      transmute(
        finess_geo    = as.character(finess_geo),
        no_beds       = readr::parse_number(as.character(total_beds_mco)),
        hospital_type = hospital_type
      ) %>%
      distinct(finess_geo, .keep_all = TRUE),
    by = "finess_geo"
  ) %>%
  mutate(
    no_beds = if_else(is.na(no_beds),
                      round(mean(no_beds, na.rm = TRUE)),
                      no_beds),
    no_beds       = as.integer(no_beds),
    hospital_type = replace_na(hospital_type, "Other")
  )

message("Hospital universe: ", nrow(hospitals), " facilities")
message("Type distribution:\n",
        paste(capture.output(count(hospitals, hospital_type)), collapse = "\n"))

# ============================================================
# 4. NETWORK METRICS FOR SEED SELECTION
# ============================================================

message("Computing network metrics for seed selection...")

# Year-aggregate graph for betweenness
g_year <- transfers %>%
  group_by(finess_geo_origin, finess_geo_target) %>%
  summarise(weight = sum(weight), .groups = "drop") %>%
  graph_from_data_frame(directed = TRUE)

centrality_tbl <- tibble(
  finess_geo  = V(g_year)$name,
  betweenness = estimate_betweenness(
    g_year, vids = V(g_year), directed = TRUE, cutoff = 5
  )
)

in_degree_tbl <- transfers %>%
  distinct(finess_geo_origin, finess_geo_target) %>%
  count(finess_geo = finess_geo_target, name = "in_degree")

out_degree_tbl <- transfers %>%
  distinct(finess_geo_origin, finess_geo_target) %>%
  count(finess_geo = finess_geo_origin, name = "out_degree")

out_strength_tbl <- transfers %>%
  group_by(finess_geo = finess_geo_origin) %>%
  summarise(out_strength = sum(weight), .groups = "drop")

seed_metrics <- hospitals %>%
  left_join(in_degree_tbl,   by = "finess_geo") %>%
  left_join(out_degree_tbl,  by = "finess_geo") %>%
  left_join(out_strength_tbl,by = "finess_geo") %>%
  left_join(centrality_tbl,  by = "finess_geo") %>%
  mutate(across(c(in_degree, out_degree, out_strength, betweenness),
                ~ replace_na(.x, 0)))

# ============================================================
# 5. SEED PANEL
# ── Fixed seeds (deterministic rules) ────────────────────────
# ── Type-based seeds (random draw per rep, resolved in grid) ─
# ============================================================

fixed_seeds <- bind_rows(
  seed_metrics %>%
    slice_max(in_degree,   n = 1, with_ties = FALSE) %>%
    transmute(finess_geo, seed_rule = "highest_in_degree"),
  
  seed_metrics %>%
    slice_max(out_degree,  n = 1, with_ties = FALSE) %>%
    transmute(finess_geo, seed_rule = "highest_out_degree"),
  
  seed_metrics %>%
    slice_max(betweenness, n = 1, with_ties = FALSE) %>%
    transmute(finess_geo, seed_rule = "highest_betweenness"),
  
  seed_metrics %>%
    slice_max(no_beds,     n = 1, with_ties = FALSE) %>%
    transmute(finess_geo, seed_rule = "largest_beds"),
  
  seed_metrics %>%
    slice_max(out_strength, n = 1, with_ties = FALSE) %>%
    transmute(finess_geo, seed_rule = "largest_outgoing")
) %>%
  # Collapse if the same hospital wins multiple rules
  group_by(finess_geo) %>%
  summarise(seed_rule = paste(sort(seed_rule), collapse = " + "),
            .groups = "drop") %>%
  mutate(seed_type = "fixed")  # resolved at grid build time

# Type-based seeds — one placeholder row each; hospital resolved per rep
type_seeds <- tibble(
  finess_geo = NA_character_,
  seed_rule  = c("random_MCO", "random_SSR", "random_MCO_SSR"),
  seed_type  = "type_random"
)

seed_panel <- bind_rows(fixed_seeds, type_seeds)

# ── Seed scenario summary table (PPT-ready) ───────────────────
# Shows which hospital each fixed rule resolves to, including
# merged labels where multiple rules point to the same hospital.
# Type-random rows show "drawn per replicate" as the ID.
seed_table_ppt <- seed_panel %>%
  left_join(
    seed_metrics %>%
      select(finess_geo, no_beds, in_degree, out_degree,
             out_strength, betweenness),
    by = "finess_geo"
  ) %>%
  left_join(
    finess_geo %>%
      select(finess_geo, hospital_type) %>%
      distinct(finess_geo, .keep_all = TRUE),
    by = "finess_geo"
  ) %>%
  transmute(
    `Seeding Rule`        = seed_rule,
    `Hospital ID`         = if_else(seed_type == "fixed",
                                    finess_geo, "drawn per replicate"),
    `Hospital Type`       = if_else(seed_type == "fixed",
                                    replace_na(hospital_type, "Unknown"),
                                    seed_rule %>%
                                      str_extract("MCO/SSR|MCO|SSR") %>%
                                      replace_na("varies")),
    `MCO Beds`            = if_else(seed_type == "fixed",
                                    as.character(no_beds), "—"),
    `In-degree`           = if_else(seed_type == "fixed",
                                    as.character(in_degree), "—"),
    `Out-degree`          = if_else(seed_type == "fixed",
                                    as.character(out_degree), "—"),
    `Out-strength`        = if_else(seed_type == "fixed",
                                    comma(out_strength), "—"),
    `Betweenness`         = if_else(seed_type == "fixed",
                                    sprintf("%.0f", betweenness), "—"),
    `Note`                = case_when(
      grepl("\\+", seed_rule) ~ "Same hospital as multiple rules (merged)",
      seed_type == "type_random" ~ paste0("Random draw from ", hospital_type,
                                          " hospitals each rep"),
      TRUE                       ~ ""
    )
  )

message("\n── Seeding scenario table ───────────────────────────────")
print(seed_table_ppt, n = Inf)

# Count how many unique fixed seeds after deduplication
n_fixed  <- sum(seed_panel$seed_type == "fixed")
n_random <- sum(seed_panel$seed_type == "type_random")
message("\nUnique seeding scenarios: ", nrow(seed_panel),
        " (", n_fixed, " fixed + ", n_random, " type-random)")

# Save as CSV for easy PPT paste
write_csv(seed_table_ppt,
          file.path(OUT_DIR, "seed_scenario_table.csv"))
message("Saved: seed_scenario_table.csv")

message("\nSeed rules in panel:")
print(seed_panel %>% select(seed_rule, seed_type))

# ============================================================
# 6. BETA SAMPLING FROM CALIBRATED DISTRIBUTIONS
#
# Tries to load beta_calibrated_params from the analysis outputs.
# Falls back to hard-coded defaults if the file is absent.
# Draws N_REPS betas per tier from a truncated normal:
#   mean = beta_median, sd = beta_iqr / 1.35
#   bounds = [beta_q25 - 2*sd, beta_q75 + 2*sd]
# ============================================================

calib_path <- here("cluster_jobs", "Outputs", "Analysis results",
                   "2026-05-10", "2026-05-10_beta_calibrated_params_SERVER.rds")

if (file.exists(calib_path)) {
  calib <- readRDS(calib_path)
  message("Loaded calibrated beta parameters from: ", calib_path)
} else {
  message("Calibration file not found — using hard-coded defaults.")
  message("Expected: ", calib_path)
  # Hard-coded fallback based on European colonisation benchmarks
  calib <- tibble(
    amr_tier      = factor(c("Low", "Mid", "High"),
                           levels = c("Low", "Mid", "High")),
    beta_median   = c(0.004,  0.012, 0.022),
    beta_q25      = c(0.002,  0.008, 0.016),
    beta_q75      = c(0.007,  0.018, 0.030),
    beta_iqr      = beta_q75 - beta_q25,
    beta_ci95_lo  = c(0.001,  0.005, 0.010),
    beta_ci95_hi  = c(0.010,  0.025, 0.040)
  )
}

# Map calibrated tiers to lowercase beta_regime labels
tier_map <- c("Low" = "low", "Mid" = "mid", "High" = "high")

set.seed(2024)

beta_lookup <- calib %>%
  mutate(beta_regime = tier_map[as.character(amr_tier)]) %>%
  select(beta_regime, beta_median, beta_q25, beta_q75, beta_iqr) %>%
  mutate(
    beta_sd = beta_iqr / 1.35,   # normal approximation: IQR ≈ 1.35 × SD
    # Draw N_REPS betas per tier from a truncated normal
    # bounded within [Q25 - 2*SD, Q75 + 2*SD] to stay epidemiologically plausible
    betas   = pmap(
      list(beta_median, beta_sd, beta_q25, beta_q75),
      function(mn, sd, lo, hi) {
        rtruncnorm(N_REPS, a = pmax(0.0001, lo - 2 * sd),
                   b = hi + 2 * sd, mean = mn, sd = sd)
      }
    )
  ) %>%
  select(beta_regime, betas) %>%
  unnest(betas) %>%
  group_by(beta_regime) %>%
  mutate(
    rep_id   = row_number(),
    sim_seed = 10000 + as.integer(factor(beta_regime)) * 1000 + rep_id
  ) %>%
  ungroup() %>%
  rename(beta_within_iter = betas)

message("Beta draws per tier:")
beta_lookup %>%
  group_by(beta_regime) %>%
  summarise(
    n     = n(),
    min   = round(min(beta_within_iter), 5),
    med   = round(median(beta_within_iter), 5),
    max   = round(max(beta_within_iter), 5),
    .groups = "drop"
  ) %>%
  print()

# ============================================================
# 7. BUILD SIMULATION GRID
# 3 tiers × N_REPS reps × seed rules
# Type-random seeds are resolved here (one draw per rep_id)
# so that all seed rules within the same rep_id share the same
# beta value (fair comparison).
# ============================================================

# Hospital lists by type (for type-random seeds)
hosp_by_type <- hospitals %>%
  group_by(hospital_type) %>%
  summarise(ids = list(finess_geo), .groups = "drop") %>%
  deframe()   # named list: type → character vector of IDs

simulation_grid <- seed_panel %>%
  tidyr::crossing(beta_lookup) %>%
  # Resolve the seed hospital for each row
  mutate(
    seed_hospital_iter = pmap_chr(
      list(seed_type, seed_rule, finess_geo, sim_seed),
      function(stype, srule, fgeo, sseed) {
        if (stype == "fixed") {
          fgeo  # already determined
        } else {
          # Type-random: pick one hospital of the right type
          target_type <- case_when(
            srule == "random_MCO"     ~ "MCO",
            srule == "random_SSR"     ~ "SSR",
            srule == "random_MCO_SSR" ~ "MCO/SSR",
            TRUE                      ~ "Other"
          )
          pool <- hosp_by_type[[target_type]]
          if (is.null(pool) || length(pool) == 0) {
            warning("No hospitals of type ", target_type, " — sampling from all")
            pool <- hospitals$finess_geo
          }
          set.seed(sseed + nchar(srule))  # unique seed per rule within rep
          sample(pool, 1)
        }
      }
    )
  ) %>%
  select(seed_rule, seed_type, beta_regime, rep_id, sim_seed,
         beta_within_iter, seed_hospital_iter)

message("Simulation grid: ", nrow(simulation_grid), " rows")
simulation_grid %>%
  count(beta_regime, seed_rule) %>%
  print(n = Inf)

write_csv(simulation_grid,
          file.path(OUT_DIR, "simulation_grid.csv"))

# ============================================================
# 8. CORE SIMULATION FUNCTIONS
# (same SIS metapopulation engine as previous scripts)
# ============================================================

initialize_hospital_state <- function(hospitals, seed_hospital,
                                      n_seed_infected = 1) {
  hospitals %>%
    mutate(
      n_infected = if_else(finess_geo == seed_hospital,
                           pmin(n_seed_infected, no_beds), 0L),
      prevalence = n_infected / no_beds
    )
}

compute_daily_infected_transfers_fast <- function(state, transfers_day) {
  if (nrow(transfers_day) == 0)
    return(tibble(finess_geo_origin = character(),
                  finess_geo_target = character(),
                  weight = integer(),
                  infected_transfer = integer()))
  
  idx_o   <- match(transfers_day$finess_geo_origin, state$finess_geo)
  prev_o  <- replace_na(state$prevalence[idx_o], 0)
  ninf_o  <- replace_na(state$n_infected[idx_o], 0L)
  
  inf_raw <- rbinom(nrow(transfers_day), transfers_day$weight, prev_o)
  inf_out <- inf_raw
  
  split_idx <- split(seq_len(nrow(transfers_day)),
                     transfers_day$finess_geo_origin)
  for (idx in split_idx) {
    avail    <- ninf_o[idx[1]]
    proposed <- sum(inf_raw[idx])
    if (avail <= 0L || proposed <= 0L) {
      inf_out[idx] <- 0L
    } else if (proposed > avail) {
      scaled   <- inf_raw[idx] * avail / proposed
      base     <- floor(scaled)
      leftover <- avail - sum(base)
      if (leftover > 0) {
        top       <- order(scaled - base, decreasing = TRUE)[seq_len(leftover)]
        base[top] <- base[top] + 1L
      }
      inf_out[idx] <- as.integer(base)
    }
  }
  
  tibble(
    finess_geo_origin = transfers_day$finess_geo_origin,
    finess_geo_target = transfers_day$finess_geo_target,
    weight            = transfers_day$weight,
    infected_transfer = as.integer(inf_out)
  )
}

simulate_one_day_agg_fast <- function(state, transfers_day, current_date,
                                      beta_within, gamma_clear,
                                      admission_prev) {
  inf_xfer <- compute_daily_infected_transfers_fast(state, transfers_day)
  
  n   <- nrow(state)
  out <- integer(n); inn <- integer(n)
  
  if (nrow(inf_xfer) > 0) {
    io <- match(inf_xfer$finess_geo_origin, state$finess_geo)
    it <- match(inf_xfer$finess_geo_target, state$finess_geo)
    for (k in seq_along(io)) {
      if (!is.na(io[k])) out[io[k]] <- out[io[k]] + inf_xfer$infected_transfer[k]
      if (!is.na(it[k])) inn[it[k]] <- inn[it[k]] + inf_xfer$infected_transfer[k]
    }
  }
  
  replaced      <- rbinom(n, out, admission_prev)
  n_after_xfer  <- pmax(0L, pmin(state$n_infected - out + inn + replaced,
                                 state$no_beds))
  n_cleared     <- rbinom(n, n_after_xfer, gamma_clear)
  n_after_clear <- n_after_xfer - n_cleared
  n_suscept     <- state$no_beds - n_after_clear
  p_inf         <- pmin(pmax(1 - exp(-beta_within * n_after_clear /
                                       state$no_beds), 0), 1)
  n_new         <- rbinom(n, n_suscept, p_inf)
  n_final       <- pmin(state$no_beds, as.integer(n_after_clear + n_new))
  
  state_new <- tibble(
    finess_geo = state$finess_geo,
    no_beds    = state$no_beds,
    n_infected = n_final,
    prevalence = n_final / state$no_beds
  )
  
  list(
    state = state_new,
    overall_summary = tibble(
      date                  = current_date,
      total_patients        = sum(state_new$no_beds),
      total_infected        = sum(state_new$n_infected),
      overall_prevalence    = sum(state_new$n_infected) / sum(state_new$no_beds),
      n_hospitals_with_case = sum(state_new$n_infected > 0)
    )
  )
}

run_sis_simulation <- function(seed_hospital, sim_seed,
                               beta_within_iter, ...) {
  set.seed(sim_seed)
  
  state <- initialize_hospital_state(hospitals, seed_hospital, N_SEED_INF)
  
  sim_dates <- seq.Date(as.Date(SIM_START), as.Date(SIM_END), by = "day")
  
  transfers_by_day <- transfers %>%
    mutate(transfer_date = as.Date(transfer_date)) %>%
    select(transfer_date, finess_geo_origin, finess_geo_target, weight) %>%
    split(.$transfer_date)
  
  overall_results <- vector("list", length(sim_dates))
  
  for (i in seq_along(sim_dates)) {
    d  <- sim_dates[i]
    td <- transfers_by_day[[as.character(d)]]
    
    if (is.null(td)) {
      td <- tibble(finess_geo_origin = character(),
                   finess_geo_target = character(),
                   weight            = integer())
    } else {
      td <- td %>% select(finess_geo_origin, finess_geo_target, weight)
    }
    
    out                  <- simulate_one_day_agg_fast(
      state, td, d, beta_within_iter, GAMMA_CLEAR, ADMISSION_PREV)
    state                <- out$state
    overall_results[[i]] <- out$overall_summary
  }
  
 

# ============================================================
# 9. RUN SIMULATIONS
# Uses parallel workers if available; falls back to sequential.
# ============================================================

message("\nRunning ", nrow(simulation_grid), " simulations...")
message("Parallel workers: checking...")

# Raise per-worker globals limit — transfers object is ~524 MB after
# doubling for 2 years, which exceeds future's default 500 MB ceiling.
options(future.globals.maxSize = 2 * 1024^3)  # 2 GB

n_workers <- min(parallel::detectCores() - 2, 8)
if (n_workers < 2) {
  plan(sequential)
  message("Running sequentially.")
} else {
  plan(multisession, workers = n_workers)
  message("Running in parallel on ", n_workers, " workers.")
}

t_start <- Sys.time()

# Progress bar — shows completed sims out of total
handlers(global = TRUE)
handlers("txtprogressbar")

n_total <- nrow(simulation_grid)
message("Starting ", n_total, " simulations (",
        N_REPS, " reps x ", n_distinct(simulation_grid$seed_rule),
        " seeds x 3 tiers) over 2 simulated years...")

with_progress({
  pb <- progressr::progressor(steps = n_total)
  
  all_simulations <- simulation_grid %>%
    mutate(
      overall_results = future_pmap(
        list(seed_hospital_iter, sim_seed, beta_within_iter),
        function(seed_hospital, sim_seed, beta_within_iter) {
          res <- run_sis_simulation(seed_hospital, sim_seed, beta_within_iter)
          pb(sprintf("beta=%s seed=%s",
                     round(beta_within_iter, 4), sim_seed))
          res
        },
        .options = furrr_options(seed = TRUE)
      )
    )
})

plan(sequential)

t_end <- Sys.time()
message("Done. Elapsed: ",
        round(as.numeric(t_end - t_start, units = "mins"), 1), " minutes")

# ============================================================
# 10. SAVE OUTPUTS
# ============================================================

saveRDS(all_simulations,
        file.path(OUT_DIR, "all_simulations.rds"))
message("Saved: all_simulations.rds")

# Long-format overall trajectory
trajectory_long <- all_simulations %>%
  select(seed_rule, beta_regime, rep_id, sim_seed,
         beta_within_iter, overall_results) %>%
  unnest(overall_results) %>%
  filter(!is.na(overall_prevalence)) %>%          # drop any failed runs
  mutate(
    beta_regime = factor(beta_regime, levels = c("low", "mid", "high")),
    # Keep seed_rule as-is — don't force levels from SEED_COLORS
    # because merged names (e.g. "highest_in_degree + largest_beds")
    # won't match and become NA, making the plots blank
    seed_rule = as.character(seed_rule)
  )

# Diagnostic: show what's actually in the data
message("\n── trajectory_long check ────────────────────────────")
message("Rows: ", nrow(trajectory_long))
message("Seed rules present: ", paste(unique(trajectory_long$seed_rule), collapse = " | "))
message("Beta regimes: ", paste(unique(trajectory_long$beta_regime), collapse = ", "))
message("Prevalence range: ",
        round(min(trajectory_long$overall_prevalence, na.rm=TRUE), 5), " – ",
        round(max(trajectory_long$overall_prevalence, na.rm=TRUE), 5))
message("Hospitals with case range: ",
        min(trajectory_long$n_hospitals_with_case, na.rm=TRUE), " – ",
        max(trajectory_long$n_hospitals_with_case, na.rm=TRUE))

# Build a dynamic colour palette matching whatever seed rules actually exist
actual_rules <- sort(unique(trajectory_long$seed_rule))
# Map known names; give merged/unknown names a distinct grey shade
base_colors <- SEED_COLORS
extra_rules <- setdiff(actual_rules, names(base_colors))
if (length(extra_rules) > 0) {
  extra_colors <- setNames(
    colorRampPalette(c("#AAAAAA", "#444444"))(length(extra_rules)),
    extra_rules
  )
  base_colors <- c(base_colors, extra_colors)
}
SEED_COLORS_DYNAMIC <- base_colors[actual_rules]

saveRDS(trajectory_long,
        file.path(OUT_DIR, "overall_trajectory_long.rds"))
message("Saved: overall_trajectory_long.rds")

# ============================================================
# 11. TRAJECTORY COMPARISON PLOTS
# ============================================================

# ── Summary: median + IQR per seed rule × beta tier × date ──
traj_summary <- trajectory_long %>%
  group_by(seed_rule, beta_regime, date) %>%
  summarise(
    prev_median = median(overall_prevalence, na.rm = TRUE),
    prev_q25    = quantile(overall_prevalence, 0.25, na.rm = TRUE),
    prev_q75    = quantile(overall_prevalence, 0.75, na.rm = TRUE),
    hosp_median = median(n_hospitals_with_case, na.rm = TRUE),
    hosp_q25    = quantile(n_hospitals_with_case, 0.25, na.rm = TRUE),
    hosp_q75    = quantile(n_hospitals_with_case, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# ── Plot 1: Network prevalence trajectories ──────────────────
p_traj <- ggplot(
  traj_summary,
  aes(x = date, y = prev_median,
      color = seed_rule, fill = seed_rule, group = seed_rule)
) +
  geom_ribbon(aes(ymin = prev_q25, ymax = prev_q75),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ beta_regime, ncol = 3,
             labeller = labeller(beta_regime = c(
               low  = "Low β (< 2% prevalence tier)",
               mid  = "Mid β (2–10% prevalence tier)",
               high = "High β (> 10% prevalence tier)"
             ))) +
  scale_color_manual(values = SEED_COLORS_DYNAMIC, name = "Seeding scenario") +
  scale_fill_manual(values  = SEED_COLORS_DYNAMIC, name = "Seeding scenario") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
  theme_ppt() +
  theme(legend.position = "bottom",
        legend.title    = element_text(face = "bold")) +
  labs(
    title    = "ARB Epidemic Trajectories by Seeding Scenario and Transmission Tier",
    subtitle = paste0("SIS metapopulation model | 2024 French hospital network | ",
                      N_REPS, " replicates per scenario | Ribbon = IQR"),
    x = NULL, y = "Network prevalence"
  )

print(p_traj)
ggsave(file.path(OUT_DIR, "plot_trajectories_by_tier.png"),
       p_traj, width = 16, height = 8, dpi = 200)

# ── Plot 2: Hospitals with ≥1 case ───────────────────────────
p_hosp <- ggplot(
  traj_summary,
  aes(x = date, y = hosp_median,
      color = seed_rule, fill = seed_rule, group = seed_rule)
) +
  geom_ribbon(aes(ymin = hosp_q25, ymax = hosp_q75),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ beta_regime, ncol = 3,
             labeller = labeller(beta_regime = c(
               low  = "Low β",
               mid  = "Mid β",
               high = "High β"
             ))) +
  scale_color_manual(values = SEED_COLORS_DYNAMIC, name = "Seeding scenario") +
  scale_fill_manual(values  = SEED_COLORS_DYNAMIC, name = "Seeding scenario") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(labels = comma) +
  theme_ppt() +
  theme(legend.position = "bottom") +
  labs(
    title    = "Hospitals with ≥1 Active Case by Seeding Scenario",
    subtitle = paste0("Median ± IQR across ", N_REPS, " replicates"),
    x = NULL, y = "Hospitals with ≥1 case"
  )

print(p_hosp)
ggsave(file.path(OUT_DIR, "plot_hosps_with_cases_by_tier.png"),
       p_hosp, width = 16, height = 8, dpi = 200)

# ── Plot 3: Final prevalence violin by seed rule × tier ──────
final_prev <- trajectory_long %>%
  group_by(seed_rule, beta_regime, rep_id) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(!is.na(overall_prevalence), is.finite(overall_prevalence))

# How many reps have non-zero prevalence per tier
message("Non-zero prevalence reps by tier:")
final_prev %>%
  group_by(beta_regime) %>%
  summarise(
    n_total     = n(),
    n_nonzero   = sum(overall_prevalence > 0),
    pct_nonzero = round(100 * n_nonzero / n_total, 1),
    .groups = "drop"
  ) %>%
  print()

# Violin needs at least 2 distinct values per group.
# Tiny jitter on exactly-zero reps keeps bandwidth finite.
# Drop any scenario/tier with fewer than 3 reps.
set.seed(99)
final_prev_plot <- final_prev %>%
  mutate(
    prev_plot = if_else(overall_prevalence == 0,
                        overall_prevalence + runif(n(), 0, 1e-6),
                        overall_prevalence)
  ) %>%
  group_by(seed_rule, beta_regime) %>%
  filter(n() >= 3) %>%
  ungroup()

p_violin <- ggplot(
  final_prev_plot,
  aes(x = seed_rule, y = prev_plot,
      fill = seed_rule, color = seed_rule)
) +
  geom_violin(alpha = 0.45, scale = "width", linewidth = 0.4,
              draw_quantiles = c(0.25, 0.50, 0.75)) +
  geom_jitter(width = 0.15, size = 1.0, alpha = 0.40) +
  facet_wrap(~ beta_regime, ncol = 3,
             labeller = labeller(beta_regime = c(
               low  = "Low beta",
               mid  = "Mid beta",
               high = "High beta"
             ))) +
  scale_fill_manual(values  = SEED_COLORS_DYNAMIC, guide = "none") +
  scale_color_manual(values = SEED_COLORS_DYNAMIC, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  coord_flip() +
  theme_ppt() +
  labs(
    title    = "Final Steady-State Prevalence by Seeding Scenario and Beta Tier",
    subtitle = paste0("Lines = Q25 / median / Q75 | Points = individual replicates | ",
                      N_REPS, " reps each"),
    x = NULL, y = "Final network prevalence"
  )


print(p_violin)
ggsave(file.path(OUT_DIR, "plot_final_prevalence_violin.png"),
       p_violin, width = 14, height = 9, dpi = 200)

# ── Summary table ─────────────────────────────────────────────
summary_table <- final_prev %>%
  group_by(beta_regime, seed_rule) %>%
  summarise(
    n_reps         = n(),
    prev_median    = median(overall_prevalence, na.rm = TRUE),
    prev_q25       = quantile(overall_prevalence, 0.25, na.rm = TRUE),
    prev_q75       = quantile(overall_prevalence, 0.75, na.rm = TRUE),
    hosp_median    = median(n_hospitals_with_case, na.rm = TRUE),
    hosp_q25       = quantile(n_hospitals_with_case, 0.25, na.rm = TRUE),
    hosp_q75       = quantile(n_hospitals_with_case, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 5)))

message("\n── Final prevalence summary ──")
print(summary_table, n = Inf)

write_csv(summary_table,
          file.path(OUT_DIR, "final_prevalence_summary.csv"))

message("\n✓ All done. Outputs in:\n  ", OUT_DIR)