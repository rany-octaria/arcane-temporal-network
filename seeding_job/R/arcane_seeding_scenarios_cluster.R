# ============================================================
# ARCANE — SEEDING SCENARIO SIMULATION (CLUSTER VERSION)
# Task 4.3: How does seeding location shape ARB spread?
# Author : Rany Octaria — MESURS Lab, CNAM
#
# Cluster design
# ──────────────
# The full grid (3 tiers × N_SEED_RULES seeds × N_REPS reps)
# is split across PBS array jobs by (beta_regime × seed_rule).
# Each job handles all N_REPS replicates for its assigned
# tier × seed combination and saves one RDS file.
# Combine with: bind_rows(map(list.files(...), readRDS))
#
# Submit: bash cluster_jobs/arcane_seeding_launch.sh
#
# Beta sampling
# ─────────────
# Each rep draws its beta from a truncated normal:
#   mean = beta_mean  (from calibrated params)
#   sd   = (beta_ci95_hi - beta_ci95_lo) / (2 × 1.96)
# Bounded at [0.0001, Inf] so beta stays positive.
# This propagates the full calibration uncertainty into each rep.
# ============================================================

# ============================================================
# 0. LIBRARIES
# ============================================================

library(tidyverse)
library(here)
library(janitor)
library(igraph)
library(truncnorm)
library(scales)

options(scipen = 999)

# ============================================================
# 1. CLUSTER CONFIG
# ============================================================

# Read indexcode from command-line argument (passed by PBS script)
args      <- commandArgs(trailingOnly = TRUE)
indexcode <- if (length(args) >= 1 && nzchar(args[1])) as.integer(args[1]) else 1L
message("INDEXCODE = ", indexcode)

# Fixed parameters
N_REPS         <- 100         # replicates per tier × seed combination
N_SEED_INF     <- 1           # one index patient on day 0
GAMMA_CLEAR    <- 1 / 387
ADMISSION_PREV <- 0
SIM_START      <- "2024-01-07"
SIM_END        <- "2025-12-31"   # 2 years (transfer matrix duplicated)

# Paths — all relative to the seeding_job project root
PROJECT_DIR <- "/media/kevinNFS2/rany/seeding_job"
data_dir    <- file.path(PROJECT_DIR, "data", "raw")
calib_dir   <- file.path(PROJECT_DIR, "data", "calibration")
run_date    <- format(Sys.Date(), "%Y-%m-%d")
out_dir     <- file.path(PROJECT_DIR, "outputs", run_date)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("PROJECT  = ", PROJECT_DIR)
message("DATA     = ", data_dir)
message("OUT_DIR  = ", out_dir)

# ============================================================
# 2. LOAD DATA
# ============================================================

message("Loading data...")

weekly <- read_csv(
  file.path(data_dir, "HBN_weekly_sliding_edgelist_2024.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

finess_geo <- read_csv(
  file.path(data_dir, "finessgeo_metadata_2024.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  rename(finess_geo = finessgeo) %>%
  mutate(hospital_type = if_else(
    is.na(hospital_type) | hospital_type == "", "Other", hospital_type))

# Duplicate transfer matrix for year 2 (repeat 2024 seasonal pattern)
transfers <- bind_rows(
  weekly %>%
    clean_names() %>%
    mutate(finess_geo_origin = as.character(finess_geo_origin),
           finess_geo_target = as.character(finess_geo_target),
           transfer_date     = as.Date(window_end),
           weight            = as.integer(weight)),
  weekly %>%
    clean_names() %>%
    mutate(finess_geo_origin = as.character(finess_geo_origin),
           finess_geo_target = as.character(finess_geo_target),
           transfer_date     = as.Date(window_end) + 365L,
           weight            = as.integer(weight))
)

message("Transfer rows (2 years): ", nrow(transfers))
rm(weekly)

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
      transmute(finess_geo    = as.character(finess_geo),
                no_beds       = readr::parse_number(as.character(total_beds_mco)),
                hospital_type = hospital_type) %>%
      distinct(finess_geo, .keep_all = TRUE),
    by = "finess_geo"
  ) %>%
  mutate(
    no_beds       = if_else(is.na(no_beds),
                            round(mean(no_beds, na.rm = TRUE)), no_beds),
    no_beds       = as.integer(no_beds),
    hospital_type = replace_na(hospital_type, "Other")
  )

message("Hospitals: ", nrow(hospitals))

# ============================================================
# 4. NETWORK METRICS FOR SEED SELECTION
# ============================================================

message("Computing seed metrics...")

g_year <- transfers %>%
  group_by(finess_geo_origin, finess_geo_target) %>%
  summarise(weight = sum(weight), .groups = "drop") %>%
  graph_from_data_frame(directed = TRUE)

seed_metrics <- hospitals %>%
  left_join(
    transfers %>% distinct(finess_geo_origin, finess_geo_target) %>%
      count(finess_geo = finess_geo_target, name = "in_degree"),
    by = "finess_geo") %>%
  left_join(
    transfers %>% distinct(finess_geo_origin, finess_geo_target) %>%
      count(finess_geo = finess_geo_origin, name = "out_degree"),
    by = "finess_geo") %>%
  left_join(
    transfers %>%
      group_by(finess_geo = finess_geo_origin) %>%
      summarise(out_strength = sum(weight), .groups = "drop"),
    by = "finess_geo") %>%
  left_join(
    tibble(finess_geo  = V(g_year)$name,
           betweenness = estimate_betweenness(g_year, directed = TRUE,
                                              cutoff = 5)),
    by = "finess_geo") %>%
  mutate(across(c(in_degree, out_degree, out_strength, betweenness),
                ~ replace_na(.x, 0)))

# ============================================================
# 5. SEED PANEL (fixed + type-random)
# ============================================================

fixed_seeds <- bind_rows(
  seed_metrics %>% slice_max(in_degree,    n = 1, with_ties = FALSE) %>%
    transmute(finess_geo, seed_rule = "highest_in_degree"),
  seed_metrics %>% slice_max(out_degree,   n = 1, with_ties = FALSE) %>%
    transmute(finess_geo, seed_rule = "highest_out_degree"),
  seed_metrics %>% slice_max(betweenness,  n = 1, with_ties = FALSE) %>%
    transmute(finess_geo, seed_rule = "highest_betweenness"),
  seed_metrics %>% slice_max(no_beds,      n = 1, with_ties = FALSE) %>%
    transmute(finess_geo, seed_rule = "largest_beds"),
  seed_metrics %>% slice_max(out_strength, n = 1, with_ties = FALSE) %>%
    transmute(finess_geo, seed_rule = "largest_outgoing")
) %>%
  group_by(finess_geo) %>%
  summarise(seed_rule = paste(sort(seed_rule), collapse = " + "),
            .groups = "drop") %>%
  mutate(seed_type = "fixed")

type_seeds <- tibble(
  finess_geo = NA_character_,
  seed_rule  = c("random_MCO", "random_SSR", "random_MCO_SSR"),
  seed_type  = "type_random"
)

seed_panel <- bind_rows(fixed_seeds, type_seeds)
message("Seed rules: ", paste(seed_panel$seed_rule, collapse = ", "))

# ============================================================
# 6. BETA SAMPLING — mean ± 95% CI → truncated normal
#
# sd = (ci95_hi - ci95_lo) / (2 × 1.96)
# This maps the calibrated 95% CI to the SD of the sampling
# distribution, so that 95% of drawn beta values fall within
# the CI. Bounded below at 0.0001 to keep beta positive.
# ============================================================

calib_rds <- file.path(calib_dir, "beta_calibrated_params_SERVER.rds")

if (!file.exists(calib_rds)) {
  calib_rds <- character(0)
}

if (length(calib_rds) == 0 || !file.exists(calib_rds[1])) {
  message("Calibrated params not found — using hard-coded defaults.")
  calib <- tibble(
    amr_tier     = factor(c("Low", "Mid", "High"),
                          levels = c("Low", "Mid", "High")),
    beta_mean    = c(0.004,  0.012, 0.022),
    beta_ci95_lo = c(0.001,  0.005, 0.010),
    beta_ci95_hi = c(0.010,  0.025, 0.040)
  )
} else {
  message("Loading calibrated betas: ", calib_rds)
  calib <- readRDS(calib_rds[1])
}

tier_map <- c("Low" = "low", "Mid" = "mid", "High" = "high")

set.seed(2024 + indexcode)   # indexcode offsets the seed so each job is independent

beta_lookup <- calib %>%
  mutate(
    beta_regime = tier_map[as.character(amr_tier)],
    # SD derived from 95% CI width: CI = mean ± 1.96 × SD
    beta_sd     = (beta_ci95_hi - beta_ci95_lo) / (2 * 1.96),
    betas = pmap(
      list(beta_mean, beta_sd),
      function(mn, sd) {
        rtruncnorm(N_REPS,
                   a    = 0.0001,   # hard lower bound: beta must be positive
                   b    = Inf,
                   mean = mn,
                   sd   = sd)
      }
    )
  ) %>%
  select(beta_regime, betas) %>%
  unnest(betas) %>%
  group_by(beta_regime) %>%
  mutate(
    rep_id   = row_number(),
    sim_seed = 10000 + as.integer(factor(beta_regime)) * 1000 +
               rep_id + indexcode * 10
  ) %>%
  ungroup() %>%
  rename(beta_within_iter = betas)

message("Beta draws per tier:")
beta_lookup %>%
  group_by(beta_regime) %>%
  summarise(
    n   = n(),
    mn  = round(mean(beta_within_iter),   5),
    lo  = round(min(beta_within_iter),    5),
    hi  = round(max(beta_within_iter),    5),
    .groups = "drop"
  ) %>%
  print()

# ============================================================
# 7. BUILD SIMULATION GRID — SPLIT BY INDEXCODE
#
# Full grid = seed_rule × beta_regime × rep_id
# Split into N_JOBS chunks (one per PBS job) using modulo.
# Each job processes its assigned rows only.
# ============================================================

N_JOBS <- 24L   # must match seq in launch script

hosp_by_type <- hospitals %>%
  group_by(hospital_type) %>%
  summarise(ids = list(finess_geo), .groups = "drop") %>%
  deframe()

simulation_grid_full <- seed_panel %>%
  crossing(beta_lookup) %>%
  mutate(
    seed_hospital_iter = pmap_chr(
      list(seed_type, seed_rule, finess_geo, sim_seed),
      function(stype, srule, fgeo, sseed) {
        if (stype == "fixed") return(fgeo)
        target_type <- case_when(
          srule == "random_MCO"     ~ "MCO",
          srule == "random_SSR"     ~ "SSR",
          srule == "random_MCO_SSR" ~ "MCO/SSR",
          TRUE                      ~ "Other"
        )
        pool <- hosp_by_type[[target_type]]
        if (is.null(pool) || length(pool) == 0) pool <- hospitals$finess_geo
        set.seed(sseed + nchar(srule))
        sample(pool, 1)
      }
    ),
    job_id = ((row_number() - 1L) %% N_JOBS) + 1L
  )

simulation_grid <- simulation_grid_full %>%
  filter(job_id == indexcode) %>%
  select(seed_rule, seed_type, beta_regime, rep_id, sim_seed,
         beta_within_iter, seed_hospital_iter)

message("Job ", indexcode, "/", N_JOBS,
        " — running ", nrow(simulation_grid), " simulations")

# ============================================================
# 8. SIMULATION ENGINE
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
                  weight = integer(), infected_transfer = integer()))

  idx_o  <- match(transfers_day$finess_geo_origin, state$finess_geo)
  prev_o <- replace_na(state$prevalence[idx_o], 0)
  ninf_o <- replace_na(state$n_infected[idx_o], 0L)
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
  tibble(finess_geo_origin  = transfers_day$finess_geo_origin,
         finess_geo_target  = transfers_day$finess_geo_target,
         weight             = transfers_day$weight,
         infected_transfer  = as.integer(inf_out))
}

simulate_one_day_agg_fast <- function(state, transfers_day, current_date,
                                      beta_within, gamma_clear, admission_prev) {
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

  state_new <- tibble(finess_geo = state$finess_geo,
                      no_beds    = state$no_beds,
                      n_infected = n_final,
                      prevalence = n_final / state$no_beds)

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

run_sis_simulation <- function(seed_hospital, sim_seed, beta_within_iter) {
  set.seed(sim_seed)
  state     <- initialize_hospital_state(hospitals, seed_hospital, N_SEED_INF)
  sim_dates <- seq.Date(as.Date(SIM_START), as.Date(SIM_END), by = "day")

  tbd <- transfers %>%
    mutate(transfer_date = as.Date(transfer_date)) %>%
    select(transfer_date, finess_geo_origin, finess_geo_target, weight) %>%
    split(.$transfer_date)

  overall_results <- vector("list", length(sim_dates))

  for (i in seq_along(sim_dates)) {
    d  <- sim_dates[i]
    td <- tbd[[as.character(d)]]
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
  bind_rows(overall_results)
}

# ============================================================
# 9. RUN SIMULATIONS (sequential — parallelism handled by PBS)
# ============================================================

message("Running ", nrow(simulation_grid), " simulations...")

n_total <- nrow(simulation_grid)

results_list <- vector("list", n_total)

for (i in seq_len(n_total)) {
  row <- simulation_grid[i, ]
  results_list[[i]] <- run_sis_simulation(
    row$seed_hospital_iter,
    row$sim_seed,
    row$beta_within_iter
  )
  if (i %% 10 == 0 || i == n_total)
    message(sprintf("  [%d/%d] seed=%s tier=%s rep=%d beta=%.5f",
                    i, n_total,
                    row$seed_rule, row$beta_regime, row$rep_id,
                    row$beta_within_iter))
}

simulation_grid$overall_results <- results_list

# ============================================================
# 10. SAVE
# ============================================================

out_rds  <- file.path(out_dir,
  sprintf("%s_seeding_sims_index_%02d.rds", run_date, indexcode))
out_done <- file.path(out_dir,
  sprintf("%s_index_%02d_DONE.txt", run_date, indexcode))

saveRDS(simulation_grid, out_rds)

writeLines(
  sprintf("Done: %s\nIndex: %d\nRows: %d\nFile: %s",
          Sys.time(), indexcode, nrow(simulation_grid), out_rds),
  out_done
)

message("Saved: ", basename(out_rds))
message("Done.")
