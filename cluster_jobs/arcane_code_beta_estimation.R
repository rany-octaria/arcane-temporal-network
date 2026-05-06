# ------------------------------------------------------------
# BETA CALIBRATION OF SIS METAPOPULATION MODEL (HOSPITAL LEVEL)
# ARB SPREAD IN EUROPEAN HOSPITAL NETWORK
# Author: Rany Octaria — 23.04.2026
#
# Model:
# - Nodes: Hospitals (capacity = beds)
# - Edges: Directed patient transfers (weekly)
# - State: Number infected per hospital
# - Dynamics:
#     * Stochastic transfer of infected patients
#     * Within-hospital transmission
#     * Clearance
# Goal:
# - Calibrate beta such that steady-state prevalence matches
#   known European ARB prevalence levels
# - Grid is split across PBS array jobs via indexcode CLI argument
# ------------------------------------------------------------

# ============================================================
# 1. SETUP
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(janitor)

# ---- Read indexcode from command-line argument ---------------
# Passed by script.sh as: Rscript arcane_code_beta_estimation.R <indexcode>
args <- commandArgs(trailingOnly = TRUE)
indexcode <- if (length(args) >= 1 && nzchar(args[1])) {
  as.integer(args[1])
} else {
  1L  # fallback for interactive / local runs
}
message("INDEXCODE = ", indexcode)

# ---- Directories (all under "cluster_jobs") -----------------
root_dir <- Sys.getenv("PBS_O_WORKDIR", unset = getwd())
root_dir <- normalizePath(root_dir)

job_dir <- file.path(root_dir, "cluster_jobs")
job_dir <- normalizePath(job_dir)

message("ROOT_DIR = ", root_dir)
message("JOB_DIR  = ", job_dir)

data_dir    <- file.path(job_dir, "data")
logs_dir    <- file.path(job_dir, "logs")
run_date    <- format(Sys.Date(), "%Y-%m-%d")
output_dir  <- file.path(job_dir, "Outputs", run_date)
log_run_dir <- file.path(logs_dir, run_date)

dir.create(data_dir,    recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(log_run_dir, recursive = TRUE, showWarnings = FALSE)

setwd(job_dir)
message("WD = ", getwd())

# ============================================================
# 2. DATA LOADING
# ============================================================

coords_beds_active <- readRDS(file.path(data_dir, "coords_beds_active.RDS"))
weekly_transfers   <- readRDS(file.path(data_dir, "weekly.RDS"))

# ============================================================
# 3. BUILD HOSPITAL UNIVERSE
# ============================================================

hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finessGeo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finessGeo_target))
) %>%
  distinct()

hospitals <- hospitals %>%
  left_join(
    coords_beds_active %>%
      clean_names() %>%
      transmute(
        finess_geo = as.character(finess_geo),
        no_beds    = readr::parse_number(as.character(no_beds))
      ),
    by = "finess_geo"
  )

mean_beds <- round(mean(hospitals$no_beds, na.rm = TRUE))

hospitals <- hospitals %>%
  mutate(
    no_beds = if_else(is.na(no_beds), mean_beds, no_beds),
    no_beds = as.integer(no_beds)
  )

message("Hospitals loaded: ", nrow(hospitals))

# ============================================================
# 4. CORE MODEL FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# 4.1 Initialise hospital-level state
# ------------------------------------------------------------
initialize_hospital_state <- function(hospitals,
                                      seed_hospital,
                                      n_seed_infected = 1) {
  hospitals %>%
    mutate(
      n_infected = if_else(
        finess_geo == seed_hospital,
        pmin(n_seed_infected, no_beds),
        0L
      ),
      prevalence = n_infected / no_beds
    )
}

# ------------------------------------------------------------
# 4.2 Compute infected transfers (edge-level)
# ------------------------------------------------------------
compute_daily_infected_transfers_fast <- function(state, transfers_day) {
  
  if (nrow(transfers_day) == 0) return(tibble())
  
  idx_origin            <- match(transfers_day$finessGeo_origin, state$finess_geo)
  prev_origin           <- state$prevalence[idx_origin]
  n_infected_origin     <- state$n_infected[idx_origin]
  
  prev_origin[is.na(prev_origin)]           <- 0
  n_infected_origin[is.na(n_infected_origin)] <- 0L
  
  infected_raw   <- rbinom(
    n    = nrow(transfers_day),
    size = transfers_day$weight,
    prob = prev_origin
  )
  
  infected_final <- infected_raw
  split_idx      <- split(seq_len(nrow(transfers_day)),
                          transfers_day$finessGeo_origin)
  
  for (idx in split_idx) {
    available <- n_infected_origin[idx[1]]
    proposed  <- sum(infected_raw[idx])
    
    if (proposed > available && available > 0) {
      scaled    <- infected_raw[idx] * available / proposed
      base      <- floor(scaled)
      remainder <- scaled - base
      leftover  <- available - sum(base)
      
      if (leftover > 0) {
        add_idx      <- order(remainder, decreasing = TRUE)[seq_len(leftover)]
        base[add_idx] <- base[add_idx] + 1L
      }
      infected_final[idx] <- base
    }
  }
  
  tibble(
    finessGeo_origin  = transfers_day$finessGeo_origin,
    finessGeo_target  = transfers_day$finessGeo_target,
    infected_transfer = infected_final
  )
}

# ------------------------------------------------------------
# 4.3 One-day SIS update (aggregated)
# ------------------------------------------------------------
simulate_one_day_agg_fast <- function(state,
                                      transfers_day,
                                      current_date,
                                      beta_within,
                                      gamma_clear,
                                      admission_prev) {
  
  transfers_inf <- compute_daily_infected_transfers_fast(state, transfers_day)
  
  infected_out <- integer(nrow(state))
  infected_in  <- integer(nrow(state))
  
  if (nrow(transfers_inf) > 0) {
    idx_out <- match(transfers_inf$finessGeo_origin, state$finess_geo)
    idx_in  <- match(transfers_inf$finessGeo_target, state$finess_geo)
    
    infected_out[as.integer(names(tapply(
      transfers_inf$infected_transfer, idx_out, sum
    )))] <- tapply(transfers_inf$infected_transfer, idx_out, sum)
    
    infected_in[as.integer(names(tapply(
      transfers_inf$infected_transfer, idx_in, sum
    )))] <- tapply(transfers_inf$infected_transfer, idx_in, sum)
  }
  
  infected_replaced <- rbinom(
    n    = nrow(state),
    size = infected_out,
    prob = admission_prev
  )
  
  n_after_transfer <- state$n_infected -
    infected_out + infected_in + infected_replaced
  n_after_transfer <- pmax(0L, pmin(n_after_transfer, state$no_beds))
  
  n_cleared      <- rbinom(n = nrow(state), size = n_after_transfer, prob = gamma_clear)
  n_after_clear  <- n_after_transfer - n_cleared
  
  n_susceptible  <- state$no_beds - n_after_clear
  p_inf          <- 1 - exp(-beta_within * (n_after_clear / state$no_beds))
  p_inf          <- pmin(pmax(p_inf, 0), 1)
  
  n_new_inf <- rbinom(n = nrow(state), size = n_susceptible, prob = p_inf)
  n_final   <- pmin(state$no_beds, n_after_clear + n_new_inf)
  
  state_new <- tibble(
    finess_geo = state$finess_geo,
    no_beds    = state$no_beds,
    n_infected = n_final,
    prevalence = n_final / state$no_beds
  )
  
  daily_summary <- state_new %>% mutate(date = current_date)
  
  overall_summary <- tibble(
    date                  = current_date,
    total_patients        = sum(state_new$no_beds),
    total_infected        = sum(state_new$n_infected),
    overall_prevalence    = sum(state_new$n_infected) / sum(state_new$no_beds),
    n_hospitals_with_case = sum(state_new$n_infected > 0)
  )
  
  list(
    state           = state_new,
    daily_summary   = daily_summary,
    overall_summary = overall_summary
  )
}

# ============================================================
# 5. FULL SIMULATION WRAPPER
# ============================================================

run_sis_simulation_agg_fast <- function(hospitals,
                                        transfers,
                                        start_date,
                                        end_date,
                                        seed_hospital,
                                        n_seed_infected,
                                        beta_within,
                                        gamma_clear,
                                        admission_prev,
                                        seed) {
  
  set.seed(seed)
  
  state <- initialize_hospital_state(hospitals, seed_hospital, n_seed_infected)
  
  sim_dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  
  transfers_by_day <- transfers %>%
    mutate(transfer_date = as.Date(window_end)) %>%
    select(transfer_date, finessGeo_origin, finessGeo_target, weight) %>%
    split(.$transfer_date)
  
  daily_results   <- vector("list", length(sim_dates))
  overall_results <- vector("list", length(sim_dates))
  
  for (i in seq_along(sim_dates)) {
    day <- sim_dates[i]
    td  <- transfers_by_day[[as.character(day)]]
    
    if (is.null(td)) {
      td <- tibble(
        finessGeo_origin = character(),
        finessGeo_target = character(),
        weight           = integer()
      )
    }
    
    out <- simulate_one_day_agg_fast(
      state, td, day, beta_within, gamma_clear, admission_prev
    )
    
    state                <- out$state
    daily_results[[i]]   <- out$daily_summary
    overall_results[[i]] <- out$overall_summary
  }
  
  list(
    final_state      = state,
    hospital_results = bind_rows(daily_results),
    overall_results  = bind_rows(overall_results)
  )
}

# ============================================================
# 6. STEADY-STATE DETECTION
# ============================================================

detect_steady_state <- function(overall_results,
                                window_size = 30,
                                range_tol   = 0.0005,
                                slope_tol   = 0.00001) {
  
  traj <- overall_results %>% arrange(date)
  
  if (nrow(traj) < window_size) return(tibble(steady_state_reached = FALSE))
  
  last  <- traj %>% slice_tail(n = window_size) %>% mutate(t = row_number())
  range <- max(last$overall_prevalence) - min(last$overall_prevalence)
  slope <- coef(lm(overall_prevalence ~ t, data = last))[2]
  
  tibble(
    steady_state_reached     = range <= range_tol && abs(slope) <= slope_tol,
    steady_state_prevalence  = median(last$overall_prevalence),
    steady_state_day         = max(last$date),
    steady_state_range       = range,
    steady_state_slope       = slope
  )
}

# ============================================================
# 7. CALIBRATION GRID — SPLIT BY INDEXCODE
# ============================================================

beta_grid  <- seq(0.001, 0.04, by = 0.001)   # 40 beta values
n_rep_beta <- 100                              # 100 reps each
n_jobs     <- 10L                              # must match launch.sh seq

set.seed(900000)

# Build the full grid once (same seed = same grid on every node)
beta_calibration_grid_full <- tidyr::crossing(
  beta_within = beta_grid,
  rep_id      = seq_len(n_rep_beta)
) %>%
  mutate(
    sim_seed      = sample.int(.Machine$integer.max, n()),
    seed_hospital = sample(hospitals$finess_geo, n(), replace = TRUE)
  )

# Assign each row to a job (1..n_jobs) and keep only this job's slice
beta_calibration_grid_full <- beta_calibration_grid_full %>%
  mutate(job_id = ((row_number() - 1L) %% n_jobs) + 1L)

beta_calibration_grid <- beta_calibration_grid_full %>%
  filter(job_id == indexcode)

message(
  "Job ", indexcode, " / ", n_jobs,
  " — running ", nrow(beta_calibration_grid), " simulations"
)

# ============================================================
# 8. RUN SIMULATIONS
# ============================================================

run_one_beta <- function(beta_within, sim_seed, seed_hospital) {
  
  sim <- run_sis_simulation_agg_fast(
    hospitals       = hospitals,
    transfers       = weekly_transfers,
    start_date      = "2024-01-01",
    end_date        = "2026-12-31",
    seed_hospital   = seed_hospital,
    n_seed_infected = 1,
    beta_within     = beta_within,
    gamma_clear     = 1 / 387,
    admission_prev  = 0,
    seed            = sim_seed
  )
  
  # Drop hospital_results — 1432 hospitals x 1096 days x 400 sims = OOM
  # Only keep overall_results (network-level) and steady state metrics
  # Note: beta_within is NOT included here — it already exists in the
  # outer beta_calibration_grid, and unnest() would error on duplicate columns
  result <- tibble(
    steady          = list(detect_steady_state(sim$overall_results)),
    overall_results = list(sim$overall_results)
  )
  
  rm(sim)
  gc()
  
  result
}

beta_calibration_runs <- beta_calibration_grid %>%
  mutate(
    sim = purrr::pmap(
      list(beta_within, sim_seed, seed_hospital),
      function(b, s, h) run_one_beta(b, s, h)
    )
  ) %>%
  tidyr::unnest(sim)   # flatten the nested tibble returned by run_one_beta

message("Simulations complete. Saving output...")

# ============================================================
# 9. SAVE OUTPUT
# ============================================================

outfile_rds  <- sprintf("%s_beta_calibration_runs_index_%02d.rds",  run_date, indexcode)
outfile_done <- sprintf("%s_index_%02d_DONE.txt", run_date, indexcode)

saveRDS(beta_calibration_runs, file = file.path(output_dir, outfile_rds))

writeLines(
  sprintf(
    "Job finished on  : %s\nIndexcode        : %s\nRows saved       : %d\nOutput file      : %s\nRoot dir         : %s\nWorking dir      : %s",
    Sys.time(), indexcode, nrow(beta_calibration_runs), outfile_rds, root_dir, getwd()
  ),
  file.path(output_dir, outfile_done)
)

message("Done. Output written to: ", file.path(output_dir, outfile_rds))