
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
# ------------------------------------------------------------


# ============================================================
# 1. SETUP
# ============================================================

library(tidyverse)
library(furrr)
library(future)
library(progressr)
library(here)

plan(multisession, workers = max(1, parallelly::availableCores() - 1))
handlers("txtprogressbar")

setwd(here("Cluster Jobs"))


# ============================================================
# 2. DATA LOADING
# ============================================================

coords_beds_active <- readRDS("coords_beds_active.RDS")
weekly_transfers  <- readRDS("weekly.RDS")


# ============================================================
# 3. BUILD HOSPITAL UNIVERSE
# ============================================================

# Hospitals appearing as origin or destination
hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finessGeo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finessGeo_target))
) %>%
  distinct()

# Attach number of beds
hospitals <- hospitals %>%
  left_join(
    coords_beds_active %>%
      clean_names() %>%
      transmute(
        finess_geo = as.character(finess_geo),
        no_beds = readr::parse_number(as.character(no_beds))
      ),
    by = "finess_geo"
  )

# Impute missing beds with mean
mean_beds <- round(mean(hospitals$no_beds, na.rm = TRUE))

hospitals <- hospitals %>%
  mutate(
    no_beds = if_else(is.na(no_beds), mean_beds, no_beds),
    no_beds = as.integer(no_beds)
  )


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
# - Binomial sampling on each transfer
# - Caps total outgoing infected per hospital
compute_daily_infected_transfers_fast <- function(state, transfers_day) {
  
  if (nrow(transfers_day) == 0) {
    return(tibble())
  }
  
  idx_origin <- match(transfers_day$finessGeo_origin, state$finess_geo)
  
  prev_origin        <- state$prevalence[idx_origin]
  n_infected_origin  <- state$n_infected[idx_origin]
  
  prev_origin[is.na(prev_origin)] <- 0
  n_infected_origin[is.na(n_infected_origin)] <- 0L
  
  infected_raw <- rbinom(
    n = nrow(transfers_day),
    size = transfers_day$weight,
    prob = prev_origin
  )
  
  infected_final <- infected_raw
  
  # Cap outgoing infections per origin hospital
  split_idx <- split(seq_len(nrow(transfers_day)),
                     transfers_day$finessGeo_origin)
  
  for (idx in split_idx) {
    available <- n_infected_origin[idx[1]]
    proposed  <- sum(infected_raw[idx])
    
    if (proposed > available && available > 0) {
      scaled   <- infected_raw[idx] * available / proposed
      base     <- floor(scaled)
      remainder <- scaled - base
      leftover <- available - sum(base)
      
      if (leftover > 0) {
        add_idx <- order(remainder, decreasing = TRUE)[seq_len(leftover)]
        base[add_idx] <- base[add_idx] + 1L
      }
      infected_final[idx] <- base
    }
  }
  
  tibble(
    finessGeo_origin = transfers_day$finessGeo_origin,
    finessGeo_target = transfers_day$finessGeo_target,
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
  
  # ---- Transfers ----
  transfers_inf <- compute_daily_infected_transfers_fast(
    state, transfers_day
  )
  
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
  
  # ---- Replacement from community ----
  infected_replaced <- rbinom(
    n = nrow(state),
    size = infected_out,
    prob = admission_prev
  )
  
  n_after_transfer <- state$n_infected -
    infected_out + infected_in + infected_replaced
  
  n_after_transfer <- pmax(0L, pmin(n_after_transfer, state$no_beds))
  
  # ---- Clearance ----
  n_cleared <- rbinom(
    n = nrow(state),
    size = n_after_transfer,
    prob = gamma_clear
  )
  
  n_after_clear <- n_after_transfer - n_cleared
  
  # ---- Within-hospital transmission ----
  n_susceptible <- state$no_beds - n_after_clear
  
  p_inf <- 1 - exp(-beta_within * (n_after_clear / state$no_beds))
  p_inf <- pmin(pmax(p_inf, 0), 1)
  
  n_new_inf <- rbinom(
    n = nrow(state),
    size = n_susceptible,
    prob = p_inf
  )
  
  n_final <- pmin(state$no_beds, n_after_clear + n_new_inf)
  
  # ---- Updated state ----
  state_new <- tibble(
    finess_geo = state$finess_geo,
    no_beds    = state$no_beds,
    n_infected = n_final,
    prevalence = n_final / state$no_beds
  )
  
  daily_summary <- state_new %>%
    mutate(date = current_date)
  
  overall_summary <- tibble(
    date = current_date,
    total_patients = sum(state_new$no_beds),
    total_infected = sum(state_new$n_infected),
    overall_prevalence = sum(state_new$n_infected) /
      sum(state_new$no_beds),
    n_hospitals_with_case = sum(state_new$n_infected > 0)
  )
  
  list(
    state = state_new,
    daily_summary = daily_summary,
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
  
  state <- initialize_hospital_state(
    hospitals, seed_hospital, n_seed_infected
  )
  
  sim_dates <- seq.Date(
    as.Date(start_date),
    as.Date(end_date),
    by = "day"
  )
  
  transfers_by_day <- transfers %>%
    mutate(transfer_date = as.Date(window_end)) %>%
    select(
      transfer_date,
      finessGeo_origin,
      finessGeo_target,
      weight
    ) %>%
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
        weight = integer()
      )
    }
    
    out <- simulate_one_day_agg_fast(
      state,
      td,
      day,
      beta_within,
      gamma_clear,
      admission_prev
    )
    
    state <- out$state
    daily_results[[i]]   <- out$daily_summary
    overall_results[[i]] <- out$overall_summary
  }
  
  list(
    final_state = state,
    hospital_results = bind_rows(daily_results),
    overall_results  = bind_rows(overall_results)
  )
}


# ============================================================
# 6. STEADY-STATE DETECTION
# ============================================================

detect_steady_state <- function(
    overall_results,
    window_size = 30,
    range_tol = 0.0005,
    slope_tol = 0.00001) {
  
  traj <- overall_results %>% arrange(date)
  
  if (nrow(traj) < window_size) {
    return(tibble(steady_state_reached = FALSE))
  }
  
  last <- traj %>% slice_tail(n = window_size) %>%
    mutate(t = row_number())
  
  range <- max(last$overall_prevalence) -
    min(last$overall_prevalence)
  
  slope <- coef(lm(overall_prevalence ~ t, data = last))[2]
  
  tibble(
    steady_state_reached = range <= range_tol &&
      abs(slope) <= slope_tol,
    steady_state_prevalence = median(last$overall_prevalence),
    steady_state_day = max(last$date),
    steady_state_range = range,
    steady_state_slope = slope
  )
}


# ============================================================
# 7. CALIBRATION GRID & PARALLEL RUNS
# ============================================================

beta_grid <- seq(0.005, 0.04, by = 0.001)
n_rep_beta <- 50

set.seed(900000)

beta_calibration_grid <- tidyr::crossing(
  beta_within = beta_grid,
  rep_id = seq_len(n_rep_beta)
) %>%
  mutate(
    sim_seed = sample.int(.Machine$integer.max, n()),
    seed_hospital = sample(hospitals$finess_geo, n(), replace = TRUE)
  )

run_one_beta <- function(beta_within, sim_seed, seed_hospital) {
  
  sim <- run_sis_simulation_agg_fast(
    hospitals = hospitals,
    transfers = weekly_transfers,
    start_date = "2024-01-01",
    end_date   = "2026-12-31",
    seed_hospital = seed_hospital,
    n_seed_infected = 1,
    beta_within = beta_within,
    gamma_clear = 1 / 387,
    admission_prev = 0,
    seed = sim_seed
  )
  
  tibble(
    beta_within = beta_within,
    steady = detect_steady_state(sim$overall_results),
    overall_results = list(sim$overall_results),
    hospital_results = list(sim$hospital_results)
  )
}

with_progress({
  p <- progressor(nrow(beta_calibration_grid))
  beta_calibration_runs <- beta_calibration_grid %>%
    mutate(
      sim = future_pmap(
        list(beta_within, sim_seed, seed_hospital),
        function(b, s, h) {
          p()
          run_one_beta(b, s, h)
        }
      )
    )
})

saveRDS(beta_calibration_runs,
        here("Outputs", "beta_calibration_runs.rds"))

    
