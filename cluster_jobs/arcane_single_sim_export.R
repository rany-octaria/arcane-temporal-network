# ============================================================
# ARCANE — SINGLE SIMULATION HOSPITAL-LEVEL DATA EXPORT
# Author: Rany Octaria
# Purpose:
#   Run ONE simulation at a chosen beta and save the full
#   per-hospital daily state for network animation.
#   This is separate from the main calibration grid so it
#   does not interfere with main results.
#
# Run on cluster:
#   qsub -q mem128G -l nodes=bioclustnew04:ppn=4 \
#        cluster_jobs/arcane_single_sim_export.sh
# Or locally:
#   Rscript --vanilla cluster_jobs/arcane_single_sim_export.R
# ============================================================

library(tidyverse)
library(janitor)

# ============================================================
# 0. CONFIG
# ============================================================

BETA_EXPORT  <- 0.02          # beta value to visualise
SIM_SEED     <- 42L           # fixed seed for reproducibility
START_DATE   <- "2024-01-01"
END_DATE     <- "2026-12-31"
GAMMA_CLEAR  <- 1 / 387
ADMIT_PREV   <- 0

root_dir   <- Sys.getenv("PBS_O_WORKDIR", unset = getwd())
job_dir    <- file.path(normalizePath(root_dir), "cluster_jobs")
data_dir   <- file.path(job_dir, "data")
run_date   <- format(Sys.Date(), "%Y-%m-%d")
output_dir <- file.path(job_dir, "Outputs", run_date)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("BETA_EXPORT = ", BETA_EXPORT)
message("SIM_SEED    = ", SIM_SEED)
message("JOB_DIR     = ", job_dir)
message("OUTPUT_DIR  = ", output_dir)

# ============================================================
# 1. LOAD DATA
# ============================================================

message("Loading data...")
coords_beds_active <- readRDS(file.path(data_dir, "coords_beds_active.RDS"))
weekly_transfers   <- readRDS(file.path(data_dir, "weekly.RDS"))

# ============================================================
# 2. BUILD HOSPITAL UNIVERSE WITH COORDINATES
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
# 3. SEED HOSPITAL — random hospital from the network
# ============================================================

set.seed(SIM_SEED)   # ensures reproducibility of the random pick
seed_hospital <- hospitals %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  slice_sample(n = 1) %>%
  pull(finess_geo)

message("Seed hospital: ", seed_hospital)

# ============================================================
# 4. SIMULATION
# ============================================================

initialize_state <- function(hospitals, seed_hospital, n_seed = 1) {
  hospitals %>%
    mutate(
      n_infected = if_else(finess_geo == seed_hospital,
                           pmin(n_seed, no_beds), 0L),
      prevalence = n_infected / no_beds
    )
}

sim_one_day <- function(state, transfers_day,
                        beta_within, gamma_clear, admission_prev) {

  if (nrow(transfers_day) == 0) {
    return(state %>% mutate(
      prevalence = n_infected / no_beds
    ))
  }

  idx_o  <- match(transfers_day$finessGeo_origin, state$finess_geo)
  prev_o <- replace_na(state$prevalence[idx_o], 0)
  ninf_o <- replace_na(state$n_infected[idx_o], 0L)

  inf_raw <- rbinom(nrow(transfers_day), transfers_day$weight, prev_o)

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
        top <- order(scaled - base, decreasing = TRUE)[seq_len(leftover)]
        base[top] <- base[top] + 1L
      }
      inf_raw[idx] <- base
    }
  }

  n     <- nrow(state)
  out   <- integer(n)
  inn   <- integer(n)
  io    <- match(transfers_day$finessGeo_origin, state$finess_geo)
  it    <- match(transfers_day$finessGeo_target, state$finess_geo)
  for (k in seq_along(io)) {
    if (!is.na(io[k])) out[io[k]] <- out[io[k]] + inf_raw[k]
    if (!is.na(it[k])) inn[it[k]] <- inn[it[k]] + inf_raw[k]
  }

  replaced      <- rbinom(n, out, admission_prev)
  n_after_xfer  <- pmax(0L, pmin(state$n_infected - out + inn + replaced,
                                  state$no_beds))
  n_cleared     <- rbinom(n, n_after_xfer, gamma_clear)
  n_after_clear <- n_after_xfer - n_cleared
  n_suscept     <- state$no_beds - n_after_clear
  p_inf         <- pmin(pmax(1 - exp(-beta_within * n_after_clear / state$no_beds), 0), 1)
  n_new         <- rbinom(n, n_suscept, p_inf)
  n_final       <- pmin(state$no_beds, n_after_clear + n_new)

  state %>% mutate(n_infected = n_final, prevalence = n_final / no_beds)
}

set.seed(SIM_SEED)
state <- initialize_state(hospitals, seed_hospital)

sim_dates <- seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = "day")

transfers_by_day <- weekly_transfers %>%
  mutate(transfer_date = as.Date(window_end)) %>%
  select(transfer_date, finessGeo_origin, finessGeo_target, weight) %>%
  split(.$transfer_date)

daily_states <- vector("list", length(sim_dates))

message("Running simulation (", length(sim_dates), " days)...")

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

  state <- sim_one_day(state, td, BETA_EXPORT, GAMMA_CLEAR, ADMIT_PREV)

  # Save per-hospital state for every day
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

# ============================================================
# 5. SAVE OUTPUTS
# ============================================================

sim_trajectory <- bind_rows(daily_states)

# Save full trajectory
out_traj <- file.path(output_dir,
  sprintf("%s_hospital_trajectory_beta%.3f_seed%d.rds",
          run_date, BETA_EXPORT, SIM_SEED))
saveRDS(sim_trajectory, out_traj)
message("Hospital trajectory saved: ", basename(out_traj))

# Save top edges for animation (saves time when loading locally)
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
  filter(!is.na(lon_o), !is.na(lon_t))

out_edges <- file.path(output_dir,
  sprintf("%s_top_edges.rds", run_date))
saveRDS(top_edges, out_edges)
message("Top edges saved: ", basename(out_edges))

# Save hospital metadata
out_hosp <- file.path(output_dir,
  sprintf("%s_hospitals_coords.rds", run_date))
saveRDS(hospitals, out_hosp)
message("Hospital coords saved: ", basename(out_hosp))

writeLines(
  sprintf("Done: %s\nbeta: %s\nseed: %s\nrows: %d\nfile: %s",
          Sys.time(), BETA_EXPORT, SIM_SEED,
          nrow(sim_trajectory), out_traj),
  file.path(output_dir, sprintf("%s_single_sim_DONE.txt", run_date))
)

message("\n✓ All done.")
