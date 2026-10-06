# =============================================================================
# calibration_incidence.R  —  Novel pathogen incidence calibration (CLUSTER)
# PATIENT FLOW:
#   Discharge  = n_admit[h] from CSV; cycling through 366-day calendar
#   Transfers  = T_mat[j,h] = round(weekly/7); exact counts, no sampling
#   I/S split  = Binomial(n_transfers, I/N) per hospital per day
#   Community  = n_admit - incoming transfers (exact); pi_vec = 0 (novel pathogen)
#   Tmax       = 3 x 366 = 1,098 days; INC_WINDOW = last 180 days
# =============================================================================
# Searches for (beta, pi_vec) combinations that produce incidence within
# Low / Moderate / High ECDC tier ranges.
#
# pi_vec is now a simulation dimension — three values run simultaneously:
#   0.001  novel/rare ARB (minimal community carriage)
#   0.010  established ARB (1% admission prevalence)
#   0.050  endemic ARB (5% admission prevalence, Southern EU MRSA-like)
#
# Grid: 24 betas x 3 pi_vec x 3 reps = 216 sims per job
#       10 jobs = 2,160 total simulations
#
# The simulation decides which (beta, pi_vec) combinations hit each tier.
# compile_prevalence.R groups results by (beta, pi_vec) when mapping tiers.
#
# MEMORY FIX: N_CORES = 8 (43 workers caused OOM crash on mem128G)
# =============================================================================

library(parallel)
library(dplyr)     # dplyr does not depend on stringi/stringr

# =============================================================================
# 0. PATHS AND JOB INDEX
# =============================================================================

ARCANE_ROOT <- Sys.getenv("ARCANE_ROOT",
                          "/media/kevinNFS2/rany/calibration_jobs")
JOB_INDEX   <- { v <- suppressWarnings(as.integer(Sys.getenv("jobindex")))
if (!is.na(v) && v > 0L) v else 1L }

DATA_DIR <- file.path(ARCANE_ROOT, "data")
OUT_DIR  <- file.path(ARCANE_ROOT, "Outputs", "incidence",
                      sprintf("job_%02d", JOB_INDEX))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. LOAD INCIDENCE TARGETS FROM data/inc_targets.rds
# =============================================================================

targets_path <- file.path(DATA_DIR, "inc_targets.rds")
if (!file.exists(targets_path))
  stop("inc_targets.rds not found. Run prevalence_to_incidence.R first.\n",
       "Expected: ", targets_path)

targets_obj    <- readRDS(targets_path)
inc_targets    <- targets_obj$inc_targets
factor_network <- targets_obj$factor_network
los_network    <- targets_obj$los_network
gamma          <- targets_obj$gamma

message("=== INCIDENCE CALIBRATION | Job ", JOB_INDEX, "/10 ===")
message("Targets loaded from: ", targets_path)
for (i in seq_len(nrow(inc_targets))) {
  r <- inc_targets[i, ]
  message(sprintf("  %-10s  %.2f - %.2f /1k pd  [%s]",
                  as.character(r$tier), r$inc_low, r$inc_high,
                  r$ecdc_category))
}

# =============================================================================
# 2. SETTINGS
# =============================================================================

beta_grid <- seq(0.005, 0.120, by = 0.005)   # 24 values

# pi_vec scenarios — community carriage on admission:
# pi_vec = 0: no community importation (novel pathogen assumption)

N_REP        <- 3
N_CYCLES       <- 3L
DAYS_PER_CYCLE <- 366L
Tmax           <- N_CYCLES * DAYS_PER_CYCLE   # 1,098 days (3 calendar years)
SS_WINDOW    <- 30L
SS_CV_THRESH <- 0.15
INC_WINDOW   <- 180L
INIT_INF     <- 5L
N_CORES      <- 8L
seed_base    <- 10000L + (JOB_INDEX - 1L) * 100000L

n_sims_job <- length(beta_grid) * N_REP
message(sprintf("Grid: %d betas x %d reps = %d sims this job (pi_vec = 0)",
                length(beta_grid), N_REP, n_sims_job))
message("Tmax          : ", Tmax, "d | N_CORES: ", N_CORES)

# =============================================================================
# 3. DATA LOADING
# =============================================================================

message("\nLoading network data...")
# weight / 7 = exact mean daily transfer count per hospital pair.
# pmax(0L): keep true zeros — do NOT force minimum 1 (that creates false routes).
weekly_transfers <- readRDS(file.path(DATA_DIR, "weekly.RDS")) %>%
  mutate(weight = pmax(0L, as.integer(round(weight / 7))))

facility_level <- readRDS(file.path(DATA_DIR, "facility_level_final.RDS")) %>%
  mutate(finess_geo = as.character(finess_geo)) %>%
  rename(incidence_esbl_all = incidence_region_type_ESBL_all)

# =============================================================================
# 4. HOSPITAL UNIVERSE
# =============================================================================

hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo=as.character(finess_geo_origin)),
  weekly_transfers %>% transmute(finess_geo=as.character(finess_geo_target))
) %>% distinct() %>%
  left_join(
    facility_level %>% transmute(
      finess_geo, hospital_type, type_spares, region,
      no_beds = as.integer(round(census_max))
    ), by="finess_geo"
  ) %>%
  mutate(
    no_beds       = as.integer(if_else(is.na(no_beds),
                                       as.integer(round(mean(no_beds,na.rm=TRUE))),
                                       no_beds)),
    type_spares   = if_else(is.na(type_spares),   "Unknown", type_spares),
    hospital_type = if_else(is.na(hospital_type), "Unknown", hospital_type),
    region        = if_else(is.na(region),         "Unknown", region)
  )

H              <- nrow(hospitals)
beds           <- hospitals$no_beds
type_etab      <- hospitals$type_spares
total_beds_sum <- sum(beds)

# Aggregate total daily transfers out per hospital (for seed selection)
transfer_out <- weekly_transfers %>%
  transmute(origin = as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out = sum(weight, na.rm = TRUE), .groups = "drop")
hospitals <- hospitals %>%
  left_join(transfer_out, by = c("finess_geo" = "origin")) %>%
  mutate(total_out = replace(total_out, is.na(total_out), 0))

# Build T_mat: exact daily transfer count matrix
# T_mat[j, h] = exact daily patients transferred from h to j
# = round(weekly_weight / 7), minimum 0 (no false routes)
message("Building T_mat (", H, " x ", H, ")...")
hosp_idx     <- setNames(seq_len(H), hospitals$finess_geo)
transfer_agg <- weekly_transfers %>%
  filter(weight > 0L) %>%
  transmute(orig = hosp_idx[as.character(finess_geo_origin)],
            dest = hosp_idx[as.character(finess_geo_target)],
            weight) %>%
  filter(!is.na(orig) & !is.na(dest))
T_mat <- matrix(0L, H, H)
for (k in seq_len(nrow(transfer_agg)))
  T_mat[transfer_agg$dest[k], transfer_agg$orig[k]] <- transfer_agg$weight[k]
storage.mode(T_mat) <- "integer"

# Precompute daily totals — constant across all 366 days
total_out_daily <- as.integer(colSums(T_mat))  # patients leaving  h per day
total_in_daily  <- as.integer(rowSums(T_mat))  # patients arriving at h per day

transfer_idx      <- which(total_out_daily > 0L)
seed_hospital_idx <- which.max(hospitals$total_out)
message("  T_mat built | Transfer-eligible hospitals: ", length(transfer_idx),
        " | Total daily transfers: ", sum(total_out_daily))

# =============================================================================
# 4b. DAILY ADMISSION MATRIX  (full-occupancy patient flow)  -- BASE R ONLY
# =============================================================================
# Source: NO_INPATIENTS_ADMISSION_DAILY_DIRCT_HBN.csv
#   columns: FinessGeo ; no_admissions ; date_entree  (dd/mm/yyyy)
#
# admit_matrix[h, d]  = total admissions to hospital h on day d  (d = 1..366)
# Inside the loop:
#   n_discharge[h]  = n_admit[h]          (discharge = admit: occupancy stable)
#   n_discharge_I   ~ Hypergeometric       (preserves colonised proportion)
#   transfers_in    routed from discharged patients via P_tr
#   community_admit = n_admit - transfers_in
#   A_I             ~ Binomial(community_admit, pi_vec)
#
# Hospitals absent from the CSV fall back to beds/7.
# Base R only -- no tidyverse/dplyr (avoids ICU/stringi cluster issues).
# CRITICAL: rebuild with matrix(pmax(...), nrow, ncol) after pmax() because
#           pmax() strips matrix dimensions.
# =============================================================================

ADMIT_FILE <- file.path(DATA_DIR, "NO_INPATIENTS_ADMISSION_DAILY_DIRCT_HBN.csv")
if (!file.exists(ADMIT_FILE))
  stop("Admission data not found: ", ADMIT_FILE)

message("\nLoading daily admission data...")
admit_raw <- read.csv(
  ADMIT_FILE, sep = ";", header = TRUE,
  col.names  = c("finess_geo", "no_admissions", "date_entree"),
  colClasses = c("character", "integer", "character"),
  stringsAsFactors = FALSE
)
admit_raw$day_of_year <- as.integer(
  format(as.Date(admit_raw$date_entree, "%d/%m/%Y"), "%j"))

# Per-hospital per-day mean admissions (aggregate handles any duplicate dates)
admit_daily <- aggregate(no_admissions ~ finess_geo + day_of_year,
                         data = admit_raw,
                         FUN  = function(x) round(mean(x, na.rm = TRUE)))
names(admit_daily)[3] <- "admissions"

# Per-hospital annual mean -- used to fill missing days
hosp_means <- aggregate(admissions ~ finess_geo,
                         data = admit_daily,
                         FUN  = function(x) as.integer(round(mean(x, na.rm = TRUE))))
names(hosp_means)[2] <- "mean_admit"

# Build H x 366 integer matrix aligned to simulation hospital order
admit_matrix <- matrix(NA_integer_, nrow = H, ncol = 366L)
rownames(admit_matrix) <- hospitals$finess_geo

for (h in seq_len(H)) {
  fgeo <- hospitals$finess_geo[h]
  sub  <- admit_daily[admit_daily$finess_geo == fgeo, ]
  if (nrow(sub) > 0L) {
    admit_matrix[h, sub$day_of_year] <- as.integer(round(sub$admissions))
    if (any(is.na(admit_matrix[h, ]))) {
      hm <- hosp_means$mean_admit[hosp_means$finess_geo == fgeo]
      hm <- if (length(hm) > 0L && !is.na(hm[1L])) hm[1L] else
              as.integer(round(beds[h] / 7L))
      admit_matrix[h, is.na(admit_matrix[h, ])] <- pmax(1L, hm)
    }
  } else {
    admit_matrix[h, ] <- pmax(1L, as.integer(round(beds[h] / 7L)))
  }
}

# Final NA sweep; rebuild as integer matrix
# (pmax strips dims on a matrix -- must call matrix() explicitly after)
admit_matrix[is.na(admit_matrix)] <- 1L
storage.mode(admit_matrix) <- "integer"
admit_matrix <- matrix(pmax(1L, as.vector(admit_matrix)), nrow = H, ncol = 366L)
rownames(admit_matrix) <- hospitals$finess_geo
stopifnot(is.matrix(admit_matrix), nrow(admit_matrix) == H,
          !any(is.na(admit_matrix)))

# T_mat already built above — no p_tr needed (transfers are exact counts)

n_in_data <- sum(hospitals$finess_geo %in% unique(admit_daily$finess_geo))
message(sprintf(
  "  admit_matrix: %d x %d | in data: %d | fallback: %d | range: %d-%d | mean: %.1f",
  nrow(admit_matrix), ncol(admit_matrix),
  n_in_data, H - n_in_data,
  min(admit_matrix), max(admit_matrix), mean(admit_matrix)))
# =============================================================================
# 5. SIMULATION FUNCTION
# pi_vec = 0 for all simulations (novel pathogen, no community importation)
# =============================================================================

# Helper: distribute n patients to destinations exactly proportional to probs.
# Uses the largest-remainder (Hamilton) method so sum(result) == n exactly.
# allocate_exact: distribute n patients to destinations proportional to weights.
# Used to split I_out and S_out across exact T_mat destinations.
# Hamilton largest-remainder ensures sum(result) == n exactly.
allocate_exact <- function(n, probs) {
  if (n == 0L || sum(probs) == 0) return(integer(length(probs)))
  exact  <- n * probs / sum(probs)
  floors <- as.integer(floor(exact))
  rem    <- n - sum(floors)
  if (rem > 0L) {
    top <- order(exact - floors, decreasing = TRUE)[seq_len(rem)]
    floors[top] <- floors[top] + 1L
  }
  floors
}


run_novel_simulation <- function(beta, seed) {
  set.seed(seed)
  p_rec        <- 1 - exp(-gamma)
  beta_vec     <- rep(beta, H)
  pi_vec_local <- 0L   # novel pathogen: no community importation   # local per simulation
  
  I_loc <- integer(H)
  I_loc[seed_hospital_idx] <- min(INIT_INF, beds[seed_hospital_idx])
  S_loc <- beds - I_loc
  
  inc_daily      <- numeric(Tmax)
  pt_days_daily  <- numeric(Tmax)
  net_prev_daily <- numeric(Tmax)
  
  for (t in seq_len(Tmax)) {
    
    # Record BEFORE transmission (correct LOS-based denominator)
    pt_days_daily[t]  <- sum(S_loc + I_loc)
    net_prev_daily[t] <- sum(I_loc) / total_beds_sum
    
    # Vectorised SIS transmission
    N       <- S_loc + I_loc
    p_inf   <- ifelse(N > 0L & is.finite(beta_vec),
                      1 - exp(-beta_vec * I_loc / pmax(N, 1L)), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    inc_daily[t] <- sum(new_inf)
    S_loc   <- S_loc - new_inf + recov
    I_loc   <- I_loc + new_inf - recov
    
    # ── Step 2: exact daily admissions (cycle through 366-day calendar) ──────
    day_idx <- ((t - 1L) %% DAYS_PER_CYCLE) + 1L
    n_admit <- admit_matrix[, day_idx]
    n_discharge <- pmin(n_admit, S_loc + I_loc)  # safety: cap at ward occupancy

    # ── Step 3: exact transfers with stochastic I/S split ────────────────────
    # Total out of h = total_out_daily[h] (from T_mat); cap at n_discharge.
    # I/S split of outgoing patients = Binomial(n_out_cap, I_loc/N) vectorised.
    # Destinations = deterministic via allocate_exact using T_mat as weights.
    n_out_cap <- pmin(total_out_daily, n_discharge)  # vectorised cap
    N_cur     <- S_loc + I_loc
    p_I_cur   <- ifelse(N_cur > 0L, I_loc / N_cur, 0.0)  # current I fraction

    # Stochastic I/S split for total outgoing (one Binomial draw per hospital)
    I_out_total <- rbinom(H, n_out_cap, p_I_cur)
    I_out_total <- pmin(I_out_total, I_loc)                     # safety
    S_out_total <- pmin(n_out_cap - I_out_total, S_loc)         # safety

    # Distribute I and S to exact destinations using T_mat as weights
    S_tr_in <- integer(H); I_tr_in <- integer(H)
    for (h in transfer_idx) {
      if (n_out_cap[h] == 0L) next
      nI <- I_out_total[h]; nS <- S_out_total[h]
      if (nI > 0L) I_tr_in <- I_tr_in + allocate_exact(nI, T_mat[, h])
      if (nS > 0L) S_tr_in <- S_tr_in + allocate_exact(nS, T_mat[, h])
    }

    # Remove outgoing from wards (transfers out + community discharges)
    n_community_discharge <- n_discharge - n_out_cap
    I_community_out <- rbinom(H, n_community_discharge, p_I_cur)
    I_community_out <- pmin(I_community_out, pmax(0L, I_loc - I_out_total))
    S_community_out <- pmin(n_community_discharge - I_community_out,
                             pmax(0L, S_loc - S_out_total))
    I_loc <- pmax(0L, I_loc - I_out_total - I_community_out)
    S_loc <- pmax(0L, S_loc - S_out_total - S_community_out)

    # ── Step 4: community admissions = n_admit - exact incoming transfers ─────
    # total_in_daily[h] = rowSums(T_mat)[h] = constant daily incoming (from data)
    # On days when incoming transfers > n_admit, community_admit = 0
    transfers_received <- S_tr_in + I_tr_in
    community_admit    <- pmax(0L, n_admit - transfers_received)
    A_I   <- rbinom(H, community_admit, pi_vec_local)
    A_S   <- community_admit - A_I
    S_loc <- S_loc + S_tr_in + A_S
    I_loc <- I_loc + I_tr_in + A_I
  }
  
  inc_rate_daily <- ifelse(pt_days_daily > 0,
                           inc_daily / pt_days_daily * 1000, 0)
  
  win          <- tail(seq_len(Tmax), INC_WINDOW)
  reported_inc <- sum(inc_daily[win]) /
    max(sum(pt_days_daily[win]), 1) * 1000
  
  ss_inc <- tail(inc_rate_daily, SS_WINDOW)
  ss_m   <- mean(ss_inc)
  ss_cv  <- if (ss_m > 0) sd(ss_inc) / ss_m else Inf
  
  list(
    beta                 = beta,
    pi_vec_val           = 0L,
    seed                 = seed,
    reported_inc         = reported_inc,
    net_prev_final       = sum(I_loc) / total_beds_sum,
    steady_state_cv      = round(ss_cv, 4),
    steady_state_reached = ss_cv < SS_CV_THRESH,
    extinct              = sum(I_loc) == 0L,
    inc_rate_daily       = inc_rate_daily,
    net_prev_daily       = net_prev_daily
  )
}

# =============================================================================
# 6. SIMULATION GRID — beta x pi_vec x rep
# =============================================================================

# pi_vec = 0: no community importation (novel pathogen assumption)
sim_grid <- expand.grid(beta   = beta_grid,
                         rep_id = seq_len(N_REP),
                         KEEP.OUT.ATTRS   = FALSE,
                         stringsAsFactors = FALSE)
sim_grid$sim_seed <- seed_base + seq_len(nrow(sim_grid)) * 7L

# =============================================================================
# 7. RUN — FORK (Linux, memory-safe)
# =============================================================================

message("\nStarting ", N_CORES, " FORK workers...")
cl <- makeCluster(N_CORES, type="FORK")
t0 <- Sys.time()

all_results <- tryCatch(
  parLapply(cl, seq_len(nrow(sim_grid)), function(i) {
    row <- sim_grid[i, ]
    run_novel_simulation(
      beta = row$beta,
      seed = row$sim_seed
    )
  }),
  finally = { try(stopCluster(cl), silent=TRUE) }
)

elapsed <- round(difftime(Sys.time(), t0, units="mins"), 1)
message("Elapsed: ", elapsed, " min")

# =============================================================================
# 8. COMPILE — pi_vec_val stored per row
# =============================================================================

scalar_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  data.frame(
    job_index            = JOB_INDEX,
    beta_within          = r$beta,
    pi_vec_val           = r$pi_vec_val,
    rep_id               = sim_grid$rep_id[i],
    sim_seed             = sim_grid$sim_seed[i],
    reported_inc         = r$reported_inc,
    net_prev_final       = r$net_prev_final,
    steady_state_cv      = r$steady_state_cv,
    steady_state_reached = r$steady_state_reached,
    extinct              = r$extinct,
    stringsAsFactors = FALSE
  )
}))

traj_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  data.frame(
    job_index   = JOB_INDEX,
    beta_within = r$beta,
    pi_vec_val  = r$pi_vec_val,
    rep_id      = sim_grid$rep_id[i],
    extinct     = r$extinct,
    day         = seq_len(Tmax),
    inc_rate    = r$inc_rate_daily,
    net_prev    = r$net_prev_daily,
    stringsAsFactors = FALSE
  )
}))

# =============================================================================
# 9. SAVE
# =============================================================================

saveRDS(list(
  scalar_df   = scalar_df,
  traj_df     = traj_df,
  job_index   = JOB_INDEX,
  datetime    = Sys.time(),
  elapsed_min = as.numeric(elapsed),
  params      = list(
    beta_grid        = beta_grid,
    pi_vec           = 0L,   # no community importation
    N_REP            = N_REP,
    Tmax             = Tmax,      # 3 x 366 = 1,098 days
    patient_flow     = "admission-based full occupancy | exact daily transfers from T_mat | Binomial I/S split",
    pi_vec           = 0L,   # no community importation (novel pathogen)
    SS_WINDOW        = SS_WINDOW, INC_WINDOW = INC_WINDOW,
    gamma            = gamma,
    factor_network   = factor_network,
    los_network      = los_network,
    H = H, total_beds = total_beds_sum,
    seed_hospital    = hospitals$finess_geo[seed_hospital_idx],
    inc_targets_file = targets_path
  )
), file.path(OUT_DIR, "results.rds"))

write.csv(scalar_df,
          file.path(OUT_DIR, "scalar_results.csv"),
          row.names = FALSE)

cat("\n=== JOB", JOB_INDEX, "DONE ===\n")
cat("Sims        :", nrow(scalar_df), "\n")
cat("Extinct     :", round(mean(scalar_df$extinct)*100, 1), "%\n")
cat("SS reached  :", round(mean(scalar_df$steady_state_reached)*100, 1), "%\n")
ne <- scalar_df %>% filter(!extinct)
if (nrow(ne) > 0) {
  cat("Inc range   :", round(min(ne$reported_inc),3),
      "-", round(max(ne$reported_inc),3), "/1k pd\n")
  cat("pi_vec = 0 (novel pathogen, no community importation)\n")
}
cat("Elapsed     :", elapsed, "min\n")
cat("Saved to    :", OUT_DIR, "\n")