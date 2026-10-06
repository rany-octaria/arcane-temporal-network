# =============================================================================
# calibration_incidence.R  —  Novel pathogen incidence calibration (CLUSTER)
# PATIENT FLOW: full-occupancy admission-based
#   n_discharge[h] = n_admit[h] from NO_INPATIENTS_ADMISSION_DAILY_DIRCT_HBN.csv
#   community_admit = n_admit - transfers_received
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
#   0.001 = novel/rare ARB (CRE low-burden, VRE Nordic)
#   0.010 = established ARB (ESBL-E moderate, MRSA moderate settings)
#   0.050 = highly endemic ARB (MRSA Southern/Eastern EU, ESBL-E high-burden)
pi_vec_scenarios <- c(0.001, 0.010, 0.050)

N_REP        <- 3
N_CYCLES        <- 2L
DAYS_PER_CYCLE  <- 366L           # 2024 is a leap year
Tmax            <- N_CYCLES * DAYS_PER_CYCLE   # 732 days
SS_WINDOW    <- 30L
SS_CV_THRESH <- 0.15
INC_WINDOW   <- 180L
INIT_INF     <- 5L
N_CORES      <- 8L
seed_base    <- 10000L + (JOB_INDEX - 1L) * 100000L

n_sims_job <- length(beta_grid) * length(pi_vec_scenarios) * N_REP
message(sprintf("Grid: %d betas x %d pi_vec x %d reps = %d sims this job",
                length(beta_grid), length(pi_vec_scenarios), N_REP, n_sims_job))
message("pi_vec values : ", paste(pi_vec_scenarios, collapse=", "))
message("Tmax          : ", Tmax, "d | N_CORES: ", N_CORES)

# =============================================================================
# 3. DATA LOADING
# =============================================================================

message("\nLoading network data...")
weekly_transfers <- readRDS(file.path(DATA_DIR, "weekly.RDS")) %>%
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

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

transfer_out <- weekly_transfers %>%
  transmute(origin=as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out=sum(weight,na.rm=TRUE), .groups="drop")
hospitals <- hospitals %>%
  left_join(transfer_out, by=c("finess_geo"="origin")) %>%
  mutate(total_out=replace(total_out, is.na(total_out), 0))
# p_tr: fraction of n_admit discharged that are inter-hospital transfers
# Denominator = mean daily admissions estimate (beds/7, updated below once
# admit_matrix is built; this initial estimate is used only for P_tr normalisation)
mean_daily_admit_est <- pmax(1.0, as.numeric(beds) / 7.0)
p_tr <- pmin(hospitals$total_out / pmax(mean_daily_admit_est, 1.0), 0.60)

message("Building P_tr (", H, " x ", H, ")...")
hosp_idx     <- setNames(seq_len(H), hospitals$finess_geo)
transfer_agg <- weekly_transfers %>%
  transmute(orig=hosp_idx[as.character(finess_geo_origin)],
            dest=hosp_idx[as.character(finess_geo_target)], weight) %>%
  filter(!is.na(orig) & !is.na(dest)) %>%
  group_by(orig, dest) %>%
  summarise(weight=sum(weight), .groups="drop")
P_tr <- matrix(0.0, H, H)
for (k in seq_len(nrow(transfer_agg)))
  P_tr[transfer_agg$dest[k], transfer_agg$orig[k]] <- transfer_agg$weight[k]
cs <- colSums(P_tr)
for (h in seq_len(H)) if (cs[h]>0) P_tr[,h] <- P_tr[,h]/cs[h]

transfer_idx      <- which(p_tr > 0 & colSums(P_tr) > 0)
seed_hospital_idx <- which.max(hospitals$total_out)
message("  Done. H=", H, " | Transfer-eligible: ", length(transfer_idx))

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

# Recompute p_tr using true mean daily admissions from admit_matrix
mean_daily_admit <- rowMeans(admit_matrix)
p_tr <- pmin(hospitals$total_out / pmax(mean_daily_admit, 1.0), 0.60)

n_in_data <- sum(hospitals$finess_geo %in% unique(admit_daily$finess_geo))
message(sprintf(
  "  admit_matrix: %d x %d | in data: %d | fallback: %d | range: %d-%d | mean: %.1f",
  nrow(admit_matrix), ncol(admit_matrix),
  n_in_data, H - n_in_data,
  min(admit_matrix), max(admit_matrix), mean(admit_matrix)))
# =============================================================================
# 5. SIMULATION FUNCTION
# pi_vec_val_sim passed per call — each simulation has its own value
# =============================================================================

# Helper: distribute n patients to destinations exactly proportional to probs.
# Uses the largest-remainder (Hamilton) method so sum(result) == n exactly.
# This replaces rmultinom — destination split matches P_tr deterministically;
# only the number of S vs I transferred (binomial/hypergeometric) is stochastic.
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


run_novel_simulation <- function(beta, seed, pi_vec_val_sim) {
  set.seed(seed)
  p_rec        <- 1 - exp(-gamma)
  beta_vec     <- rep(beta, H)
  pi_vec_local <- rep(pi_vec_val_sim, H)   # local per simulation
  
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
    
    # ── Step 2: discharge = n_admit  (full occupancy) ────────────────────────
    day_idx <- ((t - 1L) %% DAYS_PER_CYCLE) + 1L
    n_admit <- admit_matrix[, day_idx]
    n_admit <- pmin(n_admit, S_loc + I_loc)   # safety: can't discharge > present
    # Hypergeometric draw: preserves colonised proportion among leavers
    n_discharge_I <- rhyper(H,
                             pmax(0L, I_loc),
                             pmax(0L, S_loc),
                             pmin(n_admit, I_loc + S_loc))
    n_discharge_I <- pmin(n_discharge_I, I_loc)   # safety
    n_discharge_S <- pmin(n_admit - n_discharge_I, S_loc)
    I_loc <- I_loc - n_discharge_I
    S_loc <- S_loc - n_discharge_S

    # ── Step 3: inter-hospital transfers (subset of discharged) ──────────────
    S_tr_out <- rbinom(H, n_discharge_S, p_tr)
    I_tr_out <- rbinom(H, n_discharge_I, p_tr)
    S_tr_in  <- numeric(H); I_tr_in <- numeric(H)
    active_h <- which((S_tr_out + I_tr_out) > 0L)
    for (h in active_h) {
      probs <- P_tr[, h]; s <- sum(probs); if (s <= 0) next
      nS <- S_tr_out[h]; nI <- I_tr_out[h]
      if (nS > 0L) S_tr_in <- S_tr_in + allocate_exact(nS, probs)
      if (nI > 0L) I_tr_in <- I_tr_in + allocate_exact(nI, probs)
    }

    # ── Step 4: community admissions = n_admit - transfers received ───────────
    # Cap transfers that would exceed today's admissions (prevents overflow)
    transfers_received <- S_tr_in + I_tr_in
    overflow <- which(transfers_received > n_admit)
    for (h in overflow) {
      if (transfers_received[h] > 0L) {
        scale     <- n_admit[h] / transfers_received[h]
        S_tr_in[h] <- as.integer(floor(S_tr_in[h] * scale))
        I_tr_in[h] <- as.integer(floor(I_tr_in[h] * scale))
      }
    }
    transfers_received <- S_tr_in + I_tr_in
    community_admit    <- pmax(0L, n_admit - transfers_received)
    # Community colonisation uses tier-specific pi_vec
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
    pi_vec_val           = pi_vec_val_sim,
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

sim_grid <- tidyr::crossing(
  data.frame(beta       = beta_grid,        stringsAsFactors=FALSE),
  data.frame(pi_vec_sim = pi_vec_scenarios, stringsAsFactors=FALSE),
  data.frame(rep_id     = seq_len(N_REP),   stringsAsFactors=FALSE)
) %>%
  mutate(sim_seed = seed_base + row_number() * 7L)

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
      beta           = row$beta,
      seed           = row$sim_seed,
      pi_vec_val_sim = row$pi_vec_sim
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
    pi_vec_scenarios = pi_vec_scenarios,
    N_REP            = N_REP,
    Tmax             = Tmax,      N_CYCLES   = N_CYCLES,
    DAYS_PER_CYCLE   = DAYS_PER_CYCLE,
    patient_flow     = "admission-based full occupancy (n_discharge = n_admit)",
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
  cat("pi_vec breakdown:\n")
  for (pv in pi_vec_scenarios) {
    sub <- ne %>% filter(pi_vec_val == pv)
    if (nrow(sub) > 0)
      cat(sprintf("  pi=%.3f: %d sims | inc %.3f-%.3f /1k pd\n",
                  pv, nrow(sub),
                  min(sub$reported_inc), max(sub$reported_inc)))
  }
}
cat("Elapsed     :", elapsed, "min\n")
cat("Saved to    :", OUT_DIR, "\n")