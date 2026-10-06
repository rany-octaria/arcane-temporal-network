# =============================================================================
# calibration_incidence.R  —  Novel pathogen incidence calibration (CLUSTER)
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
library(dplyr)
library(tidyr)

# =============================================================================
# 0. PATHS AND JOB INDEX
# =============================================================================

ARCANE_ROOT <- Sys.getenv("ARCANE_ROOT",
                          "/media/kevinNFS2/rany/calibration_jobs")
JOB_INDEX   <- { v <- suppressWarnings(as.integer(Sys.getenv("jobindex")))
if (!is.na(v) && v > 0L) v else 1L }

DATA_DIR <- file.path(ARCANE_ROOT, "data")
OUT_DIR  <- file.path(ARCANE_ROOT, "Outputs", "prevalence",
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
N_CYCLES     <- 2L
Tmax         <- N_CYCLES * 365L
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

default_los <- facility_level %>%
  filter(!is.na(hospital_type)) %>%
  group_by(hospital_type) %>%
  summarise(pt  = sum(pt_days_total, na.rm=TRUE),
            pat = sum(patient_total,  na.rm=TRUE), .groups="drop") %>%
  mutate(los_type = pt / pat)
DEFAULT_LOS_TYPE   <- setNames(default_los$los_type, default_los$hospital_type)
GLOBAL_DEFAULT_LOS <- with(
  facility_level %>% filter(!is.na(hospital_type)) %>%
    summarise(a=sum(pt_days_total,na.rm=TRUE),
              b=sum(patient_total, na.rm=TRUE)), a/b)

hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo=as.character(finess_geo_origin)),
  weekly_transfers %>% transmute(finess_geo=as.character(finess_geo_target))
) %>% distinct() %>%
  left_join(
    facility_level %>% transmute(
      finess_geo, hospital_type, type_spares, region,
      no_beds = as.integer(round(census_max)),
      los     = pmax(as.numeric(los_mean), 1.0)
    ), by="finess_geo"
  ) %>%
  mutate(
    no_beds       = as.integer(if_else(is.na(no_beds),
                                       as.integer(round(mean(no_beds,na.rm=TRUE))),
                                       no_beds)),
    los           = coalesce(los, DEFAULT_LOS_TYPE[hospital_type],
                             GLOBAL_DEFAULT_LOS),
    type_spares   = if_else(is.na(type_spares),   "Unknown", type_spares),
    hospital_type = if_else(is.na(hospital_type), "Unknown", hospital_type),
    region        = if_else(is.na(region),         "Unknown", region)
  )

H              <- nrow(hospitals)
beds           <- hospitals$no_beds
p_exit         <- 1 / hospitals$los
type_etab      <- hospitals$type_spares
total_beds_sum <- sum(beds)

transfer_out <- weekly_transfers %>%
  transmute(origin=as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out=sum(weight,na.rm=TRUE), .groups="drop")
hospitals <- hospitals %>%
  left_join(transfer_out, by=c("finess_geo"="origin")) %>%
  mutate(total_out=replace(total_out, is.na(total_out), 0))
p_tr <- pmin(hospitals$total_out / pmax(p_exit*beds, 1), 0.60)

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
# 5. SIMULATION FUNCTION
# pi_vec_val_sim passed per call — each simulation has its own value
# =============================================================================

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
    
    # Exits
    n_exit_S <- rbinom(H, S_loc, p_exit)
    n_exit_I <- rbinom(H, I_loc, p_exit)
    S_loc    <- S_loc - n_exit_S
    I_loc    <- I_loc - n_exit_I
    
    # Sparse inter-hospital transfers
    S_tr <- numeric(H); I_tr <- numeric(H)
    active_h <- transfer_idx[(n_exit_S[transfer_idx] +
                                n_exit_I[transfer_idx]) > 0L]
    for (h in active_h) {
      nS <- rbinom(1L, n_exit_S[h], p_tr[h])
      nI <- rbinom(1L, n_exit_I[h], p_tr[h])
      if ((nS + nI) == 0L) next
      probs <- P_tr[, h]; s <- sum(probs); if (s <= 0) next
      if (nS > 0L) S_tr <- S_tr + rmultinom(1L, nS, probs)[, 1L]
      if (nI > 0L) I_tr <- I_tr + rmultinom(1L, nI, probs)[, 1L]
    }
    
    # Admissions using tier-specific pi_vec
    occ   <- S_loc + I_loc + S_tr + I_tr
    A     <- pmax(0L, beds - occ)
    A_I   <- rbinom(H, A, pi_vec_local)
    S_loc <- S_loc + S_tr + (A - A_I)
    I_loc <- I_loc + I_tr + A_I
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