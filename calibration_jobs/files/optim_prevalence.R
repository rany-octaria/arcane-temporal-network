# =============================================================================
# optim_prevalence.R  —  Prevalence calibration  (CLUSTER VERSION)
# =============================================================================
# Beta grid search: runs SIS model to MAX_DAYS, measures facility-level
# steady-state prevalence and 365-day incidence per 1,000 bed-days.
# Steady-state is assessed as a boolean at the END of the simulation
# (last 30 days, CV < 10% for ≥80% of case-bearing hospitals).
#
# Grid: seq(0.005, 0.03, by=0.001) — 26 values
# Reps: N_REP=3 per job × 10 jobs = 30 total reps per beta
# Seeds: offset by JOB_INDEX so all 10 jobs are fully independent
#
# Data location: $ARCANE_ROOT/data/
#   weekly.RDS               — daily-averaged transfer network
#   facility_level_final.RDS — beds, LOS, type, region per hospital
# =============================================================================

library(parallel)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# =============================================================================
# 0. PATHS AND JOB SETTINGS
# =============================================================================

ARCANE_ROOT <- Sys.getenv("ARCANE_ROOT",
                           "/media/kevinNFS2/rany/prev_calib_jobs")
JOB_INDEX   <- { v <- suppressWarnings(as.integer(Sys.getenv("jobindex")))
                  if (!is.na(v) && v > 0L) v else 1L }

DATA_DIR  <- file.path(ARCANE_ROOT, "data")
OUT_DIR   <- file.path(ARCANE_ROOT, "Outputs", "prevalence",
                        sprintf("job_%02d", JOB_INDEX))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. SETTINGS
# =============================================================================

beta_grid    <- seq(0.005, 0.03, by = 0.001)   # 26 values
N_REP        <- 3       # per job; 10 jobs × 3 = 30 total reps
MAX_DAYS     <- 730L    # 2 years; always runs to end
SS_WINDOW    <- 30L     # last 30 days used for SS boolean
SS_CV_THRESH <- 0.10    # CV < 10% = stable
SS_PROP_HOSP <- 0.80    # ≥80% of case-bearing hospitals must be stable
INC_WINDOW   <- 365L    # 365-day incidence lookback
INIT_PREV    <- 0.02    # 2% starting prevalence
gamma        <- 1 / 387
alpha        <- 0
pi_vec_val   <- 0.05

# FORK cluster on Linux — no clusterExport needed
N_CORES      <- max(1L, parallel::detectCores() - 1L)

# Seed block offset by job so all jobs are independent
seed_base    <- 10000L + (JOB_INDEX - 1L) * 100000L

message("=== ARCANE PREVALENCE CALIBRATION | Job ", JOB_INDEX, "/10 ===")
message("Root   : ", ARCANE_ROOT)
message("Output : ", OUT_DIR)
message("Grid   : ", min(beta_grid), "–", max(beta_grid),
        " (", length(beta_grid), " values) | Reps: ", N_REP,
        " | Cores: ", N_CORES)

# =============================================================================
# 2. AMR PREVALENCE TARGETS (ECDC EARS-Net 2022)
# =============================================================================

amr_targets <- tribble(
  ~pathogen,  ~prev_low, ~prev_mid, ~prev_high,
  "MRSA",      0.010,     0.115,     0.250,
  "VRE",       0.005,     0.080,     0.180,
  "ESBL-E",    0.020,     0.120,     0.300,
  "CRE",       0.002,     0.030,     0.100,
  "CRAB",      0.010,     0.070,     0.200
)
amr_long <- amr_targets %>%
  pivot_longer(starts_with("prev_"),
               names_to = "tier", names_prefix = "prev_",
               values_to = "target_prevalence") %>%
  mutate(tier = factor(tier, c("low","mid","high"),
                        labels = c("Low","Mid","High")))

# =============================================================================
# 3. DATA LOADING — identical to optim_france.R
# =============================================================================

message("Loading data...")
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
pi_vec         <- rep(pi_vec_val, H)
type_etab      <- hospitals$type_spares
total_beds_sum <- sum(beds)

# Transfer out-degree
transfer_out <- weekly_transfers %>%
  transmute(origin=as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out=sum(weight,na.rm=TRUE), .groups="drop")
hospitals <- hospitals %>%
  left_join(transfer_out, by=c("finess_geo"="origin")) %>%
  mutate(total_out=replace(total_out, is.na(total_out), 0))
p_tr <- pmin(hospitals$total_out / pmax(p_exit*beds, 1), 0.60)

message("Building P_tr (", H, " × ", H, ")...")
hosp_idx     <- setNames(seq_len(H), hospitals$finess_geo)
transfer_agg <- weekly_transfers %>%
  transmute(orig=hosp_idx[as.character(finess_geo_origin)],
            dest=hosp_idx[as.character(finess_geo_target)],
            weight) %>%
  filter(!is.na(orig) & !is.na(dest)) %>%
  group_by(orig,dest) %>%
  summarise(weight=sum(weight), .groups="drop")
P_tr <- matrix(0.0, H, H)
for (k in seq_len(nrow(transfer_agg)))
  P_tr[transfer_agg$dest[k], transfer_agg$orig[k]] <- transfer_agg$weight[k]
cs <- colSums(P_tr)
for (h in seq_len(H)) if (cs[h]>0) P_tr[,h] <- P_tr[,h]/cs[h]
transfer_idx <- which(p_tr > 0)
message("  Done. H=", H, " | Transfer-eligible: ", length(transfer_idx))

# =============================================================================
# 5. SIMULATION FUNCTION
# =============================================================================

run_prevalence_simulation <- function(beta, seed) {
  set.seed(seed)
  p_rec    <- 1 - exp(-gamma)
  beta_vec <- rep(beta, H)
  I_loc    <- rbinom(H, beds, pmax(rep(INIT_PREV, H), 1/beds))
  S_loc    <- beds - I_loc

  inc_buffer   <- matrix(0L,  nrow=INC_WINDOW, ncol=H)
  inc_buf_ptr  <- 1L
  prev_buffer  <- matrix(0.0, nrow=SS_WINDOW,  ncol=H)
  prev_buf_ptr <- 1L

  for (t in seq_len(MAX_DAYS)) {
    # Vectorised SIS
    N       <- S_loc + I_loc
    p_inf   <- ifelse(N>0L & is.finite(beta_vec),
                      1-exp(-beta_vec*I_loc/pmax(N,1L)), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    S_loc   <- S_loc - new_inf + recov
    I_loc   <- I_loc + new_inf - recov
    inc_buffer[inc_buf_ptr,]  <- new_inf
    inc_buf_ptr  <- inc_buf_ptr  %% INC_WINDOW + 1L
    prev_buffer[prev_buf_ptr,] <- I_loc/pmax(beds,1L)
    prev_buf_ptr <- prev_buf_ptr %% SS_WINDOW  + 1L
    # Vectorised exits
    n_exit_S <- rbinom(H, S_loc, p_exit)
    n_exit_I <- rbinom(H, I_loc, p_exit)
    S_loc    <- S_loc - n_exit_S
    I_loc    <- I_loc - n_exit_I
    # Sparse transfer loop
    S_tr <- numeric(H); I_tr <- numeric(H)
    active_h <- transfer_idx[(n_exit_S[transfer_idx]+
                                n_exit_I[transfer_idx])>0L]
    for (h in active_h) {
      nS <- rbinom(1L, n_exit_S[h], p_tr[h])
      nI <- rbinom(1L, n_exit_I[h], p_tr[h])
      if ((nS+nI)==0L) next
      probs <- P_tr[,h]
      if (nS>0L) S_tr <- S_tr + rmultinom(1L,nS,probs)[,1L]
      if (nI>0L) I_tr <- I_tr + rmultinom(1L,nI,probs)[,1L]
    }
    # Community admissions
    occ   <- S_loc+I_loc+S_tr+I_tr
    A     <- pmax(0L, beds-occ)
    A_I   <- rbinom(H, A, pi_vec)
    S_loc <- S_loc+S_tr+(A-A_I)
    I_loc <- I_loc+I_tr+A_I
  }

  # End-of-sim SS check: last SS_WINDOW (30) days
  hwc  <- which(colSums(prev_buffer) > 0)
  ss   <- FALSE
  if (length(hwc) > 0) {
    cmn <- colMeans(prev_buffer[,hwc,drop=FALSE])
    csd <- apply(prev_buffer[,hwc,drop=FALSE], 2, sd, na.rm=TRUE)
    ss  <- mean(csd/pmax(cmn,1e-9) < SS_CV_THRESH) >= SS_PROP_HOSP
  }

  inc_365  <- colSums(inc_buffer)/(beds*INC_WINDOW)*1000
  nat_inc  <- sum(inc_buffer)/(total_beds_sum*INC_WINDOW)*1000
  hosp_prv <- colMeans(prev_buffer)

  list(
    beta                 = beta,
    seed                 = seed,
    steady_state_reached = ss,
    overall_prevalence   = sum(I_loc)/total_beds_sum,
    nationwide_inc       = nat_inc,
    hosp_inc_365         = inc_365,
    hosp_prev_mean       = hosp_prv,
    hosp_type            = type_etab,
    type_inc_mean        = tapply(inc_365,  type_etab, mean, na.rm=TRUE),
    type_prev_mean       = tapply(hosp_prv, type_etab, mean, na.rm=TRUE)
  )
}

# =============================================================================
# 6. RUN GRID IN PARALLEL — FORK (Linux)
# =============================================================================

sim_grid <- expand.grid(beta=beta_grid, rep_id=seq_len(N_REP)) %>%
  mutate(sim_seed = seed_base + row_number()*7L)

message("Simulations this job: ", nrow(sim_grid),
        " (", length(beta_grid), " × ", N_REP, " reps)")

cl <- makeCluster(N_CORES, type="FORK")
t0 <- Sys.time()

all_results <- tryCatch(
  parLapply(cl, seq_len(nrow(sim_grid)), function(i) {
    row <- sim_grid[i,]
    run_prevalence_simulation(beta=row$beta, seed=row$sim_seed)
  }),
  finally = { try(stopCluster(cl), silent=TRUE) }
)

message("Elapsed: ", round(difftime(Sys.time(),t0,units="mins"),1), " min")

# =============================================================================
# 7. COMPILE AND SAVE
# =============================================================================

scalar_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  data.frame(job_index=JOB_INDEX, beta_within=r$beta,
             rep_id=sim_grid$rep_id[i], sim_seed=sim_grid$sim_seed[i],
             steady_state_reached=r$steady_state_reached,
             overall_prevalence=r$overall_prevalence,
             nationwide_inc=r$nationwide_inc, stringsAsFactors=FALSE)
}))

type_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  tibble(job_index=JOB_INDEX, beta_within=r$beta,
         rep_id=sim_grid$rep_id[i],
         type    = names(r$type_inc_mean),
         inc_1000= as.numeric(r$type_inc_mean),
         prev    = as.numeric(r$type_prev_mean))
}))

saveRDS(list(scalar_df=scalar_df, type_df=type_df,
             job_index=JOB_INDEX, datetime=Sys.time()),
        file.path(OUT_DIR, "results.rds"))

write.csv2(scalar_df, file.path(OUT_DIR, "scalar_results.csv"), row.names=FALSE)
write.csv2(type_df,   file.path(OUT_DIR, "type_results.csv"),   row.names=FALSE)

cat("\n=== JOB", JOB_INDEX, "DONE ===\n")
cat("SS reached:", mean(scalar_df$steady_state_reached)*100, "%\n")
cat("Prevalence range:",
    round(min(scalar_df$overall_prevalence),4), "–",
    round(max(scalar_df$overall_prevalence),4), "\n")
cat("Saved to:", OUT_DIR, "\n")
