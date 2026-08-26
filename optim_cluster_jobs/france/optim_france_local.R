# =============================================================================
# optim_france.R  —  Beta calibration, France-wide
# =============================================================================
#
# WHAT THIS SCRIPT DOES (plain English)
# ──────────────────────────────────────
# We want to know: "how fast does ESBL spread *within* each type of hospital?"
# That speed is called beta (β) — one value per facility type (e.g. MCO, SSR).
#
# Method:
#   1. Build a computer model that simulates ESBL spreading through all French
#      hospitals, with patients moving between them using real transfer data.
#   2. Run that model many times with different candidate beta values.
#   3. At each candidate, measure how far the simulated infection rate is from
#      the real observed rate (SPARES data).  That distance is the SSE
#      (sum of squared errors) — the lower the better.
#   4. A mathematical optimizer (Nelder-Mead) searches the space of beta
#      values to find the combination that minimises the SSE.
#   5. The best beta found is saved as our calibrated transmission rate.
#
# THE SIMULATION MODEL (SIS — Susceptible → Infected → Susceptible)
# ──────────────────────────────────────────────────────────────────
# Every day, for every hospital:
#   • Some patients who are not carrying ESBL may acquire it (rate = beta).
#   • Some patients who carry ESBL may clear it (rate = gamma = 1/387 per day,
#     meaning average colonisation lasts ~387 days).
#   • Some patients are discharged; a fraction transfer to another hospital
#     (the rest go home).  Transfers follow the real transfer network.
#   • Empty beds are filled from the community (a small fraction carry ESBL).
# We count new ESBL acquisitions in the last 365 days of a 2-year run,
# then compute the incidence rate per 1,000 bed-days per facility type.
#
# PARAMETERS NEEDED (what you must supply)
# ─────────────────────────────────────────
# From weekly.RDS (the transfer network):
#   finess_geo_origin, finess_geo_target, weight
#     → daily average patient transfer counts between hospitals
#
# From facility_level_final.RDS (one row per hospital):
#   type_spares           → facility type (MCO, SSR, MCO/SSR, PSY, HAD, CLCC…)
#   region                → administrative region
#   census_max            → maximum observed census  = bed capacity in model
#   los_mean              → mean length of stay (days) = 1/daily_discharge_prob
#   incidence_region_type_ESBL_all → SPARES observed ESBL rate (per 1,000 bed-days)
#
# HOW THIS RELATES TO THE REFERENCE CODE (01_Beta_Calibration_OPTIM_SPARES.R)
# ─────────────────────────────────────────────────────────────────────────────
# The reference sources two files we do not have:
#   source("scripts/0_import.R")      → data loading
#   source("scripts/B2_0_params.R")   → builds H, beds, p_exit, p_tr, P_tr, etc.
#
# This script replaces those two source() calls with our own data loading
# using facility_level_final.RDS and weekly.RDS.
#
# What is IDENTICAL to the reference:
#   ✓ The simulation function (run_simulation_summary) — line for line
#   ✓ The objective function and Nelder-Mead loop structure
#   ✓ Checkpoint saving (safe_saveRDS, save_objective_state)
#   ✓ gamma, alpha, Tmax, last_year_start, last_year_len values
#
# What CANNOT be verified (reference reads from B2_0_params.R which we lack):
#   ~ p_exit : we use 1/los_mean  (standard SIS assumption: discharge prob = 1/LOS)
#   ~ p_tr   : we use transfers_out / (p_exit * beds)  (transfers as fraction of discharges)
#   ~ P_tr   : we build the H×H transfer matrix from weekly.RDS (same logic expected)
#   ~ pi_vec : we set 0.05 (5% of community admissions carry ESBL)
#
# What is DIFFERENT:
#   ✗ prev_init_etab: reference starts from a pre-computed 100-year equilibrium.
#     We start at INIT_PREV = 2% infected in every hospital.
#     Impact: the first year differs in trajectory, but the second year (which
#     we use for incidence measurement) converges to the same steady state.
#     The pmax(..., 1/beds) floor ensures every hospital has ≥1 infected patient,
#     matching the reference's pmax(prev_eq, 1/beds) step.
# =============================================================================

library(parallel)
library(dplyr)

#Local Change: Setting the Work Directory
setwd("C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/optim_cluster_jobs")
getwd()
###############################################################################
###### PATHS AND JOB INDEX ######
###############################################################################

PROJECT_ROOT <- getwd()
JOB_DIR      <- file.path(PROJECT_ROOT)
DATA_DIR     <- file.path(JOB_DIR, "data")

JOB_INDEX <- { v <- suppressWarnings(as.integer(Sys.getenv("jobindex")))
if (!is.na(v) && v > 0L) v else 1L }

message("=== FRANCE-WIDE CALIBRATION  |  job ", JOB_INDEX, " of 10 ===")

###############################################################################
###### DATA LOADING ######
###############################################################################
# Wekly Transfer Data has just the transfer structure and all the facilities "involved" in this network
weekly_transfers <- readRDS(file.path(DATA_DIR, "weekly.RDS")) %>%
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))
#Facility Level data includes datasets that include all facilities included in many FRench datasets

facility_level <- readRDS(file.path(DATA_DIR, "facility_level_final.RDS")) %>%
  mutate(finess_geo = as.character(finess_geo)) %>%
  rename(incidence_esbl_all = incidence_region_type_ESBL_all)
#We used incidence from SPARES, length of Stay from PMSI, and the Finess geo number
# Facility Incidence used the SPARES data by facility type and region. 

###############################################################################
###### INCIDENCE TARGETS ######
###############################################################################
#The incidence targets is the dataset we're going to compare our simulation incidence to. this is the SPARES data

#Global Incidence Average in case the facility that we simulate is not assigned a value\
# ALL INCIDENCES ARE BY 1000 PATIENT DAYS!!!

global_inc <- mean(facility_level$incidence_esbl_all, na.rm = TRUE)
print(global_inc)

#Incidence by Type and region
# Note that cancer centers were not included in SPARES dataset, so it is going to be NaN
type_region_inc <- facility_level %>%
  group_by(type_spares, region) %>%
  summarise(type_region_mean = mean(incidence_esbl_all, na.rm = TRUE), .groups="drop")

#Mean by Region for the Facilities, which would apply for the Cancer Centers and if reg-type incidence isnt available for that
# specific region

region_inc <- facility_level %>%
  group_by(region) %>%
  summarise(region_mean = mean(incidence_esbl_all, na.rm = TRUE), .groups="drop") %>% 
  filter(!is.na(region))
region_inc

# Region Mean isn't available for Corse

#Summarizing all facility targets by prioritizing based on the available incidence data
facility_targets <- facility_level %>%
  left_join(type_region_inc, by = c("type_spares","region")) %>%
  left_join(region_inc,      by = "region") %>%
  mutate(
    target_incidence = case_when(
      !is.na(incidence_esbl_all) ~ incidence_esbl_all,
      !is.na(type_region_mean)   ~ type_region_mean,
      !is.na(region_mean)        ~ region_mean,
      TRUE                       ~ global_inc
    ),
    incidence_source = case_when(
      !is.na(incidence_esbl_all) ~ "facility",
      !is.na(type_region_mean)   ~ "type_region_mean",
      !is.na(region_mean)        ~ "region_mean",
      TRUE                       ~ "global_mean"
    )
  ) %>%
  select(finess_geo, type_spares, region, target_incidence, incidence_source)

###############################################################################
###### HOSPITAL UNIVERSE AND SIMULATION PARAMETERS ######
###############################################################################

#Generating a list of Hospitals 
hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_target))
) %>% distinct()


default_los = filter(facility_level, !is.na(hospital_type)) %>% 
  group_by(hospital_type) %>% 
  summarize(pt_days_total = sum(pt_days_total, na.rm = TRUE),
            patient_total = sum(patient_total, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(los_mean_type = pt_days_total/patient_total)

DEFAULT_LOS_TYPE <- setNames(default_los$los_mean_type, default_los$hospital_type)
DEFAULT_LOS_TYPE

global_default_los = filter(facility_level, !is.na(hospital_type)) %>% 
  summarize(pt_days_total = sum(pt_days_total, na.rm = TRUE),
            patient_total = sum(patient_total, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(los_mean= pt_days_total/patient_total
         )
GLOBAL_DEFAULT_LOS = global_default_los$los_mean
GLOBAL_DEFAULT_LOS
# #For length of Stay, let's just make the default based on the mean of length of stay for that hospital types
# DEFAULT_LOS <- c("MCO"=5.5, "SSR"=32.0, "MCO/SSR"=7.0, "PSY"=60.0,
#                  "HAD"=22.0, "CLCC"=6.0, "Other"=8.0, "Unknown"=7.0)  #Fix this to use type_spares

hospitals <- hospitals %>%
  left_join(
    facility_level %>% transmute(
      finess_geo, hospital_type, type_spares, region,
      no_beds = as.integer(round(census_max)), #Changed from initial Elise's code : using census
      los = pmax(as.numeric(los_mean), 1.0)
    ), by = "finess_geo"
  ) %>%
  left_join(facility_targets, by = c("finess_geo", "type_spares", "region")) %>%
  mutate(
    no_beds          = as.integer(if_else(is.na(no_beds),
                                          as.integer(round(mean(no_beds, na.rm=TRUE))),
                                          no_beds)),
    los              = coalesce(los, DEFAULT_LOS_TYPE[hospital_type], GLOBAL_DEFAULT_LOS), 
    #Using LOS if avaiable, or default for the type_spares, or 7
    # Elise used similar calculation, but she included the nursing homes. 
    # 
    target_incidence = if_else(is.na(target_incidence), global_inc, target_incidence),
    type_spares    = if_else(is.na(type_spares), "Unknown", type_spares),
    incidence_source = if_else(is.na(incidence_source), "global_mean", incidence_source),
    region           = if_else(is.na(region), "Unknown", region)
  )
 
# LOCAL TEST: keep only 200 hospitals (remove this block before running on cluster)
set.seed(1)
#keep <- hospitals %>% group_by(type_spares) %>% slice(1) %>% ungroup()
#extra <- hospitals %>% anti_join(keep, by="finess_geo") %>% sample_n(min(190, nrow(.)))
#hospitals <- bind_rows(keep, extra)
weekly_transfers <- weekly_transfers %>%
  filter(as.character(finess_geo_origin) %in% hospitals$finess_geo,
         as.character(finess_geo_target) %in% hospitals$finess_geo)

hosp_idx <- setNames(seq_len(nrow(hospitals)), hospitals$finess_geo)
H      <- nrow(hospitals)
beds   <- hospitals$no_beds
p_exit <- 1 / hospitals$los  #Discharge probability based on the LOS we have set before

transfer_out_df <- weekly_transfers %>%
  transmute(origin = as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out = sum(weight, na.rm=TRUE), .groups="drop")
hospitals <- hospitals %>%
  left_join(transfer_out_df, by = c("finess_geo"="origin")) %>%
  mutate(total_out = replace(total_out, is.na(total_out), 0))
p_tr <- pmin(hospitals$total_out / pmax(p_exit * beds, 1), 0.60)

message("Building P_tr (", H, " x ", H, ") ...")
transfer_agg <- weekly_transfers %>%
  transmute(orig = hosp_idx[as.character(finess_geo_origin)],
            dest = hosp_idx[as.character(finess_geo_target)],
            weight) %>%
  filter(!is.na(orig) & !is.na(dest)) %>%
  group_by(orig, dest) %>%
  summarise(weight = sum(weight), .groups="drop")

P_tr <- matrix(0.0, H, H)
for (k in seq_len(nrow(transfer_agg)))
  P_tr[transfer_agg$dest[k], transfer_agg$orig[k]] <- transfer_agg$weight[k]
cs <- colSums(P_tr)
for (h in seq_len(H)) if (cs[h] > 0) P_tr[,h] <- P_tr[,h] / cs[h]
message("  Done. Hospitals with outgoing transfers: ", sum(cs > 0))

pi_vec          <- rep(0.05, H)
type_etab_calib <- hospitals$type_spares

spares_types <- facility_targets %>%
  filter(incidence_source != "global_mean") %>% distinct(type_spares)
target_type <- hospitals %>%
  group_by(type_spares) %>%
  summarise(target_incidence = mean(target_incidence), .groups="drop")
incidence_obs <- target_type %>%
  semi_join(spares_types, by="type_spares") %>%
  with(setNames(target_incidence, type_spares))

message("Calibration types (", length(incidence_obs), "): ",
        paste(names(incidence_obs), collapse=", "))
message("Observed incidence (per 1,000 bed-days):")
print(round(incidence_obs, 3))

###############################################################################
###### PARAMÈTRES ######
###############################################################################

n_cores         <- 2L       # Windows local: keep at 2
n_rep_obj       <- 20       # enough for a reliable local test (cluster uses 100)
n_rep_valid     <- 50       # enough for a reliable local validation
n_random_starts <- 3        # fewer random starts — analytical start compensates
maxit_nm        <- 50       # enough for convergence with a good starting point

seed_objective     <- 1000  + (JOB_INDEX - 1L) * 10000L
seed_validation    <- 50000 + (JOB_INDEX - 1L) * 10000L
seed_random_starts <- 123   + (JOB_INDEX - 1L) * 10000L

# ── Narrower beta range ───────────────────────────────────────────────────────
# Biologically motivated: ESBL daily within-hospital transmission rates in
# French hospitals are unlikely below 1e-4 or above 0.05.
# A narrower range means Nelder-Mead explores less irrelevant space → faster.
lower_beta <- 1e-4
upper_beta <- 0.05

gamma           <- 1 / 387
alpha           <- 0
Tmax            <- as.integer(2 * 365L)
last_year_start <- Tmax - 364L
last_year_len   <- 365L

###############################################################################
###### INITIALISATION ######
###############################################################################

INIT_PREV      <- 0.02
prev_init_etab <- pmax(rep(INIT_PREV, H), 1 / beds)

###############################################################################
###### OUTPUT PATHS ######
###############################################################################

calibration_out_dir <- file.path(JOB_DIR, "Outputs", "france",
                                 sprintf("job_%02d", JOB_INDEX))
dir.create(calibration_out_dir, recursive = TRUE, showWarnings = FALSE)

final_recovered_beta_file <- file.path(JOB_DIR, "Outputs", "france",
                                       "recovered_best_beta.rds")

###############################################################################
###### SMART STARTING POINT — derived from observed incidence ######
# No warm start file needed.  Instead we compute an analytical estimate
# for each facility type directly from the observed incidence.
#
# Reasoning (SIS at steady state, low-prevalence approximation):
#   incidence_rate ≈ beta × prevalence × (1 − prevalence)
#   prevalence     ≈ incidence_rate / gamma          (rate in ≈ rate out)
#   → beta_est     ≈ incidence_rate / prevalence
#                  ≈ gamma  +  incidence_rate
#
# where incidence_rate = incidence_obs / 1000 (convert from per-1,000 to per-day).
# This is a rough but much better starting point than a fixed mid-range value.
# The Nelder-Mead optimizer then refines it from there.
###############################################################################

incidence_daily <- incidence_obs / 1000   # convert to per-bed-day
beta_analytical <- incidence_daily + gamma # rough steady-state estimate
beta_analytical <- pmin(pmax(beta_analytical, lower_beta), upper_beta)

message("Analytical beta starting estimates:")
print(round(beta_analytical, 6))

###############################################################################
###### SIMULATION FUNCTION ######
###############################################################################

run_simulation_summary <- function(beta_vec, alpha, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  p_rec <- 1 - exp(-gamma)
  I_loc <- rbinom(H, beds, prev_init_etab)
  S_loc <- beds - I_loc
  inc_sum_last <- numeric(H)
  for (t in seq_len(Tmax)) {
    for (i in seq_len(H)) {
      N <- S_loc[i] + I_loc[i]
      if (N <= 0) next
      p_inf   <- 1 - exp(-beta_vec[i] * I_loc[i] / N)
      new_inf <- rbinom(1, S_loc[i], p_inf)
      recov   <- rbinom(1, I_loc[i], p_rec)
      if (t >= last_year_start) inc_sum_last[i] <- inc_sum_last[i] + new_inf
      S_loc[i] <- S_loc[i] - new_inf + recov
      I_loc[i] <- I_loc[i] + new_inf - recov
    }
    S_stay <- S_loc;  I_stay <- I_loc
    S_tr   <- numeric(H);  I_tr <- numeric(H)
    handle_exit <- function(h) {
      n_exit_S <- rbinom(1, S_loc[h], p_exit[h])
      n_exit_I <- rbinom(1, I_loc[h], p_exit[h])
      if ((n_exit_S + n_exit_I) == 0) return()
      S_stay[h] <<- S_stay[h] - n_exit_S
      I_stay[h] <<- I_stay[h] - n_exit_I
      p_tr_h <- p_tr[h]
      n_tr_S <- rbinom(1, n_exit_S, p_tr_h)
      n_tr_I <- rbinom(1, n_exit_I, pmin(pmax((1-alpha)*p_tr_h, 0), 1))
      if ((n_tr_S + n_tr_I) > 0) {
        probs_dest <- P_tr[, h]
        if (!all(is.finite(probs_dest))) return()
        s <- sum(probs_dest);  if (s <= 0) return()
        probs_dest <- probs_dest / s
        if (n_tr_S > 0) {
          dest_S <- rmultinom(1, n_tr_S, probs_dest)
          S_tr <<- S_tr + dest_S[,1]
        }
        if (n_tr_I > 0) {
          dest_I <- rmultinom(1, n_tr_I, probs_dest)
          I_tr <<- I_tr + dest_I[,1]
        }
      }
    }
    for (h in seq_len(H)) handle_exit(h)
    occ   <- S_stay + I_stay + S_tr + I_tr
    A     <- pmax(0, beds - occ)
    A_I   <- rbinom(H, A, pi_vec)
    A_S   <- A - A_I
    S_loc <- S_stay + S_tr + A_S
    I_loc <- I_stay + I_tr + A_I
  }
  inc_etab       <- 1000 * inc_sum_last / (beds * last_year_len)
  incidence_type <- tapply(inc_etab, type_etab_calib, mean, na.rm=TRUE)
  incidence_type[names(incidence_obs)]
}

###############################################################################
###### CLUSTER ######
# AUTO-DETECT: FORK on Linux/macOS (fast, shared memory — no export needed).
#              PSOCK on Windows   (separate processes — must export variables).
# This block handles both so the same script works locally and on the cluster.
###############################################################################

if (exists("cl") && inherits(cl,"cluster")) try(stopCluster(cl), silent=TRUE)

PARALLEL_TYPE <- if (.Platform$OS.type == "windows") "PSOCK" else "FORK"
cl <- makeCluster(n_cores, type=PARALLEL_TYPE)
message("Cluster started: ", n_cores, " ", PARALLEL_TYPE, " workers")

# PSOCK workers start with an empty R session — they have none of the objects
# we built above.  We must ship everything the simulation function needs.
# FORK workers inherit the parent's memory automatically — nothing to export.
if (PARALLEL_TYPE == "PSOCK") {
  clusterExport(cl, varlist = c(
    "run_simulation_summary",   # the function itself
    "H",                        # number of hospitals
    "beds",                     # bed capacity vector
    "p_exit",                   # daily discharge probability
    "p_tr",                     # transfer fraction
    "P_tr",                     # transfer probability matrix
    "prev_init_etab",           # starting prevalence
    "pi_vec",                   # community ESBL prevalence
    "type_etab_calib",          # facility type per hospital
    "incidence_obs",            # observed incidence targets
    "Tmax",                     # total simulation days
    "last_year_start",          # day to start counting incidence
    "last_year_len",            # number of days to count
    "gamma",                    # clearance rate
    "alpha"                     # isolation parameter
  ))
  message("Variables exported to PSOCK workers.")
}
rep_chunks       <- split(seq_len(n_rep_obj),
                          rep(seq_len(n_cores), length.out=n_rep_obj))
rep_chunks_valid <- split(seq_len(n_rep_valid),
                          rep(seq_len(n_cores), length.out=n_rep_valid))

###############################################################################
###### CHECKPOINT PATHS ######
###############################################################################

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
checkpoint_dir <- file.path(calibration_out_dir, paste0("run_nm_france_",run_id))
dir.create(checkpoint_dir, recursive=TRUE, showWarnings=FALSE)

checkpoint_best_file        <- file.path(checkpoint_dir,"checkpoint_best_beta.rds")
checkpoint_last_file        <- file.path(checkpoint_dir,"checkpoint_last_eval.rds")
history_file                <- file.path(checkpoint_dir,"history_objective.csv")
starts_file                 <- file.path(checkpoint_dir,"starts_used.rds")
fits_file                   <- file.path(checkpoint_dir,"fits_nm.rds")
final_file                  <- file.path(checkpoint_dir,"final_validation.rds")
validation_summary_run_file <- file.path(checkpoint_dir,"validation_summary.csv")

###############################################################################
###### CHECKPOINT UTILITIES ######
###############################################################################

eval_counter <- 0L
best_value   <- Inf

safe_saveRDS <- function(object, file) {
  tmp <- paste0(file,".tmp"); saveRDS(object,tmp)
  if (file.exists(file)) file.remove(file)
  file.rename(tmp, file)
}

save_objective_state <- function(beta_type_log, beta_type, incidence_sim,
                                 objective_value, sse_by_type,
                                 is_best, start_id=NA_integer_) {
  state <- list(datetime=Sys.time(), eval_counter=eval_counter,
                start_id=start_id, objective_value=objective_value,
                sse_by_type=sse_by_type, is_best=is_best,
                n_rep_obj=n_rep_obj, n_cores=n_cores,
                seed_objective=seed_objective, beta_type_log=beta_type_log,
                beta_type=beta_type, incidence_sim=incidence_sim,
                incidence_obs=incidence_obs, lower_beta=lower_beta,
                upper_beta=upper_beta, checkpoint_dir=checkpoint_dir)
  safe_saveRDS(state, checkpoint_last_file)
  if (is_best) safe_saveRDS(state, checkpoint_best_file)
  beta_cols <- as.data.frame(as.list(beta_type), check.names=FALSE)
  names(beta_cols) <- paste0("beta_",    names(beta_type))
  inc_cols  <- as.data.frame(as.list(incidence_sim), check.names=FALSE)
  names(inc_cols)  <- paste0("inc_sim_", names(incidence_sim))
  sse_cols  <- as.data.frame(as.list(sse_by_type), check.names=FALSE)
  names(sse_cols)  <- paste0("sse_",     names(sse_by_type))
  hist_row  <- cbind(
    data.frame(eval_counter=eval_counter,
               datetime=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
               scope="France", job_index=JOB_INDEX, start_id=start_id,
               objective_value=objective_value, best_value=best_value,
               is_best=is_best, n_rep_obj=n_rep_obj, stringsAsFactors=FALSE),
    beta_cols, inc_cols, sse_cols)
  write.table(hist_row, file=history_file, sep=";", dec=".", row.names=FALSE,
              col.names=!file.exists(history_file), append=file.exists(history_file))
}

###############################################################################
###### OBJECTIVE FUNCTION ######
###############################################################################

current_start_id <- NA_integer_

objective_fn <- function(beta_type_log) {
  eval_counter <<- eval_counter + 1L
  beta_type        <- exp(beta_type_log)
  names(beta_type) <- names(incidence_obs)
  beta_vec <- beta_type[as.character(type_etab_calib)]
  if (anyNA(beta_vec)) beta_vec[is.na(beta_vec)] <- mean(beta_type, na.rm=TRUE)
  results <- parLapply(cl, X=rep_chunks,
                       fun=function(rs, beta_vec, seed_objective) {
                         do.call(rbind, lapply(rs, function(r)
                           run_simulation_summary(beta_vec=beta_vec, alpha=alpha,
                                                  seed=seed_objective+r)))
                       }, beta_vec=beta_vec, seed_objective=seed_objective)
  inc_mat       <- do.call(rbind, results)
  incidence_sim <- colMeans(inc_mat, na.rm=TRUE)[names(incidence_obs)]
  sse_by_type     <- (incidence_sim - incidence_obs)^2
  objective_value <- sum(sse_by_type, na.rm=TRUE)
  is_best <- objective_value < best_value
  if (is_best) best_value <<- objective_value
  save_objective_state(beta_type_log, beta_type, incidence_sim,
                       objective_value, sse_by_type, is_best,
                       start_id=current_start_id)
  print(data.frame(eval=eval_counter, start=current_start_id,
                   SSE=round(objective_value,6), best=round(best_value,6),
                   is_best=is_best))
  print("Simulated:"); print(round(incidence_sim, 3))
  print("Observed: "); print(round(incidence_obs, 3))
  print("SSE/type: "); print(round(sse_by_type,   3))
  objective_value
}

###############################################################################
###### BOUNDED OBJECTIVE ######
###############################################################################

lower_log <- log(lower_beta);  upper_log <- log(upper_beta)

objective_bounded <- function(beta_type_log) {
  if (any(!is.finite(beta_type_log))) return(1e12)
  penalty <- sum(pmax(beta_type_log-upper_log,0)^2 +
                   pmax(lower_log-beta_type_log,0)^2)
  if (penalty > 0) return(1e9 + 1e9*penalty)
  objective_fn(beta_type_log)
}

###############################################################################
###### STARTING POINTS ######
###############################################################################

starts <- list()
starts[[1]] <- log(beta_analytical)
starts[[2]] <- log(pmin(beta_analytical*1.5, upper_beta))
starts[[3]] <- log(pmax(beta_analytical*0.67, lower_beta))
starts[[4]] <- log(pmin(beta_analytical*2.0, upper_beta))

set.seed(seed_random_starts)
for (s in seq_len(n_random_starts)) {
  br <- pmin(pmax(beta_analytical * exp(runif(length(beta_analytical),log(0.25),log(4))),
                  lower_beta), upper_beta)
  starts[[length(starts)+1]] <- log(br[names(incidence_obs)])
}
starts <- lapply(starts, function(x)
  pmin(pmax(x[names(incidence_obs)], lower_log), upper_log))
names(starts) <- paste0("start_", seq_along(starts))

safe_saveRDS(list(datetime=Sys.time(), scope="France", job_index=JOB_INDEX,
                  starts_log=starts, starts_beta=lapply(starts,exp),
                  beta_analytical=beta_analytical, incidence_obs=incidence_obs), starts_file)

print("STARTING POINTS"); print(lapply(starts, exp))

###############################################################################
###### NELDER-MEAD + RECOVERY + VALIDATION ######
###############################################################################

tryCatch({
  
  fits <- vector("list", length(starts));  names(fits) <- names(starts)
  
  for (i in seq_along(starts)) {
    current_start_id <<- i
    print(paste("Start", names(starts)[i], "- France-wide"))
    print(round(exp(starts[[i]]),6))
    fit_i <- tryCatch(
      optim(par=starts[[i]], fn=objective_bounded, method="Nelder-Mead",
            control=list(maxit=maxit_nm, trace=1, REPORT=1, reltol=1e-4,
                         parscale=rep(1,length(starts[[i]])))),
      error=function(e) list(par=starts[[i]], value=Inf,
                             convergence=NA_integer_, message=conditionMessage(e))
    )
    fits[[i]] <- fit_i
    safe_saveRDS(list(datetime=Sys.time(), fits=fits,
                      eval_counter=eval_counter, best_value=best_value), fits_file)
    print(paste("SSE =", round(fit_i$value,6),
                "  convergence =", fit_i$convergence))
  }
  current_start_id <<- NA_integer_
  
  fit_values  <- sapply(fits, function(x) x$value)
  best_fit_id <- which.min(fit_values)
  print("SSE by start:"); print(round(fit_values,6))
  
  if (file.exists(checkpoint_best_file)) {
    ckpt          <- readRDS(checkpoint_best_file)
    beta_type_opt <- ckpt$beta_type;  beta_type_log_opt <- ckpt$beta_type_log
    if (ckpt$objective_value > fit_values[best_fit_id]) {
      beta_type_opt     <- exp(fits[[best_fit_id]]$par)
      names(beta_type_opt) <- names(incidence_obs)
      beta_type_log_opt <- log(beta_type_opt)
    }
  } else {
    beta_type_opt <- exp(fits[[best_fit_id]]$par)
    names(beta_type_opt) <- names(incidence_obs)
    beta_type_log_opt <- log(beta_type_opt)
  }
  
  beta_type_opt <- pmin(pmax(beta_type_opt[names(incidence_obs)],lower_beta),upper_beta)
  beta_opt      <- beta_type_opt[as.character(type_etab_calib)]
  if (anyNA(beta_opt)) beta_opt[is.na(beta_opt)] <- mean(beta_type_opt, na.rm=TRUE)
  
  print("OPTIMAL BETA PER TYPE:"); print(round(beta_type_opt,6))
  
  res_final <- parLapply(cl, X=rep_chunks_valid,
                         fun=function(rs, beta_opt, seed_validation) {
                           do.call(rbind, lapply(rs, function(r)
                             run_simulation_summary(beta_vec=beta_opt, alpha=alpha,
                                                    seed=seed_validation+r)))
                         }, beta_opt=beta_opt, seed_validation=seed_validation)
  
  inc_final_mat     <- do.call(rbind, res_final)
  inc_final         <- colMeans(inc_final_mat, na.rm=TRUE)[names(incidence_obs)]
  inc_final_sd      <- apply(inc_final_mat,2,sd,na.rm=TRUE)[names(incidence_obs)]
  inc_final_se      <- inc_final_sd / sqrt(n_rep_valid)
  diff_final        <- (inc_final - incidence_obs)[names(incidence_obs)]
  sse_final_by_type <- diff_final^2
  sse_final         <- sum(sse_final_by_type, na.rm=TRUE)
  
  validation_summary <- data.frame(
    scope="France", region="All", job_index=JOB_INDEX,
    type=names(incidence_obs),
    beta=as.numeric(beta_type_opt[names(incidence_obs)]),
    incidence_obs=as.numeric(incidence_obs),
    incidence_sim_mean=as.numeric(inc_final),
    incidence_sim_sd=as.numeric(inc_final_sd),
    incidence_sim_se=as.numeric(inc_final_se),
    diff=as.numeric(diff_final),
    sse=as.numeric(sse_final_by_type),
    stringsAsFactors=FALSE)
  
  final_object <- list(
    datetime=Sys.time(), scope="France", job_index=JOB_INDEX,
    beta_type_opt=beta_type_opt, beta_type_log_opt=beta_type_log_opt,
    beta_opt=beta_opt, incidence_obs=incidence_obs,
    incidence_final=inc_final, incidence_final_sd=inc_final_sd,
    incidence_final_se=inc_final_se, diff_final=diff_final,
    sse_final_by_type=sse_final_by_type, sse_final=sse_final,
    n_rep_obj=n_rep_obj, n_rep_valid=n_rep_valid, n_cores=n_cores,
    fits=fits, fit_values=fit_values, best_fit_id=best_fit_id,
    validation_summary=validation_summary, checkpoint_dir=checkpoint_dir)
  
  safe_saveRDS(final_object, final_file)
  safe_saveRDS(final_object, final_recovered_beta_file)
  write.csv2(validation_summary, file=validation_summary_run_file, row.names=FALSE)
  
  print("BETA OPTIMAL:"); print(round(beta_type_opt,6))
  print("INCIDENCE SIMULEE:"); print(round(inc_final,3))
  print("INCIDENCE OBSERVEE:"); print(round(incidence_obs,3))
  print(paste("SSE TOTALE =", round(sse_final,6)))
  print("TABLEAU VALIDATION:"); print(validation_summary)
  print("DOSSIER DE SORTIE:"); print(checkpoint_dir)
  
}, finally = {
  message("Stopping cluster ...")
  try(stopCluster(cl), silent=TRUE)
})