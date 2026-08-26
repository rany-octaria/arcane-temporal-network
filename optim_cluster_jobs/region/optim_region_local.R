# =============================================================================
# optim_region_local.R  —  Beta calibration, single region  (LOCAL VERSION)
# =============================================================================
# Based on the locally-validated optim_france.R (document index 6).
# ALL data management changes kept exactly as-is.
#
# What is different from the France version:
#   1. FOCAL_REGION set at the top — change this to run a different region
#   2. After building the full network, hospitals and P_tr are subsetted to
#      the focal region.  Inter-regional transfers are removed; P_tr columns
#      are re-normalised so within-region transfer probabilities still sum to 1.
#   3. incidence_obs uses only the types present in the focal region.
#   4. Output folder includes the region name.
#   5. beta_start → beta_analytical in starting points (fixing the inconsistency
#      from the France version where beta_start was removed but still referenced).
# =============================================================================

library(parallel)
library(dplyr)

# =============================================x

# ▶▶▶  SET YOUR FOCAL REGION HERE  ◀◀◀
# Change this string to run a different region.
# Must match exactly how the region appears in your data — check with:
#   unique(facility_level$region)
# after loading facility_level below.
# =============================================================================

print(sort(unique(facility_level$region[!is.na(facility_level$region)])))
# [1] "Auvergne-Rhône-Alpes"       "Bourgogne-Franche-Comté"    "Bretagne"                   "Centre-Val de Loire"        "Corse"                     
# [6] "Grand-Est"                  "Hauts-de-France"            "Île-de-France"              "Normandie"                  "Nouvelle-Aquitaine"        
# [11] "Occitanie"                  "Pays de la Loire"           "Provence-Alpes-Côte d'Azur"
FOCAL_REGION <- "Centre-Val de Loire"

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

message("=== REGION CALIBRATION  |  region = ", FOCAL_REGION,
        "  |  job ", JOB_INDEX, " ===")

###############################################################################
###### DATA LOADING ######
###############################################################################

weekly_transfers <- readRDS(file.path(DATA_DIR, "weekly.RDS")) %>%
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

facility_level <- readRDS(file.path(DATA_DIR, "facility_level_final.RDS")) %>%
  mutate(finess_geo = as.character(finess_geo)) %>%
  rename(incidence_esbl_all = incidence_region_type_ESBL_all)

# Quick check — print available regions so you can verify the spelling above
message("Regions in data:")
print(sort(unique(facility_level$region[!is.na(facility_level$region)])))

###############################################################################
###### INCIDENCE TARGETS ######
###############################################################################

global_inc <- mean(facility_level$incidence_esbl_all, na.rm = TRUE)
print(global_inc)

type_region_inc <- facility_level %>%
  group_by(type_spares, region) %>%
  summarise(type_region_mean = mean(incidence_esbl_all, na.rm = TRUE), .groups = "drop")

region_inc <- facility_level %>%
  group_by(region) %>%
  summarise(region_mean = mean(incidence_esbl_all, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(region))

facility_targets <- facility_level %>%
  left_join(type_region_inc, by = c("type_spares", "region")) %>%
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
###### BUILD FULL HOSPITAL UNIVERSE FIRST ######
# We build the full national network before subsetting so the P_tr matrix
# indices are consistent when we slice the regional sub-matrix.
###############################################################################

hospitals_full <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_target))
) %>% distinct()

# LOS defaults computed from data (your change — kept exactly as-is)
default_los <- filter(facility_level, !is.na(hospital_type)) %>%
  group_by(hospital_type) %>%
  summarize(pt_days_total = sum(pt_days_total, na.rm = TRUE),
            patient_total = sum(patient_total, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(los_mean_type = pt_days_total / patient_total)

DEFAULT_LOS_TYPE <- setNames(default_los$los_mean_type, default_los$hospital_type)

global_default_los <- filter(facility_level, !is.na(hospital_type)) %>%
  summarize(pt_days_total = sum(pt_days_total, na.rm = TRUE),
            patient_total = sum(patient_total, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(los_mean = pt_days_total / patient_total)

GLOBAL_DEFAULT_LOS <- global_default_los$los_mean

# # Previous hardcoded defaults (kept as reference):
# DEFAULT_LOS <- c("MCO"=5.5, "SSR"=32.0, "MCO/SSR"=7.0, "PSY"=60.0,
#                  "HAD"=22.0, "CLCC"=6.0, "Other"=8.0, "Unknown"=7.0)

hospitals_full <- hospitals_full %>%
  left_join(
    facility_level %>% transmute(
      finess_geo, hospital_type, type_spares, region,
      no_beds = as.integer(round(census_max)),
      los     = pmax(as.numeric(los_mean), 1.0)
    ), by = "finess_geo"
  ) %>%
  left_join(facility_targets, by = c("finess_geo", "type_spares", "region")) %>%
  mutate(
    no_beds          = as.integer(if_else(is.na(no_beds),
                                          as.integer(round(mean(no_beds, na.rm = TRUE))),
                                          no_beds)),
    los              = coalesce(los, DEFAULT_LOS_TYPE[hospital_type], GLOBAL_DEFAULT_LOS),
    target_incidence = if_else(is.na(target_incidence), global_inc, target_incidence),
    type_spares      = if_else(is.na(type_spares), "Unknown", type_spares),
    incidence_source = if_else(is.na(incidence_source), "global_mean", incidence_source),
    region           = if_else(is.na(region), "Unknown", region)
  )

# Build full-network index and transfer outputs
hosp_idx_full  <- setNames(seq_len(nrow(hospitals_full)), hospitals_full$finess_geo)
p_exit_full    <- 1 / hospitals_full$los

transfer_out_df <- weekly_transfers %>%
  transmute(origin = as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out = sum(weight, na.rm = TRUE), .groups = "drop")
hospitals_full <- hospitals_full %>%
  left_join(transfer_out_df, by = c("finess_geo" = "origin")) %>%
  mutate(total_out = replace(total_out, is.na(total_out), 0))
p_tr_full <- pmin(hospitals_full$total_out /
                    pmax(p_exit_full * hospitals_full$no_beds, 1), 0.60)

# Build full P_tr matrix
message("Building full P_tr (", nrow(hospitals_full), " x ", nrow(hospitals_full), ") ...")
transfer_agg <- weekly_transfers %>%
  transmute(orig = hosp_idx_full[as.character(finess_geo_origin)],
            dest = hosp_idx_full[as.character(finess_geo_target)],
            weight) %>%
  filter(!is.na(orig) & !is.na(dest)) %>%
  group_by(orig, dest) %>%
  summarise(weight = sum(weight), .groups = "drop")

P_tr_full <- matrix(0.0, nrow(hospitals_full), nrow(hospitals_full))
for (k in seq_len(nrow(transfer_agg)))
  P_tr_full[transfer_agg$dest[k], transfer_agg$orig[k]] <- transfer_agg$weight[k]
cs_full <- colSums(P_tr_full)
for (h in seq_len(nrow(hospitals_full)))
  if (cs_full[h] > 0) P_tr_full[, h] <- P_tr_full[, h] / cs_full[h]
message("  Full P_tr done.")

###############################################################################
###### SUBSET TO FOCAL REGION ######
# All simulation objects (H, beds, p_exit, p_tr, P_tr, type_etab_calib) are
# replaced with region-specific versions.
# Inter-regional transfers are dropped; P_tr columns are re-normalised so
# within-region transfer probabilities still sum to 1.
###############################################################################

keep_idx <- which(hospitals_full$region == FOCAL_REGION)

if (length(keep_idx) == 0)
  stop("Region '", FOCAL_REGION, "' not found in data.\n",
       "Available regions: ",
       paste(sort(unique(hospitals_full$region[hospitals_full$region != "Unknown"])),
             collapse = ", "))

message("Hospitals in ", FOCAL_REGION, ": ",
        length(keep_idx), " of ", nrow(hospitals_full), " total")

hospitals       <- hospitals_full[keep_idx, ]
H               <- length(keep_idx)
beds            <- hospitals_full$no_beds[keep_idx]
p_exit          <- p_exit_full[keep_idx]
p_tr            <- p_tr_full[keep_idx]
type_etab_calib <- hospitals_full$type_spares[keep_idx]

# Slice P_tr to within-region sub-matrix and re-normalise columns
P_tr  <- P_tr_full[keep_idx, keep_idx]
cs_r  <- colSums(P_tr)
for (h in seq_len(H)) if (cs_r[h] > 0) P_tr[, h] <- P_tr[, h] / cs_r[h]
message("  Regional P_tr built. Hospitals with within-region transfers: ", sum(cs_r > 0))

pi_vec <- rep(0.05, H)

# Region-specific incidence targets — only types with SPARES data in this region
spares_types <- facility_targets %>%
  filter(finess_geo %in% hospitals$finess_geo,
         incidence_source != "global_mean") %>%
  distinct(type_spares)

target_type <- hospitals %>%
  group_by(type_spares) %>%
  summarise(target_incidence = mean(target_incidence), .groups = "drop")

incidence_obs <- target_type %>%
  semi_join(spares_types, by = "type_spares") %>%
  with(setNames(target_incidence, type_spares))

if (length(incidence_obs) == 0)
  stop("No SPARES-derived incidence targets for region: ", FOCAL_REGION)

message("Calibration types in ", FOCAL_REGION,
        " (", length(incidence_obs), "): ",
        paste(names(incidence_obs), collapse = ", "))
message("Observed incidence (per 1,000 bed-days):")
print(round(incidence_obs, 3))

###############################################################################
###### PARAMÈTRES ######
###############################################################################

n_cores         <- parallel::detectCores() - 1L 
n_rep_obj       <- 20
n_rep_valid     <- 50
n_random_starts <- 3
maxit_nm        <- 50

seed_objective     <- 1000  + (JOB_INDEX - 1L) * 10000L
seed_validation    <- 50000 + (JOB_INDEX - 1L) * 10000L
seed_random_starts <- 123   + (JOB_INDEX - 1L) * 10000L

# Narrower beta range — biologically motivated, speeds up Nelder-Mead
# Previous range (wider): lower_beta <- 1e-5 / upper_beta <- 0.10
lower_beta <- 1e-4
upper_beta <- 0.05

gamma           <- 1 / 387
alpha           <- 0
Tmax            <- as.integer(2 * 365L)
last_year_start <- Tmax - 364L
last_year_len   <- 365L

# Faster local alternative: 1-year run, last 6 months measured
# (valid if starting close to steady state via warm start)
# Tmax            <- as.integer(365L)
# last_year_start <- Tmax - 181L
# last_year_len   <- 182L

###############################################################################
###### INITIALISATION ######
###############################################################################

INIT_PREV      <- 0.02
prev_init_etab <- pmax(rep(INIT_PREV, H), 1 / beds)

###############################################################################
###### OUTPUT PATHS ######
###############################################################################

# Safe folder name — replace special characters (accents, spaces, slashes)
region_safe <- gsub("[^A-Za-z0-9_-]", "_", FOCAL_REGION)

calibration_out_dir <- file.path(JOB_DIR, "Outputs", "region", region_safe,
                                 sprintf("job_%02d", JOB_INDEX))
dir.create(calibration_out_dir, recursive = TRUE, showWarnings = FALSE)

final_recovered_beta_file <- file.path(JOB_DIR, "Outputs", "region", region_safe,
                                       "recovered_best_beta.rds")

###############################################################################
###### SMART STARTING POINT — derived from observed incidence ######
# No warm start file.  Analytical estimate from SIS steady-state approximation:
#   incidence_rate ≈ beta × prevalence;  prevalence ≈ incidence_rate / gamma
#   → beta_est ≈ gamma + incidence_rate
# where incidence_rate = incidence_obs / 1000 (per-bed-day).
#
# Previous alternative (warm start from file):
#   warm_start_file <- file.path(JOB_DIR, "warm_start_france.rds")
#   if (file.exists(warm_start_file)) { ws <- readRDS(...); beta_start <- ws$beta_type_opt[...] }
###############################################################################

incidence_daily <- incidence_obs / 1000
beta_analytical <- pmin(pmax(incidence_daily + gamma, lower_beta), upper_beta)

message("Analytical beta starting estimates:")
print(round(beta_analytical, 6))

###############################################################################
###### SIMULATION FUNCTION ######
# Identical to France version — no changes.
###############################################################################

run_simulation_summary <- function(beta_vec, alpha, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  p_rec <- 1 - exp(-gamma)
  I_loc <- rbinom(H, beds, prev_init_etab)
  S_loc <- beds - I_loc
  inc_sum_last <- numeric(H)
  for (t in seq_len(Tmax)) {
    # for (i in seq_len(H)) {
    #   N <- S_loc[i] + I_loc[i]
    #   if (N <= 0) next
    #   p_inf   <- 1 - exp(-beta_vec[i] * I_loc[i] / N)
    #   new_inf <- rbinom(1, S_loc[i], p_inf)
    #   recov   <- rbinom(1, I_loc[i], p_rec)
    #   if (t >= last_year_start) inc_sum_last[i] <- inc_sum_last[i] + new_inf
    #   S_loc[i] <- S_loc[i] - new_inf + recov
    #   I_loc[i] <- I_loc[i] + new_inf - recov
    # }
    # FAST (vectorised over all H hospitals at once)
    N       <- S_loc + I_loc
    p_inf   <- ifelse(N > 0, 1 - exp(-beta_vec * I_loc / pmax(N, 1L)), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    if (t >= last_year_start) inc_sum_last <- inc_sum_last + new_inf
    S_loc   <- S_loc - new_inf + recov
    I_loc   <- I_loc + new_inf - recov
    
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
      n_tr_I <- rbinom(1, n_exit_I, pmin(pmax((1 - alpha) * p_tr_h, 0), 1))
      if ((n_tr_S + n_tr_I) > 0) {
        probs_dest <- P_tr[, h]
        if (!all(is.finite(probs_dest))) return()
        s <- sum(probs_dest);  if (s <= 0) return()
        probs_dest <- probs_dest / s
        if (n_tr_S > 0) { dest_S <- rmultinom(1, n_tr_S, probs_dest); S_tr <<- S_tr + dest_S[, 1] }
        if (n_tr_I > 0) { dest_I <- rmultinom(1, n_tr_I, probs_dest); I_tr <<- I_tr + dest_I[, 1] }
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
  incidence_type <- tapply(inc_etab, type_etab_calib, mean, na.rm = TRUE)
  incidence_type[names(incidence_obs)]
}

###############################################################################
###### CLUSTER (auto-detect PSOCK/FORK) ######
###############################################################################

if (exists("cl") && inherits(cl, "cluster")) try(stopCluster(cl), silent = TRUE)

PARALLEL_TYPE <- if (.Platform$OS.type == "windows") "PSOCK" else "FORK"
cl <- makeCluster(n_cores, type = PARALLEL_TYPE)
message("Cluster started: ", n_cores, " ", PARALLEL_TYPE, " workers")

if (PARALLEL_TYPE == "PSOCK") {
  clusterExport(cl, varlist = c(
    "run_simulation_summary", "H", "beds", "p_exit", "p_tr", "P_tr",
    "prev_init_etab", "pi_vec", "type_etab_calib", "incidence_obs",
    "Tmax", "last_year_start", "last_year_len", "gamma", "alpha"
  ))
  message("Variables exported to PSOCK workers.")
}

rep_chunks       <- split(seq_len(n_rep_obj),
                          rep(seq_len(n_cores), length.out = n_rep_obj))
rep_chunks_valid <- split(seq_len(n_rep_valid),
                          rep(seq_len(n_cores), length.out = n_rep_valid))

###############################################################################
###### CHECKPOINT PATHS ######
###############################################################################

run_id         <- format(Sys.time(), "%Y%m%d_%H%M%S")
checkpoint_dir <- file.path(calibration_out_dir,
                            paste0("run_nm_", region_safe, "_", run_id))
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

checkpoint_best_file        <- file.path(checkpoint_dir, "checkpoint_best_beta.rds")
checkpoint_last_file        <- file.path(checkpoint_dir, "checkpoint_last_eval.rds")
history_file                <- file.path(checkpoint_dir, "history_objective.csv")
starts_file                 <- file.path(checkpoint_dir, "starts_used.rds")
fits_file                   <- file.path(checkpoint_dir, "fits_nm.rds")
final_file                  <- file.path(checkpoint_dir, "final_validation.rds")
validation_summary_run_file <- file.path(checkpoint_dir, "validation_summary.csv")

###############################################################################
###### CHECKPOINT UTILITIES ######
###############################################################################

eval_counter <- 0L
best_value   <- Inf

safe_saveRDS <- function(object, file) {
  tmp <- paste0(file, ".tmp"); saveRDS(object, tmp)
  if (file.exists(file)) file.remove(file)
  file.rename(tmp, file)
}

save_objective_state <- function(beta_type_log, beta_type, incidence_sim,
                                 objective_value, sse_by_type,
                                 is_best, start_id = NA_integer_) {
  state <- list(datetime = Sys.time(), region = FOCAL_REGION,
                eval_counter = eval_counter, start_id = start_id,
                objective_value = objective_value, sse_by_type = sse_by_type,
                is_best = is_best, n_rep_obj = n_rep_obj, n_cores = n_cores,
                seed_objective = seed_objective, beta_type_log = beta_type_log,
                beta_type = beta_type, incidence_sim = incidence_sim,
                incidence_obs = incidence_obs, lower_beta = lower_beta,
                upper_beta = upper_beta, checkpoint_dir = checkpoint_dir)
  safe_saveRDS(state, checkpoint_last_file)
  if (is_best) safe_saveRDS(state, checkpoint_best_file)
  beta_cols <- as.data.frame(as.list(beta_type), check.names = FALSE)
  names(beta_cols) <- paste0("beta_",    names(beta_type))
  inc_cols  <- as.data.frame(as.list(incidence_sim), check.names = FALSE)
  names(inc_cols)  <- paste0("inc_sim_", names(incidence_sim))
  sse_cols  <- as.data.frame(as.list(sse_by_type), check.names = FALSE)
  names(sse_cols)  <- paste0("sse_",     names(sse_by_type))
  hist_row  <- cbind(
    data.frame(eval_counter = eval_counter,
               datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               region = FOCAL_REGION, job_index = JOB_INDEX, start_id = start_id,
               objective_value = objective_value, best_value = best_value,
               is_best = is_best, n_rep_obj = n_rep_obj, stringsAsFactors = FALSE),
    beta_cols, inc_cols, sse_cols)
  write.table(hist_row, file = history_file, sep = ";", dec = ".", row.names = FALSE,
              col.names = !file.exists(history_file), append = file.exists(history_file))
}

###############################################################################
###### OBJECTIVE FUNCTION ######
###############################################################################

current_start_id <- NA_integer_

objective_fn <- function(beta_type_log) {
  eval_counter <<- eval_counter + 1L
  beta_type        <- exp(beta_type_log)
  names(beta_type) <- names(incidence_obs)
  beta_vec         <- beta_type[as.character(type_etab_calib)]
  if (anyNA(beta_vec)) beta_vec[is.na(beta_vec)] <- mean(beta_type, na.rm = TRUE)
  results <- parLapply(cl, X = rep_chunks,
                       fun = function(rs, beta_vec, seed_objective) {
                         do.call(rbind, lapply(rs, function(r)
                           run_simulation_summary(beta_vec = beta_vec, alpha = alpha,
                                                  seed = seed_objective + r)))
                       }, beta_vec = beta_vec, seed_objective = seed_objective)
  inc_mat         <- do.call(rbind, results)
  incidence_sim   <- colMeans(inc_mat, na.rm = TRUE)[names(incidence_obs)]
  sse_by_type     <- (incidence_sim - incidence_obs)^2
  objective_value <- sum(sse_by_type, na.rm = TRUE)
  is_best <- objective_value < best_value
  if (is_best) best_value <<- objective_value
  save_objective_state(beta_type_log, beta_type, incidence_sim,
                       objective_value, sse_by_type, is_best,
                       start_id = current_start_id)
  print(data.frame(eval = eval_counter, region = FOCAL_REGION,
                   start = current_start_id, SSE = round(objective_value, 6),
                   best = round(best_value, 6), is_best = is_best))
  print("Simulated:"); print(round(incidence_sim, 3))
  print("Observed: "); print(round(incidence_obs, 3))
  print("SSE/type:"); print(round(sse_by_type, 3))
  objective_value
}

###############################################################################
###### BOUNDED OBJECTIVE ######
###############################################################################

lower_log <- log(lower_beta);  upper_log <- log(upper_beta)

objective_bounded <- function(beta_type_log) {
  if (any(!is.finite(beta_type_log))) return(1e12)
  penalty <- sum(pmax(beta_type_log - upper_log, 0)^2 +
                   pmax(lower_log - beta_type_log, 0)^2)
  if (penalty > 0) return(1e9 + 1e9 * penalty)
  objective_fn(beta_type_log)
}

###############################################################################
###### STARTING POINTS ######
# Using beta_analytical (incidence-derived) instead of beta_start (warm start).
# Previous approach used: starts[[1]] <- log(beta_start) from warm start file.
###############################################################################

starts <- list()
starts[[1]] <- log(beta_analytical)                               # analytical estimate
starts[[2]] <- log(pmin(beta_analytical * 1.5,  upper_beta))     # scale up ×1.5
starts[[3]] <- log(pmax(beta_analytical * 0.67, lower_beta))     # scale down ×0.67
starts[[4]] <- log(pmin(beta_analytical * 2.0,  upper_beta))     # scale up ×2

set.seed(seed_random_starts)
for (s in seq_len(n_random_starts)) {
  br <- pmin(pmax(beta_analytical * exp(runif(length(beta_analytical),
                                              log(0.25), log(4))),
                  lower_beta), upper_beta)
  starts[[length(starts) + 1]] <- log(br[names(incidence_obs)])
}
starts <- lapply(starts, function(x)
  pmin(pmax(x[names(incidence_obs)], lower_log), upper_log))
names(starts) <- paste0("start_", seq_along(starts))

safe_saveRDS(list(datetime = Sys.time(), region = FOCAL_REGION, job_index = JOB_INDEX,
                  starts_log = starts, starts_beta = lapply(starts, exp),
                  beta_analytical = beta_analytical, incidence_obs = incidence_obs),
             starts_file)
print("STARTING POINTS"); print(lapply(starts, exp))

###############################################################################
###### NELDER-MEAD + RECOVERY + VALIDATION ######
###############################################################################

tryCatch({
  
  fits <- vector("list", length(starts));  names(fits) <- names(starts)
  
  for (i in seq_along(starts)) {
    current_start_id <<- i
    print(paste("Start", names(starts)[i], "— Region:", FOCAL_REGION))
    print(round(exp(starts[[i]]), 6))
    fit_i <- tryCatch(
      optim(par = starts[[i]], fn = objective_bounded, method = "Nelder-Mead",
            control = list(maxit = maxit_nm, trace = 1, REPORT = 1, reltol = 1e-4,
                           parscale = rep(1, length(starts[[i]])))),
      error = function(e) list(par = starts[[i]], value = Inf,
                               convergence = NA_integer_, message = conditionMessage(e))
    )
    fits[[i]] <- fit_i
    safe_saveRDS(list(datetime = Sys.time(), region = FOCAL_REGION, fits = fits,
                      eval_counter = eval_counter, best_value = best_value), fits_file)
    print(paste("SSE =", round(fit_i$value, 6), "  convergence =", fit_i$convergence))
  }
  current_start_id <<- NA_integer_
  
  fit_values  <- sapply(fits, function(x) x$value)
  best_fit_id <- which.min(fit_values)
  print("SSE by start:"); print(round(fit_values, 6))
  
  if (file.exists(checkpoint_best_file)) {
    ckpt          <- readRDS(checkpoint_best_file)
    beta_type_opt <- ckpt$beta_type;  beta_type_log_opt <- ckpt$beta_type_log
    if (ckpt$objective_value > fit_values[best_fit_id]) {
      beta_type_opt     <- exp(fits[[best_fit_id]]$par)
      names(beta_type_opt) <- names(incidence_obs)
      beta_type_log_opt <- log(beta_type_opt)
    }
  } else {
    beta_type_opt     <- exp(fits[[best_fit_id]]$par)
    names(beta_type_opt) <- names(incidence_obs)
    beta_type_log_opt <- log(beta_type_opt)
  }
  
  beta_type_opt <- pmin(pmax(beta_type_opt[names(incidence_obs)], lower_beta), upper_beta)
  beta_opt      <- beta_type_opt[as.character(type_etab_calib)]
  if (anyNA(beta_opt)) beta_opt[is.na(beta_opt)] <- mean(beta_type_opt, na.rm = TRUE)
  
  print(paste("OPTIMAL BETA —", FOCAL_REGION, ":")); print(round(beta_type_opt, 6))
  
  res_final <- parLapply(cl, X = rep_chunks_valid,
                         fun = function(rs, beta_opt, seed_validation) {
                           do.call(rbind, lapply(rs, function(r)
                             run_simulation_summary(beta_vec = beta_opt, alpha = alpha,
                                                    seed = seed_validation + r)))
                         }, beta_opt = beta_opt, seed_validation = seed_validation)
  
  inc_final_mat     <- do.call(rbind, res_final)
  inc_final         <- colMeans(inc_final_mat, na.rm = TRUE)[names(incidence_obs)]
  inc_final_sd      <- apply(inc_final_mat, 2, sd, na.rm = TRUE)[names(incidence_obs)]
  inc_final_se      <- inc_final_sd / sqrt(n_rep_valid)
  diff_final        <- (inc_final - incidence_obs)[names(incidence_obs)]
  sse_final_by_type <- diff_final^2
  sse_final         <- sum(sse_final_by_type, na.rm = TRUE)
  
  validation_summary <- data.frame(
    scope  = "Region", region = FOCAL_REGION, job_index = JOB_INDEX,
    type   = names(incidence_obs),
    beta   = as.numeric(beta_type_opt[names(incidence_obs)]),
    incidence_obs      = as.numeric(incidence_obs),
    incidence_sim_mean = as.numeric(inc_final),
    incidence_sim_sd   = as.numeric(inc_final_sd),
    incidence_sim_se   = as.numeric(inc_final_se),
    diff   = as.numeric(diff_final),
    sse    = as.numeric(sse_final_by_type),
    stringsAsFactors = FALSE)
  
  final_object <- list(
    datetime = Sys.time(), scope = "Region", region = FOCAL_REGION,
    job_index = JOB_INDEX, H_region = H,
    beta_type_opt = beta_type_opt, beta_type_log_opt = beta_type_log_opt,
    beta_opt = beta_opt, incidence_obs = incidence_obs,
    incidence_final = inc_final, incidence_final_sd = inc_final_sd,
    incidence_final_se = inc_final_se, diff_final = diff_final,
    sse_final_by_type = sse_final_by_type, sse_final = sse_final,
    n_rep_obj = n_rep_obj, n_rep_valid = n_rep_valid, n_cores = n_cores,
    fits = fits, fit_values = fit_values, best_fit_id = best_fit_id,
    validation_summary = validation_summary, checkpoint_dir = checkpoint_dir)
  
  safe_saveRDS(final_object, final_file)
  safe_saveRDS(final_object, final_recovered_beta_file)
  write.csv2(validation_summary, file = validation_summary_run_file, row.names = FALSE)
  
  print(paste("REGION:", FOCAL_REGION))
  print("BETA OPTIMAL:");     print(round(beta_type_opt, 6))
  print("INCIDENCE SIMULEE:"); print(round(inc_final, 3))
  print("INCIDENCE OBSERVEE:"); print(round(incidence_obs, 3))
  print(paste("SSE TOTALE =", round(sse_final, 6)))
  print("TABLEAU VALIDATION:"); print(validation_summary)
  print("DOSSIER DE SORTIE:");  print(checkpoint_dir)
  
}, finally = {
  message("Stopping cluster ...")
  try(stopCluster(cl), silent = TRUE)
})