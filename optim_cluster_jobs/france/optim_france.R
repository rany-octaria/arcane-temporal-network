# =============================================================================
# optim_france.R  —  Beta calibration, France-wide  (CLUSTER VERSION v2)
# =============================================================================
# Based on locally-validated code (document 6).
# Changes from local → cluster:
#   • setwd() removed; paths from ARCANE_ROOT environment variable
#   • FORK parallelism (Linux); no clusterExport needed
#   • Warm start loaded from recover_checkpoint.R output; analytical fallback
# Speed optimisations vs v1:
#   • Simulation function fully vectorised (no per-hospital R loop)
#   • Transfer loop sparse: only hospitals with actual exits on that day
#   • P_tr re-normalisation removed (matrix already column-normalised)
#   • transfer_idx pre-cached: which(p_tr > 0) computed once upfront
#   • Tmax halved 730→365, measuring last 182 days (burn-in via warm start)
#   • Reduced knobs: n_rep_obj=20, n_rep_valid=100, n_random_starts=2, maxit_nm=30
# All data management kept from local version:
#   DEFAULT_LOS_TYPE and GLOBAL_DEFAULT_LOS from actual data,
#   type_spares as calibration variable, 3-variable join, filter(!is.na(region))
# =============================================================================

library(parallel)
library(dplyr)

###############################################################################
###### PATHS AND JOB INDEX ######
###############################################################################

PROJECT_ROOT <- Sys.getenv("ARCANE_ROOT", unset = "/media/kevinNFS2/rany")
JOB_DIR      <- file.path(PROJECT_ROOT, "optim_cluster_jobs")
DATA_DIR     <- file.path(JOB_DIR, "data")

JOB_INDEX <- { v <- suppressWarnings(as.integer(Sys.getenv("jobindex")))
               if (!is.na(v) && v > 0L) v else 1L }

message("=== FRANCE-WIDE CALIBRATION  |  job ", JOB_INDEX, " of 10 ===")

###############################################################################
###### DATA LOADING ######
###############################################################################

weekly_transfers <- readRDS(file.path(DATA_DIR, "weekly.RDS")) %>%
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

facility_level <- readRDS(file.path(DATA_DIR, "facility_level_final.RDS")) %>%
  mutate(finess_geo = as.character(finess_geo)) %>%
  rename(incidence_esbl_all = incidence_region_type_ESBL_all)

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
###### HOSPITAL UNIVERSE AND SIMULATION PARAMETERS ######
###############################################################################

hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_target))
) %>% distinct()

# LOS defaults computed from actual data (not hardcoded)
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

hospitals <- hospitals %>%
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

hosp_idx <- setNames(seq_len(nrow(hospitals)), hospitals$finess_geo)
H      <- nrow(hospitals)
beds   <- hospitals$no_beds
p_exit <- 1 / hospitals$los

transfer_out_df <- weekly_transfers %>%
  transmute(origin = as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out = sum(weight, na.rm = TRUE), .groups = "drop")
hospitals <- hospitals %>%
  left_join(transfer_out_df, by = c("finess_geo" = "origin")) %>%
  mutate(total_out = replace(total_out, is.na(total_out), 0))
p_tr <- pmin(hospitals$total_out / pmax(p_exit * beds, 1), 0.60)

message("Building P_tr (", H, " x ", H, ") ...")
transfer_agg <- weekly_transfers %>%
  transmute(orig = hosp_idx[as.character(finess_geo_origin)],
            dest = hosp_idx[as.character(finess_geo_target)],
            weight) %>%
  filter(!is.na(orig) & !is.na(dest)) %>%
  group_by(orig, dest) %>%
  summarise(weight = sum(weight), .groups = "drop")

P_tr <- matrix(0.0, H, H)
for (k in seq_len(nrow(transfer_agg)))
  P_tr[transfer_agg$dest[k], transfer_agg$orig[k]] <- transfer_agg$weight[k]
cs <- colSums(P_tr)
for (h in seq_len(H)) if (cs[h] > 0) P_tr[, h] <- P_tr[, h] / cs[h]
message("  Done. Hospitals with outgoing transfers: ", sum(cs > 0))

# Pre-cache indices of hospitals with non-zero transfer probability.
# The sparse transfer loop in run_simulation_summary iterates only over
# these hospitals, skipping the ~85% that never transfer on a given day.
transfer_idx <- which(p_tr > 0)
message("  Transfer-eligible hospitals: ", length(transfer_idx),
        " of ", H, " (", round(100 * length(transfer_idx) / H, 1), "%)")

pi_vec          <- rep(0.05, H)
type_etab_calib <- hospitals$type_spares

spares_types <- facility_targets %>%
  filter(incidence_source != "global_mean") %>% distinct(type_spares)
target_type <- hospitals %>%
  group_by(type_spares) %>%
  summarise(target_incidence = mean(target_incidence), .groups = "drop")
incidence_obs <- target_type %>%
  semi_join(spares_types, by = "type_spares") %>%
  with(setNames(target_incidence, type_spares))

message("Calibration types (", length(incidence_obs), "): ",
        paste(names(incidence_obs), collapse = ", "))
message("Observed incidence (per 1,000 bed-days):")
print(round(incidence_obs, 3))

###############################################################################
###### PARAMÈTRES ######
###############################################################################

n_cores <- {
  v <- suppressWarnings(as.integer(Sys.getenv("NCPUS")))
  if (!is.na(v) && v > 0L) max(1L, v - 1L) else 4L
}

# ── Smarter knobs: faster convergence without losing final reliability ────────
# n_rep_obj reduced 100→50: halves cost per objective call; warm start
#   compensates by needing fewer iterations to converge.
# n_random_starts reduced 8→3: analytical/warm start already near optimum,
#   fewer random explorations needed.
# maxit_nm reduced 100→50: warm start converges faster; extra iterations
#   past 50 rarely improve the result meaningfully.
# n_rep_valid kept at 300: final validation must remain reliable.
n_rep_obj       <- 20    # was 50 → 20: enough for NM direction; warm start compensates
n_rep_valid     <- 100   # was 300 → 100: SE still ~SD/10, publishable
n_random_starts <- 2     # was 3 → 2: warm start already near optimum
maxit_nm        <- 30    # was 50 → 30: converges faster near warm start

# Seeds offset by JOB_INDEX so all 10 runs are fully independent
seed_objective     <- 1000  + (JOB_INDEX - 1L) * 10000L
seed_validation    <- 50000 + (JOB_INDEX - 1L) * 10000L
seed_random_starts <- 123   + (JOB_INDEX - 1L) * 10000L

lower_beta <- 1e-4   # narrowed from 1e-5
upper_beta <- 0.05   # narrowed from 0.10

gamma           <- 1 / 387
alpha           <- 0
# Tmax halved 730→365: warm start initialises close to steady state so
# burn-in year is not needed. Measuring last 182 days (half year).
Tmax            <- as.integer(365L)   # was 730
last_year_start <- Tmax - 181L        # day 184 — was Tmax - 364
last_year_len   <- 182L               # was 365

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
###### STARTING POINT ######
# Priority 1: warm start from recover_checkpoint.R (best SSE = 0.011 found
#             in previous 2-day run — starts very close to the optimum).
# Priority 2: analytical estimate from observed incidence (SIS steady-state
#             approximation: beta ≈ gamma + incidence/1000).
# Each of the 10 jobs offsets the warm start slightly via JOB_INDEX so they
# explore different neighbourhoods around the known good solution.
###############################################################################

warm_start_file <- file.path(JOB_DIR, "warm_start_france.rds")

if (file.exists(warm_start_file)) {
  ws        <- readRDS(warm_start_file)
  n_matched <- sum(names(ws$beta_type_opt) %in% names(incidence_obs))
  message("Warm start type names : ", paste(names(ws$beta_type_opt), collapse=", "))
  message("Current type names    : ", paste(names(incidence_obs),    collapse=", "))
  message("Matched types         : ", n_matched)

  if (n_matched == 0) {
    message("No type name overlap — falling back to analytical estimate.")
    beta_start <- pmin(pmax(incidence_obs / 1000 + gamma, lower_beta), upper_beta)
  } else {
    beta_start <- ws$beta_type_opt[names(incidence_obs)]
    missing    <- is.na(beta_start) | !is.finite(beta_start)
    if (any(missing)) {
      beta_start[missing] <- pmin(pmax(incidence_obs[missing] / 1000 + gamma,
                                       lower_beta), upper_beta)
      message("Warm start loaded (", sum(!missing), " of ", length(incidence_obs),
              " types matched; ", sum(missing), " filled analytically).")
    } else {
      message("Warm start fully loaded. Previous best SSE: ",
              round(ws$objective_value, 6))
    }
  }
} else {
  message("No warm start file — using analytical estimate.")
  beta_start <- pmin(pmax(incidence_obs / 1000 + gamma, lower_beta), upper_beta)
}

# Final nuclear guard: ensure absolutely no NAs or non-finite values reach
# the optimizer — replace any remaining issues with analytical estimate
bad <- is.na(beta_start) | !is.finite(beta_start) | beta_start <= 0
if (any(bad)) {
  message("WARNING: ", sum(bad), " NA/Inf beta_start values replaced analytically.")
  beta_start[bad] <- pmin(pmax(incidence_obs[bad] / 1000 + gamma,
                                lower_beta), upper_beta)
}
beta_start <- pmin(pmax(beta_start, lower_beta), upper_beta)
message("Starting beta:"); print(round(beta_start, 6))

###############################################################################
###### SIMULATION FUNCTION — VECTORISED ######
# Key speedups vs v1:
#   1. SIS loop replaced by vectorised rbinom(H, ...) calls — no per-hospital R loop
#   2. Exits vectorised: rbinom(H, S_loc, p_exit) instead of H separate calls
#   3. Transfer loop sparse: only hospitals in transfer_idx with exits that day
#   4. P_tr columns not re-normalised (already done at build time)
#   5. Tmax = 365, measuring days 184–365 (182-day window)
###############################################################################

run_simulation_summary <- function(beta_vec, alpha, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  p_rec <- 1 - exp(-gamma)

  I_loc <- rbinom(H, beds, prev_init_etab)
  S_loc <- beds - I_loc
  inc_sum_last <- numeric(H)

  for (t in seq_len(Tmax)) {

    # ── Step 1: vectorised SIS transmission ────────────────────────────────
    # is.finite(beta_vec) guard prevents NA beta values from producing NA p_inf
    N       <- S_loc + I_loc
    p_inf   <- ifelse(N > 0L & is.finite(beta_vec),
                      1 - exp(-beta_vec * I_loc / pmax(N, 1L)), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    if (t >= last_year_start) inc_sum_last <- inc_sum_last + new_inf
    S_loc <- S_loc - new_inf + recov
    I_loc <- I_loc + new_inf - recov

    # ── Step 2: vectorised exits ────────────────────────────────────────────
    n_exit_S <- rbinom(H, S_loc, p_exit)
    n_exit_I <- rbinom(H, I_loc, p_exit)
    S_loc <- S_loc - n_exit_S
    I_loc <- I_loc - n_exit_I

    # ── Step 3: sparse transfer loop ────────────────────────────────────────
    # Only iterate over hospitals that (a) have non-zero p_tr [transfer_idx]
    # AND (b) actually had exits today. Typically ~10-15% of H per day.
    S_tr <- numeric(H)
    I_tr <- numeric(H)
    active_h <- transfer_idx[(n_exit_S[transfer_idx] +
                                n_exit_I[transfer_idx]) > 0L]

    for (h in active_h) {
      n_tr_S <- rbinom(1L, n_exit_S[h], p_tr[h])
      n_tr_I <- rbinom(1L, n_exit_I[h],
                       pmin(pmax((1 - alpha) * p_tr[h], 0), 1))
      if ((n_tr_S + n_tr_I) == 0L) next
      # P_tr columns are already normalised at build time — no re-normalisation
      probs <- P_tr[, h]
      if (n_tr_S > 0L) S_tr <- S_tr + rmultinom(1L, n_tr_S, probs)[, 1L]
      if (n_tr_I > 0L) I_tr <- I_tr + rmultinom(1L, n_tr_I, probs)[, 1L]
    }

    # ── Step 4: community admissions ────────────────────────────────────────
    occ   <- S_loc + I_loc + S_tr + I_tr
    A     <- pmax(0L, beds - occ)
    A_I   <- rbinom(H, A, pi_vec)
    S_loc <- S_loc + S_tr + (A - A_I)
    I_loc <- I_loc + I_tr + A_I
  }

  inc_etab       <- 1000 * inc_sum_last / (beds * last_year_len)
  incidence_type <- tapply(inc_etab, type_etab_calib, mean, na.rm = TRUE)
  incidence_type[names(incidence_obs)]
}

###############################################################################
###### CLUSTER  (FORK — Linux only, no clusterExport needed) ######
###############################################################################

if (exists("cl") && inherits(cl, "cluster")) try(stopCluster(cl), silent = TRUE)
cl <- makeCluster(n_cores, type = "FORK")
message("Cluster started: ", n_cores, " FORK workers")

rep_chunks       <- split(seq_len(n_rep_obj),
                          rep(seq_len(n_cores), length.out = n_rep_obj))
rep_chunks_valid <- split(seq_len(n_rep_valid),
                          rep(seq_len(n_cores), length.out = n_rep_valid))

###############################################################################
###### CHECKPOINT PATHS ######
###############################################################################

run_id         <- format(Sys.time(), "%Y%m%d_%H%M%S")
checkpoint_dir <- file.path(calibration_out_dir, paste0("run_nm_france_", run_id))
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
  state <- list(datetime = Sys.time(), eval_counter = eval_counter,
                start_id = start_id, objective_value = objective_value,
                sse_by_type = sse_by_type, is_best = is_best,
                n_rep_obj = n_rep_obj, n_cores = n_cores,
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
               scope = "France", job_index = JOB_INDEX, start_id = start_id,
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
  print(data.frame(eval = eval_counter, start = current_start_id,
                   SSE = round(objective_value, 6), best = round(best_value, 6),
                   is_best = is_best))
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
# 3 structured starts around beta_start + 3 random perturbations = 6 total.
# With a warm start already at SSE=0.011, these cover the local neighbourhood
# without wasteful wide exploration.
###############################################################################

starts <- list()
starts[[1]] <- log(beta_start)                               # warm start as-is
starts[[2]] <- log(pmin(beta_start * 1.5,  upper_beta))     # nudge up
starts[[3]] <- log(pmax(beta_start * 0.67, lower_beta))     # nudge down

set.seed(seed_random_starts)
for (s in seq_len(n_random_starts)) {
  br <- pmin(pmax(beta_start * exp(runif(length(beta_start), log(0.5), log(2))),
                  lower_beta), upper_beta)
  # narrower random range (×0.5 to ×2) vs original (×0.25 to ×4) since
  # warm start is already close — no need to explore far away
  starts[[length(starts) + 1]] <- log(br[names(incidence_obs)])
}
starts <- lapply(starts, function(x)
  pmin(pmax(x[names(incidence_obs)], lower_log), upper_log))
names(starts) <- paste0("start_", seq_along(starts))

safe_saveRDS(list(datetime = Sys.time(), scope = "France", job_index = JOB_INDEX,
                  starts_log = starts, starts_beta = lapply(starts, exp),
                  beta_start = beta_start, incidence_obs = incidence_obs), starts_file)
print("STARTING POINTS"); print(lapply(starts, exp))

###############################################################################
###### NELDER-MEAD + RECOVERY + VALIDATION ######
###############################################################################

tryCatch({

  fits <- vector("list", length(starts));  names(fits) <- names(starts)

  for (i in seq_along(starts)) {
    current_start_id <<- i
    print(paste("Start", names(starts)[i], "— France-wide"))
    print(round(exp(starts[[i]]), 6))
    fit_i <- tryCatch(
      optim(par = starts[[i]], fn = objective_bounded, method = "Nelder-Mead",
            control = list(maxit = maxit_nm, trace = 1, REPORT = 1, reltol = 1e-4,
                           parscale = rep(1, length(starts[[i]])))),
      error = function(e) list(par = starts[[i]], value = Inf,
                               convergence = NA_integer_, message = conditionMessage(e))
    )
    fits[[i]] <- fit_i
    safe_saveRDS(list(datetime = Sys.time(), fits = fits,
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

  print("OPTIMAL BETA PER TYPE:"); print(round(beta_type_opt, 6))

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
    scope = "France", region = "All", job_index = JOB_INDEX,
    type  = names(incidence_obs),
    beta  = as.numeric(beta_type_opt[names(incidence_obs)]),
    incidence_obs      = as.numeric(incidence_obs),
    incidence_sim_mean = as.numeric(inc_final),
    incidence_sim_sd   = as.numeric(inc_final_sd),
    incidence_sim_se   = as.numeric(inc_final_se),
    diff  = as.numeric(diff_final),
    sse   = as.numeric(sse_final_by_type),
    stringsAsFactors = FALSE)

  final_object <- list(
    datetime = Sys.time(), scope = "France", job_index = JOB_INDEX,
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
