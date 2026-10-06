# =============================================================================
# optim_france.R  —  Beta calibration, France-wide  (CLUSTER VERSION v3)
# =============================================================================
# PATIENT FLOW (updated from v2 — harmonized with calibration_incidence_new):
#   Discharge  = n_admit[h] from NO_INPATIENTS_ADMISSION_DAILY_DIRCT_HBN.csv
#   Transfers  = T_mat[j,h] = round(weekly/7); exact counts, no sampling
#   I/S split  = Binomial(n_transfers, I/N) per hospital per day
#   Community  = n_admit - incoming transfers (exact)
#   Tmax       = 3 x 366 = 1,098 days; 366-day admission cycle via modulo
#   Measurement= last 180 days (harmonized with calibration_incidence_new)
#   Incidence  = bed-weighted per type_spares: sum(new_inf)/sum(pt_days)x1000
#
# Changes from v2:
#   • LOS-based discharge removed; admission-based full occupancy
#   • p_exit, LOS, DEFAULT_LOS_TYPE removed
#   • P_tr (probability matrix) replaced by T_mat (integer count matrix)
#   • p_tr removed; transfers are exact counts not probabilities
#   • pmax(0L) for weekly weights — preserves true zero transfer routes
#   • Tmax 365 → 366; last_year_start adjusted accordingly
#   • run_simulation_summary loop updated to new patient flow
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

# pmax(0L): preserve true zero routes — do NOT force minimum 1
weekly_transfers <- readRDS(file.path(DATA_DIR, "weekly.RDS")) %>%
  mutate(weight = pmax(0L, as.integer(round(weight / 7))))

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
###### HOSPITAL UNIVERSE ######
###############################################################################

hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_target))
) %>% distinct()

# No LOS — discharge governed by daily admissions from CSV
hospitals <- hospitals %>%
  left_join(
    facility_level %>% transmute(
      finess_geo, hospital_type, type_spares, region,
      no_beds = as.integer(round(census_max))
    ), by = "finess_geo"
  ) %>%
  left_join(facility_targets, by = c("finess_geo", "type_spares", "region")) %>%
  mutate(
    no_beds          = as.integer(if_else(is.na(no_beds),
                                          as.integer(round(mean(no_beds, na.rm = TRUE))),
                                          no_beds)),
    target_incidence = if_else(is.na(target_incidence), global_inc, target_incidence),
    type_spares      = if_else(is.na(type_spares), "Unknown", type_spares),
    incidence_source = if_else(is.na(incidence_source), "global_mean", incidence_source),
    region           = if_else(is.na(region), "Unknown", region)
  )

hosp_idx <- setNames(seq_len(nrow(hospitals)), hospitals$finess_geo)
H    <- nrow(hospitals)
beds <- hospitals$no_beds

# Aggregate total daily transfers out per hospital
transfer_out_df <- weekly_transfers %>%
  filter(weight > 0L) %>%
  transmute(origin = as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out = sum(weight, na.rm = TRUE), .groups = "drop")
hospitals <- hospitals %>%
  left_join(transfer_out_df, by = c("finess_geo" = "origin")) %>%
  mutate(total_out = replace(total_out, is.na(total_out), 0))

# Build T_mat: exact daily transfer count matrix (T_mat[j, h] = from h to j)
message("Building T_mat (", H, " x ", H, ") ...")
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
total_out_daily <- as.integer(colSums(T_mat))  # leaving each h per day
total_in_daily  <- as.integer(rowSums(T_mat))  # arriving at each h per day
transfer_idx    <- which(total_out_daily > 0L)

message("  T_mat built | Transfer-eligible: ", length(transfer_idx),
        " of ", H, " (", round(100 * length(transfer_idx) / H, 1), "%)")
message("  Total daily transfers: ", sum(total_out_daily))

###############################################################################
###### ADMISSION MATRIX ######
###############################################################################

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

admit_daily <- aggregate(no_admissions ~ finess_geo + day_of_year,
                         data = admit_raw,
                         FUN  = function(x) round(mean(x, na.rm = TRUE)))
names(admit_daily)[3] <- "admissions"

hosp_means <- aggregate(admissions ~ finess_geo,
                         data = admit_daily,
                         FUN  = function(x) as.integer(round(mean(x, na.rm = TRUE))))
names(hosp_means)[2] <- "mean_admit"

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
admit_matrix[is.na(admit_matrix)] <- 1L
storage.mode(admit_matrix) <- "integer"
admit_matrix <- matrix(pmax(1L, as.vector(admit_matrix)), nrow = H, ncol = 366L)
rownames(admit_matrix) <- hospitals$finess_geo
stopifnot(is.matrix(admit_matrix), nrow(admit_matrix) == H, !any(is.na(admit_matrix)))
message(sprintf("  admit_matrix: %d x %d | range: %d-%d | mean: %.1f",
                nrow(admit_matrix), ncol(admit_matrix),
                min(admit_matrix), max(admit_matrix), mean(admit_matrix)))

###############################################################################
###### CALIBRATION SETUP ######
###############################################################################

pi_vec          <- rep(0.05, H)   # 5% community carriage on admission (ESBL-E endemic)
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
###### PARAMETERS ######
###############################################################################

n_cores <- {
  v <- suppressWarnings(as.integer(Sys.getenv("NCPUS")))
  if (!is.na(v) && v > 0L) max(1L, v - 1L) else 4L
}

n_rep_obj       <- 20
n_rep_valid     <- 100
n_random_starts <- 2
maxit_nm        <- 30

seed_objective     <- 1000  + (JOB_INDEX - 1L) * 10000L
seed_validation    <- 50000 + (JOB_INDEX - 1L) * 10000L
seed_random_starts <- 123   + (JOB_INDEX - 1L) * 10000L

lower_beta <- 1e-4
upper_beta <- 0.05

gamma           <- 1 / 387
alpha           <- 0
# Three calendar years: 366-day admission cycle repeats via modulo
N_CYCLES        <- 3L
DAYS_PER_CYCLE  <- 366L
Tmax            <- N_CYCLES * DAYS_PER_CYCLE   # 1,098 days
last_year_len   <- 180L                        # last 180 days (harmonized)
last_year_start <- Tmax - last_year_len + 1L   # day 919

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

bad <- is.na(beta_start) | !is.finite(beta_start) | beta_start <= 0
if (any(bad)) {
  message("WARNING: ", sum(bad), " NA/Inf beta_start values replaced analytically.")
  beta_start[bad] <- pmin(pmax(incidence_obs[bad] / 1000 + gamma,
                                lower_beta), upper_beta)
}
beta_start <- pmin(pmax(beta_start, lower_beta), upper_beta)
message("Starting beta:"); print(round(beta_start, 6))

###############################################################################
###### HELPER: allocate_exact ######
# Distribute n patients to destinations exactly proportional to weights.
# Hamilton largest-remainder method: sum(result) == n exactly.
###############################################################################

allocate_exact <- function(n, weights) {
  if (n == 0L || sum(weights) == 0) return(integer(length(weights)))
  exact  <- n * weights / sum(weights)
  floors <- as.integer(floor(exact))
  rem    <- n - sum(floors)
  if (rem > 0L) {
    top <- order(exact - floors, decreasing = TRUE)[seq_len(rem)]
    floors[top] <- floors[top] + 1L
  }
  floors
}

###############################################################################
###### SIMULATION FUNCTION ######
# Patient flow (v3):
#   Discharge = n_admit[h, t] (exact from CSV; one calendar year)
#   Transfers = T_mat[j, h]   (exact daily counts; weekly/7 rounded)
#   I/S split = Binomial(n_out_cap[h], I/N[h]) vectorised per hospital
#   Community = n_admit - incoming transfers (exact residual)
###############################################################################

run_simulation_summary <- function(beta_vec, alpha, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  p_rec <- 1 - exp(-gamma)

  I_loc <- rbinom(H, beds, prev_init_etab)
  S_loc <- beds - I_loc
  inc_sum_last <- numeric(H)
  pt_sum_last  <- numeric(H)   # bed-days in measurement window

  for (t in seq_len(Tmax)) {

    # ── Step 1: vectorised SIS transmission ──────────────────────────────────
    N_pre   <- S_loc + I_loc                    # pre-transmission occupancy
    p_inf   <- ifelse(N_pre > 0L & is.finite(beta_vec),
                      1 - exp(-beta_vec * I_loc / pmax(N_pre, 1L)), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    if (t >= last_year_start) {
      inc_sum_last <- inc_sum_last + new_inf    # new colonisations
      pt_sum_last  <- pt_sum_last  + N_pre      # bed-days denominator
    }
    S_loc <- S_loc - new_inf + recov
    I_loc <- I_loc + new_inf - recov

    # ── Step 2: exact daily admissions (cycle through 366-day calendar) ────────
    day_idx <- ((t - 1L) %% DAYS_PER_CYCLE) + 1L
    n_admit <- admit_matrix[, day_idx]
    n_discharge <- pmin(n_admit, S_loc + I_loc)  # safety

    # ── Step 3: exact transfers with stochastic I/S split ────────────────────
    n_out_cap <- pmin(total_out_daily, n_discharge)  # vectorised cap
    N_cur     <- S_loc + I_loc
    p_I_cur   <- ifelse(N_cur > 0L, I_loc / N_cur, 0.0)

    I_out_total <- rbinom(H, n_out_cap, p_I_cur)
    I_out_total <- pmin(I_out_total, I_loc)
    S_out_total <- pmin(n_out_cap - I_out_total, S_loc)

    S_tr_in <- integer(H); I_tr_in <- integer(H)
    for (h in transfer_idx) {
      if (n_out_cap[h] == 0L) next
      nI <- I_out_total[h]; nS <- S_out_total[h]
      if (nI > 0L) I_tr_in <- I_tr_in + allocate_exact(nI, T_mat[, h])
      if (nS > 0L) S_tr_in <- S_tr_in + allocate_exact(nS, T_mat[, h])
    }

    # Remove outgoing from wards (transfers + community discharges)
    n_community_discharge <- n_discharge - n_out_cap
    I_community_out <- rbinom(H, n_community_discharge, p_I_cur)
    I_community_out <- pmin(I_community_out, pmax(0L, I_loc - I_out_total))
    S_community_out <- pmin(n_community_discharge - I_community_out,
                             pmax(0L, S_loc - S_out_total))
    I_loc <- pmax(0L, I_loc - I_out_total - I_community_out)
    S_loc <- pmax(0L, S_loc - S_out_total - S_community_out)

    # ── Step 4: community admissions = n_admit - exact incoming transfers ─────
    transfers_received <- S_tr_in + I_tr_in
    community_admit    <- pmax(0L, n_admit - transfers_received)
    A_I   <- rbinom(H, community_admit, pi_vec)
    S_loc <- S_loc + S_tr_in + (community_admit - A_I)
    I_loc <- I_loc + I_tr_in + A_I
  }

  # Bed-weighted incidence per type_spares: sum(new_inf) / sum(pt_days) x 1,000
  # Harmonized with calibration_incidence_new denominator (actual occupancy, not capacity)
  incidence_type <- setNames(
    sapply(names(incidence_obs), function(tp) {
      idx <- which(type_etab_calib == tp)
      if (length(idx) == 0 || sum(pt_sum_last[idx]) == 0) return(NA_real_)
      1000 * sum(inc_sum_last[idx]) / sum(pt_sum_last[idx])
    }),
    names(incidence_obs)
  )
  incidence_type[names(incidence_obs)]
}

###############################################################################
###### CLUSTER (FORK) ######
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
###############################################################################

starts <- list()
starts[[1]] <- log(beta_start)
starts[[2]] <- log(pmin(beta_start * 1.5,  upper_beta))
starts[[3]] <- log(pmax(beta_start * 0.67, lower_beta))

set.seed(seed_random_starts)
for (s in seq_len(n_random_starts)) {
  br <- pmin(pmax(beta_start * exp(runif(length(beta_start), log(0.5), log(2))),
                  lower_beta), upper_beta)
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
    validation_summary = validation_summary, checkpoint_dir = checkpoint_dir,
    patient_flow = "admission-based full occupancy | exact T_mat transfers | Binomial I/S")

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
