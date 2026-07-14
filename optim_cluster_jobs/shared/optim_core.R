# =============================================================================
# optim_core.R  —  shared calibration logic
# -----------------------------------------------------------------------------
# DO NOT edit the knobs here.  All parameters (N_CORES, N_REP_OBJ, etc.)
# must be defined BEFORE this file is sourced.  The driver script,
#   optim_cluster_jobs/calibration/optim_local.R
# sets those variables and then calls:
#   source(file.path(SHARED_DIR, "optim_core.R"))
#
# FOLDER LAYOUT (everything lives under one project-root folder now —
# no more separate cluster_jobs/ or Datasets/ folders):
#   <project_root>/
#   └── optim_cluster_jobs/
#       ├── shared/
#       │   └── optim_core.R         ← this file
#       ├── calibration/
#       │   └── optim_local.R        ← driver (sets knobs, sources this file)
#       ├── data/
#       │   ├── weekly.RDS           ← transfer network
#       │   └── facility_level_final.RDS  ← facility type/region/beds/LOS/incidence
#       └── Outputs/                 ← results land here
#
# DATA SOURCE:
#   Facility-level information (type, region, bed capacity, length of stay,
#   observed incidence) comes from a SINGLE consolidated file:
#     facility_level_final.RDS
#   Specifically:
#     - Bed capacity ("beds") = the census_max column, i.e. we assume FULL
#       OCCUPANCY at each facility's observed maximum census.  This is a
#       steady-state modelling choice: every bed is always occupied, so
#       bed-days = census_max * days exactly (no separate vacancy dynamics).

#     - Length of stay = the los_mean column (falls back to DEFAULT_LOS by
#       facility type only where los_mean is NA).
#   The weekly transfer network (weekly.RDS) is the patient-flow matrix that
#   dictates facility-to-facility transfers in the steady-state model.
#   BOTH files now live in the SAME folder (DATA_DIR) — there is no longer a
#   separate DATASETS_DIR.
#
# Expected globals when this file is sourced:
#   N_CORES, N_REP_OBJ, N_REP_VALID, N_RANDOM_STARTS, MAXIT_NM
#   SEED_OBJ, SEED_VALID, SEED_STARTS
#   LOWER_BETA, UPPER_BETA
#   N_YEARS, INIT_PREV, ADMISSION_PREV, ALPHA, GAMMA_CLEAR
#   DEFAULT_LOS, MAX_P_TR
#   LOCAL_TEST (logical), N_HOSP_TEST (integer, used only when LOCAL_TEST=TRUE)
#   PARALLEL_TYPE  ("PSOCK" or "FORK")
#   DATA_DIR      — folder containing BOTH weekly.RDS and
#                   facility_level_final.RDS
#   OUT_DIR, V3_RESULT_FILE
#   BASE_TEXT
#
# NOTE: the `here` package is intentionally NOT used in this file.  `here()`
# auto-detects a project root via marker files (.here, .Rproj), which is
# fragile when the script is later moved to run on a cluster from an
# arbitrary submission directory.  All paths come from the explicit globals
# above instead.
# =============================================================================


# =============================================================================
# 1.  LIBRARIES
# =============================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(ggplot2)
library(parallel)


# =============================================================================
# 2.  DATA LOADING
# -----------------------------------------------------------------------------
# Both input files now live in the same DATA_DIR folder
# (optim_cluster_jobs/data/).
# =============================================================================

# weekly_transfers: daily-average patient-transfer counts between finess_geo
# pairs — this IS the steady-state transfer matrix that moves patients (and
# therefore colonisation) between facilities each simulated day.
# weight = 7-day rolling sum, divided by 7 and rounded to an integer daily
# average; pmax(1L, ...) keeps every reported edge active after rounding.
weekly_transfers <- readRDS(file.path(DATA_DIR, "weekly.RDS")) %>%
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

# facility_level_final.RDS: ONE consolidated row per finess_geo with
# facility_type, region, census_max (peak/full-occupancy bed count),
# los_mean (mean length of stay, days), and incidence_esbl_all (SPARES rate,
# per 1,000 patient-days).  Lives in the SAME folder as weekly.RDS (DATA_DIR).
facility_level <- readRDS(
  file.path(DATA_DIR, "facility_level_final.RDS")
) %>%
  mutate(finess_geo = as.character(finess_geo)) %>%
  rename(facility_type = type_spares,
         incidence_esbl_all = incidence_region_type_ESBL_all)  # The incidence i per 1000 patient-days


# =============================================================================
# 3.  FOUR-TIER INCIDENCE TARGETS  (same logic as v3, now sourced from
#     facility_level_final.RDS instead of node_attributes_full.RDS)
# =============================================================================
global_inc <- mean(facility_level$incidence_esbl_all, na.rm = TRUE)

type_region_inc <- facility_level %>%
  group_by(facility_type, region) %>%
  summarise(type_region_mean_inc = mean(incidence_esbl_all, na.rm = TRUE),
            .groups = "drop")

region_inc <- facility_level %>%
  group_by(region) %>%
  summarise(region_mean_inc = mean(incidence_esbl_all, na.rm = TRUE),
            .groups = "drop")

facility_targets <- facility_level %>%
  left_join(type_region_inc, by = c("facility_type", "region")) %>%
  left_join(region_inc,      by = "region") %>%
  mutate(
    target_incidence = case_when(
      !is.na(incidence_esbl_all)   ~ incidence_esbl_all,
      !is.na(type_region_mean_inc) ~ type_region_mean_inc,
      !is.na(region_mean_inc)      ~ region_mean_inc,
      TRUE                         ~ global_inc
    ),
    incidence_source = case_when(
      !is.na(incidence_esbl_all)   ~ "facility",
      !is.na(type_region_mean_inc) ~ "type_region_mean",
      !is.na(region_mean_inc)      ~ "region_mean",
      TRUE                         ~ "global_mean"
    )
  ) %>%
  select(finess_geo, facility_type, region, target_incidence, incidence_source)


# =============================================================================
# 4.  HOSPITAL UNIVERSE
# -----------------------------------------------------------------------------
# Bed capacity ("beds") is taken directly from facility_level$census_max — the
# facility's observed maximum census.  This implements the FULL-OCCUPANCY
# assumption: every bed is always filled, so bed-days = beds * days exactly,
# with no separate vacancy/occupancy dynamics needed in the simulation.
# =============================================================================
hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_target))
) %>% distinct()

# census_max as integer bed capacity; impute missing with the network-wide
# mean census_max (same imputation strategy as before, just a different source
# column).
mean_beds <- round(mean(facility_level$census_max, na.rm = TRUE))

hospitals <- hospitals %>%
  left_join(
    facility_level %>%
      transmute(finess_geo, no_beds = as.integer(round(census_max))),
    by = "finess_geo"
  ) %>%
  mutate(no_beds = as.integer(if_else(is.na(no_beds), mean_beds, no_beds))) %>%
  left_join(facility_targets, by = "finess_geo") %>%
  mutate(
    target_incidence = if_else(is.na(target_incidence), global_inc, target_incidence),
    facility_type    = if_else(is.na(facility_type),  "Unknown",    facility_type),
    incidence_source = if_else(is.na(incidence_source), "global_mean", incidence_source)
  )

# ── LOCAL TEST: optionally restrict to a small hospital subset ────────────────
# When LOCAL_TEST = TRUE the run is limited to N_HOSP_TEST hospitals so a full
# calibration cycle completes in seconds instead of hours.  The subset is
# chosen to include at least one hospital per facility type where possible.
if (LOCAL_TEST) {
  # Ensure at least one hospital of each type is included, then pad to N_HOSP_TEST
  type_reps  <- hospitals %>% group_by(facility_type) %>% slice(1) %>% ungroup()
  remaining  <- hospitals %>% anti_join(type_reps, by = "finess_geo")
  n_pad      <- max(0L, N_HOSP_TEST - nrow(type_reps))
  hospitals  <- bind_rows(type_reps, head(remaining, n_pad))
  # Restrict transfer edges to this subset
  keep_geo   <- hospitals$finess_geo
  weekly_transfers <- weekly_transfers %>%
    filter(as.character(finess_geo_origin) %in% keep_geo,
           as.character(finess_geo_target) %in% keep_geo)
  message("LOCAL_TEST: restricted to ", nrow(hospitals),
          " hospitals and ", nrow(weekly_transfers), " transfer edges")
}

H    <- nrow(hospitals)
beds <- hospitals$no_beds

message("Hospital network: H = ", H,
        "  |  total beds (census_max, full occupancy) = ",
        format(sum(beds), big.mark = ","))


# =============================================================================
# 5.  SIMULATION INPUTS  (p_exit, p_tr, dest_probs, pi_vec)
# =============================================================================

# ── 5a. LOS → daily discharge probability ────────────────────────────────────
# los_mean (mean length of stay, in days) comes directly from
# facility_level_final.RDS.  Where it is NA, fall back to the type-level
# DEFAULT_LOS constant; pmax(los, 1.0) guards against a zero/negative LOS
# producing p_exit > 1.
los_lookup <- facility_level %>%
  transmute(finess_geo, los = as.numeric(los_mean))

n_los_na <- sum(is.na(los_lookup$los))
message("LOS source: los_mean column  |  missing (filled via DEFAULT_LOS): ",
        n_los_na, " of ", nrow(los_lookup))

hospitals <- hospitals %>%
  left_join(los_lookup, by = "finess_geo") %>%
  mutate(
    los    = coalesce(los, DEFAULT_LOS[facility_type], 7.0),
    los    = pmax(los, 1.0),
    p_exit = 1 / los
  )

p_exit <- hospitals$p_exit

# ── 5b. Transfer probability p_tr[h] ─────────────────────────────────────────
hosp_idx     <- setNames(seq_len(H), hospitals$finess_geo)
transfer_out <- weekly_transfers %>%
  transmute(origin = as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out = sum(weight, na.rm = TRUE), .groups = "drop")

hospitals <- hospitals %>%
  left_join(transfer_out, by = c("finess_geo" = "origin")) %>%
  mutate(
    total_out = replace_na(total_out, 0),
    p_tr      = pmin(total_out / pmax(p_exit * beds, 1), MAX_P_TR)
  )
p_tr <- hospitals$p_tr

# ── 5c. Sparse destination probability list ───────────────────────────────────
message("Building sparse transfer destination lists ...")
transfers_by_origin <- weekly_transfers %>%
  transmute(
    origin   = as.character(finess_geo_origin),
    dest_idx = hosp_idx[as.character(finess_geo_target)],
    weight
  ) %>%
  filter(!is.na(dest_idx)) %>%
  group_by(origin) %>%
  group_split()

origin_names <- sapply(transfers_by_origin, function(x) x$origin[1])

dest_probs <- vector("list", H)
for (k in seq_along(transfers_by_origin)) {
  h <- hosp_idx[origin_names[k]]
  if (is.na(h)) next
  edges   <- transfers_by_origin[[k]]
  wts_sum <- sum(edges$weight)
  if (wts_sum <= 0) next
  dest_probs[[h]] <- setNames(edges$weight / wts_sum,
                              as.character(edges$dest_idx))
}
message("  Hospitals with outgoing transfers: ",
        sum(!sapply(dest_probs, is.null)))

pi_vec <- rep(ADMISSION_PREV, H)


# =============================================================================
# 6.  OBSERVED INCIDENCE TARGETS
# =============================================================================
spares_types <- facility_targets %>%
  filter(incidence_source != "global_mean") %>%
  distinct(facility_type)

target_type <- hospitals %>%
  group_by(facility_type) %>%
  summarise(target_incidence = mean(target_incidence), .groups = "drop")

incidence_obs <- target_type %>%
  semi_join(spares_types, by = "facility_type") %>%
  with(setNames(target_incidence, facility_type))

type_etab_calib <- hospitals$facility_type

# ── Diagnostic: facility types present in the network but with NO SPARES
#    signal at all (incidence_source == "global_mean" for every hospital of
#    that type — most commonly "Unknown", assigned in Section 4 to network
#    hospitals whose finess_geo was not found in facility_level_final.RDS).
#    These types are EXCLUDED from incidence_obs/beta_type_opt because there
#    is nothing real to calibrate against, but they still need SOME beta value
#    to participate in the simulation — handled via a fallback in Sections 12
#    and 16 below. This message lets you catch a join-key mismatch early.
orphan_types <- setdiff(unique(type_etab_calib), names(incidence_obs))
if (length(orphan_types) > 0) {
  n_orphan_hosp <- sum(type_etab_calib %in% orphan_types)
  message("\n*** WARNING: ", length(orphan_types),
          " facility type(s) in the network have NO SPARES-derived target: ",
          paste(orphan_types, collapse = ", "))
  message("    Affects ", n_orphan_hosp, " of ", H, " hospitals.")
  message("    If 'Unknown' appears above, those hospitals' finess_geo did ",
          "not match any row in facility_level_final.RDS — check for an ID ",
          "format mismatch (e.g. leading zeros, character vs numeric).")
  message("    These hospitals will receive a fallback beta (mean of ",
          "calibrated types) so the simulation can still run.\n")
}

message("Calibration types (", length(incidence_obs), "): ",
        paste(names(incidence_obs), collapse = ", "))
message("Observed incidence (per 1,000 bed-days):")
print(round(incidence_obs, 3))


# =============================================================================
# 7.  SIMULATION TIMING
# =============================================================================
Tmax            <- as.integer(N_YEARS * 365L)
last_year_start <- Tmax - 364L
last_year_len   <- 365L
prev_init_etab  <- rep(INIT_PREV, H)


# =============================================================================
# 8.  OUTPUT DIRECTORIES
# =============================================================================
run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
checkpoint_dir <- file.path(OUT_DIR, paste0("run_", run_id))
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

checkpoint_best_file    <- file.path(checkpoint_dir, "checkpoint_best_beta.rds")
checkpoint_last_file    <- file.path(checkpoint_dir, "checkpoint_last_eval.rds")
history_file            <- file.path(checkpoint_dir, "history_objective.csv")
starts_file             <- file.path(checkpoint_dir, "starts_used.rds")
fits_file               <- file.path(checkpoint_dir, "fits_nm.rds")
final_file              <- file.path(checkpoint_dir, "final_validation.rds")
validation_summary_file <- file.path(checkpoint_dir, "validation_summary.csv")
final_recovered_beta_file <- file.path(OUT_DIR, "recovered_best_beta.rds")


# =============================================================================
# 9.  CHECKPOINT INFRASTRUCTURE
# =============================================================================
eval_counter <- 0L
best_value   <- Inf

safe_saveRDS <- function(object, file) {
  tmp_file <- paste0(file, ".tmp")
  saveRDS(object, tmp_file)
  if (file.exists(file)) file.remove(file)
  file.rename(tmp_file, file)
}

save_objective_state <- function(beta_type_log, beta_type, incidence_sim,
                                 objective_value, sse_by_type,
                                 is_best, start_id = NA_integer_) {
  state <- list(
    datetime = Sys.time(), eval_counter = eval_counter,
    start_id = start_id, objective_value = objective_value,
    sse_by_type = sse_by_type, is_best = is_best,
    n_rep_obj = N_REP_OBJ, n_cores = N_CORES,
    seed_objective = SEED_OBJ, beta_type_log = beta_type_log,
    beta_type = beta_type, incidence_sim = incidence_sim,
    incidence_obs = incidence_obs, lower_beta = LOWER_BETA,
    upper_beta = UPPER_BETA, checkpoint_dir = checkpoint_dir
  )
  safe_saveRDS(state, checkpoint_last_file)
  if (is_best) safe_saveRDS(state, checkpoint_best_file)
  
  beta_cols <- as.data.frame(as.list(beta_type),     check.names = FALSE)
  inc_cols  <- as.data.frame(as.list(incidence_sim), check.names = FALSE)
  sse_cols  <- as.data.frame(as.list(sse_by_type),   check.names = FALSE)
  names(beta_cols) <- paste0("beta_",    names(beta_type))
  names(inc_cols)  <- paste0("inc_sim_", names(incidence_sim))
  names(sse_cols)  <- paste0("sse_",     names(sse_by_type))
  
  hist_row <- cbind(
    data.frame(eval_counter = eval_counter,
               datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               start_id = start_id,
               objective_value = objective_value,
               best_value = best_value, is_best = is_best,
               n_rep_obj = N_REP_OBJ, stringsAsFactors = FALSE),
    beta_cols, inc_cols, sse_cols
  )
  write.table(hist_row, file = history_file, sep = ";", dec = ".",
              row.names = FALSE,
              col.names  = !file.exists(history_file),
              append     =  file.exists(history_file))
}


# =============================================================================
# 10.  SIMULATION FUNCTION
# =============================================================================
run_simulation_summary <- function(beta_vec, alpha = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  I_loc        <- rbinom(H, beds, prev_init_etab)
  S_loc        <- beds - I_loc
  inc_sum_last <- numeric(H)
  p_rec        <- 1 - exp(-GAMMA_CLEAR)
  
  for (t in seq_len(Tmax)) {
    
    # Within-hospital transmission (vectorised over all H hospitals)
    N     <- S_loc + I_loc
    p_inf <- ifelse(N > 0, 1 - exp(-beta_vec * I_loc / N), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    if (t >= last_year_start) inc_sum_last <- inc_sum_last + new_inf
    I_loc <- pmax(0L, I_loc + new_inf - recov)
    S_loc <- pmax(0L, S_loc - new_inf + recov)
    
    # Patient discharge
    n_exit_S <- rbinom(H, S_loc, p_exit)
    n_exit_I <- rbinom(H, I_loc, p_exit)
    
    # Inter-hospital transfers (alpha reduces infected-patient transfer rate)
    n_tr_S <- rbinom(H, n_exit_S, p_tr)
    n_tr_I <- rbinom(H, n_exit_I, pmin(pmax((1 - alpha) * p_tr, 0), 1))
    
    S_stay <- S_loc - n_exit_S
    I_stay <- I_loc - n_exit_I
    
    # Distribute transfers to destinations (loop only over hospitals with exits)
    S_tr <- integer(H);  I_tr <- integer(H)
    for (h in which((n_tr_S + n_tr_I) > 0)) {
      dp <- dest_probs[[h]]
      if (is.null(dp) || length(dp) == 0) next
      dests <- as.integer(names(dp))
      if (n_tr_S[h] > 0) S_tr[dests] <- S_tr[dests] + rmultinom(1, n_tr_S[h], dp)[,1]
      if (n_tr_I[h] > 0) I_tr[dests] <- I_tr[dests] + rmultinom(1, n_tr_I[h], dp)[,1]
    }
    
    # Community admissions fill empty beds
    occ <- S_stay + I_stay + S_tr + I_tr
    A   <- pmax(0L, beds - occ)
    A_I <- rbinom(H, A, pi_vec)
    S_loc <- S_stay + S_tr + (A - A_I)
    I_loc <- I_stay + I_tr + A_I
  }
  
  # Per-hospital incidence → type-level mean (per 1,000 bed-days)
  inc_etab <- 1000 * inc_sum_last / (beds * last_year_len) #Incidence per 1000 bed-days already included 
  tapply(inc_etab, type_etab_calib, mean, na.rm = TRUE)[names(incidence_obs)]
}


# =============================================================================
# 11.  PARALLEL CLUSTER
# =============================================================================
# PSOCK: workers are separate R processes; all objects must be exported.
#        Required on Windows; works on all platforms.
# FORK:  workers inherit the parent's memory via copy-on-write.
#        Linux/macOS only; much faster for large objects (no serialisation).
#        Do NOT use on Windows — it will crash.
#
# WHY THE DEFENSIVE CLEANUP BELOW MATTERS:
# on.exit() only registers a cleanup action for when the CURRENT FUNCTION
# returns. This file is sourced as a plain script, not run inside a function,
# so on.exit() does not reliably fire when the script finishes. If you re-run
# (re-source) this script more than once in the same R session without
# restarting R, each run's makeCluster() opens N_CORES new socket connections
# that are never closed — eventually exceeding R's hard limit of ~128 open
# connections, which surfaces as:
#   "Error in summary.connection(connection) : invalid connection"
# The two lines below close any cluster left over from a previous run in this
# session BEFORE opening a new one. Cluster shutdown is now also handled
# explicitly via tryCatch(..., finally = ...) around Sections 15–17 below
# (the only sections that actually use the cluster), which works correctly
# regardless of function/script context.
if (exists("cl", envir = .GlobalEnv) &&
    inherits(get("cl", envir = .GlobalEnv), "cluster")) {
  message("Found a cluster left over from a previous run — stopping it first.")
  try(stopCluster(get("cl", envir = .GlobalEnv)), silent = TRUE)
}

message("Starting parallel cluster: type=", PARALLEL_TYPE,
        "  N_CORES=", N_CORES)

cl <- makeCluster(N_CORES, type = PARALLEL_TYPE)

if (PARALLEL_TYPE == "PSOCK") {
  # PSOCK workers start with an empty environment; export everything needed
  clusterExport(cl, varlist = c(
    "run_simulation_summary", "GAMMA_CLEAR",
    "beds", "H", "Tmax", "p_exit", "p_tr", "dest_probs",
    "prev_init_etab", "pi_vec", "type_etab_calib",
    "incidence_obs", "last_year_start", "last_year_len"
  ))
}
# FORK workers inherit everything automatically — no clusterExport needed.

rep_chunks       <- split(seq_len(N_REP_OBJ),
                          rep(seq_len(N_CORES), length.out = N_REP_OBJ))
rep_chunks_valid <- split(seq_len(N_REP_VALID),
                          rep(seq_len(N_CORES), length.out = N_REP_VALID))


# =============================================================================
# 12.  OBJECTIVE FUNCTION
# =============================================================================
current_start_id <- NA_integer_

objective_fn <- function(beta_type_log) {
  eval_counter <<- eval_counter + 1L
  
  beta_type        <- exp(beta_type_log)
  names(beta_type) <- names(incidence_obs)
  beta_vec         <- beta_type[as.character(type_etab_calib)]
  
  # Fallback for facility types with no SPARES-derived target (e.g. "Unknown"
  # hospitals whose finess_geo wasn't found in facility_level_final.RDS — see
  # the orphan_types diagnostic in Section 6). These hospitals still need a
  # beta to participate in transmission dynamics during simulation, even
  # though they are not part of the calibration target themselves.
  if (anyNA(beta_vec)) {
    fallback_beta <- mean(beta_type, na.rm = TRUE)
    beta_vec[is.na(beta_vec)] <- fallback_beta
  }
  
  results <- parLapply(
    cl, X = rep_chunks,
    fun = function(rs, beta_vec, seed_obj) {
      do.call(rbind, lapply(rs, function(r)
        run_simulation_summary(beta_vec = beta_vec, alpha = 0,
                               seed = seed_obj + r)))
    },
    beta_vec = beta_vec, seed_obj = SEED_OBJ
  )
  
  inc_mat       <- do.call(rbind, results)
  incidence_sim <- colMeans(inc_mat, na.rm = TRUE)[names(incidence_obs)]
  sse_by_type   <- (incidence_sim - incidence_obs)^2
  sse_by_type   <- sse_by_type[names(incidence_obs)]
  objective_value <- sum(sse_by_type, na.rm = TRUE)
  
  is_best <- objective_value < best_value
  if (is_best) best_value <<- objective_value
  
  save_objective_state(beta_type_log, beta_type, incidence_sim,
                       objective_value, sse_by_type, is_best,
                       start_id = current_start_id)
  
  message(sprintf("  [%4d] start=%-8s SSE=%.5f  best=%.5f  %s",
                  eval_counter, as.character(current_start_id),
                  objective_value, best_value,
                  if (is_best) "<-- BEST" else ""))
  objective_value
}


# =============================================================================
# 13.  BOUNDED OBJECTIVE
# =============================================================================
lower_log <- log(LOWER_BETA)
upper_log <- log(UPPER_BETA)

objective_bounded <- function(beta_type_log) {
  if (any(!is.finite(beta_type_log))) return(1e12)
  viol <- sum(pmax(beta_type_log - upper_log, 0)^2 +
                pmax(lower_log - beta_type_log, 0)^2)
  if (viol > 0) return(1e9 + 1e9 * viol)
  objective_fn(beta_type_log)
}


# =============================================================================
# 14.  WARM START AND STARTING POINTS
# =============================================================================
if (file.exists(V3_RESULT_FILE)) {
  v3_result   <- readRDS(V3_RESULT_FILE)
  beta_warmup <- v3_result$beta_by_type %>%
    filter(!is.na(best_beta)) %>%
    with(setNames(best_beta, facility_type))
  beta_warmup <- beta_warmup[names(incidence_obs)]
  beta_warmup[is.na(beta_warmup)] <- 0.01
  message("Warm start loaded from v3 grid calibration")
} else {
  beta_warmup <- rep(sqrt(LOWER_BETA * UPPER_BETA), length(incidence_obs))
  names(beta_warmup) <- names(incidence_obs)
  message("No v3 result found — using mid-range warm start")
}
beta_warmup <- pmin(pmax(beta_warmup, LOWER_BETA), UPPER_BETA)

starts <- list()
starts[[1]] <- log(beta_warmup)
starts[[2]] <- log(pmin(beta_warmup * 1.5, UPPER_BETA))
starts[[3]] <- log(pmax(beta_warmup * 0.67, LOWER_BETA))

set.seed(SEED_STARTS)
for (s in seq_len(N_RANDOM_STARTS)) {
  br <- beta_warmup * exp(runif(length(beta_warmup),
                                min = log(0.25), max = log(4)))
  br <- pmin(pmax(br, LOWER_BETA), UPPER_BETA)
  starts[[length(starts) + 1]] <- log(br[names(incidence_obs)])
}
starts <- lapply(starts, function(x) pmin(pmax(x[names(incidence_obs)],
                                               lower_log), upper_log))
names(starts) <- paste0("start_", seq_along(starts))
safe_saveRDS(list(datetime = Sys.time(), starts_log = starts,
                  starts_beta = lapply(starts, exp),
                  beta_warmup = beta_warmup, incidence_obs = incidence_obs),
             starts_file)
message("Starting points (", length(starts), "): ",
        paste(names(starts), collapse = ", "))


# =============================================================================
# 15-17.  NELDER-MEAD OPTIMISATION + BEST BETA RECOVERY + FINAL VALIDATION
# -----------------------------------------------------------------------------
# Wrapped in tryCatch(..., finally = ...) so the cluster is ALWAYS stopped
# when this block finishes — whether it completes normally or an error occurs
# partway through (e.g. a worker crash, an out-of-memory kill, Ctrl+C).
# Unlike on.exit(), tryCatch's `finally` fires correctly even when this code
# is run via source() rather than inside a function, which is what this
# script's lifecycle actually is. All variables assigned inside this block
# (fits, beta_type_opt, validation_summary, etc.) remain available afterward
# in the normal global environment — tryCatch does not create a new scope.
# =============================================================================
tryCatch({
  
  # ===========================================================================
  # 15.  NELDER-MEAD OPTIMISATION (multi-start)
  # ===========================================================================
  fits <- vector("list", length(starts))
  names(fits) <- names(starts)
  
  for (i in seq_along(starts)) {
    current_start_id <<- i
    message("\n--- Start ", i, "/", length(starts), " (",
            names(starts)[i], ")  beta=",
            paste(round(exp(starts[[i]]), 5), collapse = ", "), " ---")
    
    fits[[i]] <- tryCatch(
      optim(par = starts[[i]], fn = objective_bounded, method = "Nelder-Mead",
            control = list(maxit = MAXIT_NM, trace = 1, REPORT = 1,
                           reltol = 1e-4,
                           parscale = rep(1, length(starts[[i]])))),
      error = function(e) {
        message("  optim() error: ", conditionMessage(e))
        list(par = starts[[i]], value = Inf,
             convergence = NA_integer_, message = conditionMessage(e))
      }
    )
    
    safe_saveRDS(list(datetime = Sys.time(), fits = fits,
                      eval_counter = eval_counter, best_value = best_value),
                 fits_file)
    message("  → SSE = ", round(fits[[i]]$value, 6),
            "  convergence = ", fits[[i]]$convergence)
  }
  current_start_id <<- NA_integer_
  
  
  # ===========================================================================
  # 16.  BEST BETA RECOVERY
  # ===========================================================================
  fit_values  <- sapply(fits, function(x) x$value)
  best_fit_id <- which.min(fit_values)
  message("\nBest start: ", names(fits)[best_fit_id],
          "  SSE = ", round(fit_values[best_fit_id], 6))
  
  if (file.exists(checkpoint_best_file)) {
    best_ckpt      <- readRDS(checkpoint_best_file)
    beta_type_opt  <- best_ckpt$beta_type
    beta_type_log_opt <- best_ckpt$beta_type_log
    # Use checkpoint only when it is strictly better than the final optim result
    if (best_ckpt$objective_value > fit_values[best_fit_id]) {
      beta_type_opt     <- exp(fits[[best_fit_id]]$par)
      names(beta_type_opt) <- names(incidence_obs)
      beta_type_log_opt <- log(beta_type_opt)
      message("Using optim() result (better than checkpoint)")
    } else {
      message("Using checkpoint result (SSE = ",
              round(best_ckpt$objective_value, 6), ")")
    }
  } else {
    beta_type_opt     <- exp(fits[[best_fit_id]]$par)
    names(beta_type_opt) <- names(incidence_obs)
    beta_type_log_opt  <- log(beta_type_opt)
  }
  
  beta_type_opt     <- pmin(pmax(beta_type_opt[names(incidence_obs)],
                                 LOWER_BETA), UPPER_BETA)
  beta_type_log_opt <- log(beta_type_opt)
  beta_opt          <- beta_type_opt[as.character(type_etab_calib)]
  
  # Same fallback as objective_fn (Section 12): any hospital whose
  # facility_type has no SPARES-derived target (see orphan_types diagnostic
  # in Section 6) gets the mean of the calibrated type-level betas, so the
  # validation run can simulate every hospital instead of crashing.
  if (anyNA(beta_opt)) {
    na_types <- unique(type_etab_calib[is.na(beta_opt)])
    fallback_beta <- mean(beta_type_opt, na.rm = TRUE)
    message("NOTE: assigning fallback beta (", round(fallback_beta, 6),
            ") to uncalibrated facility type(s): ",
            paste(na_types, collapse = ", "))
    beta_opt[is.na(beta_opt)] <- fallback_beta
  }
  
  message("\nOptimal beta per type:")
  print(round(beta_type_opt, 6))
  
  
  # ===========================================================================
  # 17.  FINAL VALIDATION
  # ===========================================================================
  message("\nRunning final validation (", N_REP_VALID, " replicates) ...")
  
  res_final <- parLapply(
    cl, X = rep_chunks_valid,
    fun = function(rs, beta_opt, seed_val) {
      do.call(rbind, lapply(rs, function(r)
        run_simulation_summary(beta_vec = beta_opt, alpha = 0,
                               seed = seed_val + r)))
    },
    beta_opt = beta_opt, seed_val = SEED_VALID
  )
  
  inc_final_mat <- do.call(rbind, res_final)
  inc_final     <- colMeans(inc_final_mat, na.rm = TRUE)[names(incidence_obs)]
  inc_final_sd  <- apply(inc_final_mat, 2, sd, na.rm = TRUE)[names(incidence_obs)]
  inc_final_se  <- inc_final_sd / sqrt(N_REP_VALID)
  diff_final    <- (inc_final - incidence_obs)[names(incidence_obs)]
  sse_by_type   <- diff_final^2
  sse_final     <- sum(sse_by_type, na.rm = TRUE)
  
}, finally = {
  # ALWAYS runs — whether the block above succeeded or hit an error.
  message("\nStopping parallel cluster ...")
  try(stopCluster(cl), silent = TRUE)
  message("Cluster stopped.")
})


validation_summary <- data.frame(
  type               = names(incidence_obs),
  beta               = as.numeric(beta_type_opt[names(incidence_obs)]),
  incidence_obs      = as.numeric(incidence_obs),
  incidence_sim_mean = as.numeric(inc_final),
  incidence_sim_sd   = as.numeric(inc_final_sd),
  incidence_sim_se   = as.numeric(inc_final_se),
  diff               = as.numeric(diff_final),
  sse                = as.numeric(sse_by_type),
  stringsAsFactors   = FALSE
)


# =============================================================================
# 18.  VALIDATION SCATTER PLOT
# =============================================================================
p_valid <- ggplot(validation_summary,
                  aes(x = incidence_obs, y = incidence_sim_mean,
                      colour = type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey50", linewidth = 0.8) +
  geom_errorbar(aes(ymin = incidence_sim_mean - incidence_sim_sd,
                    ymax = incidence_sim_mean + incidence_sim_sd),
                width = 0, linewidth = 0.7, alpha = 0.6) +
  geom_point(size = 4) +
  geom_text(aes(label = type),
            nudge_y = 0.04 * max(validation_summary$incidence_obs, na.rm = TRUE),
            size = BASE_TEXT * 0.25, show.legend = FALSE) +
  coord_fixed() +
  scale_colour_brewer(palette = "Set1", guide = "none") +
  labs(
    title    = "Calibration: simulated vs. observed ESBL incidence",
    subtitle = paste0("N = ", N_REP_VALID, " validation reps  \u00B7  ",
                      "\u00B11 SD  \u00B7  SSE = ", round(sse_final, 4)),
    x = "Observed (SPARES, per 1,000 bed-days)",
    y = "Simulated (optimal \u03B2, per 1,000 bed-days)"
  ) +
  theme_bw(base_size = BASE_TEXT) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())


# =============================================================================
# 19.  SAVE OUTPUTS
# =============================================================================
final_object <- list(
  datetime = Sys.time(), run_id = run_id,
  beta_type_opt = beta_type_opt, beta_type_log_opt = beta_type_log_opt,
  beta_opt = beta_opt, incidence_obs = incidence_obs,
  incidence_final = inc_final, incidence_final_sd = inc_final_sd,
  incidence_final_se = inc_final_se, diff_final = diff_final,
  sse_final_by_type = sse_by_type, sse_final = sse_final,
  validation_summary = validation_summary, fits = fits,
  fit_values = fit_values, best_fit_id = best_fit_id,
  n_rep_obj = N_REP_OBJ, n_rep_valid = N_REP_VALID, n_cores = N_CORES,
  local_test = LOCAL_TEST, parallel_type = PARALLEL_TYPE,
  checkpoint_dir = checkpoint_dir
)

safe_saveRDS(final_object, final_file)
safe_saveRDS(final_object, final_recovered_beta_file)
write.csv(validation_summary, file = validation_summary_file, row.names = FALSE)

out_plot <- file.path(checkpoint_dir, "validation_scatter.png")
ggsave(out_plot, plot = p_valid, width = 8, height = 8, dpi = 150)

message("\n=== SUMMARY ===")
message("SSE (validation) = ", round(sse_final, 6))
message("Outputs: ", checkpoint_dir)
print(p_valid)


#Check if theres steady - state indicator
# Ile-de-France

#Run it exactly like Elise's code
