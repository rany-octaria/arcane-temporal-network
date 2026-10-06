# =============================================================================
# arcane_beta_calibration_optim.R
# -----------------------------------------------------------------------------
# Direct calibration of within-hospital transmission rate (beta) via
# Nelder-Mead optimisation over a stochastic SIS network simulation.
#
# APPROACH (adapted from 01_Beta_Calibration_OPTIM_SPARES.R) from Elise
#   For each candidate beta vector (one beta per facility TYPE), run N_REP_OBJ
#   independent replicates of the SIS simulation in parallel, average the
#   type-level incidence across replicates, and compute the sum of squared
#   errors (SSE) vs. the SPARES observed rates.  Nelder-Mead minimises this
#   SSE over multiple starting points (multi-start strategy).
#
# SIMULATION MODEL:
#   The simulation models explicit patient turnover each day:
#     (1) Within-hospital SIS transmission
#     (2) Discharge: each patient leaves with probability p_exit[h] = 1/LOS[h]
#     (3) Transfer: fraction p_tr[h] of discharges go to another hospital,
#         distributed according to the transfer probability matrix
#     (4) Admission: empty beds filled from the community at rate ADMISSION_PREV
#   This is richer than the direct-transfer model in v3 because it explicitly
#   separates discharge dynamics from the transfer network.
#
# UNIT CONVENTION:
#   All incidence values remain in per 1,000 bed-days throughout.
#   SSE is computed on that scale; optimizer values are therefore in
#   (per 1,000 bed-days)² units.
#
# DATA SOURCES (same RDS files as v3):
#   coords_beds_active.RDS  — bed counts per finess_geo
#   weekly.RDS              — daily-average transfer network
#   node_attributes_full.RDS — facility metadata + SPARES incidence + LOS
#   facility_targets        — built from node_attributes_full (Section 4)
# =============================================================================


# =============================================================================
# 0.  KNOBS
# =============================================================================

# ── Optimisation run settings ─────────────────────────────────────────────────
N_CORES       <- 8      # parallel workers for objective evaluation [prod: 20-30]
N_REP_OBJ     <- 5     # replicates per objective evaluation  [prod: 100]
N_REP_VALID   <- 10  # replicates for final validation run   [prod: 300]

# ── RNG seeds (fixed for reproducibility) ────────────────────────────────────
SEED_OBJ      <- 1000   # base seed for objective-function replicates
SEED_VALID    <- 50000  # base seed for validation replicates
SEED_STARTS   <- 123    # seed for random starting points

# ── Beta search space ─────────────────────────────────────────────────────────
# Optimisation is done on log(beta) to enforce positivity and provide a
# smoother landscape.  Bounds are enforced via a penalty in objective_bounded().
LOWER_BETA    <- 1e-3   # minimum daily transmission rate
UPPER_BETA    <- 0.3   # maximum daily transmission rate

##RANY CHANGED ABOVE TO A NARROWER VALUE

# ── Nelder-Mead settings ─────────────────────────────────────────────────────
N_RANDOM_STARTS <- 6    # random starting points in addition to structured ones
MAXIT_NM        <- 150  # max Nelder-Mead iterations per start  [prod: 200-500]

# ── Simulation design ─────────────────────────────────────────────────────────
N_YEARS         <- 2    # total simulation length (730 days)
# Incidence is measured in the LAST 365 days of the run; the first year serves
# as a burn-in period so the transient from the cold start does not dominate.
INIT_PREV       <- 0.02 # starting fraction of beds infected (small endemic seed
# to shorten burn-in; same as v3 grid calibration default)

ADMISSION_PREV  <- 0.05 # fraction of newly admitted patients already ESBL+ ##RANY - elise said this may be changed later
ALPHA           <- 0    # transfer-rate reduction for infected patients (0 = none;
# set to >0 to model contact-isolation reducing transfers)

GAMMA_CLEAR     <- 1 / 387  # daily clearance probability (mean colonisation 387d)

# ── Default LOS by facility type (days) ──────────────────────────────────────
# Used as fallback when per-facility LOS is absent from node_attributes_full.
# Values are approximate French national averages from SAE / PMSI.
DEFAULT_LOS <- c(
  "MCO"     =  5.5,   # short-stay acute care
  "SSR"     = 32.0,   # rehabilitation / follow-up care
  "MCO/SSR" =  7.0,   # mixed short + follow-up ward
  "PSY"     = 60.0,   # psychiatry (long admissions)
  "HAD"     = 22.0,   # hospitalisation à domicile
  "CLCC"    =  6.0,   # cancer centres (similar to MCO)
  "Other"   =  8.0,
  "Unknown" =  7.0
)
MAX_P_TR <- 0.60        # cap on the daily transfer-probability per hospital
# (prevents p_tr > 1 from LOS/transfer data mismatches)

# ── Paths ─────────────────────────────────────────────────────────────────────
DATA_DIR    <- file.path(getwd(), "cluster_jobs", "data")
OUT_DIR     <- file.path(getwd(), "Calibration_Jobs", "optim")

# Path to an existing v3 grid-calibration result: if it exists, beta_by_type
# from that run is used as the primary warm start for Nelder-Mead.
V3_RESULT_FILE <- file.path(getwd(), "beta_calibration_incidence_v3.RDS")

# ── Plot aesthetics ───────────────────────────────────────────────────────────
BASE_TEXT <- 14


# =============================================================================
# 1.  LIBRARIES
# =============================================================================
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(janitor)
library(ggplot2)
library(parallel)    # for makeCluster / parLapply


# =============================================================================
# 2.  DATA LOADING  (identical sources to arcane_beta_calibration_incidence_v3.R)
# =============================================================================

# coords_beds_active: bed counts per finess_geo (SAE CAPACT / FINESS)
coords_beds_active <- readRDS(file.path(DATA_DIR, "coords_beds_active.RDS"))

# weekly_transfers: daily-average patient-transfer counts between finess_geo pairs.
# weight = 7-day rolling sum divided by 7 and rounded to integer daily averages;
# pmax(1L,...) keeps every reported edge alive after rounding.
weekly_transfers <- readRDS(file.path(DATA_DIR, "weekly.RDS")) %>%
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

# node_attributes_full: per-finess_geo metadata, SPARES incidence, and LOS.
# incidence_esbl_all is in native SPARES units — per 1,000 patient-days — and
# is NOT rescaled here (same convention as v3).
node_attributes_full <- readRDS(
  here("Datasets", "Output Data", "node_attributes_full.RDS")
) %>%
  mutate(finess_geo = as.character(finess_geo))


# =============================================================================
# 3.  FOUR-TIER INCIDENCE TARGETS  (same logic as v3, Section 4)
# -----------------------------------------------------------------------------
# (a) facility's own SPARES rate
# (b) mean for same facility_type × region
# (c) mean for same region across all types  — covers CLCC and absent types
# (d) global mean — last resort
# =============================================================================
global_inc <- mean(node_attributes_full$incidence_esbl_all, na.rm = TRUE)

type_region_inc <- node_attributes_full %>%
  group_by(facility_type, region) %>%
  summarise(type_region_mean_inc = mean(incidence_esbl_all, na.rm = TRUE),
            .groups = "drop")

region_inc <- node_attributes_full %>%
  group_by(region) %>%
  summarise(region_mean_inc = mean(incidence_esbl_all, na.rm = TRUE),
            .groups = "drop")

facility_targets <- node_attributes_full %>%
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
# 4.  HOSPITAL UNIVERSE  (network nodes + beds + facility type)
# =============================================================================

# All unique finess_geo that appear in the transfer network
hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finessGeo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finessGeo_target))
) %>% distinct()

# Attach bed counts; impute missing with network mean
mean_beds <- round(mean(
  readr::parse_number(as.character(
    coords_beds_active %>% clean_names() %>% pull(no_beds)
  )), na.rm = TRUE
))

hospitals <- hospitals %>%
  left_join(
    coords_beds_active %>% clean_names() %>%
      transmute(finess_geo = as.character(finess_geo),
                no_beds    = readr::parse_number(as.character(no_beds))),
    by = "finess_geo"
  ) %>%
  mutate(no_beds = as.integer(if_else(is.na(no_beds), mean_beds, no_beds))) %>%
  left_join(facility_targets, by = "finess_geo") %>%
  mutate(
    target_incidence = if_else(is.na(target_incidence), global_inc, target_incidence),
    facility_type    = if_else(is.na(facility_type),  "Unknown",    facility_type),
    incidence_source = if_else(is.na(incidence_source), "global_mean", incidence_source)
  )

H    <- nrow(hospitals)               # total number of simulation nodes
beds <- hospitals$no_beds             # integer vector (length H), bed capacity

message("Hospital network: H = ", H,
        "  |  total beds = ", format(sum(beds), big.mark = ","))


# =============================================================================
# 5.  SIMULATION INPUTS — p_exit, p_tr, dest_probs, pi_vec
# -----------------------------------------------------------------------------
# The simulation models explicit daily patient turnover:
#   p_exit[h]    = daily probability any patient leaves hospital h = 1/LOS[h]
#   p_tr[h]      = fraction of leavers that are inter-hospital transfers
#                  (as opposed to going home); capped at MAX_P_TR
#   dest_probs   = list of length H; dest_probs[[h]] is a named numeric vector
#                  mapping destination hospital INDICES to transfer probabilities
#   pi_vec[h]    = ESBL+ fraction among newly admitted community patients
# =============================================================================

# ── 5a. Length-of-stay → daily discharge probability ─────────────────────────
# Prefer a per-facility LOS column in node_attributes_full (look for 'los_avg'
# or 'reg_type_los_avg').  If absent, fall back to type-level defaults above.
los_col <- intersect(c("los_avg", "reg_type_los_avg", "avg_los"),
                     names(node_attributes_full))

colnames(node_attributes_full)
if (length(los_col) > 0) {
  # Use the first matching column
  message("Using LOS column '", los_col[1], "' from node_attributes_full")
  los_lookup <- node_attributes_full %>%
    transmute(finess_geo,
              los = as.numeric(.data[[los_col[1]]]))
} else {
  message("No LOS column found in node_attributes_full; using DEFAULT_LOS by type")
  los_lookup <- tibble(finess_geo = character(), los = numeric())
}

hospitals <- hospitals %>%
  left_join(los_lookup, by = "finess_geo") %>%
  mutate(
    # Fill missing per-facility LOS with type-level defaults
    los    = coalesce(los, DEFAULT_LOS[facility_type], 7.0),
    los    = pmax(los, 1.0),          # LOS must be at least 1 day
    p_exit = 1 / los                  # daily discharge probability
  )

p_exit <- hospitals$p_exit            # numeric vector, length H


# ── 5b. Transfer probability p_tr[h] from the transfer network ───────────────
# For each hospital h, the daily expected transfer outflow is the sum of weights
# on all outgoing edges.  p_tr[h] = outflow / (p_exit[h] * beds[h]) is the
# fraction of daily discharges that leave via inter-hospital transfer.
#
# Capping at MAX_P_TR guards against data artefacts where outflow > discharges
# (can happen when transfer counts are inflated or LOS is underestimated).

# Map finess_geo → integer row index in 'hospitals' for fast matrix lookup
hosp_idx <- setNames(seq_len(H), hospitals$finess_geo)

# Build a named vector of daily outgoing transfer weight per hospital
transfer_out <- weekly_transfers %>%
  transmute(origin = as.character(finessGeo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out = sum(weight, na.rm = TRUE), .groups = "drop")

hospitals <- hospitals %>%
  left_join(transfer_out, by = c("finess_geo" = "origin")) %>%
  mutate(
    total_out = replace_na(total_out, 0),
    # Expected daily discharges (incl. transfers) = p_exit * beds
    p_tr = pmin(total_out / pmax(p_exit * beds, 1), MAX_P_TR)
  )

p_tr <- hospitals$p_tr                # numeric vector, length H


# ── 5c. Destination probability list (sparse) ─────────────────────────────────
# dest_probs[[h]] is a named numeric vector:
#   names  = destination hospital indices (as character strings for R list access)
#   values = transfer probability to that destination (sums to 1 if non-empty)
# Using a list avoids exporting a full H×H dense matrix (≥ 8·H² bytes) to
# every parallel worker; only non-zero entries are stored.
message("Building sparse transfer destination lists ...")

# Pre-group transfers by origin for fast per-hospital lookup
transfers_by_origin <- weekly_transfers %>%
  transmute(
    origin   = as.character(finessGeo_origin),
    dest_idx = hosp_idx[as.character(finessGeo_target)],
    weight
  ) %>%
  filter(!is.na(dest_idx)) %>%  # drop targets not in hospital list
  group_by(origin) %>%
  group_split()

origin_names <- sapply(transfers_by_origin, function(x) x$origin[1])

dest_probs <- vector("list", H)
for (k in seq_along(transfers_by_origin)) {
  h <- hosp_idx[origin_names[k]]
  if (is.na(h)) next
  edges   <- transfers_by_origin[[k]]
  wts     <- edges$weight
  wts_sum <- sum(wts)
  if (wts_sum <= 0) next
  # Normalised probabilities; names are destination indices as character strings
  dest_probs[[h]] <- setNames(wts / wts_sum, as.character(edges$dest_idx))
}

message("  Hospitals with outgoing transfers: ",
        sum(!sapply(dest_probs, is.null)))


# ── 5d. Community admission prevalence (constant across hospitals) ─────────────
pi_vec <- rep(ADMISSION_PREV, H)      # numeric vector, length H


# =============================================================================
# 6.  OBSERVED INCIDENCE TARGETS
# -----------------------------------------------------------------------------
# incidence_obs: named numeric vector, one value per FACILITY TYPE.
# Names must match those produced by tapply(inc_etab, type_etab_calib, mean).
# Only types with a SPARES-derived target (tiers a/b/c) are included; the
# optimiser has no meaningful signal for global-fill types.
# =============================================================================

# Types with real SPARES signal
spares_types <- facility_targets %>%
  filter(incidence_source != "global_mean") %>%
  distinct(facility_type)

# Type-level mean target (mean over all hospitals of that type)
target_type <- hospitals %>%
  group_by(facility_type) %>%
  summarise(target_incidence = mean(target_incidence), .groups = "drop")

incidence_obs <- target_type %>%
  semi_join(spares_types, by = "facility_type") %>%
  with(setNames(target_incidence, facility_type))

# type_etab_calib: character vector (length H) mapping each hospital to its type
type_etab_calib <- hospitals$facility_type

message("Calibration types (", length(incidence_obs), "): ",
        paste(names(incidence_obs), collapse = ", "))
message("Observed incidence (per 1,000 bed-days):")
print(round(incidence_obs, 3))


# =============================================================================
# 7.  SIMULATION DIMENSIONS AND TIMING
# =============================================================================

Tmax            <- as.integer(N_YEARS * 365L)    # 730 days
last_year_start <- Tmax - 364L  # day from which incidence accumulates
last_year_len   <- 365L         # accumulation window length (days)

# Initial prevalence vector: small endemic seed to shorten burn-in
prev_init_etab  <- rep(INIT_PREV, H)


# =============================================================================
# 8.  OUTPUT DIRECTORIES AND FILE PATHS
# =============================================================================
run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")

checkpoint_dir <- file.path(OUT_DIR, paste0("run_optim_", run_id))
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

# Per-run checkpoint files: allow recovery if the job is interrupted
checkpoint_best_file   <- file.path(checkpoint_dir, "checkpoint_best_beta.rds")
checkpoint_last_file   <- file.path(checkpoint_dir, "checkpoint_last_eval.rds")
history_file           <- file.path(checkpoint_dir, "history_objective.csv")
starts_file            <- file.path(checkpoint_dir, "starts_used.rds")
fits_file              <- file.path(checkpoint_dir, "fits_nm.rds")
final_file             <- file.path(checkpoint_dir, "final_validation.rds")
validation_summary_file <- file.path(checkpoint_dir, "validation_summary.csv")

# Canonical "best beta recovered" file (overwritten by every run)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
final_recovered_beta_file <- file.path(OUT_DIR, "recovered_best_beta.rds")


# =============================================================================
# 9.  CHECKPOINT INFRASTRUCTURE
# =============================================================================

# Evaluation counter and best-seen value (modified by <<- inside objective_fn)
eval_counter <- 0L
best_value   <- Inf

# ── safe_saveRDS: atomic write via temp file ──────────────────────────────────
# Writes to a .tmp file first, then renames.  Prevents a partial write from
# corrupting the checkpoint file if the process is interrupted mid-write.
safe_saveRDS <- function(object, file) {
  tmp_file <- paste0(file, ".tmp")
  saveRDS(object, tmp_file)
  if (file.exists(file)) file.remove(file)
  file.rename(tmp_file, file)
}

# ── save_objective_state: persist every evaluation to disk ───────────────────
# Records the current beta vector, simulated incidence, SSE breakdown, and
# metadata.  Appends a row to the history CSV for live monitoring.
# Arguments:
#   beta_type_log  — log-space beta vector passed to the objective
#   beta_type      — exp(beta_type_log), named by facility type
#   incidence_sim  — simulated type-level mean incidence (per 1,000 bed-days)
#   objective_value — total SSE (scalar)
#   sse_by_type    — per-type squared errors (named vector)
#   is_best        — TRUE if this is the best evaluation so far
#   start_id       — integer index of the current Nelder-Mead starting point
save_objective_state <- function(beta_type_log, beta_type, incidence_sim,
                                 objective_value, sse_by_type,
                                 is_best, start_id = NA_integer_) {
  
  state <- list(
    datetime        = Sys.time(),
    eval_counter    = eval_counter,
    start_id        = start_id,
    objective_value = objective_value,
    sse_by_type     = sse_by_type,
    is_best         = is_best,
    n_rep_obj       = N_REP_OBJ,
    n_cores         = N_CORES,
    seed_objective  = SEED_OBJ,
    beta_type_log   = beta_type_log,
    beta_type       = beta_type,
    incidence_sim   = incidence_sim,
    incidence_obs   = incidence_obs,
    lower_beta      = LOWER_BETA,
    upper_beta      = UPPER_BETA,
    checkpoint_dir  = checkpoint_dir
  )
  
  # Always overwrite the "last evaluated" checkpoint
  safe_saveRDS(state, checkpoint_last_file)
  
  # Overwrite the "best seen" checkpoint when improved
  if (is_best) safe_saveRDS(state, checkpoint_best_file)
  
  # Append a row to the history CSV (col headers written only on first row)
  hist_row <- data.frame(
    eval_counter    = eval_counter,
    datetime        = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    start_id        = start_id,
    objective_value = objective_value,
    best_value      = best_value,
    is_best         = is_best,
    n_rep_obj       = N_REP_OBJ,
    stringsAsFactors = FALSE
  )
  
  # Attach per-type beta, simulated incidence, and SSE columns
  beta_cols <- as.data.frame(as.list(beta_type),       check.names = FALSE)
  inc_cols  <- as.data.frame(as.list(incidence_sim),   check.names = FALSE)
  sse_cols  <- as.data.frame(as.list(sse_by_type),     check.names = FALSE)
  names(beta_cols) <- paste0("beta_",    names(beta_type))
  names(inc_cols)  <- paste0("inc_sim_", names(incidence_sim))
  names(sse_cols)  <- paste0("sse_",     names(sse_by_type))
  hist_row <- cbind(hist_row, beta_cols, inc_cols, sse_cols)
  
  write.table(hist_row, file = history_file, sep = ";", dec = ".",
              row.names = FALSE,
              col.names  = !file.exists(history_file),   # header on first write
              append     =  file.exists(history_file))
}


# =============================================================================
# 10.  SIMULATION FUNCTION
# -----------------------------------------------------------------------------
# run_simulation_summary() runs the SIS model for Tmax days and returns the
# mean incidence per facility TYPE over the final 365 days (per 1,000 bed-days).
#
# DAILY DYNAMICS (for each hospital h):
#   Step 1 — Within-hospital transmission:
#     new_inf[h] ~ Binomial(S[h], 1 - exp(-beta[h] * I[h]/N[h]))
#     recov[h]   ~ Binomial(I[h], 1 - exp(-gamma))
#
#   Step 2 — Patient discharge:
#     n_exit_S[h] ~ Binomial(S[h], p_exit[h])
#     n_exit_I[h] ~ Binomial(I[h], p_exit[h])
#
#   Step 3 — Inter-hospital transfers:
#     n_tr_S[h] ~ Binomial(n_exit_S[h], p_tr[h])
#     n_tr_I[h] ~ Binomial(n_exit_I[h], (1-alpha)*p_tr[h])  [alpha=0 → same rate]
#     Destination drawn from dest_probs[[h]] using rmultinom.
#
#   Step 4 — Community admissions:
#     New patients fill empty beds; fraction pi_vec[h] are ESBL+.
#
# INCIDENCE ACCUMULATION:
#   inc_sum_last[h] accumulates new_inf[h] for t >= last_year_start.
#   Final incidence = 1000 * inc_sum_last / (beds * last_year_len)  [per 1,000 bed-days]
#
# RETURN VALUE: named numeric vector of length = length(incidence_obs),
#   giving the type-level mean incidence for the types in incidence_obs.
#
# PARAMETERS (all accessed from the parallel worker's environment):
#   beta_vec  — numeric vector (length H): per-hospital daily transmission rate
#   alpha     — scalar: transfer reduction factor for infected patients (0 = none)
#   seed      — integer or NULL
# =============================================================================
run_simulation_summary <- function(beta_vec, alpha = 0, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # ── Initialise prevalence state ────────────────────────────────────────────
  # I_loc[h]: number of infected beds in hospital h at day t
  # S_loc[h]: number of susceptible beds
  I_loc <- rbinom(H, beds, prev_init_etab)   # endemic seed (INIT_PREV fraction)
  S_loc <- beds - I_loc
  
  # Accumulator for new infections in the last year
  inc_sum_last <- numeric(H)
  
  # Pre-compute daily recovery probability (constant across time and hospitals)
  p_rec <- 1 - exp(-GAMMA_CLEAR)
  
  # ── Day-by-day simulation ─────────────────────────────────────────────────
  for (t in seq_len(Tmax)) {
    
    # ── Step 1: within-hospital SIS transmission (vectorised) ─────────────────
    N     <- S_loc + I_loc                          # current occupancy per hospital
    # Force of infection: probability a susceptible acquires ESBL today.
    # ifelse guards against division by zero for empty hospitals.
    p_inf <- ifelse(N > 0, 1 - exp(-beta_vec * I_loc / N), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    
    # Accumulate incidence for the last-year window
    if (t >= last_year_start) {
      inc_sum_last <- inc_sum_last + new_inf
    }
    
    # Update S and I after transmission and recovery
    I_loc <- pmax(0L, I_loc + new_inf - recov)
    S_loc <- pmax(0L, S_loc - new_inf + recov)
    
    # ── Step 2: patient discharge (vectorised) ─────────────────────────────────
    # Each patient independently leaves with probability p_exit[h] each day.
    # n_exit_S / n_exit_I = number of susceptible / infected patients leaving.
    n_exit_S <- rbinom(H, S_loc, p_exit)
    n_exit_I <- rbinom(H, I_loc, p_exit)
    
    # ── Step 3: inter-hospital transfers ──────────────────────────────────────
    # Of the exiting patients, fraction p_tr[h] transfer (vs. discharge home).
    # Infected patients transfer at rate (1-alpha)*p_tr[h]; with alpha=0 this
    # equals p_tr[h] (no isolation effect on transfer probability).
    n_tr_S <- rbinom(H, n_exit_S, p_tr)
    n_tr_I <- rbinom(H, n_exit_I, pmin(pmax((1 - alpha) * p_tr, 0), 1))
    
    # Remaining after removing all exits (transfers + home discharges)
    S_stay <- S_loc - n_exit_S
    I_stay <- I_loc - n_exit_I
    
    # Distribute transfers to destination hospitals according to dest_probs.
    # We loop only over hospitals that actually have outgoing transfers today
    # (n_tr_S[h] + n_tr_I[h] > 0) to avoid the cost of rmultinom on H hospitals.
    S_tr <- integer(H)
    I_tr <- integer(H)
    
    active_h <- which((n_tr_S + n_tr_I) > 0)
    for (h in active_h) {
      dp <- dest_probs[[h]]
      if (is.null(dp) || length(dp) == 0) next    # no destination data → skip
      
      # Destination indices stored as character names; convert back to integer
      dests <- as.integer(names(dp))
      
      if (n_tr_S[h] > 0) {
        draws <- rmultinom(1, n_tr_S[h], dp)[, 1]
        S_tr[dests] <- S_tr[dests] + draws
      }
      if (n_tr_I[h] > 0) {
        draws <- rmultinom(1, n_tr_I[h], dp)[, 1]
        I_tr[dests] <- I_tr[dests] + draws
      }
    }
    
    # ── Step 4: community admissions fill empty beds ────────────────────────────
    # Occupancy after transfers have arrived and non-transfer exits have left.
    occ <- S_stay + I_stay + S_tr + I_tr
    A   <- pmax(0L, beds - occ)         # empty beds to fill
    A_I <- rbinom(H, A, pi_vec)         # ESBL+ admissions from community
    A_S <- A - A_I                      # susceptible admissions
    
    S_loc <- S_stay + S_tr + A_S
    I_loc <- I_stay + I_tr + A_I
  }
  
  # ── Compute incidence per 1,000 bed-days (last year) ─────────────────────────
  # Full-occupancy denominator: beds * last_year_len (consistent with SPARES)
  inc_etab <- 1000 * inc_sum_last / (beds * last_year_len)
  
  # Aggregate to type level (mean over hospitals of each type) and align to
  # incidence_obs ordering.  Returns only types present in incidence_obs.
  type_means <- tapply(inc_etab, type_etab_calib, mean, na.rm = TRUE)
  type_means[names(incidence_obs)]
}


# =============================================================================
# 11.  PARALLEL CLUSTER
# =============================================================================

# Create cluster: each worker gets its own RNG stream and environment
cl <- makeCluster(N_CORES)
on.exit(stopCluster(cl), add = TRUE)   # ensure cluster closes even on error

# Export all variables that run_simulation_summary() needs from the global env.
# These are NOT passed as arguments (they never change during the optimisation),
# so exporting once is more efficient than serialising them on every objective call.
clusterExport(cl, varlist = c(
  "run_simulation_summary",
  "GAMMA_CLEAR",     # clearance rate constant
  "beds",            # bed capacity vector (length H)
  "H",               # number of hospitals (scalar)
  "Tmax",            # simulation length (scalar)
  "p_exit",          # daily discharge probability per hospital
  "p_tr",            # daily transfer fraction per hospital
  "dest_probs",      # sparse destination probability list
  "prev_init_etab",  # initial infected fraction per hospital
  "pi_vec",          # community ESBL+ admission rate per hospital
  "type_etab_calib", # facility type per hospital (for tapply aggregation)
  "incidence_obs",   # observed type-level incidence (names + values)
  "last_year_start", # day from which incidence accumulates
  "last_year_len"    # number of accumulation days
))

# Distribute replicates evenly across cores.
# rep_chunks[[k]] = integer vector of replicate IDs assigned to worker k.
rep_chunks <- split(seq_len(N_REP_OBJ),
                    rep(seq_len(N_CORES), length.out = N_REP_OBJ))

rep_chunks_valid <- split(seq_len(N_REP_VALID),
                          rep(seq_len(N_CORES), length.out = N_REP_VALID))


# =============================================================================
# 12.  OBJECTIVE FUNCTION
# -----------------------------------------------------------------------------
# objective_fn(beta_type_log): evaluates the SSE between simulated and observed
# facility-type-level incidence, averaged over N_REP_OBJ parallel replicates.
#
# OPTIMISATION PARAMETERISATION:
#   The optimiser works in LOG SPACE: par = log(beta_type), one element per type.
#   Taking exp() maps back to positive betas; this avoids the need to explicitly
#   constrain the search space and provides a smoother landscape because the
#   relevant range spans several orders of magnitude.
#
# PARALLEL EVALUATION:
#   parLapply dispatches chunks of replicates to cluster workers.  Each worker
#   runs its chunk sequentially with different seeds (SEED_OBJ + replicate_id),
#   ensuring reproducibility while exploiting parallelism.
#   Results are combined with rbind; colMeans gives the per-type mean incidence.
#
# SIDE EFFECTS:
#   eval_counter and best_value are modified via <<-.
#   save_objective_state() is called on every evaluation.
# =============================================================================

current_start_id <- NA_integer_   # set to the current start index before each optim()

objective_fn <- function(beta_type_log) {
  
  eval_counter <<- eval_counter + 1L
  
  # Convert log-space parameters back to transmission rates (one per type)
  beta_type        <- exp(beta_type_log)
  names(beta_type) <- names(incidence_obs)
  
  # Expand type-level betas to a per-hospital vector by matching facility type
  beta_vec <- beta_type[as.character(type_etab_calib)]
  
  if (anyNA(beta_vec)) {
    stop("NA in beta_vec: check that type_etab_calib names match incidence_obs names")
  }
  
  # ── Parallel replicates ───────────────────────────────────────────────────
  # Each worker runs its chunk of replicates, returning a matrix whose rows
  # are per-replicate type-level incidence vectors.
  results <- parLapply(
    cl,
    X = rep_chunks,
    fun = function(rs, beta_vec, seed_obj) {
      out <- lapply(rs, function(r)
        run_simulation_summary(beta_vec = beta_vec, alpha = 0,
                               seed = seed_obj + r))
      do.call(rbind, out)
    },
    beta_vec = beta_vec,
    seed_obj = SEED_OBJ
  )
  
  # Combine all workers' results into one matrix; average across replicates
  inc_mat        <- do.call(rbind, results)
  incidence_sim  <- colMeans(inc_mat, na.rm = TRUE)
  incidence_sim  <- incidence_sim[names(incidence_obs)]
  
  # ── Compute SSE (sum of squared errors by facility type) ───────────────────
  # sse_by_type[k] = (simulated_mean[k] - observed[k])²
  # objective_value = Σ sse_by_type  — the scalar the optimiser minimises
  sse_by_type      <- (incidence_sim - incidence_obs)^2
  sse_by_type      <- sse_by_type[names(incidence_obs)]
  objective_value  <- sum(sse_by_type, na.rm = TRUE)
  
  # Track improvement for checkpoint logic
  is_best <- objective_value < best_value
  if (is_best) best_value <<- objective_value
  
  # Persist state and print progress
  save_objective_state(beta_type_log, beta_type, incidence_sim,
                       objective_value, sse_by_type, is_best,
                       start_id = current_start_id)
  
  message(sprintf("  [%4d] start=%s  SSE=%.6f  best=%.6f  %s",
                  eval_counter, current_start_id,
                  objective_value, best_value,
                  if (is_best) "<-- NEW BEST" else ""))
  
  objective_value
}


# =============================================================================
# 13.  BOUNDED OBJECTIVE (Nelder-Mead log-space wrapper)
# -----------------------------------------------------------------------------
# Nelder-Mead in optim() does not natively support box constraints.
# objective_bounded() wraps objective_fn() with a smooth penalty:
#   - Any log(beta) outside [log(LOWER_BETA), log(UPPER_BETA)] incurs a large
#     quadratic penalty, pushing the simplex back inside the feasible region.
#   - Non-finite parameter values return 1e12 (hard guard against Inf/NaN).
#
# The penalty value (1e9 base + 1e9 * sum_of_violations²) is chosen to be
# much larger than any realistic SSE value so the optimiser never prefers an
# out-of-bounds point over any in-bounds point.
# =============================================================================
lower_log <- log(LOWER_BETA)
upper_log <- log(UPPER_BETA)

objective_bounded <- function(beta_type_log) {
  
  # Hard guard: non-finite parameters cannot be exponentiated meaningfully
  if (any(!is.finite(beta_type_log))) return(1e12)
  
  # Out-of-bounds penalty: quadratic in the amount of violation
  below_penalty <- pmax(lower_log - beta_type_log, 0)^2
  above_penalty <- pmax(beta_type_log - upper_log, 0)^2
  total_violation <- sum(below_penalty + above_penalty)
  
  if (total_violation > 0) {
    return(1e9 + 1e9 * total_violation)
  }
  
  # In-bounds: evaluate the actual stochastic objective
  objective_fn(beta_type_log)
}


# =============================================================================
# 14.  WARM START AND STARTING POINTS
# -----------------------------------------------------------------------------
# The primary warm start comes from the v3 grid calibration result (beta_by_type
# from arcane_beta_calibration_incidence_v3.R).  If that file is not found, a
# simple uniform starting point (mid-range beta for each type) is used.
#
# Additional starting points are constructed by:
#   (a) scaling the warm start by ×1.5 (global up-shift)
#   (b) N_RANDOM_STARTS multiplicative random perturbations ×exp(U[log(0.25), log(4)])
# All starts are clamped to [LOWER_BETA, UPPER_BETA] before log-transformation.
# =============================================================================

# Attempt to load v3 grid calibration betas as warm start
if (file.exists(V3_RESULT_FILE)) {
  v3_result   <- readRDS(V3_RESULT_FILE)
  beta_warmup <- v3_result$beta_by_type %>%
    filter(!is.na(best_beta)) %>%
    with(setNames(best_beta, facility_type))
  # Keep only types that appear in incidence_obs
  beta_warmup <- beta_warmup[names(incidence_obs)]
  beta_warmup[is.na(beta_warmup)] <- 0.01  # fallback for any missing types
  message("Loaded warm start from v3 grid calibration: ",
          paste(round(beta_warmup, 5), collapse = ", "))
} else {
  # Fallback: uniform start at the geometric midpoint of the search range
  beta_warmup <- rep(sqrt(LOWER_BETA * UPPER_BETA), length(incidence_obs))
  names(beta_warmup) <- names(incidence_obs)
  message("No v3 result found — using mid-range starting betas: ",
          round(beta_warmup[1], 5))
}

# Clamp warm start to the feasible region
beta_warmup <- pmin(pmax(beta_warmup, LOWER_BETA), UPPER_BETA)

# ── Structured starting points ────────────────────────────────────────────────
starts <- list()

# Start 1: warm start as-is (best available prior estimate)
starts[[1]] <- log(beta_warmup)

# Start 2: all betas scaled up by 50 % (explore higher transmission region)
beta_up <- pmin(beta_warmup * 1.5, UPPER_BETA)
beta_up <- pmax(beta_up, LOWER_BETA)
starts[[2]] <- log(beta_up[names(incidence_obs)])

# Start 3: all betas scaled down by 33 % (explore lower transmission region)
beta_dn <- pmax(beta_warmup * 0.67, LOWER_BETA)
beta_dn <- pmin(beta_dn, UPPER_BETA)
starts[[3]] <- log(beta_dn[names(incidence_obs)])

# ── Random starting points: log-uniform perturbations of the warm start ───────
# Each random start multiplies the warm start by exp(U[log(0.25), log(4)]),
# giving a ×0.25 to ×4 random scaling per type independently.
set.seed(SEED_STARTS)
for (s in seq_len(N_RANDOM_STARTS)) {
  beta_rand <- beta_warmup * exp(
    runif(length(beta_warmup), min = log(0.25), max = log(4))
  )
  beta_rand <- pmin(pmax(beta_rand, LOWER_BETA), UPPER_BETA)
  starts[[length(starts) + 1]] <- log(beta_rand[names(incidence_obs)])
}

# Final alignment: ensure every start vector is named and within log-bounds
starts <- lapply(starts, function(x) {
  x <- x[names(incidence_obs)]
  pmin(pmax(x, lower_log), upper_log)
})
names(starts) <- paste0("start_", seq_along(starts))

# Save start manifest to disk
safe_saveRDS(
  list(datetime       = Sys.time(),
       starts_log     = starts,
       starts_beta    = lapply(starts, exp),
       beta_warmup    = beta_warmup,
       n_random_starts = N_RANDOM_STARTS,
       seed_starts    = SEED_STARTS,
       lower_beta     = LOWER_BETA,
       upper_beta     = UPPER_BETA,
       incidence_obs  = incidence_obs),
  starts_file
)

message("\n=== Starting points (", length(starts), " total) ===")
print(lapply(starts, function(s) round(exp(s), 5)))


# =============================================================================
# 15.  MULTI-START NELDER-MEAD OPTIMISATION
# -----------------------------------------------------------------------------
# For each starting point, optim() with method="Nelder-Mead" minimises
# objective_bounded() in log-beta space.  The bounded wrapper enforces the
# feasible region via a penalty, so the simplex never leaves a reasonable range.
#
# tryCatch wraps each optim() call so a single failed start does not abort the
# entire run — failed starts return value = Inf so they are ignored in selection.
#
# CONTROL PARAMETERS:
#   maxit   = max iterations (trade-off between precision and runtime)
#   trace=1 + REPORT=1: print progress every iteration (visible in cluster logs)
#   reltol  = relative tolerance for convergence detection
#   parscale: all log-beta coordinates are on similar scales; parscale=1 is fine
# =============================================================================
fits <- vector("list", length(starts))
names(fits) <- names(starts)

for (i in seq_along(starts)) {
  
  current_start_id <<- i   # passed to save_objective_state via global
  
  message("\n============================================================")
  message("Optimising start ", names(starts)[i], " (", i, "/", length(starts), ")")
  message("Starting betas: ", paste(round(exp(starts[[i]]), 5), collapse = ", "))
  message("============================================================")
  
  fit_i <- tryCatch(
    optim(
      par     = starts[[i]],
      fn      = objective_bounded,
      method  = "Nelder-Mead",
      control = list(
        maxit    = MAXIT_NM,
        trace    = 1,
        REPORT   = 1,
        reltol   = 1e-4,
        parscale = rep(1, length(starts[[i]]))
      )
    ),
    error = function(e) {
      message("  optim() error for start ", i, ": ", conditionMessage(e))
      list(par = starts[[i]], value = Inf,
           convergence = NA_integer_, message = conditionMessage(e))
    }
  )
  
  fits[[i]] <- fit_i
  
  # Checkpoint the full fits list after every start (allows partial recovery)
  safe_saveRDS(
    list(datetime             = Sys.time(),
         fits                 = fits,
         eval_counter         = eval_counter,
         best_value           = best_value,
         checkpoint_best_file = checkpoint_best_file,
         checkpoint_last_file = checkpoint_last_file,
         beta_warmup          = beta_warmup),
    fits_file
  )
  
  message("  Start ", i, " completed — SSE = ", round(fit_i$value, 6),
          "  convergence code = ", fit_i$convergence)
}

current_start_id <<- NA_integer_


# =============================================================================
# 16.  BEST BETA RECOVERY
# -----------------------------------------------------------------------------
# The best beta is taken from the checkpoint file if it recorded a better value
# than the final optim() result (possible if optim() diverges after the best
# evaluation mid-run).  Fall back to the best optim() fit if the file is absent.
# =============================================================================

# Objective values across all starts
fit_values  <- sapply(fits, function(x) x$value)
best_fit_id <- which.min(fit_values)

message("\n=== Final SSE values by start ===")
print(round(fit_values, 6))
message("Best start: ", names(fits)[best_fit_id],
        "  SSE = ", round(fit_values[[best_fit_id]], 6))

if (file.exists(checkpoint_best_file)) {
  
  # The checkpoint may contain the best beta found at any evaluation, including
  # ones visited mid-run that optim() subsequently moved away from.
  best_checkpoint  <- readRDS(checkpoint_best_file)
  beta_type_opt    <- best_checkpoint$beta_type
  beta_type_log_opt <- best_checkpoint$beta_type_log
  
  message("\nBest checkpoint SSE  = ", round(best_checkpoint$objective_value, 6))
  message("Best optim() SSE     = ", round(fit_values[best_fit_id], 6))
  
  # Use checkpoint only if it is strictly better
  if (best_checkpoint$objective_value > fit_values[best_fit_id]) {
    message("  → Using optim() result (better than checkpoint)")
    beta_type_opt    <- exp(fits[[best_fit_id]]$par)
    names(beta_type_opt) <- names(incidence_obs)
    beta_type_log_opt <- log(beta_type_opt)
  } else {
    message("  → Using checkpoint result")
  }
  
} else {
  beta_type_opt     <- exp(fits[[best_fit_id]]$par)
  names(beta_type_opt) <- names(incidence_obs)
  beta_type_log_opt  <- log(beta_type_opt)
}

# Ensure ordering matches incidence_obs and clamp to feasible range
beta_type_opt     <- pmin(pmax(beta_type_opt[names(incidence_obs)], LOWER_BETA), UPPER_BETA)
beta_type_log_opt <- log(beta_type_opt)

# Expand to per-hospital vector for the validation run
beta_opt <- beta_type_opt[as.character(type_etab_calib)]
if (anyNA(beta_opt)) {
  stop("NA in beta_opt: mismatch between type_etab_calib and names(beta_type_opt)")
}

message("\n=== Optimal beta per facility type ===")
print(round(beta_type_opt, 6))


# =============================================================================
# 17.  FINAL VALIDATION
# -----------------------------------------------------------------------------
# Run N_REP_VALID replicates with the optimal beta to obtain a stable estimate
# of the achieved incidence with uncertainty quantification (SD and SE).
# Uses a separate seed (SEED_VALID) so the validation is independent of the
# objective-function replicates used during optimisation.
# =============================================================================
message("\nRunning final validation (N_REP_VALID = ", N_REP_VALID, ") ...")

res_final <- parLapply(
  cl,
  X  = rep_chunks_valid,
  fun = function(rs, beta_opt, seed_val) {
    out <- lapply(rs, function(r)
      run_simulation_summary(beta_vec = beta_opt, alpha = 0,
                             seed = seed_val + r))
    do.call(rbind, out)
  },
  beta_opt = beta_opt,
  seed_val = SEED_VALID
)

inc_final_mat <- do.call(rbind, res_final)

# Per-type summary statistics across validation replicates
inc_final    <- colMeans(inc_final_mat, na.rm = TRUE)[names(incidence_obs)]
inc_final_sd <- apply(inc_final_mat, 2, sd, na.rm = TRUE)[names(incidence_obs)]
inc_final_se <- inc_final_sd / sqrt(N_REP_VALID)

# Residuals and per-type SSE at validation
diff_final        <- (inc_final - incidence_obs)[names(incidence_obs)]
sse_final_by_type <- diff_final^2
sse_final         <- sum(sse_final_by_type, na.rm = TRUE)

# Validation summary table (one row per facility type)
validation_summary <- data.frame(
  type               = names(incidence_obs),
  beta               = as.numeric(beta_type_opt[names(incidence_obs)]),
  incidence_obs      = as.numeric(incidence_obs),
  incidence_sim_mean = as.numeric(inc_final),
  incidence_sim_sd   = as.numeric(inc_final_sd),
  incidence_sim_se   = as.numeric(inc_final_se),
  diff               = as.numeric(diff_final),
  sse                = as.numeric(sse_final_by_type),
  stringsAsFactors   = FALSE
)

message("\n=== Validation summary ===")
print(validation_summary)


# =============================================================================
# 18.  VALIDATION SCATTER PLOT
# -----------------------------------------------------------------------------
# Simulated vs. observed incidence at the final beta; error bars = ±1 SD.
# =============================================================================
p_valid <- ggplot(
  validation_summary,
  aes(x = incidence_obs, y = incidence_sim_mean, colour = type)
) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey50", linewidth = 0.8) +
  geom_errorbar(aes(ymin = incidence_sim_mean - incidence_sim_sd,
                    ymax = incidence_sim_mean + incidence_sim_sd),
                width = 0, linewidth = 0.7, alpha = 0.6) +
  geom_point(size = 4) +
  geom_text(aes(label = type), nudge_y = 0.03 * max(validation_summary$incidence_obs),
            size = BASE_TEXT * 0.25, show.legend = FALSE) +
  coord_fixed() +
  scale_colour_brewer(palette = "Set1", guide = "none") +
  labs(
    title    = "Optimisation calibration: simulated vs. observed ESBL incidence",
    subtitle = paste0("Final validation (N = ", N_REP_VALID,
                      " reps)  \u00B7  error bars = \u00B11 SD  \u00B7  ",
                      "dashed = 1:1 line  \u00B7  SSE = ", round(sse_final, 4)),
    x = "Observed incidence — SPARES target (per 1,000 bed-days)",
    y = "Simulated incidence — optimal \u03B2 (per 1,000 bed-days)"
  ) +
  theme_bw(base_size = BASE_TEXT) +
  theme(
    plot.title    = element_text(face = "bold", size = BASE_TEXT + 2),
    plot.subtitle = element_text(colour = "grey40", size = BASE_TEXT - 1),
    panel.grid.minor = element_blank()
  )


# =============================================================================
# 19.  SAVE ALL OUTPUTS
# =============================================================================

# Build the comprehensive final object for archival and downstream use
final_object <- list(
  datetime             = Sys.time(),
  beta_type_opt        = beta_type_opt,        # optimal beta per facility type
  beta_type_log_opt    = beta_type_log_opt,     # log-space version
  beta_opt             = beta_opt,              # expanded to per-hospital
  incidence_obs        = incidence_obs,         # SPARES targets used
  incidence_final      = inc_final,             # validation mean
  incidence_final_sd   = inc_final_sd,          # validation SD
  incidence_final_se   = inc_final_se,          # validation SE
  diff_final           = diff_final,
  sse_final_by_type    = sse_final_by_type,
  sse_final            = sse_final,
  validation_summary   = validation_summary,
  fits                 = fits,
  fit_values           = fit_values,
  best_fit_id          = best_fit_id,
  n_rep_obj            = N_REP_OBJ,
  n_rep_valid          = N_REP_VALID,
  n_cores              = N_CORES,
  seed_objective       = SEED_OBJ,
  seed_validation      = SEED_VALID,
  seed_random_starts   = SEED_STARTS,
  lower_beta           = LOWER_BETA,
  upper_beta           = UPPER_BETA,
  n_random_starts      = N_RANDOM_STARTS,
  maxit_nm             = MAXIT_NM,
  beta_warmup          = beta_warmup,
  checkpoint_dir       = checkpoint_dir,
  checkpoint_best_file = checkpoint_best_file,
  checkpoint_last_file = checkpoint_last_file,
  history_file         = history_file,
  starts_file          = starts_file,
  fits_file            = fits_file,
  final_file           = final_file
)

safe_saveRDS(final_object, final_file)
safe_saveRDS(final_object, final_recovered_beta_file)

write.csv2(validation_summary, file = validation_summary_file, row.names = FALSE)

out_plot <- file.path(checkpoint_dir, "validation_scatter.png")
ggsave(out_plot, plot = p_valid, width = 8, height = 8, dpi = 150)

# ── Console summary ───────────────────────────────────────────────────────────
message("\n====================================================================")
message("OPTIMAL BETA PER FACILITY TYPE")
message("====================================================================")
print(round(beta_type_opt, 6))
message("\nSIMULATED INCIDENCE (validation mean  ±  SD)")
print(data.frame(type   = names(inc_final),
                 obs    = round(incidence_obs, 3),
                 sim    = round(inc_final,     3),
                 sd     = round(inc_final_sd,  3),
                 rel_err = round(abs(diff_final) / incidence_obs, 3)))
message("\nFINAL SSE = ", round(sse_final, 6))
message("\nOutputs written to:")
message("  ", final_file)
message("  ", final_recovered_beta_file)
message("  ", validation_summary_file)
message("  ", out_plot)
print(p_valid)