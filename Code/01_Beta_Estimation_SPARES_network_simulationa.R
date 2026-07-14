# =============================================================================
# arcane_beta_calibration_incidence_v3.R
# -----------------------------------------------------------------------------
# Calibrate within-hospital transmission rate (beta) against SPARES ESBL
# incidence using a FIXED 2-YEAR SIS network simulation with 6-MONTH
# CHECKPOINTS for per-hospital incidence evaluation.
#
# UNIT CONVENTION (applies throughout the entire script):
#   All incidence values are in per 1,000 bed-days.
#   - SPARES observed rates (incidence_esbl_all) are kept in their native
#     SPARES units (per 1,000 patient-days) and used directly as targets.
#   - Simulated window incidence is scaled by * 1,000 at the point of
#     calculation (Section 7) so both sides share the same units.
#   No /1000 or *1000 conversions appear anywhere else in the script.
#
# PRIMARY REPORTING LEVEL: facility_type x region.
#   SPARES records exactly one incidence value per (facility_type, region,
#   year).  All comparisons between simulated and observed rates therefore
#   operate at this stratum level.  A type-level summary (mean over regions)
#   is derived secondarily for the trajectory plot and the pooled best-beta.
#
# DESIGN OVERVIEW:
#   (1) Run exactly N_YEARS * 365 days per (beta, replicate) combination.
#   (2) At each of the N_CHECKPOINTS evenly-spaced checkpoints (every ~3 months
#       for the default N_CHECKPOINTS = 8 over 2 years), compute per-hospital
#       window incidence (new infections / full-occupancy bed-days) and
#       aggregate to facility_type x region.
#   (3) At each checkpoint, compare each stratum's simulated mean window
#       incidence directly to its SPARES target.  "Steady state" is declared
#       when the relative error is below CONV_THRESHOLD for ALL strata with
#       SPARES-derived targets.  The simulation does NOT stop early — this
#       flag records WHEN (if ever) the simulated incidence matches the target.
#   (4) At end of run, report and plot results at the facility_type x region
#       level to match the SPARES data structure.
#
# INCIDENCE TARGET FALLBACK (four tiers — Section 4):
#   (a) Facility's own SPARES rate           — preferred
#   (b) Type x region mean from SPARES       — same type, same region
#   (c) Regional mean across all types       — for CLCC and fully absent combos
#   (d) Global mean                          — last resort (rarely reached)
#
# PLOTS:
#   Plot A — Checkpoint trajectories: per-type mean incidence at months 6, 12,
#            18, 24; one line per beta; faceted by facility_type.
#   Plot B — Calibration scatter: simulated vs. observed at facility_type x
#            region level for the final window; faceted by beta; 1:1 line.
# =============================================================================


# =============================================================================
# 0.  KNOBS  —  scale up for production runs
# =============================================================================

BETA_GRID      <- seq(0.005, 0.04, by = 0.005)  # beta candidates to evaluate (8 values)
N_REP          <- 10    # independent replicates per beta  [prod: 20–100]
SIM_START      <- "2024-01-01"   # first day of the simulation calendar

# Run structure ────────────────────────────────────────────────────────────────
N_YEARS        <- 2   # total simulation length in years (730 days)
N_CHECKPOINTS  <- 8   # evaluation checkpoints per run; 8 over 2 years = one per ~3 months
# (checkpoint days: ~91, 182, 274, 365, 456, 548, 639, 730)

# Convergence criterion ────────────────────────────────────────────────────────
# At each checkpoint the last-year incidence is summed by facility_type and
# compared to the SPARES target via a normalised least-squares criterion:
#   SS_norm = Σ(sim_type - obs_type)² / Σ obs_type²
# SS_norm = 0 → perfect fit; SS_norm = 1 → residuals as large as observations.
# Convergence is declared when SS_norm < CONV_THRESHOLD.
CONV_THRESHOLD <- 0.05   # normalised RSS threshold  [tighten / relax as needed]

# Epidemiological parameters ──────────────────────────────────────────────────
GAMMA_CLEAR    <- 1 / 387   # daily clearance prob. (mean colonisation ~387 days)
ADMISSION_PREV <- 0.05      # fraction of newly admitted patients already ESBL+
INIT_PREV      <- 0         # starting fraction of beds infected (0 = cold start)
BASE_SEED      <- 20260423  # root RNG seed; each (beta, rep) gets BASE_SEED + row

# Plot aesthetics ─────────────────────────────────────────────────────────────
BASE_TEXT      <- 14   # ggplot2 base font size — increase for PowerPoint export


# =============================================================================
# 1.  LIBRARIES
# =============================================================================
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(janitor)
library(ggplot2)


# =============================================================================
# 2.  DATA LOADING
# =============================================================================
data_dir <- file.path(getwd(), "cluster_jobs", "data")

# coords_beds_active: bed counts per finess_geo from SAE CAPACT / FINESS.
# Used to set each hospital's carrying capacity (no_beds) in the simulation.
coords_beds_active <- readRDS(file.path(data_dir, "coords_beds_active.RDS"))

# weekly_transfers: patient-transfer counts between finess_geo pairs.
# The 'weight' field is a 7-day rolling sum; dividing by 7 and rounding gives
# integer daily-average counts.  pmax(1L, ...) keeps every reported edge active
# even after rounding (a weight of 0 would silently drop a transfer link).
weekly_transfers <- readRDS(file.path(data_dir, "weekly.RDS")) %>%
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

# node_attributes_full: one row per finess_geo; carries facility_type, region,
# and ESBL incidence from SPARES.
# incidence_esbl_all is in SPARES native units — per 1,000 patient-days.
# It is NOT rescaled here.  The simulated window incidence is scaled to match
# (multiplied by 1,000 in Section 7), keeping the conversion in one place.
node_attributes_full <- readRDS(
  here("Datasets", "Output Data", "node_attributes_full.RDS")
) %>%
  mutate(finess_geo = as.character(finess_geo))  # incidence_esbl_all unchanged


# =============================================================================
# 3.  EXTEND TRANSFER CALENDAR TO N_YEARS
# -----------------------------------------------------------------------------
# The raw transfer data covers one calendar year (identified by its window_end
# dates).  To provide transfer edges throughout the full N_YEARS simulation
# we replicate the annual pattern by shifting all dates forward by 365 days per
# additional year — a periodic-network assumption (same seasonal pattern each
# year).  Leap years are not handled; the 365-day approximation is acceptable
# for the simulation horizon used here.
#
# The function maps original dates onto the simulation calendar:
#   year 1: sim_start + (original_date - base_start)
#   year 2: year 1 date + 365 days
#   ...
# Result is a named list keyed by date string ("YYYY-MM-DD") for O(1) lookup
# inside the day-by-day simulation loop.
# =============================================================================
extend_transfers <- function(transfers, n_years, sim_start_date) {
  # Anchor point: the earliest date in the raw transfer data.
  # All original dates are expressed as day-offsets from this anchor so that
  # year 1 of the simulation starts exactly at sim_start_date.
  base_start <- min(as.Date(transfers$window_end))
  sim_start  <- as.Date(sim_start_date)
  
  lapply(seq_len(n_years), function(y) {
    transfers %>%
      mutate(
        transfer_date =
          sim_start +
          as.integer(as.Date(window_end) - base_start) +  # within-year day offset
          (y - 1L) * 365L                                  # inter-year shift
      ) %>%
      select(transfer_date, finessGeo_origin, finessGeo_target, weight)
  }) %>%
    bind_rows() %>%
    split(.$transfer_date)   # list indexed by "YYYY-MM-DD" for fast daily lookup
}

message("Building ", N_YEARS, "-year transfer calendar ...")
transfers_ext <- extend_transfers(weekly_transfers, N_YEARS, SIM_START)
message("  ", length(transfers_ext), " distinct transfer dates in extended calendar")


# =============================================================================
# 4.  PER-FACILITY INCIDENCE TARGETS — four-tier fallback
# -----------------------------------------------------------------------------
# SPARES does not report rates for every finess_geo or even every
# (facility_type, region) combination.  We assign each facility a target
# incidence using the following priority, from most to least local:
#
#   (a) Facility's own SPARES rate  (incidence_esbl_all is non-NA)
#       → the best estimate: the facility's own observed incidence.
#
#   (b) Mean SPARES rate for the same facility_type IN THE SAME REGION
#       → applies when the individual facility has no own rate but other
#         facilities of the same type in the same region are in SPARES.
#         Using the same region avoids importing epidemiological patterns from
#         geographically distant hospitals of the same type.
#
#   (c) Mean SPARES rate across ALL types IN THE SAME REGION
#       → applies when an entire facility type is absent from SPARES within
#         that region (e.g. CLCC, which has no SPARES coverage nationwide).
#         The regional mean still reflects local AMR pressure even without
#         type-specific information.
#
#   (d) Global mean across all facilities and regions
#       → absolute last resort; should be reached only by facilities in
#         regions where SPARES has no coverage at all.
#
# incidence_source records which tier was used.  It is carried forward to
# Section 5 where tiers (a)–(c) are treated as having real SPARES signal
# (included in convergence checks) and tier (d) is excluded.
# =============================================================================

# ── Tier (d): global mean — denominator for the final fallback ───────────────
global_inc <- mean(node_attributes_full$incidence_esbl_all, na.rm = TRUE)

# ── Tier (b): mean per facility_type x region ─────────────────────────────────
# Computed from facilities that DO have SPARES data (na.rm = TRUE excludes NAs).
# For types with no SPARES coverage in a given region this will be NA, and
# those facilities fall through to tier (c) or (d).
type_region_inc <- node_attributes_full %>%
  group_by(facility_type, region) %>%
  summarise(
    type_region_mean_inc = mean(incidence_esbl_all, na.rm = TRUE),
    .groups = "drop"
  )

# ── Tier (c): mean per region across all facility types ──────────────────────
# Captures the local AMR burden regardless of facility type.  This is the
# appropriate fill for CLCC (Centres de Lutte Contre le Cancer), which are
# absent from SPARES nationwide and therefore have no type_region_mean_inc.
region_inc <- node_attributes_full %>%
  group_by(region) %>%
  summarise(
    region_mean_inc = mean(incidence_esbl_all, na.rm = TRUE),
    .groups = "drop"
  )

# ── Assemble targets with cascading case_when ─────────────────────────────────
facility_targets <- node_attributes_full %>%
  left_join(type_region_inc, by = c("facility_type", "region")) %>%
  left_join(region_inc,      by = "region") %>%
  mutate(
    target_incidence = case_when(
      !is.na(incidence_esbl_all)   ~ incidence_esbl_all,     # (a) own rate
      !is.na(type_region_mean_inc) ~ type_region_mean_inc,   # (b) type×region mean
      !is.na(region_mean_inc)      ~ region_mean_inc,         # (c) regional mean
      TRUE                         ~ global_inc               # (d) global fallback
    ),
    incidence_source = case_when(
      !is.na(incidence_esbl_all)   ~ "facility",
      !is.na(type_region_mean_inc) ~ "type_region_mean",
      !is.na(region_mean_inc)      ~ "region_mean",
      TRUE                         ~ "global_mean"
    )
  ) %>%
  select(finess_geo, facility_type, region, target_incidence, incidence_source)

message("Incidence target sources:",
        "  own=",              sum(facility_targets$incidence_source == "facility"),
        "  type×region=",      sum(facility_targets$incidence_source == "type_region_mean"),
        "  region-mean=",      sum(facility_targets$incidence_source == "region_mean"),
        "  global-fallback=",  sum(facility_targets$incidence_source == "global_mean"))


# =============================================================================
# 5.  HOSPITAL UNIVERSE + GROUP-LEVEL OBSERVED TARGETS
# =============================================================================

# Collect every unique finess_geo that appears in the transfer network (as
# origin or destination).  These are the simulation nodes.
hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finessGeo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finessGeo_target))
) %>% distinct()

# Attach bed counts.  parse_number() handles values stored as character strings
# (e.g. "120 lits").  Facilities not found in coords_beds_active receive the
# network-wide mean bed count rather than being dropped or set to 0.
mean_beds <- round(mean(
  readr::parse_number(as.character(
    coords_beds_active %>% clean_names() %>% pull(no_beds)
  )),
  na.rm = TRUE
))

hospitals <- hospitals %>%
  left_join(
    coords_beds_active %>% clean_names() %>%
      transmute(finess_geo = as.character(finess_geo),
                no_beds    = readr::parse_number(as.character(no_beds))),
    by = "finess_geo"
  ) %>%
  mutate(no_beds = as.integer(if_else(is.na(no_beds), mean_beds, no_beds))) %>%
  # Attach facility_type, region, and target_incidence from the fallback table
  left_join(facility_targets, by = "finess_geo") %>%
  # Final safety net for network nodes absent from node_attributes_full entirely
  # (these are rare; they receive the global mean and are excluded from the
  #  convergence check via spares_groups below)
  mutate(
    target_incidence = if_else(is.na(target_incidence), global_inc, target_incidence),
    facility_type    = if_else(is.na(facility_type), "Unknown", facility_type),
    incidence_source = if_else(is.na(incidence_source), "global_mean", incidence_source)
  )

message("Network hospitals: ", nrow(hospitals),
        "  |  total beds: ", format(sum(hospitals$no_beds), big.mark = ","))

# spares_groups: the set of (facility_type, region) strata for which SPARES
# provides a meaningful observed rate — i.e., tiers (a), (b), or (c).
# - tier (a) "facility"         own SPARES rate
# - tier (b) "type_region_mean" SPARES data exists for this type in this region
# - tier (c) "region_mean"      SPARES data exists in this region (any type)
# All three carry genuine regional AMR signal and are included in the
# convergence check and the calibration scatter plot.
# Only tier (d) "global_mean" is excluded: it is a script-wide constant with
# no local signal and would artificially inflate the convergence criterion.
spares_groups <- facility_targets %>%
  filter(incidence_source != "global_mean") %>%
  distinct(facility_type, region)

# Group-level observed targets (used in convergence check, Section 11, and plots).
# target_type_region is the PRIMARY reference: one row per stratum, matching
# the SPARES data structure.
# target_type is a secondary summary used only for the Plot A reference line.
target_type_region <- hospitals %>%
  group_by(facility_type, region) %>%
  summarise(target_incidence = mean(target_incidence), .groups = "drop")

target_type <- hospitals %>%
  group_by(facility_type) %>%
  summarise(target_incidence = mean(target_incidence), .groups = "drop")


# =============================================================================
# 6.  CORE SIS MODEL FUNCTIONS
# =============================================================================

# 6.1  Initialise hospital states ─────────────────────────────────────────────
# Every hospital starts with INIT_PREV fraction of its beds infected.
# Seeding all hospitals simultaneously (rather than a single index case) means
# the system approaches endemic equilibrium quickly instead of spending the
# first months on epidemic growth — important for a 2-year calibration run
# where early transient behaviour should not dominate the window averages.
#
# State columns:
#   n_infected  : current number of colonised beds
#   prevalence  : n_infected / no_beds (used as transmission probability)
#   n_new_inf   : incident cases on the CURRENT day (refreshed each step)
#   cum_new_inf : cumulative incident cases since day 1 (snapshotted at checkpoints)
initialize_hospital_state <- function(hospitals, init_prev) {
  hospitals %>%
    mutate(
      n_infected  = as.integer(round(no_beds * init_prev)),
      prevalence  = n_infected / no_beds,
      n_new_inf   = 0L,
      cum_new_inf = 0L
    )
}

# 6.2  Binomial draws for infected patient transfers ──────────────────────────
# For each directed transfer edge (origin → target, weight w), draw the number
# of infected patients transferred as Binomial(w, prevalence_origin).
#
# Conservation constraint: the total infected transfers OUT of a hospital
# cannot exceed the number of infected beds available.  If the sum of binomial
# draws over all outgoing edges exceeds the available pool, all draws are
# rescaled proportionally and re-allocated as integers using the largest-
# remainder method (floor + assign remainders to highest-fractional edges).
# This prevents the model from "creating" infected patients during busy periods.
compute_daily_infected_transfers_fast <- function(state, transfers_day) {
  if (nrow(transfers_day) == 0) return(tibble())
  
  idx_origin        <- match(transfers_day$finessGeo_origin, state$finess_geo)
  prev_origin       <- state$prevalence[idx_origin]
  n_infected_origin <- state$n_infected[idx_origin]
  prev_origin[is.na(prev_origin)]             <- 0
  n_infected_origin[is.na(n_infected_origin)] <- 0L
  
  # Initial binomial draws (may violate the conservation constraint per origin)
  infected_raw   <- rbinom(nrow(transfers_day), transfers_day$weight, prev_origin)
  infected_final <- infected_raw
  
  # Enforce conservation per origin hospital
  split_idx <- split(seq_len(nrow(transfers_day)), transfers_day$finessGeo_origin)
  for (idx in split_idx) {
    available <- n_infected_origin[idx[1]]   # infected beds available at this origin
    proposed  <- sum(infected_raw[idx])       # total draws across all outgoing edges
    if (proposed > available && available > 0) {
      # Rescale draws proportionally, then distribute the integer remainder
      # to the edges with the largest fractional parts (largest-remainder method)
      scaled    <- infected_raw[idx] * available / proposed
      base      <- floor(scaled)
      remainder <- scaled - base
      leftover  <- available - sum(base)
      if (leftover > 0) {
        top_idx       <- order(remainder, decreasing = TRUE)[seq_len(leftover)]
        base[top_idx] <- base[top_idx] + 1L
      }
      infected_final[idx] <- base
    }
  }
  
  tibble(finessGeo_origin  = transfers_day$finessGeo_origin,
         finessGeo_target  = transfers_day$finessGeo_target,
         infected_transfer = infected_final)
}

# 6.3  One-day SIS update ─────────────────────────────────────────────────────
# Four sequential steps per hospital per day:
#
#   Step 1 — Patient transfers:
#     Infected patients leave to (and arrive from) other hospitals according to
#     the daily transfer network.  Discharged infected patients (infected_out)
#     are replaced by newly admitted patients drawn from the community at rate
#     admission_prev (currently 0, so replacements are susceptible).
#
#   Step 2 — Post-transfer population:
#     n_after_transfer = n_infected - infected_out + infected_in + infected_replaced
#     Clamped to [0, no_beds].
#
#   Step 3 — Stochastic clearance (SIS recovery):
#     Each colonised patient independently clears with probability gamma_clear
#     (daily clearance rate = 1 / mean_colonisation_duration).
#
#   Step 4 — Within-hospital transmission:
#     Susceptible patients acquire ESBL from colonised patients via a
#     density-dependent force of infection:
#         lambda = 1 - exp(-beta * prevalence)
#     New infections n_new_inf are drawn as Binomial(n_susceptible, lambda).
#
# n_new_inf is stored in the state so the checkpoint block in Section 7 can
# read it without needing to retain a per-facility daily time series.
# cum_new_inf accumulates throughout the run; it is snapshotted at each
# checkpoint to derive the window-specific incidence.
simulate_one_day <- function(state, transfers_day,
                             beta_within, gamma_clear, admission_prev) {
  # ── Step 1: resolve transfers ────────────────────────────────────────────────
  transfers_inf <- compute_daily_infected_transfers_fast(state, transfers_day)
  infected_out  <- integer(nrow(state))
  infected_in   <- integer(nrow(state))
  
  if (nrow(transfers_inf) > 0) {
    idx_out <- match(transfers_inf$finessGeo_origin, state$finess_geo)
    idx_in  <- match(transfers_inf$finessGeo_target, state$finess_geo)
    # tapply sums over multiple edges per hospital; names give the row index
    infected_out[as.integer(names(tapply(transfers_inf$infected_transfer, idx_out, sum)))] <-
      tapply(transfers_inf$infected_transfer, idx_out, sum)
    infected_in[as.integer(names(tapply(transfers_inf$infected_transfer, idx_in,  sum)))] <-
      tapply(transfers_inf$infected_transfer, idx_in,  sum)
  }
  
  # ── Step 2: community admissions replace departing patients ─────────────────
  infected_replaced <- rbinom(nrow(state), infected_out, admission_prev)
  n_after_transfer  <- pmax(0L,
                            pmin(state$n_infected
                                 - infected_out + infected_in + infected_replaced,
                                 state$no_beds))
  
  # ── Step 3: stochastic clearance ────────────────────────────────────────────
  n_cleared     <- rbinom(nrow(state), n_after_transfer, gamma_clear)
  n_after_clear <- n_after_transfer - n_cleared
  
  # ── Step 4: new within-hospital transmission ─────────────────────────────────
  n_susceptible <- state$no_beds - n_after_clear
  # Force of infection: probability a susceptible acquires ESBL today
  p_inf         <- pmin(pmax(1 - exp(-beta_within * (n_after_clear / state$no_beds)), 0), 1)
  n_new_inf     <- rbinom(nrow(state), n_susceptible, p_inf)
  n_final       <- pmin(state$no_beds, n_after_clear + n_new_inf)
  
  # dplyr mutate evaluates expressions left-to-right within a single call, so
  # n_new_inf (the column) is available when cum_new_inf is updated below.
  state %>% mutate(
    n_infected  = n_final,
    prevalence  = n_final / no_beds,
    n_new_inf   = n_new_inf,              # today's new cases (overwritten each day)
    cum_new_inf = cum_new_inf + n_new_inf  # running total since day 1
  )
}


# =============================================================================
# 7.  2-YEAR SIMULATION WITH ROLLING 1-YEAR CHECKPOINTS
# -----------------------------------------------------------------------------
# Runs exactly total_days = N_YEARS * 365 days.
#
# CHECKPOINT SCHEDULE
#   Checkpoint days are equally spaced across total_days; the last is always
#   pinned to total_days.  For N_YEARS=2, N_CHECKPOINTS=8:
#     ckpt_days ≈ {91, 182, 274, 365, 456, 548, 639, 730}
#     checkpoint months ≈ {3, 6, 9, 12, 15, 18, 21, 24}
#
# WINDOW INCIDENCE — 1-YEAR LOOKBACK
#   At each checkpoint k the incidence is computed over the last 365 days
#   (or all elapsed days for early checkpoints, where < 365 days have run):
#     anchor_day    = max(0,  ckpt_days[k] - 365)
#     window_days   = ckpt_days[k] - anchor_day       (= min(365, ckpt_days[k]))
#     window_inc    = (cum_new_inf_now - cum_new_inf_anchor) /
#                     (no_beds * window_days)  * 1,000
#   cum_new_inf at each required anchor day is snapshotted during the loop.
#   cum_new_inf at day 0 is always 0 (initialize_hospital_state sets it to 0L).
#   Multiplying by 1,000 converts to per 1,000 bed-days, matching SPARES units.
#   Full occupancy is assumed (no_beds * days as denominator).
#
# CONVERGENCE CHECK — FACILITY-TYPE LEAST SQUARES
#   At each checkpoint:
#   1. Average per-hospital window incidence to facility_type level (all regions
#      pooled).  Using type-level aggregation reduces noise from small strata.
#   2. Compute normalised RSS vs. SPARES type-level targets:
#        SS_norm = Σ(sim_type - obs_type)²  /  Σ obs_type²
#      (only types with SPARES-derived targets, tiers a/b/c, are included)
#   3. Convergence is declared when SS_norm < conv_threshold.
#   The simulation NEVER stops early; ss_checkpoint records the first checkpoint
#   where SS_norm drops below the threshold, or remains NA for the full run.
#
# RETURN VALUE
#   $checkpoints   long tibble: one row per hospital × checkpoint (N_CHECKPOINTS
#                  rows per hospital); window_days reflects the actual lookback
#                  used (min(365, elapsed days)).
#   $final_state   complete state tibble at the last simulation day
#   $ss_checkpoint integer or NA
# =============================================================================
run_sis_2year <- function(
    hospitals,
    transfers_ext,          # named list of transfer tibbles, keyed by date string
    sim_start,              # character "YYYY-MM-DD" — simulation calendar start
    beta_within,            # within-hospital transmission rate (per colonised bed per day)
    gamma_clear,            # daily clearance probability
    admission_prev,         # fraction of new admissions already ESBL+
    init_prev,              # starting fraction of beds infected
    seed,                   # RNG seed for this replicate
    n_years        = N_YEARS,
    n_checkpoints  = N_CHECKPOINTS,
    conv_threshold = CONV_THRESHOLD
) {
  set.seed(seed)
  state       <- initialize_hospital_state(hospitals, init_prev)
  total_days  <- as.integer(n_years * 365L)
  sim_start_d <- as.Date(sim_start)
  
  # ── Checkpoint schedule ──────────────────────────────────────────────────────
  # Evenly space N_CHECKPOINTS points across total_days; last is always total_days.
  ckpt_days <- as.integer(round(seq(total_days / n_checkpoints,
                                    total_days,
                                    length.out = n_checkpoints)))
  
  # Approximate month labels (used on plot axis and in progress messages)
  ckpt_months <- round(ckpt_days / 30.4, 1)
  
  # ── 1-year lookback anchors ───────────────────────────────────────────────────
  # For each checkpoint, the lookback start = max(0, checkpoint_day - 365).
  # Checkpoints before day 365 use all elapsed days (anchor = 0, snap = all zeros).
  anchor_days    <- pmax(0L, ckpt_days - 365L)
  unique_anchors <- unique(anchor_days[anchor_days > 0L])   # days needing a snapshot
  
  # Snapshot store keyed by simulation day (as character string).
  # Day 0 is always available: cum_new_inf = 0L at initialisation regardless
  # of init_prev (only the infected COUNT is set; the accumulator starts at 0).
  cum_snapshots        <- list()
  cum_snapshots[["0"]] <- rep(0L, nrow(state))   # baseline: no infections recorded yet
  
  # Storage for per-checkpoint results and convergence flag
  ckpt_list     <- vector("list", n_checkpoints)
  ss_checkpoint <- NA_integer_
  
  # Reusable empty-transfer tibble for simulation days with no recorded edges
  empty_td <- tibble(finessGeo_origin = character(),
                     finessGeo_target = character(),
                     weight           = integer())
  
  # ── Day-by-day simulation loop ────────────────────────────────────────────────
  for (day_idx in seq_len(total_days)) {
    
    # Look up the transfer edges for today's simulation date.  transfers_ext is
    # a named list keyed by "YYYY-MM-DD"; NULL means no transfers recorded.
    day_key <- as.character(sim_start_d + (day_idx - 1L))
    td      <- transfers_ext[[day_key]]
    if (is.null(td)) td <- empty_td
    
    # Advance model state by one day (transmission, clearance, transfers)
    state <- simulate_one_day(state, td, beta_within, gamma_clear, admission_prev)
    
    # ── Store lookback snapshot if today is a required anchor day ───────────────
    # Anchor days (max(0, ckpt_day - 365)) were pre-computed; only those > 0
    # need explicit storage (day 0 is already initialised in cum_snapshots).
    if (day_idx %in% unique_anchors) {
      cum_snapshots[[as.character(day_idx)]] <- state$cum_new_inf
    }
    
    # ── Checkpoint: evaluate if this day is a scheduled checkpoint ─────────────
    cp_idx <- which(ckpt_days == day_idx)   # length 0 on non-checkpoint days
    if (length(cp_idx) == 1L) {
      cp_num     <- cp_idx
      anchor_day <- anchor_days[cp_num]          # lookback start: max(0, today-365)
      snap       <- cum_snapshots[[as.character(anchor_day)]]  # cumulative total there
      w_days     <- day_idx - anchor_day         # actual window: min(365, day_idx)
      
      # New infections over the past w_days:
      #   subtract cum_new_inf at the anchor from today's total.
      #   For checkpoints before day 365: anchor_day = 0, snap = all zeros,
      #   so the full run-to-date is used automatically.
      w_new_inf <- state$cum_new_inf - snap
      
      # Window incidence per 1,000 bed-days (full-occupancy denominator).
      # This is the only place in the script where the *1,000 scaling is applied.
      w_inc <- (w_new_inf / (state$no_beds * w_days)) * 1000
      
      # Store one record per hospital for this checkpoint
      ckpt_list[[cp_num]] <- state %>%
        mutate(
          window_new_inf   = w_new_inf,
          window_incidence = w_inc,          # per 1,000 bed-days; matches SPARES units
          checkpoint_num   = cp_num,
          checkpoint_month = ckpt_months[cp_num],
          window_days      = w_days          # actual lookback: min(365, day_idx)
        ) %>%
        select(finess_geo, facility_type, region, no_beds,
               window_new_inf, window_incidence,
               checkpoint_num, checkpoint_month, window_days)
      
      # ── Convergence check: facility_type least squares ────────────────────────
      # Has the simulated incidence over the past year converged to the SPARES
      # target at the facility TYPE level?
      #
      # Step 1 — aggregate per-hospital incidence to facility_type (pool regions).
      # Step 2 — compute normalised RSS against type-level SPARES targets:
      #            SS_norm = Σ(sim_type - obs_type)² / Σ obs_type²
      #          Only types with SPARES-derived targets (tiers a/b/c) contribute.
      # Step 3 — declare convergence when SS_norm < conv_threshold.
      #
      # Aggregating to TYPE rather than type × region reduces noise from small
      # regional strata and concentrates the signal in the clearest contrast.
      # This uses the current replicate only (single run); the multi-rep
      # consensus is computed in Section 10 after all simulations finish.
      if (is.na(ss_checkpoint)) {   # evaluate only until first convergence
        
        # Step 1: type-level mean incidence (mean over all hospitals of that type)
        type_sim <- ckpt_list[[cp_num]] %>%
          group_by(facility_type) %>%
          summarise(sim_mean = mean(window_incidence, na.rm = TRUE), .groups = "drop")
        
        # Keep only types with real SPARES signal (tiers a/b/c, not global fill)
        spares_types <- facility_targets %>%
          filter(incidence_source != "global_mean") %>%
          distinct(facility_type)
        
        # Step 2: squared differences between simulated and observed type means
        ls_data <- type_sim %>%
          semi_join(spares_types, by = "facility_type") %>%
          left_join(target_type %>% rename(obs = target_incidence),
                    by = "facility_type") %>%
          filter(!is.na(obs)) %>%
          mutate(sq_diff = (sim_mean - obs)^2)
        
        if (nrow(ls_data) > 0) {
          SS_num   <- sum(ls_data$sq_diff)           # Σ (sim - obs)²
          SS_denom <- sum(ls_data$obs^2) + 1e-12     # Σ obs²  (prevents division by 0)
          SS_norm  <- SS_num / SS_denom
          
          # Step 3: convergence test
          if (SS_norm < conv_threshold) {
            ss_checkpoint <- cp_num
            message(sprintf(
              "    [beta=%.4f] Converged at checkpoint %d (month ~%.0f)  SS_norm=%.4f",
              beta_within, cp_num, ckpt_months[cp_num], SS_norm
            ))
          }
        }
      }
    }  # end checkpoint block
  }  # end day loop
  
  list(
    checkpoints   = bind_rows(ckpt_list),
    final_state   = state,
    ss_checkpoint = ss_checkpoint,
    ckpt_months   = ckpt_months
  )
}


# =============================================================================
# 8.  CALIBRATION GRID
# =============================================================================
# All (beta, replicate) combinations.  Each row gets a unique RNG seed
# derived from BASE_SEED so results are fully reproducible.
grid <- tidyr::crossing(beta_within = BETA_GRID, rep_id = seq_len(N_REP)) %>%
  mutate(sim_seed = BASE_SEED + row_number())

message("\nCalibration grid: ", nrow(grid), " simulations  (",
        length(BETA_GRID), " beta values  x  ", N_REP, " rep(s))")


# =============================================================================
# 9.  RUN ALL SIMULATIONS
# =============================================================================
# pmap iterates over (beta_within, sim_seed) pairs and calls run_sis_2year for
# each.  Each call is independent; for a large N_REP production run consider
# wrapping this in future_pmap() (furrr package) for parallel execution after
# setting options(future.globals.maxSize = 2 * 1024^3).
message("Running simulations (", N_YEARS, " years, ",
        N_CHECKPOINTS, " checkpoints per run) ...")

sim_results <- grid %>%
  mutate(result = pmap(
    list(beta_within, sim_seed),
    function(b, s) {
      message(sprintf("  beta = %.4f   seed = %d", b, s))
      run_sis_2year(
        hospitals      = hospitals,
        transfers_ext  = transfers_ext,
        sim_start      = SIM_START,
        beta_within    = b,
        gamma_clear    = GAMMA_CLEAR,
        admission_prev = ADMISSION_PREV,
        init_prev      = INIT_PREV,
        seed           = s
      )
    }
  ))


# =============================================================================
# 10.  EXTRACT AND AGGREGATE CHECKPOINT DATA
# =============================================================================

# ── 10a. Full per-hospital checkpoint table ───────────────────────────────────
# Unnesting gives one row per: hospital × checkpoint × replicate × beta.
# window_incidence is in per 1,000 bed-days (already scaled in Section 7).
ckpt_all <- sim_results %>%
  mutate(ckpt = map(result, "checkpoints")) %>%
  select(beta_within, rep_id, ckpt) %>%
  unnest(ckpt)

# ── 10b. PRIMARY REPORTING: facility_type x region x checkpoint ───────────────
# SPARES reports exactly one incidence value per (facility_type, region, year),
# so aggregating the simulation to this stratum level is the natural comparison.
#
# Two-step aggregation:
#   Step 1 — per-rep stratum mean:
#     Average window_incidence over all hospitals in the same (type, region)
#     group for each replicate separately.  This mirrors the SPARES observation
#     model, where the recorded rate is the mean over all surveyed hospitals in
#     the stratum.  n_hospitals records how many hospitals contributed.
#   Step 2 — across-rep summary:
#     Average the per-rep stratum means to obtain the point estimate (mean_inc).
#     sd_inc is the between-replicate SD, quantifying run-to-run stochasticity.
#     It is NA when N_REP == 1 (single replicate gives no variability estimate).
ckpt_type_region <- ckpt_all %>%
  # Step 1: stratum mean within each replicate
  group_by(beta_within, rep_id, facility_type, region,
           checkpoint_num, checkpoint_month) %>%
  summarise(
    mean_inc_rep = mean(window_incidence, na.rm = TRUE),
    n_hospitals  = n(),      # hospitals in this (type, region) in the network
    .groups      = "drop"
  ) %>%
  # Step 2: aggregate across replicates
  group_by(beta_within, facility_type, region, checkpoint_num, checkpoint_month) %>%
  summarise(
    mean_inc    = mean(mean_inc_rep),   # point estimate
    sd_inc      = sd(mean_inc_rep),     # between-rep uncertainty (NA if N_REP==1)
    n_hospitals = first(n_hospitals),
    .groups     = "drop"
  )

# ── 10c. Facility-type-level summary (derived from 10b; for Plot A) ───────────
# Average the type×region stratum means over regions within each facility type.
# This is a mean-of-means: each region contributes equally regardless of how
# many hospitals it contains.  Used only for the trajectory plot (Plot A) where
# showing all type×region combinations would produce too many lines.
# No SD is propagated because averaging over regions adds a second aggregation
# layer that makes the SD hard to interpret.
ckpt_type <- ckpt_type_region %>%
  group_by(beta_within, facility_type, checkpoint_num, checkpoint_month) %>%
  summarise(
    mean_inc = mean(mean_inc),   # unweighted mean over regions within type
    .groups  = "drop"
  )

# ── 10d. Final-checkpoint summaries (month ~24) ───────────────────────────────
# The last checkpoint is the end-of-run estimate used for best-beta matching
# (Section 11), the calibration scatter (Plot B), and the console report.
final_cp_num <- max(ckpt_type_region$checkpoint_num)

# TYPE x REGION (primary): directly comparable to SPARES stratum rates
final_type_region <- ckpt_type_region %>%
  filter(checkpoint_num == final_cp_num) %>%
  rename(sim_incidence = mean_inc,
         sd_incidence  = sd_inc)

# TYPE-LEVEL (derived from final_type_region): used for pooled best-beta only
final_type <- ckpt_type %>%
  filter(checkpoint_num == final_cp_num) %>%
  rename(sim_incidence = mean_inc)

# ── 10e. Steady-state checkpoint summary ─────────────────────────────────────
# ss_cp: the first checkpoint (1–4) at which the convergence criterion was
# satisfied.  NA indicates the criterion was never met within the 2-year run.
# pmap_int is used instead of map_int to safely handle the rare case where
# r$ss_checkpoint is NULL (R returns NULL for missing list elements).
ss_summary <- sim_results %>%
  mutate(ss_cp = pmap_int(list(result), function(r) {
    v <- r$ss_checkpoint
    if (is.null(v) || is.na(v)) NA_integer_ else as.integer(v)
  })) %>%
  select(beta_within, rep_id, ss_cp)


# =============================================================================
# 11.  FIND BEST BETA — linear interpolation on final-checkpoint stratum means
# -----------------------------------------------------------------------------
# For each target stratum the simulated mean incidence increases approximately
# monotonically with beta (in expectation, after averaging across replicates).
# pick_beta() uses stats::approx() to find the beta that would produce exactly
# the observed target by linear interpolation between the two nearest grid points.
# If the target lies outside the simulated range, the nearest grid boundary is
# returned (no extrapolation).
# =============================================================================
pick_beta <- function(beta_vec, sim_inc_vec, target) {
  o <- order(beta_vec)
  b <- beta_vec[o]
  y <- sim_inc_vec[o]
  if (target <= min(y)) return(min(b))   # target below minimum simulated value
  if (target >= max(y)) return(max(b))   # target above maximum simulated value
  approx(x = y, y = b, xout = target, ties = "ordered")$y
}

# (A) Best beta per facility_type (pooled over regions)
#     Uses the type-level final means (ckpt_type → final_type).
beta_by_type <- target_type %>%
  rowwise() %>%
  mutate(best_beta = {
    g <- filter(final_type, facility_type == .data$facility_type)
    if (nrow(g) == 0) NA_real_
    else pick_beta(g$beta_within, g$sim_incidence, target_incidence)
  }) %>%
  ungroup()

# (B) Best beta per facility_type x region
#     Uses the type×region final means (final_type_region) — PRIMARY result.
beta_by_type_region <- target_type_region %>%
  rowwise() %>%
  mutate(best_beta = {
    g <- filter(final_type_region,
                facility_type == .data$facility_type,
                region        == .data$region)
    if (nrow(g) == 0) NA_real_
    else pick_beta(g$beta_within, g$sim_incidence, target_incidence)
  }) %>%
  ungroup()


# =============================================================================
# 12.  PLOT A — CHECKPOINT TRAJECTORIES BY FACILITY TYPE
# -----------------------------------------------------------------------------
# How does simulated incidence evolve across the four 6-month windows?
#
# Data:  ckpt_type — type-level means (averaged over regions), one value per
#        (beta, facility_type, checkpoint).  Derived from ckpt_type_region.
# X-axis: simulation month (6, 12, 18, 24).
# Y-axis: mean incidence in per 1,000 bed-days.
# Lines:  one per beta value, coloured with a plasma scale.
# Reference: dashed firebrick line = SPARES target per facility type
#            (averaged over regions, from target_type).
# Facets: one panel per facility_type; free y-scale to show within-type
#         dynamics without large-type panels compressing small-type ones.
# =============================================================================
p_traj <- ggplot(
  ckpt_type,
  aes(x      = checkpoint_month,
      y      = mean_inc,
      colour = factor(round(beta_within, 4)),
      group  = beta_within)
) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 2.8, alpha = 0.9) +
  # SPARES observed target per type (mean over regions), dashed firebrick
  geom_hline(
    data         = target_type,
    aes(yintercept = target_incidence),
    linetype     = "dashed", colour = "firebrick",
    linewidth    = 0.75, inherit.aes = FALSE
  ) +
  facet_wrap(~ facility_type, scales = "free_y", nrow = 2) +
  scale_colour_viridis_d(name = "\u03B2 (within-hospital)", option = "plasma", end = 0.9) +
  scale_x_continuous(
    name   = "Simulation month",
    breaks = unique(ckpt_type$checkpoint_month)
  ) +
  scale_y_continuous(name = "ESBL incidence (new cases per 1,000 bed-days)") +
  labs(
    title    = "Simulated ESBL incidence at 6-month checkpoints by facility type",
    subtitle = paste0(
      "Mean over regions and hospitals within type  \u00B7  ",
      N_REP, " replicate(s) per \u03B2  \u00B7  ",
      "dashed = SPARES observed target"
    )
  ) +
  theme_bw(base_size = BASE_TEXT) +
  theme(
    plot.title       = element_text(face = "bold", size = BASE_TEXT + 2),
    plot.subtitle    = element_text(colour = "grey40", size = BASE_TEXT - 1),
    legend.position  = "bottom",
    legend.title     = element_text(size = BASE_TEXT - 1),
    legend.text      = element_text(size = BASE_TEXT - 2),
    strip.text       = element_text(face = "bold", size = BASE_TEXT - 1),
    panel.grid.minor = element_blank()
  )


# =============================================================================
# 13.  PLOT B — CALIBRATION SCATTER  (final window, facility_type x region)
# -----------------------------------------------------------------------------
# Is the simulated incidence at the final checkpoint (month ~24) close to the
# SPARES observed rate?  Each point represents one facility_type x region
# stratum with a SPARES-derived target; global-fill strata are excluded.
#
# X-axis: SPARES observed target (per 1,000 patient-days = per 1,000 bed-days).
# Y-axis: simulated mean window incidence (per 1,000 bed-days).
# 1:1 line: points ON the line = perfect calibration.
#           Points ABOVE = over-estimated; BELOW = under-estimated.
# Colour:  facility_type — reveals whether systematic bias is type-specific.
# Facets:  one panel per beta — immediately shows which beta minimises scatter
#          around the 1:1 line.
# coord_fixed(ratio=1) ensures the 1:1 line appears at exactly 45 degrees.
# =============================================================================
scatter_data <- final_type_region %>%
  semi_join(spares_groups, by = c("facility_type", "region")) %>%
  left_join(target_type_region, by = c("facility_type", "region")) %>%
  filter(!is.na(target_incidence))
# Both sim_incidence and target_incidence are in per 1,000 bed-days;
# no further rescaling needed.

# Shared axis range: ensures the 1:1 line spans the full plot area
ax_max <- max(c(scatter_data$sim_incidence, scatter_data$target_incidence),
              na.rm = TRUE) * 1.05

p_scatter <- ggplot(scatter_data,
                    aes(x = target_incidence, y = sim_incidence,
                        colour = facility_type)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey50", linewidth = 0.75) +
  geom_point(size = 2.5, alpha = 0.82) +
  coord_fixed(ratio = 1, xlim = c(0, ax_max), ylim = c(0, ax_max)) +
  facet_wrap(~ paste0("\u03B2 = ", round(beta_within, 4)), nrow = 2) +
  scale_colour_brewer(name = "Facility type", palette = "Set1") +
  labs(
    title    = "Calibration check: simulated vs. observed incidence (final 6-month window)",
    subtitle = paste0(
      "Each point = one facility type \u00D7 region  \u00B7  ",
      "dashed = 1:1 perfect calibration  \u00B7  ",
      "per 1,000 bed-days"
    ),
    x = "Observed incidence — SPARES target (per 1,000 bed-days)",
    y = "Simulated incidence — final window mean (per 1,000 bed-days)"
  ) +
  theme_bw(base_size = BASE_TEXT) +
  theme(
    plot.title       = element_text(face = "bold", size = BASE_TEXT + 2),
    plot.subtitle    = element_text(colour = "grey40", size = BASE_TEXT - 1),
    legend.position  = "bottom",
    legend.title     = element_text(size = BASE_TEXT - 1),
    legend.text      = element_text(size = BASE_TEXT - 2),
    strip.text       = element_text(size = BASE_TEXT - 2),
    panel.grid.minor = element_blank()
  )


# =============================================================================
# 14.  CONSOLE REPORTS
# =============================================================================

# ── Primary report: facility_type x region (matches SPARES data structure) ────
cat("\n=== FINAL-WINDOW INCIDENCE BY FACILITY TYPE x REGION ===\n")
cat("Units: per 1,000 bed-days  |  final ~6-month window  |  mean over reps\n\n")

final_type_region %>%
  semi_join(spares_groups, by = c("facility_type", "region")) %>%
  left_join(
    target_type_region %>% rename(target_inc = target_incidence),
    by = c("facility_type", "region")
  ) %>%
  filter(!is.na(target_inc)) %>%
  mutate(
    sim    = round(sim_incidence, 3),  # simulated mean incidence
    sd     = round(sd_incidence,  3),  # between-rep SD (NA if N_REP==1)
    target = round(target_inc,    3),  # SPARES observed target
    rel_err = round(abs(sim_incidence - target_inc) / (target_inc + 1e-12), 3)
  ) %>%
  select(beta_within, facility_type, region, sim, sd, target, rel_err) %>%
  arrange(facility_type, region, beta_within) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

# ── Secondary report: best-beta tables ────────────────────────────────────────
cat("\n=== BEST BETA per FACILITY TYPE (pooled over regions) ===\n")
beta_by_type %>%
  mutate(target = round(target_incidence, 3),
         best_beta = round(best_beta, 4)) %>%
  select(facility_type, target, best_beta) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n=== BEST BETA per FACILITY TYPE x REGION (head 20) ===\n")
beta_by_type_region %>%
  arrange(facility_type, region) %>%
  mutate(target = round(target_incidence, 3),
         best_beta = round(best_beta, 4)) %>%
  select(facility_type, region, target, best_beta) %>%
  head(20) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

# ── Steady-state summary ──────────────────────────────────────────────────────
cat("\n=== STEADY-STATE CHECKPOINT SUMMARY ===\n")
cat("(checkpoint 1=month~6, 2=~12, 3=~18, 4=~24; NA = not reached in 2 years)\n\n")
ss_summary %>%
  group_by(beta_within) %>%
  summarise(
    n_reps       = n(),
    n_ss         = sum(!is.na(ss_cp)),
    pct_ss       = round(mean(!is.na(ss_cp)) * 100),
    median_ss_cp = median(ss_cp, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)


# =============================================================================
# 15.  SAVE OUTPUTS
# =============================================================================
out_rds  <- file.path(getwd(), "beta_calibration_incidence_v3.RDS")
out_traj <- file.path(getwd(), "beta_calibration_checkpoints.png")
out_scat <- file.path(getwd(), "beta_calibration_scatter.png")

saveRDS(
  list(
    # Primary results
    beta_by_type        = beta_by_type,
    beta_by_type_region = beta_by_type_region,
    # Final-checkpoint incidence tables
    final_type_region   = final_type_region,   # primary: type x region
    final_type          = final_type,           # secondary: type-level
    # Full checkpoint trajectory tables
    ckpt_type_region    = ckpt_type_region,    # primary: type x region x checkpoint
    ckpt_type           = ckpt_type,           # secondary: type x checkpoint (for plots)
    ckpt_all            = ckpt_all,            # raw: hospital x checkpoint
    # Supporting objects
    ss_summary          = ss_summary,
    facility_targets    = facility_targets,
    settings = list(
      BETA_GRID      = BETA_GRID,
      N_REP          = N_REP,
      SIM_START      = SIM_START,
      N_YEARS        = N_YEARS,
      N_CHECKPOINTS  = N_CHECKPOINTS,
      INIT_PREV      = INIT_PREV,
      CONV_THRESHOLD = CONV_THRESHOLD
    )
  ),
  out_rds
)

ggsave(out_traj, plot = p_traj,    width = 14, height = 9,  dpi = 150)
ggsave(out_scat, plot = p_scatter, width = 14, height = 9,  dpi = 150)

message("\nOutputs saved:")
message("  ", out_rds)
message("  ", out_traj, "  (checkpoint trajectories by facility type)")
message("  ", out_scat, "  (calibration scatter by facility type x region)")

print(p_traj)
print(p_scatter)