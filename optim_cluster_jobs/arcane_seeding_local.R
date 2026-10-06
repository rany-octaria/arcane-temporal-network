# =============================================================================
# arcane_seeding_local.R  —  Seeding scenario simulation  (LOCAL VERSION)
# =============================================================================
# ARCANE Project — Task 4.3: How does seeding location shape ARB spread?
# Author: Rany Octaria — MESuRS/Cnam
#
# WHAT THIS SCRIPT DOES
# ─────────────────────
# Introduces a novel ARB into one "seed" hospital and tracks how it spreads
# across the French inter-hospital transfer network under 8 seeding scenarios
# and 3 beta tiers (Low / Mid / High transmissibility).
#
# KEY DESIGN CHOICES
# ──────────────────
# • Data and network: identical to optim_france.R (weekly.RDS,
#   facility_level_final.RDS, same P_tr matrix construction)
# • SIS model: exact same handle_exit closure as optim_france.R,
#   but tracking the daily time series rather than last-year incidence
# • Beta assignment: per-hospital beta_vec drawn from a truncated normal
#   distribution whose parameters come from the regional calibration
#   (compiled_region.rds):
#     mean = regional wgm for that hospital's type × region
#           (falls back to national wgm if region is missing)
#     SD   = (ci_hi_95 − ci_lo_95) / (2 × 1.96)
# • 3 tiers: Low (mean = ci_lo_95), Mid (mean = wgm), High (mean = ci_hi_95)
# • Novel pathogen: pi_vec = 0 (no community reservoir)
# • Starts with N_SEED_INF = 1 infected patient in the seed hospital
# • Local knobs: n_cores = detectCores() − 2, N_REPS = 10, Tmax = 365
# =============================================================================

library(parallel)
library(dplyr)
library(tidyr)
library(truncnorm)

# =============================================================================
# 0. PATHS AND LOCAL SETTINGS
# =============================================================================

setwd("C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/optim_cluster_jobs")

DATA_DIR     <- file.path(getwd(), "data")
COMPILED_DIR <- file.path(getwd(),
                          "Outputs", "region_cluster", "compiled")
OUT_DIR      <- file.path(dirname(getwd()), "Outputs", "seeding")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Local performance knobs ───────────────────────────────────────────────────
# Increase N_REPS and Tmax for the cluster version.
N_REPS        <- 500      # replicates per tier × seed rule (cluster: 100)
N_SEED_INF    <- 1      # index patients on day 0
Tmax          <- 365L    # days to simulate (1 year; cluster: 730)
N_CORES       <- max(1L, parallel::detectCores() - 4L)  # leaves 2 cores free

message("=== ARCANE SEEDING SCENARIOS (LOCAL) ===")
message("Cores: ", N_CORES, " | Reps: ", N_REPS, " | Tmax: ", Tmax, " days")

# =============================================================================
# 1. LOAD DATA — identical to optim_france.R
# =============================================================================

message("Loading transfer network...")
weekly_transfers <- readRDS(file.path(DATA_DIR, "weekly.RDS")) %>%
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

message("Loading facility characteristics...")
facility_level <- readRDS(file.path(DATA_DIR, "facility_level_final.RDS")) %>%
  mutate(finess_geo = as.character(finess_geo)) %>%
  rename(incidence_esbl_all = incidence_region_type_ESBL_all)

# =============================================================================
# 2. BUILD HOSPITAL UNIVERSE — identical to optim_france.R
# =============================================================================

# LOS defaults from data
default_los <- facility_level %>%
  filter(!is.na(hospital_type)) %>%
  group_by(hospital_type) %>%
  summarise(pt_days = sum(pt_days_total, na.rm = TRUE),
            pat     = sum(patient_total,  na.rm = TRUE),
            .groups = "drop") %>%
  mutate(los_type = pt_days / pat)
DEFAULT_LOS_TYPE   <- setNames(default_los$los_type, default_los$hospital_type)
GLOBAL_DEFAULT_LOS <- with(
  facility_level %>%
    filter(!is.na(hospital_type)) %>%
    summarise(a = sum(pt_days_total, na.rm=TRUE),
              b = sum(patient_total,  na.rm=TRUE)),
  a / b
)

hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_origin)),
  weekly_transfers %>% transmute(finess_geo = as.character(finess_geo_target))
) %>% distinct() %>%
  left_join(
    facility_level %>% transmute(
      finess_geo, hospital_type, type_spares, region,
      no_beds = as.integer(round(census_max)),
      los     = pmax(as.numeric(los_mean), 1.0)
    ), by = "finess_geo"
  ) %>%
  mutate(
    no_beds       = as.integer(if_else(is.na(no_beds),
                                       as.integer(round(mean(no_beds, na.rm=TRUE))),
                                       no_beds)),
    los           = coalesce(los, DEFAULT_LOS_TYPE[hospital_type], GLOBAL_DEFAULT_LOS),
    type_spares   = if_else(is.na(type_spares),   "Unknown", type_spares),
    hospital_type = if_else(is.na(hospital_type), "Unknown", hospital_type),
    region        = if_else(is.na(region),         "Unknown", region)
  )

H      <- nrow(hospitals)
beds   <- hospitals$no_beds
p_exit <- 1 / hospitals$los
message("Hospitals: ", H)

# Transfer out-degree for p_tr
transfer_out_df <- weekly_transfers %>%
  transmute(origin = as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out = sum(weight, na.rm = TRUE), .groups = "drop")
hospitals <- hospitals %>%
  left_join(transfer_out_df, by = c("finess_geo" = "origin")) %>%
  mutate(total_out = replace(total_out, is.na(total_out), 0))
p_tr <- pmin(hospitals$total_out / pmax(p_exit * beds, 1), 0.60)

# Build P_tr — column-normalised H × H transfer matrix
message("Building P_tr (", H, " × ", H, ")...")
hosp_idx     <- setNames(seq_len(H), hospitals$finess_geo)
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

# Novel pathogen: no community reservoir (pi_vec = 0)
# (contrast with optim where pi_vec = 0.05 for ESBL in endemic setting)
pi_vec   <- rep(0.0, H)
gamma    <- 1 / 387
alpha    <- 0

# =============================================================================
# 3. LOAD CALIBRATION RESULTS — beta by type × region
# =============================================================================

message("Loading calibration results...")

# Load region_final_all_summaries from compiled folder
# Columns: scope, region, job_index, type, beta, incidence_obs,
#          incidence_sim_mean, incidence_sim_sd, incidence_sim_se,
#          diff, sse, H_region, sse_total, n_rep
rfa_path <- file.path(COMPILED_DIR, "region_final_all_summaries.rds")
if (!file.exists(rfa_path)) {
  # Try CSV fallback
  rfa_path <- file.path(COMPILED_DIR, "region_final_all_summaries.csv")
  if (!file.exists(rfa_path))
    stop("region_final_all_summaries not found in: ", COMPILED_DIR)
  region_final_all_summaries <- read.csv2(rfa_path, stringsAsFactors = FALSE)
} else {
  region_final_all_summaries <- readRDS(rfa_path)
}

message("Loaded region_final_all_summaries: ", nrow(region_final_all_summaries),
        " rows | Types: ", paste(unique(region_final_all_summaries$type), collapse=", "))

# Regional betas: one row per type × region — use `type` column directly
# (matches hospital_type in hospitals dataset)
regional_betas <- region_final_all_summaries %>%
  filter(!is.na(beta), beta > 0) %>%
  select(type, region, beta_regional = beta, H_region)

# Compute national weighted geometric mean and CI per type
# from the spread of regional betas (weighted by H_region)
national_betas <- regional_betas %>%
  group_by(type) %>%
  summarise(
    n_regions = n(),
    # Weighted geometric mean: exp(sum(w * log(beta)) / sum(w))
    wgm       = exp(sum(H_region * log(beta_regional)) / sum(H_region)),
    # SD of log-beta across regions (for truncated normal draws)
    log_sd    = sd(log(beta_regional)),
    # Approximate 95% CI on the geometric mean scale
    ci_lo_95  = exp(log(exp(sum(H_region * log(beta_regional)) / sum(H_region))) -
                      1.96 * sd(log(beta_regional))),
    ci_hi_95  = exp(log(exp(sum(H_region * log(beta_regional)) / sum(H_region))) +
                      1.96 * sd(log(beta_regional))),
    .groups   = "drop"
  ) %>%
  mutate(
    # SD for truncated normal: derived from 95% CI width
    beta_sd  = (ci_hi_95 - ci_lo_95) / (2 * 1.96),
    beta_sd  = if_else(is.na(beta_sd) | beta_sd <= 0, wgm * 0.3, beta_sd)
  )

message("National beta summary by type:")
print(national_betas %>% select(type, wgm, ci_lo_95, ci_hi_95, n_regions))

# Full lookup: regional beta + national summary, joined on type
beta_lookup <- regional_betas %>%
  left_join(national_betas %>%
              select(type, wgm, ci_lo_95, ci_hi_95, beta_sd),
            by = "type") %>%
  mutate(lower_b = 1e-4, upper_b = 0.05)

message("Types with calibrated betas: ",
        paste(unique(beta_lookup$type), collapse = ", "))

# =============================================================================
# 4. ASSIGN PER-HOSPITAL BETA VECTORS — 3 tiers × N_REPS draws
#
# For each tier and rep, draw one beta per hospital from truncnorm:
#   Low  : mean = ci_lo_95 of its type (lower-transmissibility scenario)
#   Mid  : mean = wgm of its type × region (central calibrated estimate)
#   High : mean = ci_hi_95 of its type (upper-transmissibility scenario)
#   SD   = (ci_hi_95 − ci_lo_95) / (2 × 1.96)  [national CI width]
#
# Hospitals whose type or region is not calibrated fall back to the
# national weighted geometric mean for their type.
# =============================================================================

# Attach calibration parameters to each hospital using hospital_type column
# (matches the `type` column in region_final_all_summaries)
hospitals_beta <- hospitals %>%
  left_join(
    regional_betas %>% rename(beta_regional = beta_regional),
    by = c("hospital_type" = "type", "region" = "region")
  ) %>%
  left_join(
    national_betas %>% select(type, wgm, ci_lo_95, ci_hi_95, beta_sd),
    by = c("hospital_type" = "type")
  ) %>%
  mutate(
    # Single mean per hospital: regional beta if available, else national wgm
    beta_mean = if_else(!is.na(beta_regional), beta_regional, wgm),
    beta_sd   = if_else(is.na(beta_sd) | beta_sd <= 0, wgm * 0.3, beta_sd),
    lower_b   = 1e-4,
    upper_b   = 0.05
  )

# Draw N_REPS independent beta_vec — one per rep, no tier structure.
# Each hospital's beta is drawn from:
#   truncnorm(mean = regional_beta (or national wgm as fallback),
#             sd   = estimated from CI width,
#             a    = 1e-4, b = 0.05)
set.seed(42)
beta_draws <- lapply(seq_len(N_REPS), function(rep_id) {
  mn <- hospitals_beta$beta_mean
  sd <- hospitals_beta$beta_sd
  lo <- hospitals_beta$lower_b
  hi <- hospitals_beta$upper_b
  mn[is.na(mn)] <- 0.004   # global fallback if type not calibrated
  sd[is.na(sd)] <- 0.001
  rtruncnorm(H, a = lo, b = hi, mean = mn, sd = sd)
})

all_draws <- unlist(beta_draws)
message("Beta draws: ", N_REPS, " vectors of length ", H)
message(sprintf("  Overall: median=%.5f  [%.5f, %.5f]",
                median(all_draws),
                quantile(all_draws, 0.025),
                quantile(all_draws, 0.975)))

# =============================================================================
# 5. NETWORK METRICS FOR SEED SELECTION
# =============================================================================

message("Computing seed metrics...")
library(igraph)

g_agg <- weekly_transfers %>%
  group_by(finess_geo_origin, finess_geo_target) %>%
  summarise(weight = sum(weight), .groups = "drop") %>%
  igraph::graph_from_data_frame(directed = TRUE)

in_deg  <- degree(g_agg, mode = "in")
out_deg <- degree(g_agg, mode = "out")
out_str <- strength(g_agg, mode = "out")
btwn    <- igraph::estimate_betweenness(g_agg, directed = TRUE, cutoff = 5)

seed_metrics <- hospitals %>%
  left_join(tibble(finess_geo  = V(g_agg)$name,
                   in_degree   = as.integer(in_deg),
                   out_degree  = as.integer(out_deg),
                   out_strength= as.numeric(out_str),
                   betweenness = as.numeric(btwn)),
            by = "finess_geo") %>%
  mutate(across(c(in_degree, out_degree, out_strength, betweenness),
                ~ replace_na(.x, 0)))

# =============================================================================
# 6. SEED PANEL — fixed network-based + type-stratified random
# =============================================================================

fixed_seeds <- bind_rows(
  seed_metrics %>% slice_max(in_degree,    n=1, with_ties=FALSE) %>%
    transmute(finess_geo, seed_rule = "highest_in_degree"),
  seed_metrics %>% slice_max(out_degree,   n=1, with_ties=FALSE) %>%
    transmute(finess_geo, seed_rule = "highest_out_degree"),
  seed_metrics %>% slice_max(betweenness,  n=1, with_ties=FALSE) %>%
    transmute(finess_geo, seed_rule = "highest_betweenness"),
  seed_metrics %>% slice_max(no_beds,      n=1, with_ties=FALSE) %>%
    transmute(finess_geo, seed_rule = "largest_beds"),
  seed_metrics %>% slice_max(out_strength, n=1, with_ties=FALSE) %>%
    transmute(finess_geo, seed_rule = "largest_outgoing")
) %>%
  # If two rules select the same hospital, collapse them
  group_by(finess_geo) %>%
  summarise(seed_rule = paste(sort(seed_rule), collapse = " + "),
            .groups = "drop") %>%
  mutate(seed_type = "fixed")

type_seeds <- tibble(
  finess_geo = NA_character_,
  seed_rule  = c("random_MCO", "random_SSR", "random_MCO_SSR"),
  seed_type  = "type_random"
)

seed_panel <- bind_rows(fixed_seeds, type_seeds)
message("Seed rules (", nrow(seed_panel), "): ",
        paste(seed_panel$seed_rule, collapse = ", "))

# Type pools for random seeds — keyed by hospital_type values
hosp_by_type <- hospitals %>%
  group_by(hospital_type) %>%
  summarise(ids = list(finess_geo), .groups = "drop") %>%
  with(setNames(ids, hospital_type))

# =============================================================================
# 7. BUILD SIMULATION GRID
# =============================================================================
sim_grid <- seed_panel %>%
  tidyr::crossing(tibble(rep_id = seq_len(N_REPS))) %>%
  mutate(
    sim_id   = row_number(),
    sim_seed = 10000L + sim_id * 7L,
    seed_hospital = mapply(
      function(stype, srule, fgeo, sseed) {
        if (stype == "fixed") return(fgeo)
        target <- switch(srule,
                         "random_MCO"     = "MCO",
                         "random_SSR"     = "SSR",
                         "random_MCO_SSR" = "MCO/SSR",
                         "Other"
        )
        pool <- hosp_by_type[[target]]
        if (is.null(pool) || length(pool) == 0) {
          message("  WARNING: no hospitals for hospital_type='", target, "' — using all")
          pool <- hospitals$finess_geo
        }
        set.seed(sseed)
        sample(pool, 1)
      },
      seed_type, seed_rule, finess_geo, sim_seed,
      SIMPLIFY = TRUE, USE.NAMES = FALSE
    )
  )
message("Total simulations: ", nrow(sim_grid),
        " (", nrow(seed_panel), " seed rules × ", N_REPS, " reps)")

# =============================================================================
# 8. SIS SIMULATION — identical structure to optim_france.R
#
# Differences from the calibration simulation:
#   • Starts with N_SEED_INF infected in one seed hospital (not 2% everywhere)
#   • pi_vec = 0: novel pathogen, no community re-seeding
#   • Returns daily time series instead of last-year incidence
#   • beta_vec varies per hospital (not uniform by type)
# =============================================================================

run_seeding_simulation <- function(seed_hospital, sim_seed, beta_vec) {
  
  set.seed(sim_seed)
  
  p_rec <- 1 - exp(-gamma)
  
  # Initialize: seed hospital gets N_SEED_INF infected, all others susceptible
  I_loc <- integer(H)
  seed_idx <- which(hospitals$finess_geo == seed_hospital)
  if (length(seed_idx) > 0)
    I_loc[seed_idx] <- min(as.integer(N_SEED_INF), beds[seed_idx])
  S_loc <- beds - I_loc
  
  daily <- vector("list", Tmax)
  
  for (t in seq_len(Tmax)) {
    
    # ── Within-hospital SIS transmission ──────────────────────────────────────
    for (i in seq_len(H)) {
      N <- S_loc[i] + I_loc[i]
      if (N <= 0) next
      p_inf   <- 1 - exp(-beta_vec[i] * I_loc[i] / N)
      new_inf <- rbinom(1L, S_loc[i], p_inf)
      recov   <- rbinom(1L, I_loc[i], p_rec)
      S_loc[i] <- S_loc[i] - new_inf + recov
      I_loc[i] <- I_loc[i] + new_inf - recov
    }
    
    # ── Patient discharges and transfers ──────────────────────────────────────
    S_stay <- S_loc;  I_stay <- I_loc
    S_tr   <- numeric(H);  I_tr <- numeric(H)
    
    handle_exit <- function(h) {
      n_exit_S <- rbinom(1L, S_loc[h], p_exit[h])
      n_exit_I <- rbinom(1L, I_loc[h], p_exit[h])
      if ((n_exit_S + n_exit_I) == 0L) return()
      S_stay[h] <<- S_stay[h] - n_exit_S
      I_stay[h] <<- I_stay[h] - n_exit_I
      p_tr_h <- p_tr[h]
      n_tr_S <- rbinom(1L, n_exit_S, p_tr_h)
      n_tr_I <- rbinom(1L, n_exit_I,
                       pmin(pmax((1 - alpha) * p_tr_h, 0), 1))
      if ((n_tr_S + n_tr_I) > 0) {
        probs <- P_tr[, h]
        if (!all(is.finite(probs))) return()
        s <- sum(probs);  if (s <= 0) return()
        probs <- probs / s
        if (n_tr_S > 0) { dS <- rmultinom(1L,n_tr_S,probs); S_tr <<- S_tr+dS[,1] }
        if (n_tr_I > 0) { dI <- rmultinom(1L,n_tr_I,probs); I_tr <<- I_tr+dI[,1] }
      }
    }
    
    for (h in seq_len(H)) handle_exit(h)
    
    # ── Community admissions — pi_vec = 0 for novel pathogen ─────────────────
    occ   <- S_stay + I_stay + S_tr + I_tr
    A     <- pmax(0L, beds - occ)
    A_I   <- rbinom(H, A, pi_vec)   # all zeros: no community reservoir
    S_loc <- S_stay + S_tr + (A - A_I)
    I_loc <- I_stay + I_tr + A_I
    
    daily[[t]] <- data.frame(
      day                  = t,
      total_infected       = sum(I_loc),
      n_hospitals_infected = sum(I_loc > 0L),
      overall_prevalence   = sum(I_loc) / sum(beds),
      pathogen_extinct     = sum(I_loc) == 0L
    )
  }
  
  do.call(rbind, daily)
}

# =============================================================================
# 9. RUN SIMULATIONS WITH PSOCK PARALLELISM
# =============================================================================

message("\nStarting cluster: ", N_CORES, " PSOCK workers...")
if (exists("cl") && inherits(cl, "cluster")) try(stopCluster(cl), silent = TRUE)
cl <- makeCluster(N_CORES, type = "PSOCK")

clusterExport(cl, varlist = c(
  "run_seeding_simulation",
  "H", "beds", "p_exit", "p_tr", "P_tr", "pi_vec",
  "gamma", "alpha", "Tmax", "N_SEED_INF",
  "hospitals", "beta_draws", "sim_grid"
))

message("Running ", nrow(sim_grid), " simulations across ", N_CORES, " cores...")
t_start <- Sys.time()

results_list <- tryCatch({
  
  parLapply(cl, seq_len(nrow(sim_grid)), function(i) {
    row    <- sim_grid[i, ]
    rep_id <- row$rep_id
    bvec   <- beta_draws[[rep_id]]
    ts     <- run_seeding_simulation(row$seed_hospital, row$sim_seed, bvec)
    ts$sim_id        <- row$sim_id
    ts$seed_rule     <- row$seed_rule
    ts$seed_type     <- row$seed_type
    ts$rep_id        <- rep_id
    ts$seed_hospital <- row$seed_hospital
    ts
  })
  
}, finally = {
  message("Stopping cluster...")
  try(stopCluster(cl), silent = TRUE)
})

t_end <- Sys.time()
message("Done. Elapsed: ", round(difftime(t_end, t_start, units = "mins"), 1), " min")

# =============================================================================
# 10. COMPILE AND SAVE
# =============================================================================

all_results <- bind_rows(results_list)

# Attach seed hospital metadata
seed_meta <- seed_metrics %>%
  select(finess_geo, hospital_type, region,
         in_degree, out_degree, betweenness, no_beds)

all_results <- all_results %>%
  left_join(seed_meta, by = c("seed_hospital" = "finess_geo"))

# Summary: peak infected, day of peak, final prevalence per simulation
sim_summary <- all_results %>%
  group_by(sim_id, seed_rule, seed_type, rep_id,
           seed_hospital, hospital_type, region) %>%
  summarise(
    peak_infected        = max(total_infected),
    day_of_peak          = day[which.max(total_infected)],
    peak_hospitals       = max(n_hospitals_infected),
    final_infected       = total_infected[day == max(day)],
    final_prevalence     = overall_prevalence[day == max(day)],
    ever_extinct         = any(pathogen_extinct),
    day_extinct          = if (any(pathogen_extinct))
      min(day[pathogen_extinct]) else NA_integer_,
    .groups              = "drop"
  )

# Save
run_date <- format(Sys.Date(), "%Y%m%d")
run_date
saveRDS(all_results,
        file.path(OUT_DIR, paste0("seeding_timeseries_", run_date, ".rds")))
saveRDS(sim_summary,
        file.path(OUT_DIR, paste0("seeding_summary_",    run_date, ".rds")))
write.csv2(sim_summary,
           file.path(OUT_DIR, paste0("seeding_summary_", run_date, ".csv")),
           row.names = FALSE)

message("\n=== DONE ===")
message("Time series saved to: seeding_timeseries_", run_date, ".rds")
message("Summary saved to:     seeding_summary_",    run_date, ".rds")
message("Total rows in results: ", nrow(all_results))
message("Simulations completed: ", length(unique(all_results$sim_id)))

# Quick preview
cat("\n=== SIMULATION SUMMARY (first 10 rows) ===\n")
print(head(sim_summary %>% arrange(seed_rule, rep_id), 10))

source("analyze_seeding_results.R")
