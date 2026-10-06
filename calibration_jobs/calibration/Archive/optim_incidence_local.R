# =============================================================================
# optim_prevalence_local.R  —  Novel pathogen incidence calibration (LOCAL)
# =============================================================================
# DESIGN
# ──────
# Novel pathogen introduction: pi_vec = 0, seeded in one hospital.
# Spreads within hospitals (beta) and between hospitals (transfer matrix).
# Pathogen either goes extinct or reaches network-level endemicity.
# Incidence is tracked daily per 1,000 patient-days (LOS-based denominator).
# SS detection uses CV of daily incidence in the last 30 days.
#
# TRANSFER MATRIX EXTENSION
# ─────────────────────────
# The annual average P_tr is cycled N_CYCLES times so the simulation can
# run beyond the observation window. Tmax = N_CYCLES x 365 days.
# Adjust N_CYCLES to extend (e.g. 2 = 730 days, 3 = 1095 days).
#
# INCIDENCE DENOMINATOR
# ─────────────────────
# Daily incidence = new_infections / sum(S_loc + I_loc) x 1000
# i.e. per 1,000 patient-days (occupied beds that day), not per total beds.
# This matches how SPARES and ECDC HAI report hospital-acquired incidence.
#
# GRID SEARCH
# ───────────
# For each beta, multiple reps are run. Some reps go extinct (incidence -> 0),
# others establish endemicity. The analysis separates these and maps the
# endemic incidence to Low / Mid / High tiers.
# =============================================================================

library(parallel)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# =============================================================================
# 0. PATHS
# =============================================================================

LOCAL_ROOT <- "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/calibration_jobs"
DATA_DIR   <- file.path(LOCAL_ROOT, "data")
OUT_DIR    <- file.path(LOCAL_ROOT, "Outputs", "prevalence", "local")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

JOB_INDEX <- 1L

# =============================================================================
# 1. SETTINGS
# =============================================================================

# Beta grid
beta_grid    <- seq(0.005, 0.03, by = 0.002)  # 13 values (cluster: by=0.001)
N_REP        <- 10

# Simulation length — controlled by N_CYCLES x 365
# The annual average P_tr is cycled N_CYCLES times.
# N_CYCLES = 2 gives 730 days, 3 gives 1095 days, etc.
N_CYCLES     <- 2L                         # adjust here to extend run
DAYS_PER_CYCLE <- 365L
Tmax         <- N_CYCLES * DAYS_PER_CYCLE  # 730 days

# Steady-state detection (last SS_WINDOW days of simulation)
SS_WINDOW    <- 30L    # days used for SS check at end of sim
SS_CV_THRESH <- 0.15   # CV of daily incidence < 15% = stable
# (higher than prevalence-based 10% because
# incidence is noisier than prevalence)
SS_PROP_HOSP <- 0.80   # not used for network-level SS check
# (kept for potential per-hospital extension)

# Incidence calculation
INC_WINDOW   <- 180L   # lookback window for reported incidence (last 180 days)
# shorter than Tmax to measure endemic level, not
# including early transient growth phase

# Pathogen parameters
INIT_INF     <- 5L     # number of index cases seeded on day 0
# placed in the hospital with highest out-strength
gamma        <- 1 / 387  # daily decolonisation rate (ESBL-E mean ~387 days)
# biological clearance only — not discharge
alpha        <- 0        # no isolation of colonised transfers
pi_vec_val   <- 0        # no community importation (novel pathogen)

# Extinction threshold: if total infected < EXTINCT_THRESH for EXTINCT_DAYS
# consecutive days, flag as extinct (early termination optional)
EXTINCT_THRESH <- 1L

# Parallel
N_CORES      <- max(1L, parallel::detectCores() - 2L)
seed_base    <- 10000L + (JOB_INDEX - 1L) * 100000L

message("=== ARCANE NOVEL PATHOGEN INCIDENCE CALIBRATION (LOCAL) ===")
message("Beta    : ", min(beta_grid), " to ", max(beta_grid),
        " (", length(beta_grid), " values)")
message("Reps    : ", N_REP, " | Cores: ", N_CORES)
message("Tmax    : ", Tmax, " days (", N_CYCLES, " x ", DAYS_PER_CYCLE, "d cycles)")
message("pi_vec  : ", pi_vec_val, " (novel pathogen)")
message("Seed    : ", INIT_INF, " index case(s) in highest out-strength hospital")

# =============================================================================
# 2. INCIDENCE TARGETS — three tiers (ECDC HAI + SPARES benchmarks)
# =============================================================================
# Targeting nationwide incidence per 1,000 patient-days (LOS-based).
# Directly comparable to SPARES and ECDC HAI surveillance reports.
#
# At SIS steady state: incidence ~ gamma x prevalence x 1000 = 2.58 x prev
# so 1% prevalence ~ 0.026 per 1,000 patient-days.
#
# Low  (0.03-0.15): CRE in France/Germany (~0.03-0.08),
#                   VRE in Scandinavia (~0.03-0.10),
#                   CPE in low-burden EU (~0.02-0.12)
#
# Mid  (0.15-0.60): ESBL-E in France/UK (~0.30-0.55),
#                   MRSA moderate settings (~0.15-0.40),
#                   VRE medium-burden EU ICUs (~0.20-0.50)
#
# High (0.60-1.50): MRSA Southern/Eastern EU (~0.70-1.20),
#                   ESBL-E high-burden (~0.60-1.00),
#                   CRAB Mediterranean ICUs (~0.80-1.50)
#
# Sources: ECDC HAI 2022; SPARES France 2020-2023; ECDC EARS-Net 2022.
# =============================================================================

inc_targets <- tribble(
  ~tier,  ~inc_low, ~inc_mid, ~inc_high,
  ~ecdc_examples,
  "Low",  0.03, 0.08, 0.15,
  "CRE in France/Germany, VRE in Scandinavia, CPE low-burden EU",
  "Mid",  0.15, 0.35, 0.60,
  "ESBL-E in France/UK, MRSA Germany/Belgium, VRE medium-burden EU",
  "High", 0.60, 0.90, 1.50,
  "MRSA Greece/Romania, ESBL-E Southern/Eastern EU, CRAB Mediterranean"
)

message("\nIncidence targets (per 1,000 patient-days):")
for (i in seq_len(nrow(inc_targets)))
  message(sprintf("  %-6s %.2f-%.2f /1k pd  (mid %.2f)  [%s]",
                  inc_targets$tier[i],
                  inc_targets$inc_low[i], inc_targets$inc_high[i],
                  inc_targets$inc_mid[i],
                  inc_targets$ecdc_examples[i]))

# =============================================================================
# 3. DATA LOADING — identical to optim_france.R
# =============================================================================

message("\nLoading data from: ", DATA_DIR)
if (!file.exists(file.path(DATA_DIR, "weekly.RDS")))
  stop("weekly.RDS not found in ", DATA_DIR)

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
  summarise(pt  = sum(pt_days_total, na.rm = TRUE),
            pat = sum(patient_total,  na.rm = TRUE), .groups = "drop") %>%
  mutate(los_type = pt / pat)
DEFAULT_LOS_TYPE   <- setNames(default_los$los_type, default_los$hospital_type)
GLOBAL_DEFAULT_LOS <- with(
  facility_level %>% filter(!is.na(hospital_type)) %>%
    summarise(a = sum(pt_days_total, na.rm = TRUE),
              b = sum(patient_total,  na.rm = TRUE)), a / b)

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
                                       as.integer(round(mean(no_beds, na.rm = TRUE))),
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
  transmute(origin = as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out = sum(weight, na.rm = TRUE), .groups = "drop")
hospitals <- hospitals %>%
  left_join(transfer_out, by = c("finess_geo" = "origin")) %>%
  mutate(total_out = replace(total_out, is.na(total_out), 0))
p_tr <- pmin(hospitals$total_out / pmax(p_exit * beds, 1), 0.60)

# Build P_tr
message("Building P_tr (", H, " x ", H, ")...")
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

# Transfer-eligible hospitals: p_tr > 0 AND valid destinations
transfer_idx <- which(p_tr > 0 & colSums(P_tr) > 0)
message("  Done. H = ", H,
        " | Beds: ", format(total_beds_sum, big.mark = ","),
        " | Transfer-eligible: ", length(transfer_idx))

# Seed hospital: highest out-transfer strength (most connected sender)
seed_hospital_idx <- which.max(hospitals$total_out)
message("Seed hospital: ", hospitals$finess_geo[seed_hospital_idx],
        " (", hospitals$hospital_type[seed_hospital_idx], ", ",
        hospitals$region[seed_hospital_idx], ")",
        " | out-strength: ", round(hospitals$total_out[seed_hospital_idx], 1))

# =============================================================================
# 5. SIMULATION FUNCTION
# =============================================================================
# KEY DESIGN NOTES:
# - pi_vec = 0: pathogen spreads ONLY via within-hospital transmission
#               and inter-hospital transfers — no external importation
# - Tmax = N_CYCLES x 365: P_tr (annual average) reused cyclically
# - Incidence denominator: sum(S_loc + I_loc) = total patient-days that day
#   This is LOS-based: a hospital with longer LOS has more patient-days
#   and contributes more to the denominator (correctly lowering incidence)
# - Full daily incidence trajectory saved for plotting
# - SS check: CV of daily network incidence in last SS_WINDOW days < 15%
# =============================================================================

run_novel_simulation <- function(beta, seed) {
  
  set.seed(seed)
  p_rec    <- 1 - exp(-gamma)   # daily biological decolonisation probability
  beta_vec <- rep(beta, H)
  
  # Initialise: seed INIT_INF infected patients in the most-connected hospital
  I_loc    <- integer(H)
  I_loc[seed_hospital_idx] <- min(INIT_INF, beds[seed_hospital_idx])
  S_loc    <- beds - I_loc
  
  # Daily trajectory containers — length Tmax
  inc_daily     <- numeric(Tmax)  # new infections per day (network total)
  pt_days_daily <- numeric(Tmax)  # patient-days per day (occupied beds)
  net_prev_daily<- numeric(Tmax)  # network-wide prevalence per day
  
  for (t in seq_len(Tmax)) {
    
    # ── Step 1: within-hospital SIS transmission (vectorised) ────────────────
    N       <- S_loc + I_loc
    p_inf   <- ifelse(N > 0L & is.finite(beta_vec),
                      1 - exp(-beta_vec * I_loc / pmax(N, 1L)), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)   # biological clearance only
    S_loc   <- S_loc - new_inf + recov
    I_loc   <- I_loc + new_inf - recov
    
    # Record daily incidence — denominator is patient-days (occupied beds)
    occ_today        <- S_loc + I_loc
    inc_daily[t]     <- sum(new_inf)
    pt_days_daily[t] <- sum(occ_today)
    net_prev_daily[t]<- sum(I_loc) / max(total_beds_sum, 1)
    
    # ── Step 2: discharges (vectorised) ──────────────────────────────────────
    n_exit_S <- rbinom(H, S_loc, p_exit)
    n_exit_I <- rbinom(H, I_loc, p_exit)
    S_loc    <- S_loc - n_exit_S
    I_loc    <- I_loc - n_exit_I
    
    # ── Step 3: inter-hospital transfers (sparse loop) ───────────────────────
    S_tr <- numeric(H); I_tr <- numeric(H)
    active_h <- transfer_idx[(n_exit_S[transfer_idx] +
                                n_exit_I[transfer_idx]) > 0L]
    for (h in active_h) {
      nS    <- rbinom(1L, n_exit_S[h], p_tr[h])
      nI    <- rbinom(1L, n_exit_I[h], p_tr[h])
      if ((nS + nI) == 0L) next
      probs <- P_tr[, h]
      s     <- sum(probs); if (s <= 0) next
      if (nS > 0L) S_tr <- S_tr + rmultinom(1L, nS, probs)[, 1L]
      if (nI > 0L) I_tr <- I_tr + rmultinom(1L, nI, probs)[, 1L]
    }
    
    # ── Step 4: admissions — pi_vec = 0, only susceptibles admitted ──────────
    occ   <- S_loc + I_loc + S_tr + I_tr
    A     <- pmax(0L, beds - occ)
    A_I   <- rbinom(H, A, pi_vec)   # all zeros: no community reservoir
    S_loc <- S_loc + S_tr + (A - A_I)
    I_loc <- I_loc + I_tr + A_I
  }
  
  # ── Compute daily incidence rate (per 1,000 patient-days) ─────────────────
  # Avoid division by zero on days with no occupancy
  inc_rate_daily <- ifelse(pt_days_daily > 0,
                           inc_daily / pt_days_daily * 1000,
                           0)
  
  # ── Reported incidence: mean over last INC_WINDOW days ───────────────────
  inc_window_days <- tail(seq_len(Tmax), INC_WINDOW)
  reported_inc    <- sum(inc_daily[inc_window_days]) /
    max(sum(pt_days_daily[inc_window_days]), 1) * 1000
  
  # ── Steady-state check: CV of daily incidence in last SS_WINDOW days ──────
  ss_days    <- tail(seq_len(Tmax), SS_WINDOW)
  ss_inc     <- inc_rate_daily[ss_days]
  ss_mean    <- mean(ss_inc)
  ss_cv      <- if (ss_mean > 0) sd(ss_inc) / ss_mean else Inf
  ss_reached <- ss_cv < SS_CV_THRESH
  
  # ── Extinction: pathogen gone by end of sim ───────────────────────────────
  extinct <- sum(I_loc) == 0L
  
  list(
    beta               = beta,
    seed               = seed,
    reported_inc       = reported_inc,      # per 1,000 pd — main output
    net_prev_final     = sum(I_loc) / total_beds_sum,
    steady_state_cv    = round(ss_cv, 4),
    steady_state_reached = ss_reached,
    extinct            = extinct,
    inc_rate_daily     = inc_rate_daily,    # full 730-day trajectory
    net_prev_daily     = net_prev_daily,    # full 730-day prevalence trajectory
    type_inc_mean      = tapply(             # per-type mean incidence
      inc_daily[inc_window_days] /
        pmax(pt_days_daily[inc_window_days] / H, 1) * 1000,
      type_etab, mean, na.rm = TRUE)
  )
}

# =============================================================================
# 6. RUN GRID — PSOCK (Windows)
# =============================================================================

sim_grid <- expand.grid(beta = beta_grid, rep_id = seq_len(N_REP)) %>%
  mutate(sim_seed = seed_base + row_number() * 7L)

message("\nStarting ", N_CORES, " PSOCK workers...")
message("Simulations: ", nrow(sim_grid),
        " (", length(beta_grid), " betas x ", N_REP, " reps) | Tmax = ", Tmax, "d")

cl <- makeCluster(N_CORES, type = "PSOCK")
clusterExport(cl, varlist = c(
  "run_novel_simulation",
  "H", "beds", "p_exit", "p_tr", "P_tr", "pi_vec", "gamma",
  "transfer_idx", "type_etab", "total_beds_sum",
  "seed_hospital_idx", "INIT_INF",
  "Tmax", "SS_WINDOW", "SS_CV_THRESH", "INC_WINDOW",
  "sim_grid"
))

t0 <- Sys.time()
all_results <- tryCatch(
  parLapply(cl, seq_len(nrow(sim_grid)), function(i) {
    row <- sim_grid[i, ]
    run_novel_simulation(beta = row$beta, seed = row$sim_seed)
  }),
  finally = { try(stopCluster(cl), silent = TRUE) }
)
elapsed <- round(difftime(Sys.time(), t0, units = "mins"), 1)
message("Elapsed: ", elapsed, " min")

# =============================================================================
# 7. COMPILE
# =============================================================================

scalar_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  data.frame(
    job_index            = JOB_INDEX,
    beta_within          = r$beta,
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

# Full daily trajectory (incidence rate and prevalence)
traj_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r   <- all_results[[i]]
  len <- length(r$inc_rate_daily)
  tibble(
    beta_within    = r$beta,
    rep_id         = sim_grid$rep_id[i],
    sim_id         = i,
    extinct        = r$extinct,
    day            = seq_len(len),
    inc_rate       = r$inc_rate_daily,
    net_prev       = r$net_prev_daily
  )
}))

# Per-beta summary
beta_summary <- scalar_df %>%
  group_by(beta_within) %>%
  summarise(
    n_reps        = n(),
    pct_extinct   = round(mean(extinct) * 100, 1),
    pct_ss        = round(mean(steady_state_reached & !extinct) * 100, 1),
    inc_median    = round(median(reported_inc[!extinct], na.rm = TRUE), 4),
    inc_q25       = round(quantile(reported_inc[!extinct], 0.25, na.rm = TRUE), 4),
    inc_q75       = round(quantile(reported_inc[!extinct], 0.75, na.rm = TRUE), 4),
    prev_median   = round(median(net_prev_final[!extinct], na.rm = TRUE), 4),
    mean_ss_cv    = round(mean(steady_state_cv[!extinct], na.rm = TRUE), 4),
    .groups = "drop"
  )

cat("\n=== BETA SUMMARY ===\n")
print(beta_summary, n = Inf)

# =============================================================================
# 8. INCIDENCE-BASED TIER MAPPING
# =============================================================================

# For each (beta, rep) find which tier its reported_inc falls into
# Only non-extinct reps are eligible
scalar_eligible <- scalar_df %>% filter(!extinct, reported_inc > 0)

scalar_with_tier <- scalar_eligible %>%
  tidyr::crossing(inc_targets %>%
                    select(tier, inc_low, inc_mid, inc_high)) %>%
  filter(reported_inc >= inc_low & reported_inc <= inc_high)

tier_analysis <- scalar_with_tier %>%
  group_by(tier, inc_low, inc_mid, inc_high) %>%
  summarise(
    n_qualifying  = n(),
    n_betas       = n_distinct(beta_within),
    best_beta     = beta_within[which.min(abs(reported_inc - inc_mid))],
    mean_beta     = round(mean(beta_within),           6),
    median_beta   = round(median(beta_within),         6),
    ci_lo_95      = round(quantile(beta_within, 0.025),6),
    ci_hi_95      = round(quantile(beta_within, 0.975),6),
    mean_inc_ach  = round(mean(reported_inc), 4),
    .groups = "drop"
  ) %>%
  left_join(inc_targets %>%
              select(tier, ecdc_examples), by = "tier") %>%
  mutate(tier = factor(tier, levels = c("Low","Mid","High"))) %>%
  arrange(tier)

cat("\n=== TIER ANALYSIS (incidence-based) ===\n")
print(tier_analysis %>%
        select(tier, inc_low, inc_high, n_qualifying, n_betas,
               best_beta, mean_beta, ci_lo_95, ci_hi_95, mean_inc_ach),
      n = Inf)

cat("\n=== RECOMMENDED BETA VALUES ===\n")
for (i in seq_len(nrow(tier_analysis))) {
  r <- tier_analysis[i, ]
  cat(sprintf(
    "  %-6s (%.2f-%.2f /1k pd): best=%.4f | mean=%.4f [95%% CI: %.4f-%.4f]\n",
    as.character(r$tier),
    r$inc_low, r$inc_high,
    r$best_beta, r$mean_beta, r$ci_lo_95, r$ci_hi_95
  ))
}

# =============================================================================
# 9. PLOTS
# =============================================================================

tier_pal  <- c(Low = "#009E73", Mid = "#E69F00", High = "#D55E00")
tier_pal2 <- c(Low = "#b2dfdb", Mid = "#ffe0b2", High = "#ffccbc")

# ── Plot 1: Beta vs incidence curve with tier range bands ────────────────────
p1 <- ggplot(beta_summary %>% filter(!is.na(inc_median)),
             aes(x = beta_within)) +
  geom_rect(data = inc_targets,
            aes(xmin = -Inf, xmax = Inf,
                ymin = inc_low, ymax = inc_high, fill = tier),
            alpha = 0.15, inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = inc_q25, ymax = inc_q75),
              fill = "#56B4E9", alpha = 0.4) +
  geom_line(aes(y = inc_median),  colour = "#0072B2", linewidth = 1.3) +
  geom_point(aes(y = inc_median), colour = "#0072B2", size = 3) +
  geom_hline(data = inc_targets,
             aes(yintercept = inc_mid, colour = tier),
             linetype = "dashed", linewidth = 0.9) +
  geom_label(data = inc_targets,
             aes(x = min(beta_summary$beta_within, na.rm = TRUE),
                 y = inc_mid,
                 label = paste0(tier, " (", inc_low, "-", inc_high, ")"),
                 colour = tier),
             hjust = 0, size = 3.2, show.legend = FALSE) +
  scale_fill_manual(values = tier_pal2, name = "Tier range") +
  scale_colour_manual(values = tier_pal, name = "Tier midpoint") +
  scale_x_continuous(labels = function(x) sprintf("%.3f", x)) +
  labs(title    = "beta vs endemic incidence (non-extinct reps only)",
       subtitle = "Median +/- IQR | Shaded = tier ranges | Dashed = midpoints",
       x = "beta (per day)", y = "Incidence (per 1,000 patient-days)") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# ── Plot 2: Extinction probability by beta ────────────────────────────────────
p2 <- beta_summary %>%
  ggplot(aes(x = beta_within, y = pct_extinct, fill = pct_extinct)) +
  geom_col(width = diff(range(beta_summary$beta_within)) /
             length(unique(beta_summary$beta_within)) * 0.8) +
  geom_text(aes(label = paste0(pct_extinct, "%")),
            vjust = -0.4, size = 3.3) +
  scale_fill_gradient(high = "#e74c3c", low = "#2ecc71", guide = "none") +
  scale_x_continuous(labels = function(x) sprintf("%.3f", x)) +
  scale_y_continuous(limits = c(0, 112),
                     expand = expansion(mult = c(0, 0))) +
  labs(title    = "Extinction probability by beta",
       subtitle = "Novel pathogen with pi_vec = 0 — lower beta = higher extinction risk",
       x = "beta (per day)", y = "% simulations going extinct") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# ── Plot 3: Full incidence trajectory — one line per simulation ───────────────
# Assign tier label to each simulation based on reported_inc
traj_with_tier <- traj_df %>%
  left_join(scalar_df %>% select(beta_within, rep_id, reported_inc, extinct),
            by = c("beta_within","rep_id")) %>%
  left_join(
    scalar_with_tier %>% select(beta_within, rep_id, tier) %>% distinct(),
    by = c("beta_within","rep_id")
  ) %>%
  mutate(
    tier    = if_else(extinct, "Extinct", as.character(tier)),
    tier    = replace_na(tier, "No tier"),
    sim_key = paste0(beta_within, "_", rep_id)
  )

# Rolling 7-day mean for smoother lines
traj_smooth <- traj_with_tier %>%
  arrange(sim_key, day) %>%
  group_by(sim_key, tier, beta_within, rep_id, extinct) %>%
  mutate(inc_7d = zoo::rollmean(inc_rate, k = 7, fill = NA, align = "right")) %>%
  ungroup()

p3_colours <- c(tier_pal, Extinct = "grey70", `No tier` = "grey85")

p3 <- ggplot(traj_smooth %>% filter(!is.na(inc_7d)),
             aes(x = day, y = inc_7d,
                 group = sim_key, colour = tier)) +
  geom_line(alpha = 0.25, linewidth = 0.4) +
  geom_vline(xintercept = Tmax - INC_WINDOW,
             linetype = "dashed", colour = "grey30", linewidth = 0.7) +
  annotate("text", x = Tmax - INC_WINDOW + 5,
           y = max(traj_smooth$inc_7d, na.rm = TRUE) * 0.9,
           label = "Incidence\nwindow start",
           hjust = 0, size = 3, colour = "grey30") +
  scale_colour_manual(values = p3_colours, name = "Tier") +
  labs(title    = "Daily incidence trajectory — all simulations",
       subtitle = paste0("7-day rolling mean | One line per simulation | ",
                         Tmax, "-day run | Dashed = start of incidence measurement window"),
       x = "Day", y = "Incidence (per 1,000 patient-days, 7d mean)") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# ── Plot 4: Beta CI per tier ──────────────────────────────────────────────────
p4 <- tier_analysis %>%
  ggplot(aes(x = tier, colour = tier)) +
  geom_linerange(aes(ymin = ci_lo_95, ymax = ci_hi_95),
                 linewidth = 3, alpha = 0.4) +
  geom_point(aes(y = mean_beta),   size = 6, shape = 18) +
  geom_point(aes(y = best_beta),   size = 3, shape = 1, colour = "grey30") +
  geom_point(aes(y = median_beta), size = 3, shape = 5, colour = "grey40") +
  scale_colour_manual(values = tier_pal, guide = "none") +
  scale_y_continuous(labels = scientific) +
  labs(title    = "Beta distribution per incidence tier",
       subtitle = "Diamond = mean | Circle = best-fit | Pentagon = median | Bar = 95% CI",
       x = "Incidence tier", y = "beta (per day)") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# Save plots (zoo required for rolling mean)
if (!requireNamespace("zoo", quietly = TRUE)) install.packages("zoo")

ggsave(file.path(OUT_DIR, "01_beta_vs_incidence.png"),   p1, width=12, height=6, dpi=150)
ggsave(file.path(OUT_DIR, "02_extinction_by_beta.png"),  p2, width=10, height=5, dpi=150)
ggsave(file.path(OUT_DIR, "03_incidence_trajectory.png"),p3, width=12, height=6, dpi=150)
ggsave(file.path(OUT_DIR, "04_beta_ci_per_tier.png"),    p4, width=8,  height=6, dpi=150)
message("4 plots saved.")

# =============================================================================
# 10. SAVE
# =============================================================================

saveRDS(list(
  scalar_df     = scalar_df,
  traj_df       = traj_df,
  beta_summary  = beta_summary,
  tier_analysis = tier_analysis,
  inc_targets   = inc_targets,
  job_index     = JOB_INDEX,
  datetime      = Sys.time(),
  elapsed_min   = as.numeric(elapsed),
  params        = list(
    beta_grid    = beta_grid, N_REP = N_REP,
    Tmax         = Tmax, N_CYCLES = N_CYCLES,
    SS_WINDOW    = SS_WINDOW, INC_WINDOW = INC_WINDOW,
    pi_vec       = pi_vec_val, gamma = gamma,
    H = H, total_beds = total_beds_sum,
    seed_hospital = hospitals$finess_geo[seed_hospital_idx]
  )
), file.path(OUT_DIR, "results_local.rds"))

write.csv2(beta_summary,  file.path(OUT_DIR, "beta_summary.csv"),  row.names = FALSE)
write.csv2(tier_analysis, file.path(OUT_DIR, "tier_analysis.csv"), row.names = FALSE)

cat("\n=== DONE ===\n")
cat("H            :", H, "hospitals |",
    format(total_beds_sum, big.mark = ","), "beds\n")
cat("Elapsed      :", elapsed, "min\n")
cat("Extinct      :", round(mean(scalar_df$extinct)*100, 1), "% of all reps\n")
cat("SS reached   :", round(mean(scalar_df$steady_state_reached)*100, 1),
    "% of all reps\n")
cat("Inc range    :", round(min(scalar_df$reported_inc[!scalar_df$extinct]), 4),
    "-", round(max(scalar_df$reported_inc[!scalar_df$extinct]), 4),
    "/1k pd (non-extinct)\n")
cat("Saved to     :", OUT_DIR, "\n")