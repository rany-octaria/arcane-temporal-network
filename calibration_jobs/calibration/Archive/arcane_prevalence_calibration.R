# =============================================================================
# arcane_prevalence_calibration.R
# =============================================================================
# WHAT THIS DOES
# ─────────────────────────────────────────────────────────────────────────────
# Grid search over β to find the value that simultaneously achieves:
#
#   Target 1 — FACILITY-LEVEL INCIDENCE
#     Mean incidence per 1,000 bed-days (365-day lookback) by facility type
#     matches the AMR literature target (Low / Mid / High tier)
#
#   Target 2 — STEADY STATE
#     Cross-sectional prevalence (I/N per hospital) is stable within the
#     last 60 days for ≥80% of hospitals that have at least one case.
#     Stability criterion: CV of daily prevalence over 60 days < 10%.
#
#   Output also includes nationwide incidence (per 1,000 bed-days).
#
# DATA — identical loading to optim_france.R
#   weekly.RDS             → transfer network
#   facility_level_final.RDS → beds, LOS, type, region
#
# BETA GRID
#   seq(0.005, 0.03, by = 0.001) — 26 values, 50 reps each
#
# SIMULATION
#   Same vectorised SIS + sparse transfer loop as optim_france.R (v2)
#   Runs up to MAX_DAYS = 1095 (3 years)
#   Stops early if BOTH conditions are met (SS detected + ≥365 days elapsed)
# =============================================================================

library(parallel)
library(dplyr)
library(tidyr)
library(truncnorm)

setwd("C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/calibration_jobs")

DATA_DIR <- file.path(getwd(), "data")
OUT_DIR  <- file.path(dirname(getwd()), "Outputs", "prevalence_calibration")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 0. LOCAL SETTINGS
# =============================================================================

# ── Beta grid ─────────────────────────────────────────────────────────────────
# Full grid (cluster): seq(0.005, 0.03, by = 0.001) — 26 values × 100 reps
# Local coarser grid: 13 values × 5 reps = 65 simulations — ~20–40 min
beta_grid      <- seq(0.005, 0.03, by = 0.002)   # 13 values

JOB_INDEX <- { v <- suppressWarnings(as.integer(Sys.getenv("jobindex")))
               if (!is.na(v) && v > 0L) v else 1L }

# ── Beta grid ─────────────────────────────────────────────────────────────────
# Full grid (cluster): seq(0.005, 0.03, by = 0.001) — 26 values
# Local coarser grid: 13 values — use for testing
beta_grid      <- seq(0.005, 0.03, by = 0.002)   # 13 values (local)
# beta_grid   <- seq(0.005, 0.03, by = 0.001)   # 26 values (cluster)

# Cluster: 10 jobs × 3 reps = 30 total reps per beta (sufficient, see math)
N_REP          <- 3       # reps per job; total = N_REP × 10 jobs = 30
MAX_DAYS       <- 730L    # 2-year run (always runs to end — no early stopping)
SS_WINDOW      <- 30L     # last 30 days used for SS boolean at end of sim
SS_CV_THRESH   <- 0.10    # CV < 10% = stable
SS_PROP_HOSP   <- 0.80    # fraction of case-bearing hospitals that must be stable
INC_WINDOW     <- 180L    # 6-month lookback for incidence (cluster: 365)
N_CORES        <- max(1L, parallel::detectCores() - 2L)

gamma      <- 1 / 387   #Carriage clearance
alpha      <- 0 
pi_vec_val <- 0.01 #Admission prevalence

# Seed offset by JOB_INDEX so all 10 jobs are independent
seed_base <- 10000L + (JOB_INDEX - 1L) * 100000L

message("=== ARCANE PREVALENCE CALIBRATION (LOCAL) ===")
message("Job: ", JOB_INDEX, " | Reps this job: ", N_REP,
        " | Beta grid: ", min(beta_grid), "–", max(beta_grid),
        " (", length(beta_grid), " values)")
message("Total reps across 10 jobs: ", N_REP * 10,
        " | Max days: ", MAX_DAYS, " | SS check: last ", SS_WINDOW, " days")

# =============================================================================
# 1. AMR PREVALENCE TARGETS (ECDC EARS-Net benchmarks)
# =============================================================================
# Three tiers (Low / Mid / High) per pathogen.
# We calibrate β to hit each tier's target FACILITY-LEVEL PREVALENCE,
# then check that the 365-day incidence rate is also biologically reasonable.
# =============================================================================

amr_targets <- tribble(
  ~pathogen,   ~prev_low, ~prev_mid, ~prev_high,
  "MRSA",       0.010,     0.115,     0.250,
  "VRE",        0.005,     0.080,     0.180,
  "ESBL-E",     0.020,     0.120,     0.300,
  "CRE",        0.002,     0.030,     0.100,
  "CRAB",       0.010,     0.070,     0.200
)

amr_long <- amr_targets %>%
  pivot_longer(starts_with("prev_"),
               names_to    = "tier",
               names_prefix = "prev_",
               values_to   = "target_prevalence") %>%
  mutate(tier = factor(tier, c("low","mid","high"),
                        labels = c("Low","Mid","High")))

cat("\n=== AMR PREVALENCE TARGETS ===\n")
print(amr_long, n = Inf)

# =============================================================================
# 2. DATA LOADING — identical to optim_france.R
# =============================================================================

message("\nLoading transfer network...")
weekly_transfers <- readRDS(file.path(DATA_DIR, "weekly.RDS")) %>%
  mutate(weight = pmax(1L, as.integer(round(weight / 7))))

message("Loading facility data...")
facility_level <- readRDS(file.path(DATA_DIR, "facility_level_final.RDS")) %>%
  mutate(finess_geo = as.character(finess_geo)) %>%
  rename(incidence_esbl_all = incidence_region_type_ESBL_all)

# =============================================================================
# 3. HOSPITAL UNIVERSE — identical to optim_france.R
# =============================================================================

default_los <- facility_level %>%
  filter(!is.na(hospital_type)) %>%
  group_by(hospital_type) %>%
  summarise(pt_days = sum(pt_days_total, na.rm=TRUE),
            pat     = sum(patient_total,  na.rm=TRUE), .groups="drop") %>%
  mutate(los_type = pt_days / pat)
DEFAULT_LOS_TYPE   <- setNames(default_los$los_type, default_los$hospital_type)
GLOBAL_DEFAULT_LOS <- with(
  facility_level %>% filter(!is.na(hospital_type)) %>%
    summarise(a=sum(pt_days_total,na.rm=TRUE), b=sum(patient_total,na.rm=TRUE)),
  a/b)

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
    los           = coalesce(los, DEFAULT_LOS_TYPE[hospital_type], GLOBAL_DEFAULT_LOS),
    type_spares   = if_else(is.na(type_spares),   "Unknown", type_spares),
    hospital_type = if_else(is.na(hospital_type), "Unknown", hospital_type),
    region        = if_else(is.na(region),         "Unknown", region)
  )

H      <- nrow(hospitals)
beds   <- hospitals$no_beds
p_exit <- 1 / hospitals$los
pi_vec <- rep(pi_vec_val, H)
message("Hospitals: ", H, " | Total beds: ", format(sum(beds), big.mark=","))

# Transfer out-degree
transfer_out_df <- weekly_transfers %>%
  transmute(origin=as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out=sum(weight,na.rm=TRUE), .groups="drop")
hospitals <- hospitals %>%
  left_join(transfer_out_df, by=c("finess_geo"="origin")) %>%
  mutate(total_out=replace(total_out, is.na(total_out), 0))
p_tr <- pmin(hospitals$total_out / pmax(p_exit * beds, 1), 0.60)

# P_tr matrix
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
message("  Done. Transfer-eligible: ", sum(cs>0))

# Pre-cache hospitals with non-zero p_tr
transfer_idx   <- which(p_tr > 0)
type_etab      <- hospitals$type_spares
total_beds_sum <- sum(beds)

# =============================================================================
# LOCAL SUBSET — keep ~300 hospitals to make the run feasible
# Remove this entire block before running on the cluster.
# =============================================================================
set.seed(1)
keep  <- hospitals %>% group_by(type_spares) %>% slice(1) %>% ungroup()
extra <- hospitals %>% anti_join(keep, by="finess_geo") %>%
  sample_n(min(290, nrow(.)))
hospitals_sub   <- bind_rows(keep, extra)
keep_idx        <- which(hospitals$finess_geo %in% hospitals_sub$finess_geo)

# Subset all simulation vectors
hospitals       <- hospitals_sub
H               <- nrow(hospitals)
beds            <- beds[keep_idx]
p_exit          <- p_exit[keep_idx]
p_tr            <- p_tr[keep_idx]
pi_vec          <- pi_vec[keep_idx]
P_tr            <- P_tr[keep_idx, keep_idx]
# Re-normalise P_tr columns after subsetting
cs2 <- colSums(P_tr)
for (h in seq_len(H)) if (cs2[h] > 0) P_tr[, h] <- P_tr[, h] / cs2[h]

transfer_idx    <- which(p_tr > 0)
type_etab       <- hospitals$type_spares
total_beds_sum  <- sum(beds)

# Subset transfers for weekly_transfers (needed for transfer_agg rebuild if used)
weekly_transfers <- weekly_transfers %>%
  filter(as.character(finess_geo_origin) %in% hospitals$finess_geo,
         as.character(finess_geo_target) %in% hospitals$finess_geo)

message("LOCAL SUBSET: ", H, " hospitals | ",
        format(sum(beds), big.mark=","), " beds")

# =============================================================================
# 4. SIMULATION FUNCTION — vectorised SIS + steady-state detection
# =============================================================================
# Returns per-hospital and nationwide summary for one (beta, seed) pair.
# Tracks:
#   inc_sum_last    : cumulative new infections in last INC_WINDOW days
#   prev_buffer     : rolling SS_WINDOW × H matrix of daily I/N (last 60 days)
#   steady_state_*  : when and at what prevalence SS was detected
# =============================================================================

INIT_PREV <- 0.02    # 2% starting prevalence (same as optim)

run_prevalence_simulation <- function(beta, seed) {

  set.seed(seed)
  p_rec    <- 1 - exp(-gamma)
  beta_vec <- rep(beta, H)

  I_loc <- rbinom(H, beds, pmax(rep(INIT_PREV, H), 1/beds))
  S_loc <- beds - I_loc

  # Circular incidence buffer — last INC_WINDOW days
  inc_buffer   <- matrix(0L,  nrow = INC_WINDOW, ncol = H)
  inc_buf_ptr  <- 1L

  # Circular prevalence buffer — last SS_WINDOW days (for end-of-sim check)
  prev_buffer  <- matrix(0.0, nrow = SS_WINDOW,  ncol = H)
  prev_buf_ptr <- 1L

  # ── Always run to MAX_DAYS — no early stopping ───────────────────────────
  for (t in seq_len(MAX_DAYS)) {

    # Step 1: vectorised SIS
    N       <- S_loc + I_loc
    p_inf   <- ifelse(N > 0L & is.finite(beta_vec),
                      1 - exp(-beta_vec * I_loc / pmax(N, 1L)), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    S_loc   <- S_loc - new_inf + recov
    I_loc   <- I_loc + new_inf - recov

    inc_buffer[inc_buf_ptr, ]  <- new_inf
    inc_buf_ptr  <- inc_buf_ptr  %% INC_WINDOW + 1L

    prev_buffer[prev_buf_ptr, ] <- I_loc / pmax(beds, 1L)
    prev_buf_ptr <- prev_buf_ptr %% SS_WINDOW  + 1L

    # Step 2: vectorised exits
    n_exit_S <- rbinom(H, S_loc, p_exit)
    n_exit_I <- rbinom(H, I_loc, p_exit)
    S_loc    <- S_loc - n_exit_S
    I_loc    <- I_loc - n_exit_I

    # Step 3: sparse transfer loop
    S_tr <- numeric(H); I_tr <- numeric(H)
    active_h <- transfer_idx[(n_exit_S[transfer_idx] +
                                n_exit_I[transfer_idx]) > 0L]
    for (h in active_h) {
      n_tr_S <- rbinom(1L, n_exit_S[h], p_tr[h])
      n_tr_I <- rbinom(1L, n_exit_I[h], p_tr[h])
      if ((n_tr_S + n_tr_I) == 0L) next
      probs <- P_tr[, h]
      if (n_tr_S > 0L) S_tr <- S_tr + rmultinom(1L, n_tr_S, probs)[, 1L]
      if (n_tr_I > 0L) I_tr <- I_tr + rmultinom(1L, n_tr_I, probs)[, 1L]
    }

    # Step 4: community admissions
    occ   <- S_loc + I_loc + S_tr + I_tr
    A     <- pmax(0L, beds - occ)
    A_I   <- rbinom(H, A, pi_vec)
    S_loc <- S_loc + S_tr + (A - A_I)
    I_loc <- I_loc + I_tr + A_I
  }

  # ── End-of-sim SS check: look at last SS_WINDOW (30) days ───────────────
  # For hospitals that had any case in that window, check if their
  # daily prevalence CV < 10%.  SS = TRUE if ≥80% of those hospitals stable.
  hosp_with_cases      <- which(colSums(prev_buffer) > 0)
  steady_state_reached <- FALSE
  if (length(hosp_with_cases) > 0) {
    col_means <- colMeans(prev_buffer[, hosp_with_cases, drop = FALSE])
    col_sds   <- apply(prev_buffer[, hosp_with_cases, drop = FALSE],
                       2, sd, na.rm = TRUE)
    cv_vals   <- col_sds / pmax(col_means, 1e-9)
    steady_state_reached <- mean(cv_vals < SS_CV_THRESH) >= SS_PROP_HOSP
  }

  # ── Outputs ──────────────────────────────────────────────────────────────
  inc_365            <- colSums(inc_buffer) / (beds * INC_WINDOW) * 1000
  nationwide_inc     <- sum(inc_buffer) / (total_beds_sum * INC_WINDOW) * 1000
  hosp_prevalence    <- colMeans(prev_buffer)
  overall_prevalence <- sum(I_loc) / total_beds_sum

  type_inc  <- tapply(inc_365,         type_etab, mean, na.rm = TRUE)
  type_prev <- tapply(hosp_prevalence, type_etab, mean, na.rm = TRUE)

  list(
    beta                 = beta,
    seed                 = seed,
    steady_state_reached = steady_state_reached,   # boolean only
    overall_prevalence   = overall_prevalence,
    nationwide_inc       = nationwide_inc,
    hosp_inc_365         = inc_365,
    hosp_prev_mean       = hosp_prevalence,
    hosp_type            = type_etab,
    type_inc_mean        = type_inc,
    type_prev_mean       = type_prev
  )
}

# =============================================================================
# 5. RUN GRID SEARCH IN PARALLEL
# =============================================================================

sim_grid <- expand.grid(
  beta   = beta_grid,
  rep_id = seq_len(N_REP)
) %>%
  mutate(sim_seed = seed_base + row_number() * 7L)

message("\nSimulation grid: ", nrow(sim_grid), " runs",
        " (", length(beta_grid), " betas × ", N_REP, " reps)")
message("Starting cluster: ", N_CORES, " PSOCK workers...")

if (exists("cl") && inherits(cl,"cluster")) try(stopCluster(cl), silent=TRUE)

PARALLEL_TYPE <- if (.Platform$OS.type == "windows") "PSOCK" else "FORK"
cl <- makeCluster(N_CORES, type = PARALLEL_TYPE)
message("Cluster started: ", N_CORES, " ", PARALLEL_TYPE, " workers")


clusterExport(cl, varlist=c(
  "run_prevalence_simulation",
  "H","beds","p_exit","p_tr","P_tr","pi_vec","gamma","alpha",
  "transfer_idx","type_etab","total_beds_sum",
  "INIT_PREV","MAX_DAYS","SS_WINDOW","SS_CV_THRESH",
  "SS_PROP_HOSP","INC_WINDOW", "sim_grid"
))

t0 <- Sys.time()
message("Running simulations...")

all_results <- tryCatch({
  parLapply(cl, seq_len(nrow(sim_grid)), function(i) {
    row <- sim_grid[i,]
    run_prevalence_simulation(beta=row$beta, seed=row$sim_seed)
  })
}, finally={
  message("Stopping cluster...")
  try(stopCluster(cl), silent=TRUE)
})

message("Elapsed: ", round(difftime(Sys.time(),t0,units="mins"),1), " min")

# =============================================================================
# 6. COMPILE RESULTS
# =============================================================================

message("Compiling results...")

scalar_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  data.frame(
    beta_within          = r$beta,
    rep_id               = sim_grid$rep_id[i],
    sim_seed             = sim_grid$sim_seed[i],
    steady_state_reached = r$steady_state_reached,  # boolean only
    overall_prevalence   = r$overall_prevalence,
    nationwide_inc       = r$nationwide_inc,
    stringsAsFactors = FALSE
  )
}))

# Type-level incidence and prevalence per rep
type_inc_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  tibble(
    beta_within = r$beta,
    rep_id      = sim_grid$rep_id[i],
    type        = names(r$type_inc_mean),
    inc_1000    = as.numeric(r$type_inc_mean),
    prev_mean   = as.numeric(r$type_prev_mean)
  )
}))

# =============================================================================
# 7. STEADY-STATE AND INCIDENCE SUMMARY PER BETA
# =============================================================================

beta_summary <- scalar_df %>%
  group_by(beta_within) %>%
  summarise(
    n_reps          = n(),
    prop_ss_reached = round(mean(steady_state_reached, na.rm=TRUE), 3),
    prev_mean       = round(mean(overall_prevalence, na.rm=TRUE), 4),
    prev_median     = round(median(overall_prevalence, na.rm=TRUE), 4),
    prev_q25        = round(quantile(overall_prevalence, 0.25, na.rm=TRUE), 4),
    prev_q75        = round(quantile(overall_prevalence, 0.75, na.rm=TRUE), 4),
    inc_mean        = round(mean(nationwide_inc, na.rm=TRUE), 4),
    inc_median      = round(median(nationwide_inc, na.rm=TRUE), 4),
    inc_q25         = round(quantile(nationwide_inc, 0.25, na.rm=TRUE), 4),
    inc_q75         = round(quantile(nationwide_inc, 0.75, na.rm=TRUE), 4),
    .groups = "drop"
  )

cat("\n=== BETA SUMMARY (prevalence and incidence at steady state) ===\n")
print(beta_summary, n=Inf)

# Type-level summary
type_summary <- type_inc_df %>%
  group_by(beta_within, type) %>%
  summarise(
    inc_median  = round(median(inc_1000, na.rm=TRUE), 4),
    inc_q25     = round(quantile(inc_1000, 0.25, na.rm=TRUE), 4),
    inc_q75     = round(quantile(inc_1000, 0.75, na.rm=TRUE), 4),
    prev_median = round(median(prev_mean, na.rm=TRUE), 4),
    .groups = "drop"
  )

# =============================================================================
# 8. MAP BETA TO AMR TIERS
# =============================================================================

# For each AMR pathogen × tier, find the beta whose median steady-state
# prevalence is closest to the target.
find_best_beta <- function(target_prev) {
  beta_summary %>%
    mutate(dist = abs(prev_median - target_prev)) %>%
    slice_min(dist, n=1, with_ties=FALSE) %>%
    select(beta_within, prev_median, prev_q25, prev_q75,
           inc_median, dist)
}

amr_mapping <- amr_long %>%
  mutate(best = lapply(target_prevalence, find_best_beta)) %>%
  tidyr::unnest(best) %>%
  mutate(
    fit = case_when(
      dist < 0.01 ~ "Excellent",
      dist < 0.03 ~ "Good",
      dist < 0.06 ~ "Fair",
      TRUE        ~ "Poor"
    )
  )

cat("\n=== BETA → AMR PREVALENCE TIER MAPPING ===\n")
print(amr_mapping %>%
        select(pathogen, tier, target_prevalence, beta_within,
               prev_median, inc_median, dist, fit),
      n=Inf)

# =============================================================================
# 9. NATIONWIDE INCIDENCE SUMMARY
# =============================================================================

nationwide_summary <- scalar_df %>%
  group_by(beta_within) %>%
  summarise(
    nat_inc_median = median(nationwide_inc, na.rm=TRUE),
    nat_inc_q25    = quantile(nationwide_inc, 0.25, na.rm=TRUE),
    nat_inc_q75    = quantile(nationwide_inc, 0.75, na.rm=TRUE),
    .groups = "drop"
  )

cat("\n=== NATIONWIDE INCIDENCE (per 1,000 bed-days, 365-day lookback) ===\n")
print(nationwide_summary, n=Inf)

# =============================================================================
# 10. PLOTS
# =============================================================================

library(ggplot2)
library(scales)

OI <- list(green="#009E73", orange="#E69F00", red="#D55E00",
           blue="#0072B2", sky="#56B4E9", pink="#CC79A7", grey="#999999")
tier_colors <- c("Low"=OI$green, "Mid"=OI$orange, "High"=OI$red)

# ── Plot 1: Beta vs prevalence curve + AMR targets ───────────────────────────
p1 <- ggplot(beta_summary, aes(x=beta_within)) +
  geom_ribbon(aes(ymin=prev_q25, ymax=prev_q75),
              fill=OI$sky, alpha=0.25) +
  geom_line(aes(y=prev_median), colour=OI$blue, linewidth=1.1) +
  geom_point(aes(y=prev_median), colour=OI$blue, size=2.5) +
  geom_hline(data=amr_long,
             aes(yintercept=target_prevalence, colour=tier,
                 linetype=pathogen), linewidth=0.7, alpha=0.85) +
  scale_colour_manual(values=tier_colors, name="AMR tier") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  scale_x_continuous(breaks=beta_grid,
                     labels=function(x) sprintf("%.3f",x)) +
  labs(title="β vs facility-level steady-state prevalence",
       subtitle="Median ± IQR | Horizontal lines = ECDC AMR targets",
       x="β (within-hospital transmission rate)",
       y="Network-wide prevalence (I/N)") +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"))

# ── Plot 2: Beta vs nationwide incidence ─────────────────────────────────────
p2 <- ggplot(beta_summary, aes(x=beta_within)) +
  geom_ribbon(aes(ymin=inc_q25, ymax=inc_q75),
              fill=OI$orange, alpha=0.25) +
  geom_line(aes(y=inc_median), colour=OI$red, linewidth=1.1) +
  geom_point(aes(y=inc_median), colour=OI$red, size=2.5) +
  scale_x_continuous(breaks=beta_grid,
                     labels=function(x) sprintf("%.3f",x)) +
  labs(title="β vs nationwide incidence (365-day lookback)",
       subtitle="Median ± IQR across replicates",
       x="β", y="Incidence (per 1,000 bed-days)") +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"))

# ── Plot 3: AMR mapping heatmap ───────────────────────────────────────────────
p3 <- ggplot(amr_mapping,
             aes(x=tier, y=fct_reorder(pathogen, -beta_within),
                 fill=prev_median)) +
  geom_tile(colour="white", linewidth=1.2) +
  geom_text(aes(label=sprintf("β=%.4f\n%.1f%%",
                               beta_within, prev_median*100)),
            size=3.5, fontface="bold", colour="white") +
  scale_fill_gradientn(colours=c(OI$green,OI$orange,OI$red),
                       labels=percent_format(accuracy=0.1),
                       name="Simulated\nprevalence") +
  scale_x_discrete(position="top") +
  labs(title="Best-fit β by AMR pathogen and prevalence tier",
       x="Prevalence tier", y=NULL) +
  theme_minimal(base_size=12) +
  theme(panel.grid=element_blank(),
        axis.text.y=element_text(face="italic"),
        plot.title=element_text(face="bold"))

# ── Plot 4: Steady-state detection rate per beta ──────────────────────────────
p4 <- ggplot(beta_summary, aes(x=beta_within, y=prop_ss_reached,
                                fill=prop_ss_reached)) +
  geom_col(width=0.0008) +
  geom_hline(yintercept=0.80, linetype="dashed", colour=OI$red) +
  scale_fill_gradient(low=OI$sky, high=OI$green, guide="none") +
  scale_y_continuous(labels=percent_format(), limits=c(0,1)) +
  scale_x_continuous(breaks=beta_grid,
                     labels=function(x) sprintf("%.3f",x)) +
  labs(title="Proportion of replicates reaching steady state",
       subtitle="Dashed = 80% threshold",
       x="β", y="% reaching SS") +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"))

# Save
ggsave(file.path(OUT_DIR,"01_beta_vs_prevalence.png"),     p1, width=12,height=6,dpi=150)
ggsave(file.path(OUT_DIR,"02_beta_vs_incidence.png"),      p2, width=12,height=6,dpi=150)
ggsave(file.path(OUT_DIR,"03_amr_mapping_heatmap.png"),    p3, width=10,height=7,dpi=150)
ggsave(file.path(OUT_DIR,"04_steady_state_rate.png"),      p4, width=12,height=5,dpi=150)
message("Plots saved.")

# =============================================================================
# 11. SAVE ALL OUTPUTS
# =============================================================================

run_date <- format(Sys.Date(),"%Y%m%d")

saveRDS(list(
  scalar_df          = scalar_df,
  type_inc_df        = type_inc_df,
  beta_summary       = beta_summary,
  type_summary       = type_summary,
  nationwide_summary = nationwide_summary,
  amr_mapping        = amr_mapping,
  amr_targets        = amr_long,
  params = list(beta_grid=beta_grid, N_REP=N_REP,
                MAX_DAYS=MAX_DAYS, SS_WINDOW=SS_WINDOW,
                INC_WINDOW=INC_WINDOW, pi_vec=pi_vec_val)
), file.path(OUT_DIR, paste0("prevalence_calibration_", run_date, ".rds")))

write.csv2(amr_mapping %>%
             select(pathogen, tier, target_prevalence, beta_within,
                    prev_median, inc_median, dist, fit),
           file.path(OUT_DIR, paste0("amr_beta_mapping_", run_date, ".csv")),
           row.names=FALSE)
write.csv2(beta_summary,
           file.path(OUT_DIR, paste0("beta_summary_",    run_date, ".csv")),
           row.names=FALSE)
write.csv2(type_summary,
           file.path(OUT_DIR, paste0("type_summary_",    run_date, ".csv")),
           row.names=FALSE)

cat("\n=== DONE ===\n")
cat("Results saved to:", OUT_DIR, "\n")
