# =============================================================================
# calibration_incidence_local.R  —  Novel pathogen incidence calibration (LOCAL)
# =============================================================================
# Same logic as calibration_incidence.R but adapted for local Windows use:
#   - PSOCK parallelism with explicit clusterExport
#   - Hospital subset (~300) for speed
#   - Coarser beta grid (by=0.010, 12 values)
#   - Fewer reps (N_REP=3)
#   - detectCores()-2 cores
#
# Run prevalence_to_incidence.R first to generate data/inc_targets.rds.
# =============================================================================

library(parallel)
library(dplyr)

# =============================================================================
# 0. PATHS
# =============================================================================

LOCAL_ROOT <- "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/calibration_jobs"
DATA_DIR   <- file.path(LOCAL_ROOT, "data")
OUT_DIR    <- file.path(LOCAL_ROOT, "Outputs", "prevalence", "local")
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)

JOB_INDEX <- 1L

# =============================================================================
# 1. LOAD INCIDENCE TARGETS FROM data/inc_targets.rds
# =============================================================================

targets_path <- file.path(DATA_DIR, "inc_targets.rds")
if (!file.exists(targets_path))
  stop("inc_targets.rds not found. Run prevalence_to_incidence.R first.\n",
       "Expected: ", targets_path)

targets_obj    <- readRDS(targets_path)
inc_targets    <- targets_obj$inc_targets
los_by_type    <- targets_obj$los_by_type
los_network    <- targets_obj$los_network
factor_network <- targets_obj$factor_network
gamma          <- targets_obj$gamma
pi_vec_val     <- targets_obj$pi_vec_val

message("=== INCIDENCE CALIBRATION (LOCAL) ===")
message("Targets loaded from: ", targets_path)
for (i in seq_len(nrow(inc_targets))) {
  r <- inc_targets[i, ]
  message(sprintf("  %-10s  %.3f - %.3f /1k pd  [%s]",
                  as.character(r$tier), r$inc_low, r$inc_high,
                  r$ecdc_category))
}

# =============================================================================
# 2. SETTINGS
# =============================================================================

beta_grid    <- seq(0.005, 0.120, by = 0.010)   # 12 values (cluster: by=0.005)
N_REP        <- 3
N_CYCLES     <- 2L
Tmax         <- N_CYCLES * 365L                  # 730 days
SS_WINDOW    <- 30L
SS_CV_THRESH <- 0.15
INC_WINDOW   <- 180L
INIT_INF     <- 5L
N_CORES      <- max(1L, parallel::detectCores() - 2L)
seed_base    <- 10000L + (JOB_INDEX - 1L) * 100000L

message("Grid  : ", min(beta_grid), "-", max(beta_grid),
        " (", length(beta_grid), " values)")
message("Reps  : ", N_REP, " | Tmax: ", Tmax, "d | Cores: ", N_CORES)

# =============================================================================
# 3. DATA LOADING
# =============================================================================

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
  summarise(pt  = sum(pt_days_total, na.rm=TRUE),
            pat = sum(patient_total,  na.rm=TRUE), .groups="drop") %>%
  mutate(los_type = pt / pat)
DEFAULT_LOS_TYPE   <- setNames(default_los$los_type, default_los$hospital_type)
GLOBAL_DEFAULT_LOS <- with(
  facility_level %>% filter(!is.na(hospital_type)) %>%
    summarise(a=sum(pt_days_total,na.rm=TRUE),
              b=sum(patient_total, na.rm=TRUE)), a/b)

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

transfer_out <- weekly_transfers %>%
  transmute(origin=as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out=sum(weight,na.rm=TRUE), .groups="drop")
hospitals <- hospitals %>%
  left_join(transfer_out, by=c("finess_geo"="origin")) %>%
  mutate(total_out=replace(total_out, is.na(total_out), 0))
p_tr <- pmin(hospitals$total_out / pmax(p_exit*beds, 1), 0.60)

message("Building P_tr (", H, " x ", H, ")...")
hosp_idx     <- setNames(seq_len(H), hospitals$finess_geo)
transfer_agg <- weekly_transfers %>%
  transmute(orig=hosp_idx[as.character(finess_geo_origin)],
            dest=hosp_idx[as.character(finess_geo_target)], weight) %>%
  filter(!is.na(orig) & !is.na(dest)) %>%
  group_by(orig, dest) %>%
  summarise(weight=sum(weight), .groups="drop")
P_tr <- matrix(0.0, H, H)
for (k in seq_len(nrow(transfer_agg)))
  P_tr[transfer_agg$dest[k], transfer_agg$orig[k]] <- transfer_agg$weight[k]
cs <- colSums(P_tr)
for (h in seq_len(H)) if (cs[h]>0) P_tr[,h] <- P_tr[,h]/cs[h]

# =============================================================================
# LOCAL SUBSET — ~300 hospitals for speed
# Remove this block for full-network runs.
# =============================================================================
set.seed(1)
keep     <- hospitals %>% group_by(hospital_type) %>% slice(1) %>% ungroup()
extra    <- hospitals %>% anti_join(keep, by="finess_geo") %>%
              sample_n(min(290, nrow(.)))
hosp_sub <- bind_rows(keep, extra)
keep_idx <- which(hospitals$finess_geo %in% hosp_sub$finess_geo)

hospitals <- hosp_sub
H         <- nrow(hospitals)
beds      <- beds[keep_idx]
p_exit    <- p_exit[keep_idx]
p_tr      <- p_tr[keep_idx]
pi_vec    <- pi_vec[keep_idx]
P_tr      <- P_tr[keep_idx, keep_idx]
cs2 <- colSums(P_tr)
for (h in seq_len(H)) if (cs2[h]>0) P_tr[,h] <- P_tr[,h]/cs2[h]
# =============================================================================

transfer_idx      <- which(p_tr > 0 & colSums(P_tr) > 0)
total_beds_sum    <- sum(beds)
type_etab         <- hospitals$type_spares
seed_hospital_idx <- which.max(hospitals$total_out)

message("LOCAL SUBSET: ", H, " hospitals | ",
        format(sum(beds), big.mark=","), " beds")

# =============================================================================
# 5. SIMULATION FUNCTION
# =============================================================================

run_novel_simulation <- function(beta, seed) {
  set.seed(seed)
  p_rec    <- 1 - exp(-gamma)
  beta_vec <- rep(beta, H)

  I_loc <- integer(H)
  I_loc[seed_hospital_idx] <- min(INIT_INF, beds[seed_hospital_idx])
  S_loc <- beds - I_loc

  inc_daily      <- numeric(Tmax)
  pt_days_daily  <- numeric(Tmax)
  net_prev_daily <- numeric(Tmax)

  for (t in seq_len(Tmax)) {

    pt_days_daily[t]  <- sum(S_loc + I_loc)
    net_prev_daily[t] <- sum(I_loc) / total_beds_sum

    N       <- S_loc + I_loc
    p_inf   <- ifelse(N > 0L & is.finite(beta_vec),
                      1 - exp(-beta_vec * I_loc / pmax(N, 1L)), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    inc_daily[t] <- sum(new_inf)
    S_loc   <- S_loc - new_inf + recov
    I_loc   <- I_loc + new_inf - recov

    n_exit_S <- rbinom(H, S_loc, p_exit)
    n_exit_I <- rbinom(H, I_loc, p_exit)
    S_loc    <- S_loc - n_exit_S
    I_loc    <- I_loc - n_exit_I

    S_tr <- numeric(H); I_tr <- numeric(H)
    active_h <- transfer_idx[(n_exit_S[transfer_idx] +
                                n_exit_I[transfer_idx]) > 0L]
    for (h in active_h) {
      nS <- rbinom(1L, n_exit_S[h], p_tr[h])
      nI <- rbinom(1L, n_exit_I[h], p_tr[h])
      if ((nS + nI) == 0L) next
      probs <- P_tr[, h]; s <- sum(probs); if (s <= 0) next
      if (nS > 0L) S_tr <- S_tr + rmultinom(1L, nS, probs)[, 1L]
      if (nI > 0L) I_tr <- I_tr + rmultinom(1L, nI, probs)[, 1L]
    }

    occ   <- S_loc + I_loc + S_tr + I_tr
    A     <- pmax(0L, beds - occ)
    A_I   <- rbinom(H, A, pi_vec)
    S_loc <- S_loc + S_tr + (A - A_I)
    I_loc <- I_loc + I_tr + A_I
  }

  inc_rate_daily <- ifelse(pt_days_daily > 0,
                            inc_daily / pt_days_daily * 1000, 0)
  win          <- tail(seq_len(Tmax), INC_WINDOW)
  reported_inc <- sum(inc_daily[win]) /
                  max(sum(pt_days_daily[win]), 1) * 1000
  ss_inc <- tail(inc_rate_daily, SS_WINDOW)
  ss_m   <- mean(ss_inc)
  ss_cv  <- if (ss_m > 0) sd(ss_inc) / ss_m else Inf

  list(
    beta                 = beta,
    seed                 = seed,
    reported_inc         = reported_inc,
    net_prev_final       = sum(I_loc) / total_beds_sum,
    steady_state_cv      = round(ss_cv, 4),
    steady_state_reached = ss_cv < SS_CV_THRESH,
    extinct              = sum(I_loc) == 0L,
    inc_rate_daily       = inc_rate_daily,
    net_prev_daily       = net_prev_daily
  )
}

# =============================================================================
# 6. RUN — PSOCK (Windows)
# =============================================================================

sim_grid <- expand.grid(beta=beta_grid, rep_id=seq_len(N_REP)) %>%
  mutate(sim_seed = seed_base + row_number() * 7L)

message("\nStarting ", N_CORES, " PSOCK workers...")
message("Simulations: ", nrow(sim_grid))

cl <- makeCluster(N_CORES, type="PSOCK")
clusterExport(cl, varlist=c(
  "run_novel_simulation",
  "H","beds","p_exit","p_tr","P_tr","pi_vec","gamma",
  "transfer_idx","total_beds_sum","seed_hospital_idx",
  "INIT_INF","Tmax","SS_WINDOW","SS_CV_THRESH","INC_WINDOW",
  "sim_grid"
))
t0 <- Sys.time()
all_results <- tryCatch(
  parLapply(cl, seq_len(nrow(sim_grid)), function(i) {
    row <- sim_grid[i, ]
    run_novel_simulation(beta=row$beta, seed=row$sim_seed)
  }),
  finally = { try(stopCluster(cl), silent=TRUE) }
)
elapsed <- round(difftime(Sys.time(), t0, units="mins"), 1)
message("Elapsed: ", elapsed, " min")

# =============================================================================
# 7. COMPILE AND SAVE
# =============================================================================

scalar_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  data.frame(
    job_index=JOB_INDEX, beta_within=r$beta,
    rep_id=sim_grid$rep_id[i], sim_seed=sim_grid$sim_seed[i],
    reported_inc=r$reported_inc, net_prev_final=r$net_prev_final,
    steady_state_cv=r$steady_state_cv,
    steady_state_reached=r$steady_state_reached,
    extinct=r$extinct, stringsAsFactors=FALSE
  )
}))

traj_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  data.frame(
    job_index=JOB_INDEX, beta_within=r$beta,
    rep_id=sim_grid$rep_id[i], extinct=r$extinct,
    day=seq_len(Tmax), inc_rate=r$inc_rate_daily,
    net_prev=r$net_prev_daily, stringsAsFactors=FALSE
  )
}))

# Quick tier mapping for local inspection
scalar_eligible <- scalar_df %>% filter(!extinct, reported_inc > 0)
scalar_with_tier <- scalar_eligible %>%
  tidyr::crossing(inc_targets %>% select(tier, inc_low, inc_high)) %>%
  filter(reported_inc >= inc_low & reported_inc <= inc_high)

tier_summary <- scalar_with_tier %>%
  group_by(tier, inc_low, inc_high) %>%
  summarise(n=n(), best_beta=beta_within[which.min(abs(reported_inc-(inc_low+inc_high)/2))],
            mean_beta=round(mean(beta_within),5),
            ci_lo=round(quantile(beta_within,0.025),5),
            ci_hi=round(quantile(beta_within,0.975),5),
            mean_inc=round(mean(reported_inc),3), .groups="drop")

cat("\n=== LOCAL TIER SUMMARY ===\n")
print(tier_summary, n=Inf)

saveRDS(list(
  scalar_df    = scalar_df,
  traj_df      = traj_df,
  tier_summary = tier_summary,
  inc_targets  = inc_targets,
  job_index    = JOB_INDEX,
  datetime     = Sys.time(),
  elapsed_min  = as.numeric(elapsed),
  params       = list(
    beta_grid=beta_grid, N_REP=N_REP, Tmax=Tmax,
    pi_vec=pi_vec_val, gamma=gamma,
    factor_network=factor_network, los_network=los_network,
    H=H, total_beds=total_beds_sum,
    inc_targets_file=targets_path
  )
), file.path(OUT_DIR, "results_local.rds"))

write.csv(scalar_df,    file.path(OUT_DIR, "scalar_results.csv"), row.names=FALSE)
write.csv(tier_summary, file.path(OUT_DIR, "tier_summary.csv"),   row.names=FALSE)

cat("\n=== LOCAL RUN DONE ===\n")
cat("H           :", H, "hospitals\n")
cat("Elapsed     :", elapsed, "min\n")
cat("Extinct     :", round(mean(scalar_df$extinct)*100,1), "%\n")
cat("Saved to    :", OUT_DIR, "\n")
