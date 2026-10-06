# =============================================================================
# arcane_seeding_novel.R  —  Seeding simulation, NOVEL PATHOGEN (LOCAL)
# REVISED PATIENT FLOW: full occupancy via daily admission data
# =============================================================================
# Patient flow (replaces p_exit / LOS):
#   n_discharge[h]   = n_admit[h]  from NO_ADMISSION_DAILY_DIRCT_HBN.csv
#   n_discharge_I[h] ~ Hypergeometric(n_admit, I_loc, beds)
#   n_discharge_S[h] = n_admit - n_discharge_I
#   transfers_out    = rbinom(n_discharge, p_tr) routed via P_tr
#   transfers_in     = received from all other hospitals
#   community_admit  = n_admit - transfers_in  (total - transfers = community)
#   cap transfers_in to n_admit (refuse excess -> no overflow)
#   A_I ~ Binomial(community_admit, pi_vec)
#
# Full occupancy guaranteed: S_loc + I_loc = beds at end of each day.
# =============================================================================

library(parallel)
library(dplyr)
library(tidyr)
library(igraph)
crossing <- tidyr::crossing

ARCANE_ROOT <- Sys.getenv("ARCANE_ROOT",
                          "/media/kevinNFS2/rany/calibration_jobs")
JOB_INDEX   <- { v <- suppressWarnings(as.integer(Sys.getenv("jobindex")))
if (!is.na(v) && v > 0L) v else 1L }
DATA_DIR    <- file.path(ARCANE_ROOT, "data")
OUT_DIR     <- file.path(ARCANE_ROOT, "Outputs", "seeding_novel",
                         sprintf("job_%02d", JOB_INDEX))
ADMIT_FILE  <- file.path(DATA_DIR, "NO_ADMISSION_DAILY_DIRCT_HBN.csv")
dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)

# =============================================================================
# 1. TIER BETA PARAMETERS
# =============================================================================

tier_df <- data.frame(
  tier=c("Low","Moderate"), inc_low=c(1.3,14.3), inc_high=c(14.3,28.7),
  mean_beta=c(0.0549,0.102), ci_lo_95=c(0.030,0.085), ci_hi_95=c(0.080,0.120),
  pi_vec_val=c(0,0), source=c("calibrated","calibrated"),
  stringsAsFactors=FALSE
) %>%
  mutate(tier=factor(tier,levels=c("Low","Moderate")),
         beta_sd=(ci_hi_95-ci_lo_95)/(2*1.96),
         beta_sd=if_else(beta_sd<=0|is.na(beta_sd), mean_beta*0.10, beta_sd))

cat("=== TIER BETA PARAMETERS ===\n")
print(tier_df[,c("tier","mean_beta","beta_sd","ci_lo_95","ci_hi_95","pi_vec_val")],
      row.names=FALSE)

# =============================================================================
# 2. SETTINGS
# =============================================================================

N_REP          <- 15
N_SEED_INF     <- 5L
DAYS_PER_CYCLE <- 366L          # 2024 is leap year
N_CYCLES       <- 2L
Tmax           <- N_CYCLES * DAYS_PER_CYCLE   # 732 days
gamma          <- 1 / 387
SS_WINDOW      <- 30L
N_CORES        <- 8L                           # fixed — 43 workers caused OOM

message("Reps: ", N_REP, " | Tmax: ", Tmax, "d | Cores: ", N_CORES)

# =============================================================================
# 3. NETWORK DATA LOADING
# =============================================================================

weekly_transfers <- readRDS(file.path(DATA_DIR,"weekly.RDS")) %>%
  mutate(weight=pmax(1L, as.integer(round(weight/7))))

facility_level <- readRDS(file.path(DATA_DIR,"facility_level_final.RDS")) %>%
  mutate(finess_geo=as.character(finess_geo)) %>%
  rename(incidence_esbl_all=incidence_region_type_ESBL_all)

# =============================================================================
# 4. HOSPITAL UNIVERSE (no LOS / p_exit needed)
# =============================================================================

hospitals <- bind_rows(
  weekly_transfers %>% transmute(finess_geo=as.character(finess_geo_origin)),
  weekly_transfers %>% transmute(finess_geo=as.character(finess_geo_target))
) %>% distinct() %>%
  left_join(facility_level %>% transmute(
    finess_geo, hospital_type, type_spares, region,
    no_beds=as.integer(round(census_max))
  ), by="finess_geo") %>%
  mutate(
    no_beds      =as.integer(if_else(is.na(no_beds),
                                     as.integer(round(mean(no_beds,na.rm=TRUE))),
                                     no_beds)),
    type_spares  =if_else(is.na(type_spares),  "Unknown",type_spares),
    hospital_type=if_else(is.na(hospital_type),"Unknown",hospital_type),
    region       =if_else(is.na(region),        "Unknown",region)
  )

H              <- nrow(hospitals)
beds           <- hospitals$no_beds
total_beds_sum <- sum(beds)

transfer_out <- weekly_transfers %>%
  transmute(origin=as.character(finess_geo_origin), weight) %>%
  group_by(origin) %>%
  summarise(total_out=sum(weight,na.rm=TRUE),.groups="drop")
hospitals <- hospitals %>%
  left_join(transfer_out, by=c("finess_geo"="origin")) %>%
  mutate(total_out=replace(total_out,is.na(total_out),0))

# Transfer probability: fraction of discharges transferred out
# Normalised by mean daily admissions (not p_exit*beds)
mean_daily_admit_est <- pmax(1, as.numeric(beds) / 7)
p_tr <- pmin(hospitals$total_out / pmax(mean_daily_admit_est, 1), 0.60)

message("Building P_tr (", H, " x ", H, ")...")
hosp_idx <- setNames(seq_len(H), hospitals$finess_geo)
transfer_agg <- weekly_transfers %>%
  transmute(orig=hosp_idx[as.character(finess_geo_origin)],
            dest=hosp_idx[as.character(finess_geo_target)], weight) %>%
  filter(!is.na(orig)&!is.na(dest)) %>%
  group_by(orig,dest) %>% summarise(weight=sum(weight),.groups="drop")
P_tr <- matrix(0.0,H,H)
for (k in seq_len(nrow(transfer_agg)))
  P_tr[transfer_agg$dest[k],transfer_agg$orig[k]] <- transfer_agg$weight[k]
cs <- colSums(P_tr)
for (h in seq_len(H)) if (cs[h]>0) P_tr[,h] <- P_tr[,h]/cs[h]
message("  Done. H=",H)

# =============================================================================
# 5. DAILY ADMISSION MATRIX
# =============================================================================
# admit_matrix[h, d] = total admissions to hospital h on day d (d = 1..366)
# Community admissions = total - transfers received (computed in loop).
# Hospitals not in data: fallback = beds / 7.
# =============================================================================

message("Loading daily admission data from: ", ADMIT_FILE)

admit_raw <- read.csv(ADMIT_FILE, sep=";", header=TRUE,
                      col.names=c("finess_geo","no_admissions","date_entree"),
                      colClasses=c("character","integer","character"),
                      stringsAsFactors=FALSE)
admit_raw$day_of_year <- as.integer(
  format(as.Date(admit_raw$date_entree, "%d/%m/%Y"), "%j"))

# Mean admissions per hospital per day (handles duplicates)
admit_daily <- admit_raw %>%
  group_by(finess_geo, day_of_year) %>%
  summarise(admissions = round(mean(no_admissions, na.rm=TRUE)),
            .groups = "drop") %>%
  mutate(finess_geo = as.character(finess_geo))

# Pivot to wide: one row per hospital, one col per day (named "1".."366")
admit_wide <- admit_daily %>%
  tidyr::pivot_wider(names_from  = day_of_year,
                     values_from = admissions) %>%
  mutate(finess_geo = as.character(finess_geo))

# Annual mean per hospital (used to fill missing days)
admit_mean_h <- admit_daily %>%
  group_by(finess_geo) %>%
  summarise(mean_admit = as.integer(round(mean(admissions, na.rm=TRUE))),
            .groups = "drop")

# Build H x 366 integer matrix row by row
admit_matrix <- matrix(NA_integer_, nrow=H, ncol=366L)
rownames(admit_matrix) <- hospitals$finess_geo

n_in_data <- 0L
for (h in seq_len(H)) {
  fgeo <- hospitals$finess_geo[h]
  row  <- admit_wide[admit_wide$finess_geo == fgeo, , drop=FALSE]
  
  if (nrow(row) > 0L) {
    n_in_data <- n_in_data + 1L
    for (d in 1:366) {
      v <- row[[as.character(d)]]
      admit_matrix[h, d] <- if (!is.null(v) && length(v) > 0L && !is.na(v[1]))
        as.integer(v[1]) else NA_integer_
    }
    # Fill missing days with this hospital's annual mean
    hm_row <- admit_mean_h$mean_admit[admit_mean_h$finess_geo == fgeo]
    hm <- if (length(hm_row) > 0L && !is.na(hm_row[1])) hm_row[1] else
      as.integer(round(beds[h] / 7))
    admit_matrix[h, is.na(admit_matrix[h, ])] <- hm
  } else {
    # Hospital not in data: fallback = beds / 7
    admit_matrix[h, ] <- pmax(1L, as.integer(round(beds[h] / 7)))
  }
}

# Force to proper integer matrix — must do as.matrix() before pmax
admit_matrix[is.na(admit_matrix)] <- 1L
admit_matrix <- as.matrix(admit_matrix)
storage.mode(admit_matrix) <- "integer"
admit_matrix <- matrix(pmax(1L, as.vector(admit_matrix)), nrow=H, ncol=366L)
rownames(admit_matrix) <- hospitals$finess_geo

stopifnot(is.matrix(admit_matrix))
stopifnot(nrow(admit_matrix) == H)
stopifnot(ncol(admit_matrix) == 366L)
stopifnot(!any(is.na(admit_matrix)))
message(sprintf("  admit_matrix: %d x %d | In data: %d | fallback: %d | range: %d-%d | mean: %.1f",
                nrow(admit_matrix), ncol(admit_matrix),
                n_in_data, H - n_in_data,
                min(admit_matrix), max(admit_matrix), mean(admit_matrix)))

# =============================================================================
# 6. PER-HOSPITAL BETA DRAWS (base R, no truncnorm package)
# =============================================================================

rtruncnorm_base <- function(n, a=-Inf, b=Inf, mean=0, sd=1) {
  p_a <- pnorm(a, mean=mean, sd=sd)
  p_b <- pnorm(b, mean=mean, sd=sd)
  qnorm(runif(n, p_a, p_b), mean=mean, sd=sd)
}

set.seed(42)
beta_draws <- lapply(levels(tier_df$tier), function(t) {
  tr <- tier_df[tier_df$tier==t,]
  lapply(seq_len(N_REP), function(r)
    rtruncnorm_base(H, a=1e-4, b=0.30, mean=tr$mean_beta, sd=tr$beta_sd))
}) %>% setNames(levels(tier_df$tier))

# =============================================================================
# 7. SEED PANEL
# =============================================================================

g_agg <- weekly_transfers %>%
  group_by(finess_geo_origin,finess_geo_target) %>%
  summarise(weight=sum(weight),.groups="drop") %>%
  igraph::graph_from_data_frame(directed=TRUE)

seed_metrics <- hospitals %>%
  left_join(tibble(
    finess_geo  =names(igraph::degree(g_agg,mode="in")),
    in_degree   =as.integer(igraph::degree(g_agg,mode="in")),
    out_degree  =as.integer(igraph::degree(g_agg,mode="out")),
    out_strength=as.numeric(igraph::strength(g_agg,mode="out")),
    betweenness =as.numeric(igraph::betweenness(g_agg,directed=TRUE,normalized=FALSE))
  ), by="finess_geo") %>%
  mutate(across(c(in_degree,out_degree,out_strength,betweenness),~replace_na(.x,0)))

fixed_seeds <- bind_rows(
  seed_metrics %>% slice_max(in_degree,   n=1,with_ties=FALSE) %>%
    transmute(finess_geo,seed_rule="highest_in_degree"),
  seed_metrics %>% slice_max(out_degree,  n=1,with_ties=FALSE) %>%
    transmute(finess_geo,seed_rule="highest_out_degree"),
  seed_metrics %>% slice_max(betweenness, n=1,with_ties=FALSE) %>%
    transmute(finess_geo,seed_rule="highest_betweenness"),
  seed_metrics %>% slice_max(no_beds,     n=1,with_ties=FALSE) %>%
    transmute(finess_geo,seed_rule="largest_beds"),
  seed_metrics %>% slice_max(out_strength,n=1,with_ties=FALSE) %>%
    transmute(finess_geo,seed_rule="largest_outgoing")
) %>%
  group_by(finess_geo) %>%
  summarise(seed_rule=paste(sort(seed_rule),collapse=" + "),.groups="drop") %>%
  mutate(seed_type="fixed")

type_seeds <- tibble(finess_geo=NA_character_,
                     seed_rule=c("random_MCO","random_SSR","random_MCO_SSR"),
                     seed_type="type_random")
seed_panel <- bind_rows(fixed_seeds, type_seeds)

hosp_by_type <- hospitals %>%
  group_by(hospital_type) %>%
  summarise(ids=list(finess_geo),.groups="drop") %>%
  with(setNames(ids,hospital_type))

# =============================================================================
# 8. SIMULATION GRID
# =============================================================================

sim_grid <- seed_panel %>%
  tidyr::crossing(
    tidyr::crossing(
      tibble(tier=levels(tier_df$tier)),
      tibble(rep_id=seq_len(N_REP))
    )
  ) %>%
  mutate(
    sim_id=row_number(), sim_seed=20000L+(JOB_INDEX-1L)*100000L+sim_id*7L,
    seed_hospital=mapply(function(stype,srule,fgeo,sseed) {
      if (stype=="fixed") return(fgeo)
      target <- switch(srule,random_MCO="MCO",random_SSR="SSR",
                       random_MCO_SSR="MCO/SSR","Other")
      pool <- hosp_by_type[[target]]
      if (is.null(pool)||length(pool)==0) pool <- hospitals$finess_geo
      set.seed(sseed); sample(pool,1)
    }, seed_type,seed_rule,finess_geo,sim_seed, SIMPLIFY=TRUE,USE.NAMES=FALSE)
  )

message("Simulations: ",nrow(sim_grid))

# =============================================================================
# 9. SIMULATION FUNCTION — ADMISSION-BASED PATIENT FLOW
# =============================================================================

run_seeding_simulation <- function(seed_hospital, sim_seed,
                                   beta_vec, pi_vec_val_sim) {
  set.seed(sim_seed)
  p_rec        <- 1 - exp(-gamma)
  pi_vec_local <- rep(pi_vec_val_sim, H)
  
  I_loc <- integer(H)
  idx   <- which(hospitals$finess_geo==seed_hospital)
  if (length(idx)>0) I_loc[idx] <- min(as.integer(N_SEED_INF), beds[idx])
  S_loc <- beds - I_loc
  
  net_prev_daily      <- numeric(Tmax)
  net_inc_daily       <- numeric(Tmax)
  fac_prev_mean_daily <- numeric(Tmax)
  fac_inc_mean_daily  <- numeric(Tmax)
  n_hosp_inf_daily    <- integer(Tmax)
  
  for (t in seq_len(Tmax)) {
    day_idx <- ((t-1L) %% DAYS_PER_CYCLE) + 1L
    
    # Today's admissions = today's discharges (full occupancy enforced)
    n_admit <- admit_matrix[, day_idx]
    n_admit <- pmin(n_admit, S_loc+I_loc)   # can't discharge more than present
    
    # (1) Record and SIS transmission
    N_loc <- S_loc + I_loc
    net_prev_daily[t]      <- sum(I_loc) / total_beds_sum
    fac_prev_mean_daily[t] <- mean(I_loc / pmax(beds, 1L))
    n_hosp_inf_daily[t]    <- sum(I_loc > 0L)
    
    p_inf   <- ifelse(N_loc>0L & is.finite(beta_vec),
                      1-exp(-beta_vec*I_loc/pmax(N_loc,1L)), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    
    net_inc_daily[t] <- if (sum(N_loc)>0) sum(new_inf)/sum(N_loc)*1000 else 0
    active <- N_loc>0L
    fac_inc_mean_daily[t] <- if (any(active))
      mean(new_inf[active]/N_loc[active]*1000) else 0
    
    S_loc <- S_loc - new_inf + recov
    I_loc <- I_loc + new_inf - recov
    
    # (2) Discharges = n_admit (Hypergeometric draw)
    n_admit       <- as.integer(n_admit)
    n_discharge_I <- rhyper(H,
                            pmax(0L, I_loc),
                            pmax(0L, S_loc),
                            pmin(n_admit, pmax(0L, I_loc + S_loc)))
    n_discharge_I <- pmin(n_discharge_I, I_loc)
    n_discharge_S <- pmin(n_admit - n_discharge_I, S_loc)
    
    I_loc <- I_loc - n_discharge_I
    S_loc <- S_loc - n_discharge_S
    
    # (3) Transfers: subset of discharged patients
    S_tr_out <- rbinom(H, n_discharge_S, p_tr)
    I_tr_out <- rbinom(H, n_discharge_I, p_tr)
    
    S_tr_in <- numeric(H); I_tr_in <- numeric(H)
    active_h <- which((S_tr_out+I_tr_out)>0L)
    for (h in active_h) {
      probs <- P_tr[,h]; s <- sum(probs); if (s<=0) next
      nS <- S_tr_out[h]; nI <- I_tr_out[h]
      if (nS>0L) S_tr_in <- S_tr_in + rmultinom(1L,nS,probs)[,1L]
      if (nI>0L) I_tr_in <- I_tr_in + rmultinom(1L,nI,probs)[,1L]
    }
    
    # (4) Cap transfers to prevent overflow, compute community admissions
    transfers_received <- S_tr_in + I_tr_in
    overflow <- which(transfers_received > n_admit)
    for (h in overflow) {
      if (transfers_received[h]>0L) {
        scale     <- n_admit[h] / transfers_received[h]
        S_tr_in[h] <- as.integer(floor(S_tr_in[h]*scale))
        I_tr_in[h] <- as.integer(floor(I_tr_in[h]*scale))
      }
    }
    transfers_received <- S_tr_in + I_tr_in
    community_admit    <- pmax(0L, n_admit - transfers_received)
    
    # (5) Community colonisation at admission
    A_I   <- rbinom(H, community_admit, pi_vec_local)
    A_S   <- community_admit - A_I
    
    S_loc <- S_loc + S_tr_in + A_S
    I_loc <- I_loc + I_tr_in + A_I
  }
  
  ss_days <- tail(seq_len(Tmax), SS_WINDOW)
  list(
    net_cumulative_inc      = mean(net_inc_daily),
    fac_cumulative_inc_mean = mean(fac_inc_mean_daily),
    net_ss_inc              = mean(net_inc_daily[ss_days]),
    fac_ss_inc_mean         = mean(fac_inc_mean_daily[ss_days]),
    net_ss_prev_pct         = mean(net_prev_daily[ss_days])*100,
    fac_ss_prev_mean_pct    = mean(fac_prev_mean_daily[ss_days])*100,
    net_final_prev_pct      = net_prev_daily[Tmax]*100,
    fac_final_prev_mean_pct = fac_prev_mean_daily[Tmax]*100,
    peak_n_hosp_inf         = max(n_hosp_inf_daily),
    ever_extinct            = sum(I_loc)==0L,
    net_prev_daily          = net_prev_daily*100,
    fac_prev_mean_daily     = fac_prev_mean_daily*100,
    net_inc_daily           = net_inc_daily,
    fac_inc_mean_daily      = fac_inc_mean_daily,
    n_hosp_inf_daily        = n_hosp_inf_daily
  )
}

# =============================================================================
# 10. RUN — PSOCK
# =============================================================================

message("\nStarting ",N_CORES," FORK workers (cluster)...")
if (exists("cl")&&inherits(cl,"cluster")) try(stopCluster(cl),silent=TRUE)
cl <- makeCluster(N_CORES, type="FORK")

t0 <- Sys.time()
message("Running ",nrow(sim_grid)," simulations...")
results_list <- tryCatch({
  parLapply(cl, seq_len(nrow(sim_grid)), function(i) {
    row  <- sim_grid[i,]
    tr   <- tier_df[tier_df$tier==row$tier,]
    bvec <- beta_draws[[as.character(row$tier)]][[row$rep_id]]
    res  <- run_seeding_simulation(row$seed_hospital,row$sim_seed,
                                   bvec,tr$pi_vec_val)
    res$sim_id=row$sim_id; res$seed_rule=row$seed_rule
    res$seed_type=row$seed_type; res$tier=as.character(row$tier)
    res$rep_id=row$rep_id; res$seed_hospital=row$seed_hospital
    res
  })
}, finally={message("Stopping cluster..."); try(stopCluster(cl),silent=TRUE)})

elapsed <- round(difftime(Sys.time(),t0,units="mins"),1)
message("Done. Elapsed: ",elapsed," min")

# =============================================================================
# 11. COMPILE AND SAVE
# =============================================================================

sim_summary <- bind_rows(lapply(results_list, function(r) {
  data.frame(
    sim_id=r$sim_id, seed_rule=r$seed_rule, seed_type=r$seed_type,
    tier=r$tier, rep_id=r$rep_id, seed_hospital=r$seed_hospital,
    ever_extinct=r$ever_extinct, peak_n_hosp_inf=r$peak_n_hosp_inf,
    net_cumulative_inc     =round(r$net_cumulative_inc,4),
    fac_cumulative_inc_mean=round(r$fac_cumulative_inc_mean,4),
    net_ss_inc             =round(r$net_ss_inc,4),
    fac_ss_inc_mean        =round(r$fac_ss_inc_mean,4),
    net_ss_prev_pct        =round(r$net_ss_prev_pct,4),
    fac_ss_prev_mean_pct   =round(r$fac_ss_prev_mean_pct,4),
    net_final_prev_pct     =round(r$net_final_prev_pct,4),
    fac_final_prev_mean_pct=round(r$fac_final_prev_mean_pct,4),
    stringsAsFactors=FALSE
  )
})) %>%
  left_join(seed_metrics %>% select(finess_geo,hospital_type,region,
                                    in_degree,out_degree,betweenness,no_beds),
            by=c("seed_hospital"="finess_geo")) %>%
  left_join(tier_df %>% select(tier,pi_vec_val,mean_beta,beta_sd) %>%
              mutate(tier=as.character(tier)), by="tier")

traj_df <- bind_rows(lapply(results_list, function(r) {
  data.frame(sim_id=r$sim_id, tier=r$tier, seed_rule=r$seed_rule,
             seed_type=r$seed_type, rep_id=r$rep_id, day=seq_len(Tmax),
             net_prev_pct=r$net_prev_daily,
             fac_prev_mean_pct=r$fac_prev_mean_daily,
             net_inc=r$net_inc_daily, fac_inc_mean=r$fac_inc_mean_daily,
             n_hosp_inf=r$n_hosp_inf_daily, stringsAsFactors=FALSE)
}))

run_date <- format(Sys.Date(),"%Y%m%d")
saveRDS(list(sim_summary=sim_summary, traj_df=traj_df,
             tier_df=tier_df, sim_grid=sim_grid,
             params=list(N_REP=N_REP,Tmax=Tmax,N_CYCLES=N_CYCLES,
                         N_SEED_INF=N_SEED_INF,gamma=gamma,
                         SS_WINDOW=SS_WINDOW,pi_vec="0 (novel)",
                         patient_flow="admission-based, full occupancy")),
        file.path(OUT_DIR,paste0("seeding_results_",run_date,".rds")))
write.csv(sim_summary,
          file.path(OUT_DIR,paste0("seeding_summary_",run_date,".csv")),
          row.names=FALSE)

cat("\n=== DONE ===\n")
cat("Simulations:",nrow(sim_summary),"\n")
cat("Elapsed    :",elapsed,"min\n")
cat("Extinct    :",round(mean(sim_summary$ever_extinct)*100,1),"%\n")
ne <- sim_summary[!sim_summary$ever_extinct,]
if (nrow(ne)>0)
  cat("Prev range :", round(min(ne$net_ss_prev_pct),2),
      "-", round(max(ne$net_ss_prev_pct),2), "% (should be <=100)\n")
cat("Saved to   :",OUT_DIR,"\n")