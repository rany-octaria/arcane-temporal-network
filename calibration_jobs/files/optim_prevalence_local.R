# =============================================================================
# optim_prevalence_local.R  —  Prevalence calibration  (LOCAL VERSION)
# =============================================================================
# Same logic as optim_prevalence.R but with:
#   • 300-hospital subset (removes the ~2,519 → ~300 size bottleneck)
#   • Coarser beta grid (by=0.002, 13 values instead of 26)
#   • INC_WINDOW=180 days (6-month lookback instead of 365)
#   • PSOCK parallelism (Windows-compatible)
#   • JOB_INDEX hardcoded to 1
#
# Expected runtime: ~20–40 min locally with detectCores()-2 workers
#
# DATA: same files as cluster — copy or symlink from optim_cluster_jobs/data/
#   weekly.RDS + facility_level_final.RDS → prev_calib_jobs/data/
# =============================================================================

library(parallel)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# =============================================================================
# 0. PATHS
# =============================================================================

LOCAL_ROOT <- "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/prev_calib_jobs"
DATA_DIR   <- file.path(LOCAL_ROOT, "data")
OUT_DIR    <- file.path(LOCAL_ROOT, "Outputs", "prevalence", "local")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

JOB_INDEX <- 1L   # always 1 for local

# =============================================================================
# 1. LOCAL SETTINGS
# =============================================================================

beta_grid    <- seq(0.005, 0.03, by = 0.002)   # 13 values (cluster: by=0.001)
N_REP        <- 3
MAX_DAYS     <- 730L
SS_WINDOW    <- 30L
SS_CV_THRESH <- 0.10
SS_PROP_HOSP <- 0.80
INC_WINDOW   <- 180L    # 6 months (cluster: 365)
INIT_PREV    <- 0.02
gamma        <- 1 / 387
alpha        <- 0
pi_vec_val   <- 0.05
N_CORES      <- max(1L, parallel::detectCores() - 2L)
seed_base    <- 10000L + (JOB_INDEX - 1L) * 100000L

message("=== ARCANE PREVALENCE CALIBRATION (LOCAL) ===")
message("Beta grid: ", min(beta_grid), "–", max(beta_grid),
        " (", length(beta_grid), " values)")
message("Reps: ", N_REP, " | Cores: ", N_CORES,
        " | Max days: ", MAX_DAYS, " | INC_WINDOW: ", INC_WINDOW)

# =============================================================================
# 2. AMR PREVALENCE TARGETS (ECDC EARS-Net 2022)
# =============================================================================

amr_targets <- tribble(
  ~pathogen,  ~prev_low, ~prev_mid, ~prev_high,
  "MRSA",      0.010,     0.115,     0.250,
  "VRE",       0.005,     0.080,     0.180,
  "ESBL-E",    0.020,     0.120,     0.300,
  "CRE",       0.002,     0.030,     0.100,
  "CRAB",      0.010,     0.070,     0.200
)
amr_long <- amr_targets %>%
  pivot_longer(starts_with("prev_"),
               names_to = "tier", names_prefix = "prev_",
               values_to = "target_prevalence") %>%
  mutate(tier = factor(tier, c("low","mid","high"),
                        labels = c("Low","Mid","High")))

# =============================================================================
# 3. DATA LOADING
# =============================================================================

message("Loading data from: ", DATA_DIR)
if (!file.exists(file.path(DATA_DIR, "weekly.RDS")))
  stop("weekly.RDS not found in ", DATA_DIR,
       "\nCopy from optim_cluster_jobs/data/ — see README.")

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

message("Building P_tr (full network: ", H, " hospitals)...")
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

# =============================================================================
# LOCAL SUBSET — ~300 hospitals for speed
# ── Remove this block before running on cluster ─────────────────────────────
set.seed(1)
keep  <- hospitals %>% group_by(type_spares) %>% slice(1) %>% ungroup()
extra <- hospitals %>% anti_join(keep, by="finess_geo") %>%
  sample_n(min(290, nrow(.)))
hosp_sub <- bind_rows(keep, extra)
keep_idx <- which(hospitals$finess_geo %in% hosp_sub$finess_geo)

hospitals  <- hosp_sub
H          <- nrow(hospitals)
beds       <- beds[keep_idx]
p_exit     <- p_exit[keep_idx]
p_tr       <- p_tr[keep_idx]
pi_vec     <- pi_vec[keep_idx]
P_tr       <- P_tr[keep_idx, keep_idx]
cs2 <- colSums(P_tr)
for (h in seq_len(H)) if (cs2[h]>0) P_tr[,h] <- P_tr[,h]/cs2[h]
# ── End of local subset block ────────────────────────────────────────────────

transfer_idx   <- which(p_tr > 0)
type_etab      <- hospitals$type_spares
total_beds_sum <- sum(beds)

message("LOCAL SUBSET: ", H, " hospitals | ",
        format(sum(beds), big.mark=","), " beds | ",
        length(transfer_idx), " transfer-eligible")

# =============================================================================
# 5. SIMULATION FUNCTION
# =============================================================================

run_prevalence_simulation <- function(beta, seed) {
  set.seed(seed)
  p_rec    <- 1 - exp(-gamma)
  beta_vec <- rep(beta, H)
  I_loc    <- rbinom(H, beds, pmax(rep(INIT_PREV, H), 1/beds))
  S_loc    <- beds - I_loc

  inc_buffer   <- matrix(0L,  nrow=INC_WINDOW, ncol=H)
  inc_buf_ptr  <- 1L
  prev_buffer  <- matrix(0.0, nrow=SS_WINDOW,  ncol=H)
  prev_buf_ptr <- 1L

  for (t in seq_len(MAX_DAYS)) {
    N       <- S_loc + I_loc
    p_inf   <- ifelse(N>0L & is.finite(beta_vec),
                      1-exp(-beta_vec*I_loc/pmax(N,1L)), 0)
    new_inf <- rbinom(H, S_loc, p_inf)
    recov   <- rbinom(H, I_loc, p_rec)
    S_loc   <- S_loc - new_inf + recov
    I_loc   <- I_loc + new_inf - recov
    inc_buffer[inc_buf_ptr,]   <- new_inf
    inc_buf_ptr  <- inc_buf_ptr  %% INC_WINDOW + 1L
    prev_buffer[prev_buf_ptr,] <- I_loc/pmax(beds,1L)
    prev_buf_ptr <- prev_buf_ptr %% SS_WINDOW  + 1L
    n_exit_S <- rbinom(H, S_loc, p_exit)
    n_exit_I <- rbinom(H, I_loc, p_exit)
    S_loc    <- S_loc - n_exit_S
    I_loc    <- I_loc - n_exit_I
    S_tr <- numeric(H); I_tr <- numeric(H)
    active_h <- transfer_idx[(n_exit_S[transfer_idx]+
                                n_exit_I[transfer_idx])>0L]
    for (h in active_h) {
      nS <- rbinom(1L, n_exit_S[h], p_tr[h])
      nI <- rbinom(1L, n_exit_I[h], p_tr[h])
      if ((nS+nI)==0L) next
      probs <- P_tr[,h]
      if (nS>0L) S_tr <- S_tr + rmultinom(1L,nS,probs)[,1L]
      if (nI>0L) I_tr <- I_tr + rmultinom(1L,nI,probs)[,1L]
    }
    occ   <- S_loc+I_loc+S_tr+I_tr
    A     <- pmax(0L, beds-occ)
    A_I   <- rbinom(H, A, pi_vec)
    S_loc <- S_loc+S_tr+(A-A_I)
    I_loc <- I_loc+I_tr+A_I
  }

  hwc <- which(colSums(prev_buffer) > 0)
  ss  <- FALSE
  if (length(hwc) > 0) {
    cmn <- colMeans(prev_buffer[,hwc,drop=FALSE])
    csd <- apply(prev_buffer[,hwc,drop=FALSE], 2, sd, na.rm=TRUE)
    ss  <- mean(csd/pmax(cmn,1e-9) < SS_CV_THRESH) >= SS_PROP_HOSP
  }

  inc_365  <- colSums(inc_buffer)/(beds*INC_WINDOW)*1000
  nat_inc  <- sum(inc_buffer)/(total_beds_sum*INC_WINDOW)*1000
  hosp_prv <- colMeans(prev_buffer)

  list(
    beta                 = beta,
    seed                 = seed,
    steady_state_reached = ss,
    overall_prevalence   = sum(I_loc)/total_beds_sum,
    nationwide_inc       = nat_inc,
    hosp_inc_365         = inc_365,
    hosp_prev_mean       = hosp_prv,
    hosp_type            = type_etab,
    type_inc_mean        = tapply(inc_365,  type_etab, mean, na.rm=TRUE),
    type_prev_mean       = tapply(hosp_prv, type_etab, mean, na.rm=TRUE)
  )
}

# =============================================================================
# 6. RUN GRID — PSOCK (Windows)
# =============================================================================

sim_grid <- expand.grid(beta=beta_grid, rep_id=seq_len(N_REP)) %>%
  mutate(sim_seed = seed_base + row_number()*7L)

message("Simulations: ", nrow(sim_grid),
        " (", length(beta_grid), " betas × ", N_REP, " reps)")

cl <- makeCluster(N_CORES, type="PSOCK")
clusterExport(cl, varlist=c(
  "run_prevalence_simulation",
  "H","beds","p_exit","p_tr","P_tr","pi_vec","gamma",
  "transfer_idx","type_etab","total_beds_sum",
  "INIT_PREV","MAX_DAYS","SS_WINDOW","SS_CV_THRESH","SS_PROP_HOSP","INC_WINDOW"
))
t0 <- Sys.time()
all_results <- tryCatch(
  parLapply(cl, seq_len(nrow(sim_grid)), function(i) {
    row <- sim_grid[i,]
    run_prevalence_simulation(beta=row$beta, seed=row$sim_seed)
  }),
  finally = { try(stopCluster(cl), silent=TRUE) }
)
message("Elapsed: ", round(difftime(Sys.time(),t0,units="mins"),1), " min")

# =============================================================================
# 7. COMPILE, MAP TO AMR TIERS, SAVE
# =============================================================================

scalar_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  data.frame(job_index=JOB_INDEX, beta_within=r$beta,
             rep_id=sim_grid$rep_id[i], sim_seed=sim_grid$sim_seed[i],
             steady_state_reached=r$steady_state_reached,
             overall_prevalence=r$overall_prevalence,
             nationwide_inc=r$nationwide_inc, stringsAsFactors=FALSE)
}))

type_df <- bind_rows(lapply(seq_along(all_results), function(i) {
  r <- all_results[[i]]
  tibble(job_index=JOB_INDEX, beta_within=r$beta,
         rep_id=sim_grid$rep_id[i],
         type    = names(r$type_inc_mean),
         inc_1000= as.numeric(r$type_inc_mean),
         prev    = as.numeric(r$type_prev_mean))
}))

beta_summary <- scalar_df %>%
  group_by(beta_within) %>%
  summarise(
    n_reps          = n(),
    prop_ss         = round(mean(steady_state_reached,na.rm=TRUE),3),
    prev_median     = round(median(overall_prevalence,na.rm=TRUE),4),
    prev_q25        = round(quantile(overall_prevalence,0.25,na.rm=TRUE),4),
    prev_q75        = round(quantile(overall_prevalence,0.75,na.rm=TRUE),4),
    inc_median      = round(median(nationwide_inc,na.rm=TRUE),4),
    .groups="drop")

# Map beta to AMR tiers
find_best <- function(tgt) {
  beta_summary %>% mutate(d=abs(prev_median-tgt)) %>%
    slice_min(d,n=1,with_ties=FALSE) %>%
    select(beta_within,prev_median,prev_q25,prev_q75,inc_median,d)
}
amr_mapping <- amr_long %>%
  mutate(best=lapply(target_prevalence, find_best)) %>%
  tidyr::unnest(best) %>%
  mutate(fit=case_when(d<0.01~"Excellent",d<0.03~"Good",
                       d<0.06~"Fair",TRUE~"Poor"))

cat("\n=== BETA SUMMARY ===\n"); print(beta_summary, n=Inf)
cat("\n=== AMR MAPPING ===\n")
print(amr_mapping %>% select(pathogen,tier,target_prevalence,
                               beta_within,prev_median,inc_median,fit), n=Inf)

# Plots
tier_colors <- c(Low="#009E73",Mid="#E69F00",High="#D55E00")

p1 <- ggplot(beta_summary, aes(x=beta_within)) +
  geom_ribbon(aes(ymin=prev_q25,ymax=prev_q75), fill="#56B4E9", alpha=0.25) +
  geom_line(aes(y=prev_median), colour="#0072B2", linewidth=1.1) +
  geom_point(aes(y=prev_median), colour="#0072B2", size=2.5) +
  geom_hline(data=amr_long, aes(yintercept=target_prevalence,
             colour=tier, linetype=pathogen), linewidth=0.7, alpha=0.85) +
  scale_colour_manual(values=tier_colors, name="AMR tier") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  scale_x_continuous(breaks=beta_grid, labels=function(x) sprintf("%.3f",x)) +
  labs(title="β vs facility-level steady-state prevalence (LOCAL — subset)",
       subtitle="Median ± IQR | ECDC AMR targets overlaid",
       x="β", y="Network prevalence (I/N)") +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"))

p2 <- ggplot(amr_mapping, aes(x=tier, y=fct_reorder(pathogen,-beta_within),
                               fill=prev_median)) +
  geom_tile(colour="white", linewidth=1.2) +
  geom_text(aes(label=sprintf("β=%.4f\n%.1f%%",beta_within,prev_median*100)),
            size=3.5, fontface="bold", colour="white") +
  scale_fill_gradientn(colours=c("#009E73","#E69F00","#D55E00"),
                       labels=percent_format(accuracy=0.1),
                       name="Simulated\nprevalence") +
  scale_x_discrete(position="top") +
  labs(title="Best-fit β by AMR pathogen and prevalence tier",
       x="Prevalence tier", y=NULL) +
  theme_minimal(base_size=12) +
  theme(panel.grid=element_blank(),
        axis.text.y=element_text(face="italic"),
        plot.title=element_text(face="bold"))

ggsave(file.path(OUT_DIR,"01_beta_vs_prevalence.png"), p1, width=12,height=6,dpi=150)
ggsave(file.path(OUT_DIR,"02_amr_mapping_heatmap.png"),p2, width=10,height=7,dpi=150)

# Save
saveRDS(list(scalar_df=scalar_df, type_df=type_df,
             beta_summary=beta_summary, amr_mapping=amr_mapping,
             datetime=Sys.time()),
        file.path(OUT_DIR, "results_local.rds"))
write.csv2(amr_mapping %>%
             select(pathogen,tier,target_prevalence,beta_within,
                    prev_median,inc_median,fit),
           file.path(OUT_DIR,"amr_mapping.csv"), row.names=FALSE)
write.csv2(beta_summary, file.path(OUT_DIR,"beta_summary.csv"), row.names=FALSE)

cat("\n=== LOCAL RUN DONE ===\n")
cat("Results saved to:", OUT_DIR, "\n")
