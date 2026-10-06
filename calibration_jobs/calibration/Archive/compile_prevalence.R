# =============================================================================
# compile_prevalence.R  —  Pool all calibration jobs and find best beta
# =============================================================================
# Reads from two separate output folders (original run + high-beta extension)
# and pools all simulations into one flat dataset before tier mapping.
#
# KEY FIX: COMPILED_DIR is a single independent path — never derived from
# RUN_FOLDERS (which is a vector). file.path(vector, "compiled") produces
# a vector of paths and breaks dir.create().
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# =============================================================================
# 0. PATHS
# =============================================================================

LOCAL_ROOT <- "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/calibration_jobs"
DATA_DIR   <- file.path(LOCAL_ROOT, "data")

# Folders containing job_XX subfolders — add or remove entries as needed
RUN_FOLDERS <- c(
  file.path(LOCAL_ROOT, "Outputs", "prevalence_new")#,       # original: 0.005-0.120
 # file.path(LOCAL_ROOT, "Outputs", "prevalence_high")   # extension: 0.125-0.350
)

# Compiled output — one fixed path, NOT derived from RUN_FOLDERS
COMPILED_DIR <- file.path(LOCAL_ROOT, "Outputs", "prevalence_compiled_new")
dir.create(COMPILED_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. LOAD INCIDENCE TARGETS
# =============================================================================

targets_obj    <- readRDS(file.path(DATA_DIR, "inc_targets.rds"))
inc_targets    <- targets_obj$inc_targets
factor_network <- targets_obj$factor_network
los_network    <- targets_obj$los_network
gamma          <- targets_obj$gamma
pi_vec_val     <- targets_obj$pi_vec_val

message("Incidence targets:")
for (i in seq_len(nrow(inc_targets))) {
  r <- inc_targets[i, ]
  message(sprintf("  %-10s  %.3f - %.3f /1k pd  [%s]",
                  as.character(r$tier), r$inc_low, r$inc_high,
                  r$ecdc_category))
}

# =============================================================================
# 2. DISCOVER ALL JOB FOLDERS ACROSS BOTH RUNS
# =============================================================================

message("\nScanning run folders...")
job_dirs <- c()

for (folder in RUN_FOLDERS) {
  if (!dir.exists(folder)) {
    message("  NOT FOUND (skipping): ", folder)
    next
  }
  found <- list.dirs(folder, full.names = TRUE, recursive = FALSE)
  found <- found[grepl("job_\\d+$", basename(found))]
  message("  ", basename(folder), ": ", length(found), " job folders found")
  job_dirs <- c(job_dirs, found)
}

message("Total job folders: ", length(job_dirs))
if (length(job_dirs) == 0)
  stop("No job folders found. Check RUN_FOLDERS paths above.")

# =============================================================================
# 3. LOAD AND POOL
# =============================================================================

all_scalar  <- list()
all_traj    <- list()
params_ref  <- NULL
jobs_loaded <- 0

for (jdir in sort(job_dirs)) {
  rds_path <- file.path(jdir, "results.rds")
  if (!file.exists(rds_path)) {
    message("  MISSING: ", jdir)
    next
  }
  
  res        <- readRDS(rds_path)
  run_label  <- basename(dirname(jdir))
  
  sc <- res$scalar_df %>%
    mutate(run_folder = run_label,
           global_id  = paste0(run_label, "_j", job_index, "_r", rep_id))
  
  key <- paste0(run_label, "_", basename(jdir))
  all_scalar[[key]] <- sc
  if (!is.null(res$traj_df))
    all_traj[[key]] <- res$traj_df %>% mutate(run_folder = run_label)
  if (is.null(params_ref)) params_ref <- res$params
  
  jobs_loaded <- jobs_loaded + 1
  message(sprintf("  %-45s  %d sims | beta %.3f-%.3f | extinct %.0f%%",
                  key, nrow(sc),
                  min(sc$beta_within), max(sc$beta_within),
                  mean(sc$extinct) * 100))
}

scalar_df <- bind_rows(all_scalar)
traj_df   <- if (length(all_traj) > 0) bind_rows(all_traj) else NULL

message("\nJobs loaded  : ", jobs_loaded)
message("Total sims   : ", nrow(scalar_df))
message("Beta range   : ", min(scalar_df$beta_within),
        " - ", max(scalar_df$beta_within))
message("Unique betas : ", n_distinct(scalar_df$beta_within))

# =============================================================================
# 4. BETA SUMMARY
# =============================================================================

beta_summary <- scalar_df %>%
  group_by(beta_within) %>%
  summarise(
    n_reps      = n(),
    n_jobs      = n_distinct(job_index),
    pct_extinct = round(mean(extinct) * 100, 1),
    pct_ss      = round(mean(steady_state_reached & !extinct) * 100, 1),
    inc_mean    = round(mean(reported_inc[!extinct],             na.rm=TRUE), 4),
    inc_median  = round(median(reported_inc[!extinct],           na.rm=TRUE), 4),
    inc_q25     = round(quantile(reported_inc[!extinct], 0.25,  na.rm=TRUE), 4),
    inc_q75     = round(quantile(reported_inc[!extinct], 0.75,  na.rm=TRUE), 4),
    inc_sd      = round(sd(reported_inc[!extinct],               na.rm=TRUE), 4),
    prev_median = round(median(net_prev_final[!extinct],         na.rm=TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(beta_within)

cat("\n=== BETA SUMMARY ===\n")
print(beta_summary, n = Inf)

# =============================================================================
# 5. TIER MAPPING
# =============================================================================

scalar_eligible  <- scalar_df %>% filter(!extinct, reported_inc > 0)

scalar_with_tier <- scalar_eligible %>%
  tidyr::crossing(inc_targets %>% select(tier, inc_low, inc_high)) %>%
  filter(reported_inc >= inc_low & reported_inc <= inc_high)

if (nrow(scalar_with_tier) == 0)
  warning("No qualifying reps for any tier — check inc_targets ranges vs reported_inc.")

tier_analysis <- scalar_with_tier %>%
  group_by(tier, inc_low, inc_high) %>%
  summarise(
    n_reps       = n(),
    n_jobs       = n_distinct(job_index),
    n_betas      = n_distinct(beta_within),
    best_beta    = beta_within[which.min(abs(
      reported_inc - (inc_low + inc_high) / 2))],
    mean_beta    = round(mean(beta_within),            5),
    median_beta  = round(median(beta_within),          5),
    ci_lo_95     = round(quantile(beta_within, 0.025), 5),
    ci_hi_95     = round(quantile(beta_within, 0.975), 5),
    mean_inc_ach = round(mean(reported_inc),           4),
    sd_inc_ach   = round(sd(reported_inc),             4),
    .groups = "drop"
  ) %>%
  left_join(inc_targets %>% select(tier, ecdc_category, ecdc_examples),
            by = "tier") %>%
  mutate(tier = factor(tier, levels = c("Low","Moderate","High"))) %>%
  arrange(tier)

cat("\n=== TIER ANALYSIS ===\n")
print(tier_analysis %>%
        select(tier, ecdc_category, inc_low, inc_high,
               n_reps, n_jobs, n_betas,
               best_beta, mean_beta, ci_lo_95, ci_hi_95,
               mean_inc_ach), n = Inf)

cat("\n=== RECOMMENDED BETA VALUES ===\n")
for (i in seq_len(nrow(tier_analysis))) {
  r <- tier_analysis[i, ]
  cat(sprintf(
    "  %-10s (%.2f-%.2f /1k pd): best=%.5f | mean=%.5f | 95%% CI [%.5f, %.5f]\n",
    as.character(r$tier), r$inc_low, r$inc_high,
    r$best_beta, r$mean_beta, r$ci_lo_95, r$ci_hi_95))
  cat(sprintf("             %d reps | %d jobs | achieved %.4f /1k pd\n",
              r$n_reps, r$n_jobs, r$mean_inc_ach))
}

# =============================================================================
# 6. PLOTS
# =============================================================================

tier_pal  <- c(Low="#009E73", Moderate="#E69F00", High="#D55E00")
tier_pal2 <- c(Low="#b2dfdb", Moderate="#ffe0b2", High="#ffccbc")

p1 <- ggplot(beta_summary %>% filter(!is.na(inc_median)),
             aes(x = beta_within)) +
  geom_rect(data = inc_targets,
            aes(xmin=-Inf, xmax=Inf,
                ymin=inc_low, ymax=inc_high, fill=tier),
            alpha=0.15, inherit.aes=FALSE) +
  geom_ribbon(aes(ymin=inc_q25, ymax=inc_q75), fill="#56B4E9", alpha=0.35) +
  geom_line(aes(y=inc_median),  colour="#0072B2", linewidth=1.2) +
  geom_point(aes(y=inc_median), colour="#0072B2", size=2.5) +
  scale_fill_manual(values=tier_pal2, name="Tier range") +
  scale_x_continuous(labels=function(x) sprintf("%.3f", x)) +
  labs(title    = paste0("Beta vs incidence — ", jobs_loaded,
                         " jobs | ", nrow(scalar_df), " simulations"),
       subtitle = "Median +/- IQR | Shaded = ECDC tier ranges",
       x = "beta (per day)", y = "Incidence (per 1,000 patient-days)") +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"))

p2 <- beta_summary %>%
  ggplot(aes(x=beta_within, y=pct_extinct, fill=pct_extinct)) +
  geom_col(width = diff(range(beta_summary$beta_within)) /
             n_distinct(beta_summary$beta_within) * 0.8) +
  geom_text(aes(label=paste0(pct_extinct,"%")), vjust=-0.4, size=2.8) +
  scale_fill_gradient(high="#e74c3c", low="#2ecc71", guide="none") +
  scale_x_continuous(labels=function(x) sprintf("%.3f", x)) +
  scale_y_continuous(limits=c(0,112), expand=expansion(mult=c(0,0))) +
  labs(title="Extinction rate by beta (all pooled reps)",
       x="beta (per day)", y="% extinct") +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"))

p3 <- tier_analysis %>%
  ggplot(aes(x=tier, colour=tier)) +
  geom_linerange(aes(ymin=ci_lo_95, ymax=ci_hi_95),
                 linewidth=3.5, alpha=0.4) +
  geom_point(aes(y=mean_beta),   size=6,   shape=18) +
  geom_point(aes(y=best_beta),   size=3.5, shape=1,  colour="grey30") +
  geom_point(aes(y=median_beta), size=3.5, shape=5,  colour="grey40") +
  scale_colour_manual(values=tier_pal, guide="none") +
  scale_y_continuous(labels=scientific) +
  labs(title    = "Recommended beta per ECDC incidence tier",
       subtitle = "Diamond=mean | Circle=best-fit | Pentagon=median | Bar=95% CI",
       x="Tier", y="beta (per day)") +
  theme_bw(base_size=12) +
  theme(panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"))

ggsave(file.path(COMPILED_DIR,"01_beta_vs_incidence.png"),  p1, width=12,height=6,dpi=150)
ggsave(file.path(COMPILED_DIR,"02_extinction_by_beta.png"), p2, width=10,height=5,dpi=150)
ggsave(file.path(COMPILED_DIR,"03_best_beta_per_tier.png"), p3, width=8, height=6,dpi=150)
message("Plots saved.")

# =============================================================================
# 7. SAVE
# =============================================================================

run_date <- format(Sys.Date(), "%Y%m%d")

saveRDS(list(
  scalar_df    = scalar_df,
  beta_summary = beta_summary,
  tier_analysis= tier_analysis,
  inc_targets  = inc_targets,
  n_jobs       = jobs_loaded,
  total_sims   = nrow(scalar_df),
  run_folders  = RUN_FOLDERS,
  datetime     = Sys.time(),
  params       = params_ref
), file.path(COMPILED_DIR, paste0("compiled_prevalence_", run_date, ".rds")))

write.csv(beta_summary,
          file.path(COMPILED_DIR, "beta_summary.csv"),  row.names=FALSE)
write.csv(tier_analysis,
          file.path(COMPILED_DIR, "tier_analysis.csv"), row.names=FALSE)

cat("\n=== DONE ===\n")
cat("Jobs loaded  :", jobs_loaded, "\n")
cat("Total sims   :", nrow(scalar_df), "\n")
cat("Beta range   :", min(scalar_df$beta_within),
    "-", max(scalar_df$beta_within), "\n")
cat("Saved to     :", COMPILED_DIR, "\n")