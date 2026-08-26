# =============================================================================
# analyze_france_results.R
# Loads all France-wide calibration results, summarises beta estimates,
# and plots observed vs simulated incidence.
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# ── Point this to your local Outputs/france folder ───────────────────────────
OUT_DIR <- "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/optim_cluster_jobs/Outputs/france"

###############################################################################
# 1. LOAD ALL RESULTS
###############################################################################

rds_files <- list.files(OUT_DIR,
                        pattern  = "final_validation\\.rds$",
                        recursive = TRUE,
                        full.names = TRUE)
rds_files <- rds_files[!grepl("/compiled/", rds_files)]

if (length(rds_files) == 0) stop("No final_validation.rds files found in ", OUT_DIR)
message("Found ", length(rds_files), " completed jobs.")

results <- lapply(rds_files, readRDS)

###############################################################################
# 2. COMBINED VALIDATION SUMMARY (one row per type × job)
###############################################################################

all_summaries <- bind_rows(lapply(results, function(r) {
  r$validation_summary %>%
    mutate(job_index = r$job_index,
           sse_total = r$sse_final)
}))

# SSE by job — which run was best?
sse_by_job <- all_summaries %>%
  distinct(job_index, sse_total) %>%
  arrange(sse_total)

cat("\n=== SSE BY JOB (sorted, lower = better) ===\n")
print(sse_by_job)

best_job <- sse_by_job$job_index[1]
best_sse <- sse_by_job$sse_total[1]
cat("\nBest job:", best_job, " | SSE:", round(best_sse, 6), "\n")

###############################################################################
# 3. BETA ESTIMATES BY TYPE ACROSS ALL JOBS
###############################################################################

beta_all <- all_summaries %>%
  select(job_index, sse_total, type, beta) %>%
  arrange(type, sse_total)

cat("\n=== BETA PER TYPE × JOB ===\n")
print(beta_all %>% pivot_wider(names_from = type, values_from = beta))

# Best beta (from lowest-SSE job)
beta_best <- all_summaries %>%
  filter(job_index == best_job) %>%
  select(type, beta, incidence_obs, incidence_sim_mean,
         incidence_sim_sd, diff, sse)

cat("\n=== BEST BETA (job ", best_job, ", SSE = ", round(best_sse, 6), ") ===\n")
print(beta_best)

###############################################################################
# 4. PLOT A — Observed vs Simulated incidence (best job)
###############################################################################

p1 <- ggplot(beta_best,
             aes(x = incidence_obs, y = incidence_sim_mean, colour = type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey50", linewidth = 0.8) +
  geom_errorbar(aes(ymin = incidence_sim_mean - incidence_sim_sd,
                    ymax = incidence_sim_mean + incidence_sim_sd),
                width = 0, linewidth = 0.7, alpha = 0.5) +
  geom_point(size = 4) +
  geom_text(aes(label = type),
            nudge_y = max(beta_best$incidence_obs) * 0.04,
            size = 3.2, show.legend = FALSE) +
  coord_fixed() +
  scale_colour_brewer(palette = "Set1") +
  labs(
    title    = "Calibration: simulated vs. observed ESBL incidence (best job)",
    subtitle = paste0("Job ", best_job, "  |  SSE = ", round(best_sse, 4),
                      "  |  ±1 SD across ", results[[1]]$n_rep_valid, " replicates"),
    x        = "Observed incidence (SPARES, per 1,000 bed-days)",
    y        = "Simulated incidence (per 1,000 bed-days)",
    colour   = "Facility type"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

print(p1)

###############################################################################
# 5. PLOT B — Beta estimates across all jobs (variability check)
###############################################################################

p2 <- ggplot(beta_all, aes(x = reorder(type, beta, median),
                            y = beta, colour = factor(job_index))) +
  geom_point(size = 3, position = position_jitter(width = 0.1, seed = 1)) +
  geom_point(data = filter(beta_all, job_index == best_job),
             shape = 8, size = 5, colour = "black", stroke = 1.2) +
  scale_y_log10(labels = scales::scientific) +
  scale_colour_viridis_d(name = "Job") +
  labs(
    title    = "Beta estimates across all jobs",
    subtitle = paste0("★ = best job (", best_job, ")  |  log10 scale"),
    x        = "Facility type",
    y        = "Beta (per day, log scale)"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title       = element_text(face = "bold"),
        axis.text.x      = element_text(angle = 30, hjust = 1),
        legend.position  = "right",
        panel.grid.minor = element_blank())

print(p2)

###############################################################################
# 6. PLOT C — Observed vs Simulated for ALL jobs (small multiples)
###############################################################################

p3 <- ggplot(all_summaries,
             aes(x = incidence_obs, y = incidence_sim_mean)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey50") +
  geom_errorbar(aes(ymin = incidence_sim_mean - incidence_sim_sd,
                    ymax = incidence_sim_mean + incidence_sim_sd),
                width = 0, alpha = 0.4) +
  geom_point(aes(colour = type), size = 3) +
  facet_wrap(~ paste0("Job ", job_index, "\nSSE=", round(sse_total, 3)),
             ncol = 5) +
  scale_colour_brewer(palette = "Set1", name = "Type") +
  labs(
    title = "Observed vs simulated incidence — all jobs",
    x     = "Observed (per 1,000 bed-days)",
    y     = "Simulated (per 1,000 bed-days)"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title       = element_text(face = "bold"),
        legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92"))

print(p3)

###############################################################################
# 7. SAVE OUTPUTS
###############################################################################

plot_dir <- file.path(OUT_DIR, "analysis")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(plot_dir, "obs_vs_sim_best.png"),
       plot = p1, width = 7, height = 7, dpi = 150)
ggsave(file.path(plot_dir, "beta_across_jobs.png"),
       plot = p2, width = 9, height = 6, dpi = 150)
ggsave(file.path(plot_dir, "obs_vs_sim_all_jobs.png"),
       plot = p3, width = 14, height = 8, dpi = 150)

write.csv2(all_summaries,
           file.path(plot_dir, "all_jobs_validation_summary.csv"),
           row.names = FALSE)
write.csv2(beta_best,
           file.path(plot_dir, "best_job_beta.csv"),
           row.names = FALSE)

message("\nPlots and tables saved to: ", plot_dir)
