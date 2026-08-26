# =============================================================================
# analyze_region_results.R
# Loads the regional calibration result and produces diagnostics:
#   1. Simulation quality summary (observed vs simulated per type)
#   2. Optimisation convergence (SSE by starting point)
#   3. Iteration history (how SSE evolved during Nelder-Mead)
#   4. Observed vs simulated incidence plot
#   5. Beta estimates with uncertainty bar
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# ── Point this to your region output folder ───────────────────────────────────
# The folder name is the sanitised region name (accents/spaces replaced by _)
REGION_NAME <- "Île-de-France"
region_safe <- gsub("[^A-Za-z0-9_-]", "_", REGION_NAME)

OUT_DIR <- file.path(
  "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/optim_cluster_jobs",
  "Outputs", "region", region_safe
)

###############################################################################
# 1. LOAD RESULT
###############################################################################

rds_files <- list.files(OUT_DIR, pattern = "final_validation\\.rds$",
                         recursive = TRUE, full.names = TRUE)
if (length(rds_files) == 0) stop("No final_validation.rds found under: ", OUT_DIR)

# If multiple runs exist, take the most recent
rds_files <- sort(rds_files, decreasing = TRUE)
message("Loading: ", rds_files[1])
r <- readRDS(rds_files[1])

###############################################################################
# 2. SIMULATION METADATA
###############################################################################

cat("\n=== SIMULATION METADATA ===\n")
cat("Region              :", r$region, "\n")
cat("Hospitals in region :", r$H_region, "\n")
cat("Parallel replicates :", r$n_rep_valid, "(validation)  /", r$n_rep_obj, "(objective)\n")
cat("Cores used          :", r$n_cores, "\n")
cat("Calibration types   :", paste(names(r$incidence_obs), collapse = ", "), "\n")

###############################################################################
# 3. OPTIMISATION CONVERGENCE — SSE by starting point
###############################################################################

cat("\n=== OPTIMISATION CONVERGENCE ===\n")
convergence_df <- data.frame(
  start      = names(r$fit_values),
  sse        = round(r$fit_values, 6),
  converged  = !is.na(r$fit_values) & is.finite(r$fit_values)
) %>% arrange(sse)

print(convergence_df)
cat("Best start    :", names(r$fit_values)[r$best_fit_id], "\n")
cat("Best SSE      :", round(r$fit_values[r$best_fit_id], 6), "\n")
cat("Checkpoint SSE:", round(r$sse_final, 6), "(after validation)\n")

###############################################################################
# 4. SIMULATION QUALITY — observed vs simulated per type
###############################################################################

val <- r$validation_summary

cat("\n=== SIMULATION QUALITY BY FACILITY TYPE ===\n")
quality <- val %>%
  mutate(
    ratio        = round(incidence_sim_mean / incidence_obs, 3),
    pct_error    = round(100 * diff / incidence_obs, 1),
    cv_sim       = round(100 * incidence_sim_sd / incidence_sim_mean, 1)
  ) %>%
  select(type, beta, incidence_obs, incidence_sim_mean, incidence_sim_sd,
         incidence_sim_se, diff, pct_error, ratio, cv_sim, sse)

print(quality, digits = 4)

cat("\nTotal SSE      :", round(r$sse_final, 6), "\n")
cat("Mean abs error :", round(mean(abs(val$diff)), 4), "per 1,000 bed-days\n")
cat("Max abs error  :", round(max(abs(val$diff)), 4), "per 1,000 bed-days\n")
cat("R² (sim vs obs):", round(cor(val$incidence_obs, val$incidence_sim_mean)^2, 4), "\n")

###############################################################################
# 5. ITERATION HISTORY (if history file exists)
###############################################################################

history_files <- list.files(OUT_DIR, pattern = "history_objective\\.csv$",
                              recursive = TRUE, full.names = TRUE)
if (length(history_files) > 0) {
  hist_df <- read.table(sort(history_files, decreasing = TRUE)[1],
                         sep = ";", dec = ".", header = TRUE,
                         stringsAsFactors = FALSE)
  cat("\n=== ITERATION HISTORY ===\n")
  cat("Total objective evaluations :", nrow(hist_df), "\n")
  cat("Evaluations reaching new best:", sum(hist_df$is_best), "\n")
  cat("Final best SSE seen          :", round(min(hist_df$objective_value, na.rm=TRUE), 6), "\n")
} else {
  hist_df <- NULL
  cat("\nNo history file found.\n")
}

###############################################################################
# 6. PLOTS
###############################################################################

plot_dir <- file.path(OUT_DIR, "analysis")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Colour palette matching your project conventions
type_colours <- c(
  "MCO"                            = "#0057B8",
  "SSR"                            = "#C1392B",
  "MCO/SSR"                        = "#9B1D8A",
  "PSY"                            = "#F5A623",
  "HAD"                            = "#00B4D8",
  "Cancer centre (CLCC)"           = "#2ECC71",
  "General public hospital"        = "#0057B8",
  "Private for profit hospital"    = "#E74C3C",
  "Private not-for-profit hospital"= "#9B1D8A",
  "Rehabilitation hospital"        = "#C1392B",
  "University hospital"            = "#F5A623",
  "Unknown"                        = "#555555"
)

# ── Plot A: Observed vs simulated (main calibration plot) ────────────────────
axis_max <- max(c(val$incidence_obs,
                  val$incidence_sim_mean + val$incidence_sim_sd)) * 1.1

pA <- ggplot(val, aes(x = incidence_obs, y = incidence_sim_mean, colour = type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey50", linewidth = 0.9) +
  geom_errorbar(aes(ymin = pmax(incidence_sim_mean - incidence_sim_sd, 0),
                    ymax = incidence_sim_mean + incidence_sim_sd),
                width = 0, linewidth = 0.8, alpha = 0.55) +
  geom_point(size = 4.5) +
  geom_text(aes(label = type),
            hjust = -0.12, vjust = 0.4, size = 3.5, show.legend = FALSE) +
  scale_colour_manual(values = type_colours, na.value = "#555555") +
  scale_x_continuous(limits = c(0, axis_max)) +
  scale_y_continuous(limits = c(0, axis_max)) +
  coord_fixed() +
  labs(
    title    = paste("Calibration:", REGION_NAME),
    subtitle = paste0(r$n_rep_valid, " validation replicates  |  ±1 SD  |  ",
                      "SSE = ", round(r$sse_final, 4),
                      "  |  H = ", r$H_region, " hospitals"),
    x        = "Observed incidence (SPARES, per 1,000 bed-days)",
    y        = "Simulated incidence (per 1,000 bed-days)",
    colour   = "Facility type"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title     = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.minor = element_blank())

print(pA)
ggsave(file.path(plot_dir, "obs_vs_sim.png"), pA, width = 7, height = 7, dpi = 150)

# ── Plot B: Beta estimates per type ──────────────────────────────────────────
pB <- ggplot(val, aes(x = reorder(type, beta), y = beta, fill = type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = formatC(beta, format = "e", digits = 2)),
            hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = type_colours, na.value = "#555555") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  coord_flip() +
  labs(
    title    = paste("Optimal β per facility type —", REGION_NAME),
    subtitle = "Daily within-hospital ESBL transmission rate",
    x        = NULL, y = "Beta (per day)"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.minor = element_blank())

print(pB)
ggsave(file.path(plot_dir, "beta_by_type.png"), pB, width = 8, height = 5, dpi = 150)

# ── Plot C: Percentage error per type ────────────────────────────────────────
pC <- quality %>%
  ggplot(aes(x = reorder(type, abs(pct_error)), y = pct_error, fill = pct_error > 0)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  geom_text(aes(label = paste0(ifelse(pct_error > 0, "+", ""), pct_error, "%")),
            hjust = ifelse(quality$pct_error > 0, -0.1, 1.1), size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#2980B9")) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  coord_flip() +
  labs(
    title    = paste("% error (simulated − observed) —", REGION_NAME),
    subtitle = "Positive = model over-estimates; Negative = under-estimates",
    x = NULL, y = "% error"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "none",
        panel.grid.minor = element_blank())

print(pC)
ggsave(file.path(plot_dir, "pct_error_by_type.png"), pC, width = 8, height = 5, dpi = 150)

# ── Plot D: SSE convergence across starts ─────────────────────────────────────
finite_sse <- convergence_df %>% filter(converged)
if (nrow(finite_sse) > 0) {
  pD <- ggplot(finite_sse, aes(x = reorder(start, sse), y = sse)) +
    geom_col(fill = "#0057B8", width = 0.6) +
    geom_text(aes(label = round(sse, 4)), hjust = -0.1, size = 3.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    coord_flip() +
    labs(title    = paste("SSE by starting point —", REGION_NAME),
         subtitle = "Lower = better fit",
         x = NULL, y = "SSE") +
    theme_bw(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  print(pD)
  ggsave(file.path(plot_dir, "sse_by_start.png"), pD, width = 7, height = 5, dpi = 150)
}

# ── Plot E: Iteration history (if available) ──────────────────────────────────
if (!is.null(hist_df) && nrow(hist_df) > 0) {
  pE <- ggplot(hist_df, aes(x = eval_counter, y = objective_value)) +
    geom_line(colour = "grey70", linewidth = 0.5) +
    geom_point(aes(colour = is_best), size = 1.5) +
    geom_line(aes(y = best_value), colour = "#E74C3C",
              linewidth = 0.8, linetype = "solid") +
    scale_colour_manual(values = c("FALSE" = "grey60", "TRUE" = "#0057B8"),
                        labels = c("FALSE" = "Evaluated", "TRUE" = "New best")) +
    scale_y_log10() +
    labs(title    = paste("Optimisation history —", REGION_NAME),
         subtitle = "Red line = running best SSE",
         x = "Objective evaluation #", y = "SSE (log scale)",
         colour = NULL) +
    theme_bw(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom",
          panel.grid.minor = element_blank())
  print(pE)
  ggsave(file.path(plot_dir, "iteration_history.png"), pE,
         width = 9, height = 5, dpi = 150)
}

###############################################################################
# 7. SAVE TABLES
###############################################################################

write.csv2(quality,
           file.path(plot_dir, "simulation_quality.csv"), row.names = FALSE)
write.csv2(convergence_df,
           file.path(plot_dir, "convergence_by_start.csv"), row.names = FALSE)

message("\nAll outputs saved to: ", plot_dir)
message("Plots: obs_vs_sim | beta_by_type | pct_error_by_type | sse_by_start | iteration_history")
