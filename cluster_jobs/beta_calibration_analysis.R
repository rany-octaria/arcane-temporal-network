# ============================================================
# ARCANE — RESULTS ANALYSIS & PATHOGEN-AGNOSTIC BETA CALIBRATION
# Author: Rany Octaria
# Description:
#   1. Loads simulation output (local or cluster)
#   2. Extracts trajectories and steady-state diagnostics
#   3. Calibrates beta to colonization/carriage prevalence tiers
#      (pathogen-agnostic — based on European screening data)
#   4. Produces colorblind-safe PPT-resolution plots
#
# Tier definitions (colonization/carriage, screening-based):
#   LOW  < 2%   — CPE/CRE (N/W EU), VRE (N. EU nursing homes), CRAB
#   MID  2–10%  — MRSA (hospital/ICU, N/W EU), VRE (DE/IT wards),
#                 ESBL-E (NL/Nordic hospitals)
#   HIGH > 10%  — ESBL-E (CH/BE/FR hospitals, nursing homes broadly),
#                 MRSA (S/E EU, rehab), MDRO overall in nursing homes
#
# Run AFTER: arcane_code_beta_estimation.R (local or cluster)
# ============================================================

library(tidyverse)
library(here)
library(ggdist)     # install.packages("ggdist")
library(patchwork)  # install.packages("patchwork")
library(scales)

# ============================================================
# 0. CONFIG
# ============================================================

run_date  <- "2026-05-10"

# Toggle: TRUE = local run output, FALSE = cluster sharded RDS files
USE_LOCAL <- FALSE

# Suffix baked into all output filenames
run_suffix <- if (USE_LOCAL) "LOCAL" else "SERVER"

# Where the simulation script saved its RDS output
sim_dir    <- here::here("cluster_jobs", "Outputs", run_date)

# All analysis plots and datasets saved here
output_dir <- here::here("cluster_jobs", "Outputs", "Analysis results", run_date)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("Run suffix     : ", run_suffix)
message("Sim input dir  : ", sim_dir)
message("Analysis output: ", output_dir)

# Okabe-Ito colorblind-safe palette
OI <- list(
  orange = "#E69F00", sky    = "#56B4E9", green  = "#009E73",
  yellow = "#F0E442", blue   = "#0072B2", red    = "#D55E00",
  pink   = "#CC79A7", black  = "#000000", grey   = "#999999"
)

tier_colors <- c("Low" = OI$green, "Mid" = OI$orange, "High" = OI$red)

# ============================================================
# 1. LOAD SIMULATION OUTPUT
# ============================================================

if (USE_LOCAL) {
  
  message("Loading LOCAL simulation output...")
  beta_calibration_runs <- readRDS(
    file.path(sim_dir,
              paste0(run_date, "_beta_calibration_runs_LOCAL.rds"))
  )
  
} else {
  
  message("Loading CLUSTER sharded RDS files...")
  rds_files <- list.files(
    sim_dir,
    pattern    = "_beta_calibration_runs_index_\\d+\\.rds",
    full.names = TRUE
  )
  if (length(rds_files) == 0)
    stop("No cluster RDS files found in: ", sim_dir)
  
  beta_calibration_runs <- map(rds_files, readRDS) %>% bind_rows()
  message("Loaded ", length(rds_files), " shards — ",
          nrow(beta_calibration_runs), " total rows")
}

message("Columns: ", paste(names(beta_calibration_runs), collapse = ", "))

# ============================================================
# 2. NORMALISE COLUMN NAMES
# Handles both local (foreach) and cluster (pmap+unnest) outputs
# ============================================================

if ("seed_hospital" %in% names(beta_calibration_runs) &&
    !"seed_hospital_iter" %in% names(beta_calibration_runs)) {
  beta_calibration_runs <- beta_calibration_runs %>%
    rename(seed_hospital_iter = seed_hospital)
}

if (!"row_id" %in% names(beta_calibration_runs)) {
  beta_calibration_runs <- beta_calibration_runs %>%
    mutate(row_id = row_number())
}

HAS_SIM_COL <- "sim" %in% names(beta_calibration_runs)

# ============================================================
# 3. EXTRACT OVERALL TRAJECTORY
# ============================================================

message("Extracting overall trajectories...")

if (HAS_SIM_COL) {
  beta_trajectory_long <- beta_calibration_runs %>%
    transmute(
      beta_within, rep_id, row_id, sim_seed, seed_hospital_iter,
      overall_results = map(sim, "overall_results")
    ) %>%
    unnest(overall_results)
} else {
  beta_trajectory_long <- beta_calibration_runs %>%
    transmute(
      beta_within, rep_id, row_id, sim_seed, seed_hospital_iter,
      overall_results
    ) %>%
    unnest(overall_results)
}

beta_trajectory_long <- beta_trajectory_long %>%
  mutate(
    date       = as.Date(date),
    beta_label = sprintf("β = %.4f", beta_within)
  )

message("Trajectory rows: ", nrow(beta_trajectory_long))

# ============================================================
# 4. STEADY-STATE DIAGNOSTICS
# ============================================================

message("Extracting steady-state diagnostics...")

if (HAS_SIM_COL) {
  beta_calibration_steady_state <- beta_calibration_runs %>%
    transmute(
      beta_within, rep_id, row_id, sim_seed, seed_hospital_iter,
      steady_state_info = map(sim, "steady")
    ) %>%
    unnest(steady_state_info)
} else {
  beta_calibration_steady_state <- beta_calibration_runs %>%
    transmute(
      beta_within, rep_id, row_id, sim_seed, seed_hospital_iter,
      steady_state_info = steady
    ) %>%
    unnest(steady_state_info)
}

message("\n--- Steady state reached? ---")
beta_calibration_steady_state %>% count(steady_state_reached) %>% print()

message("\n--- Proportion steady by beta ---")
beta_calibration_steady_state %>%
  group_by(beta_within) %>%
  summarise(
    prop_steady          = mean(steady_state_reached, na.rm = TRUE),
    median_ss_prevalence = median(steady_state_prevalence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# ============================================================
# 5. FINAL PREVALENCE (last simulated day per rep)
# ============================================================

beta_calibration_final <- beta_trajectory_long %>%
  group_by(beta_within, rep_id, row_id, sim_seed, seed_hospital_iter) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    final_total_infected      = total_infected,
    final_overall_prevalence  = overall_prevalence,
    final_hospitals_with_case = n_hospitals_with_case
  )

message("\n--- Final prevalence sample ---")
beta_calibration_final %>%
  select(beta_within, rep_id, sim_seed,
         seed_hospital_iter, final_overall_prevalence) %>%
  print(n = 20)

# ============================================================
# 6. TRAJECTORY SUMMARY (median + IQR per beta per day)
# ============================================================

beta_trajectory_summary <- beta_trajectory_long %>%
  group_by(beta_within, beta_label, date) %>%
  summarise(
    prevalence_median = median(overall_prevalence,             na.rm = TRUE),
    prevalence_mean   = mean(overall_prevalence,               na.rm = TRUE),
    prevalence_q25    = quantile(overall_prevalence, 0.25,     na.rm = TRUE),
    prevalence_q75    = quantile(overall_prevalence, 0.75,     na.rm = TRUE),
    prevalence_q10    = quantile(overall_prevalence, 0.10,     na.rm = TRUE),
    prevalence_q90    = quantile(overall_prevalence, 0.90,     na.rm = TRUE),
    n_reps            = n(),
    .groups = "drop"
  )

# ============================================================
# 7. BETA PREVALENCE SUMMARY PER BETA VALUE
# ============================================================

beta_summary <- beta_calibration_final %>%
  group_by(beta_within) %>%
  summarise(
    n_reps       = n(),
    prev_mean    = mean(final_overall_prevalence,           na.rm = TRUE),
    prev_median  = median(final_overall_prevalence,         na.rm = TRUE),
    prev_sd      = sd(final_overall_prevalence,             na.rm = TRUE),
    prev_se      = prev_sd / sqrt(n_reps),
    prev_ci95_lo = prev_mean - qt(0.975, df = n_reps - 1) * prev_se,
    prev_ci95_hi = prev_mean + qt(0.975, df = n_reps - 1) * prev_se,
    prev_q25     = quantile(final_overall_prevalence, 0.25, na.rm = TRUE),
    prev_q75     = quantile(final_overall_prevalence, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

message("\nBeta prevalence summary:")
print(beta_summary, n = Inf)

# ============================================================
# 8. PATHOGEN-AGNOSTIC COLONIZATION PREVALENCE TIERS
#
#  LOW  < 2%   CPE/CRE (N/W EU hospitals), VRE (nursing homes,
#              Northern EU), CRAB
#  MID  2–10%  MRSA (hospital general wards/ICU admission, N/W EU),
#              VRE (German/Italian hospital wards),
#              ESBL-E (Dutch/Nordic hospitals)
#  HIGH > 10%  ESBL-E (Swiss, Belgian, French hospitals; nursing
#              homes broadly), MRSA (Southern/Eastern EU, rehab),
#              MDRO overall in nursing homes
# ============================================================

amr_tiers <- tribble(
  ~amr_tier, ~description,                                               ~prev_min, ~prev_max,
  "Low",     "CPE/CRE (N/W EU), VRE (N. EU nursing homes), CRAB",       0.000,     0.020,
  "Mid",     "MRSA (N/W EU wards/ICU), VRE (DE/IT), ESBL-E (NL/Nordic)", 0.020,    0.100,
  "High",    "ESBL-E (CH/BE/FR), MRSA (S/E EU), MDRO nursing homes",    0.100,     0.400
) %>%
  mutate(amr_tier = factor(amr_tier, levels = c("Low", "Mid", "High")))

# ============================================================
# 9. TAG EACH SIM REP INTO A TIER
# ============================================================

beta_final_tiered <- beta_calibration_final %>%
  mutate(
    amr_tier = case_when(
      final_overall_prevalence <  0.020                                         ~ "Low",
      final_overall_prevalence >= 0.020 & final_overall_prevalence <  0.100    ~ "Mid",
      # High tier: no upper cap — full range above 10%
      final_overall_prevalence >= 0.100                                         ~ "High",
      TRUE ~ NA_character_
    ),
    amr_tier = factor(amr_tier, levels = c("Low", "Mid", "High"))
  ) %>%
  filter(!is.na(amr_tier))

message("\n--- Rep count per tier (High tier: full range >= 10%) ---")
beta_final_tiered %>% count(amr_tier) %>% print()

# ============================================================
# 10. BETA SUMMARY PER TIER
#     Mean ± 95% CI (t-distribution) and Median + IQR
# ============================================================

beta_agnostic_summary <- beta_final_tiered %>%
  group_by(amr_tier) %>%
  summarise(
    n_reps        = n(),
    n_beta_values = n_distinct(beta_within),
    
    # Mean ± 95% CI
    beta_mean     = mean(beta_within),
    beta_sd       = sd(beta_within),
    beta_se       = beta_sd / sqrt(n_reps),
    beta_ci95_lo  = beta_mean - qt(0.975, df = n_reps - 1) * beta_se,
    beta_ci95_hi  = beta_mean + qt(0.975, df = n_reps - 1) * beta_se,
    
    # Median + IQR
    beta_median   = median(beta_within),
    beta_q25      = quantile(beta_within, 0.25),
    beta_q75      = quantile(beta_within, 0.75),
    beta_iqr      = IQR(beta_within),
    
    # Prevalence stats in that tier
    prev_mean     = mean(final_overall_prevalence),
    prev_median   = median(final_overall_prevalence),
    prev_q25      = quantile(final_overall_prevalence, 0.25),
    prev_q75      = quantile(final_overall_prevalence, 0.75),
    prev_ci95_lo  = prev_mean - qt(0.975, df = n_reps - 1) *
      (sd(final_overall_prevalence) / sqrt(n_reps)),
    prev_ci95_hi  = prev_mean + qt(0.975, df = n_reps - 1) *
      (sd(final_overall_prevalence) / sqrt(n_reps)),
    .groups = "drop"
  ) %>%
  left_join(amr_tiers, by = "amr_tier")

message("\n--- Pathogen-agnostic beta summary ---")
beta_agnostic_summary %>%
  select(amr_tier, n_reps,
         beta_mean, beta_ci95_lo, beta_ci95_hi,
         beta_median, beta_q25, beta_q75,
         prev_mean, prev_median) %>%
  print()

# ============================================================
# 10b. CALIBRATED BETA PARAMETERS — CLEAN DATAFRAME
#      This is the primary output for downstream modelling steps.
#      One row per tier, containing:
#        - beta_median  : point estimate to use as the reference beta
#        - beta_mean    : alternative central estimate
#        - beta_q25/q75 : IQR — plausible range for sensitivity analyses
#        - beta_ci95_lo/hi : 95% CI for uncertainty propagation
#        - prev_median  : median steady-state prevalence achieved
#      High tier covers the full range (>= 10%, no upper cap).
# ============================================================

beta_calibrated_params <- beta_agnostic_summary %>%
  transmute(
    amr_tier,
    description,
    prev_range        = sprintf("%.0f%%–%.0f%%", prev_min * 100, prev_max * 100),
    n_reps,
    # Point estimates
    beta_median,
    beta_mean,
    # IQR — use for sensitivity analyses
    beta_q25,
    beta_q75,
    beta_iqr,
    # 95% CI — use for uncertainty propagation
    beta_ci95_lo,
    beta_ci95_hi,
    # Achieved steady-state prevalence with these betas
    prev_median,
    prev_mean,
    prev_q25,
    prev_q75
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 6)))

message("\n--- Calibrated beta parameters (ready for downstream modelling) ---")
print(beta_calibrated_params)

# ============================================================
# 11. SAVE ALL OUTPUTS
# ============================================================

out_file <- function(name, ext) sprintf("%s_%s.%s", name, run_suffix, ext)

saveRDS(beta_trajectory_long,
        file.path(output_dir, out_file("beta_trajectory_long",          "rds")))
saveRDS(beta_trajectory_summary,
        file.path(output_dir, out_file("beta_trajectory_summary",       "rds")))
saveRDS(beta_calibration_final,
        file.path(output_dir, out_file("beta_calibration_final",        "rds")))
saveRDS(beta_calibration_steady_state,
        file.path(output_dir, out_file("beta_calibration_steady_state", "rds")))
saveRDS(beta_final_tiered,
        file.path(output_dir, out_file("beta_final_tiered",             "rds")))
saveRDS(beta_agnostic_summary,
        file.path(output_dir, out_file("beta_agnostic_summary",         "rds")))

saveRDS(beta_calibrated_params,
        file.path(output_dir, out_file("beta_calibrated_params",        "rds")))

write_csv(
  beta_agnostic_summary %>% mutate(across(where(is.numeric), ~ round(.x, 6))),
  file.path(output_dir, out_file("beta_agnostic_summary", "csv"))
)

# Primary output for downstream modelling — clean flat CSV
write_csv(
  beta_calibrated_params,
  file.path(output_dir, out_file("beta_calibrated_params", "csv"))
)

message("All outputs saved with suffix: ", run_suffix)

# ============================================================
# 12. SHARED HELPERS
# ============================================================

out_file <- function(name, ext) sprintf("%s_%s.%s", name, run_suffix, ext)

save_ppt <- function(plot, filename, w = 3840, h = 2160) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot     = plot,
    width    = w, height = h, units = "px", dpi = 300
  )
  message("Saved: ", filename)
}

common_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 15, face = "bold"),
    plot.subtitle    = element_text(size = 11, color = "grey40"),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.title       = element_text(size = 12)
  )

# ============================================================

# ============================================================
# 13. PLOT 1 — Trajectory summary: median + IQR per beta
# ============================================================

n_betas     <- n_distinct(beta_trajectory_summary$beta_within)
beta_colors <- colorRampPalette(c(OI$green, OI$sky, OI$orange, OI$red))(n_betas)

plot_trajectories <- ggplot(
  beta_trajectory_summary,
  aes(x = date, y = prevalence_median,
      color = factor(beta_within), fill = factor(beta_within))
) +
  geom_ribbon(aes(ymin = prevalence_q25, ymax = prevalence_q75),
              alpha = 0.20, color = NA) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~ beta_label, scales = "free_y", ncol = 5) +
  scale_color_manual(values = beta_colors, guide = "none") +
  scale_fill_manual(values  = beta_colors, guide = "none") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  common_theme +
  theme(
    strip.text       = element_text(size = 9, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    axis.text.x      = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y      = element_text(size = 7)
  ) +
  labs(
    x        = "Date",
    y        = "Overall prevalence",
    title    = "Median Prevalence Trajectory by β",
    subtitle = "Ribbon = IQR across stochastic replicates | SIS metapopulation | 1,432 hospitals"
  )

print(plot_trajectories)
save_ppt(plot_trajectories, out_file("plot_01_trajectories", "png"))

# ============================================================
# 14. PLOT 2 — Beta vs Steady-State Prevalence Curve
#     with colonization tier bands + vertical lines at the
#     median beta of each tier (from beta_agnostic_summary)
# Text sizes are ~35% larger than common_theme for PPT legibility
# ============================================================

# Pull median beta per tier for the vertical lines
tier_median_betas <- beta_agnostic_summary %>%
  select(amr_tier, beta_median) %>%
  mutate(
    tier_color = tier_colors[as.character(amr_tier)],
    # Label positioned just above the x-axis
    vline_label = sprintf("%s tier\nβ = %.4f", amr_tier, beta_median)
  )

plot_beta_curve <- ggplot(beta_summary,
                          aes(x = beta_within, y = prev_mean)) +
  
  # ── Horizontal tier bands ──────────────────────────────────
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = 0.000, ymax = 0.020, fill = OI$green,  alpha = 0.07) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = 0.020, ymax = 0.100, fill = OI$orange, alpha = 0.07) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = 0.100, ymax = Inf,   fill = OI$red,    alpha = 0.07) +
  
  # ── Horizontal tier boundary lines ────────────────────────
  geom_hline(yintercept = 0.020, linetype = "dashed",
             color = OI$orange, linewidth = 0.8) +
  geom_hline(yintercept = 0.100, linetype = "dashed",
             color = OI$red,    linewidth = 0.8) +
  
  # ── Horizontal tier labels ─────────────────────────────────
  annotate("text", x = min(beta_summary$beta_within),
           y = 0.010, hjust = 0, size = 5, fontface = "bold",
           color = OI$green,  label = "LOW  (<2%)") +
  annotate("text", x = min(beta_summary$beta_within),
           y = 0.058, hjust = 0, size = 5, fontface = "bold",
           color = OI$orange, label = "MID  (2–10%)") +
  annotate("text", x = min(beta_summary$beta_within),
           y = 0.115, hjust = 0, size = 5, fontface = "bold",
           color = OI$red,    label = "HIGH  (>10%)") +
  
  # ── Vertical lines at median beta per tier ─────────────────
  geom_vline(
    data        = tier_median_betas,
    aes(xintercept = beta_median, color = amr_tier),
    linetype    = "solid", linewidth = 1.1, alpha = 0.85,
    show.legend = FALSE
  ) +
  
  # ── Vertical line labels — top of plot ────────────────────
  geom_text(
    data = tier_median_betas,
    aes(x = beta_median, y = Inf,
        label = vline_label, color = amr_tier),
    vjust = 1.3, hjust = 0.5, size = 4.5, fontface = "bold",
    lineheight = 0.9, show.legend = FALSE
  ) +
  
  # ── 95% CI ribbon + mean line + points ─────────────────────
  geom_ribbon(aes(ymin = prev_ci95_lo, ymax = prev_ci95_hi),
              fill = OI$sky, alpha = 0.30) +
  geom_line(color  = OI$blue, linewidth = 1.4) +
  geom_point(color = OI$blue, size = 3.5) +
  
  scale_color_manual(values = tier_colors, guide = "none") +
  scale_x_continuous(
    breaks = unique(beta_summary$beta_within),
    labels = function(x) sprintf("%.3f", x)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.08))  # extra top space for vline labels
  ) +
  
  # ── Text sizes ~35% bigger than common_theme ───────────────
  common_theme +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y  = element_text(size = 13),
    axis.title   = element_text(size = 17),
    plot.title   = element_text(size = 21, face = "bold"),
    plot.subtitle= element_text(size = 15, color = "grey40")
  ) +
  labs(
    x        = "β (within-hospital transmission rate)",
    y        = "Steady-state prevalence",
    title    = "SIS Model: β vs Steady-State Prevalence",
    subtitle = paste0(
      "Mean ± 95% CI  |  Shaded = colonization/carriage tiers  |  ",
      "Vertical lines = median β per tier (European screening benchmarks)")
  )

print(plot_beta_curve)
save_ppt(plot_beta_curve, out_file("plot_02_beta_prevalence_curve", "png"))

# ============================================================
# 15. PLOT 3 — Distribution of prevalence per beta
#     coloured by tier; "density curve" replaces "half-eye"
# ============================================================

plot_raincloud <- ggplot(
  beta_final_tiered,
  aes(x = final_overall_prevalence,
      y = fct_reorder(sprintf("β=%.4f", beta_within), beta_within),
      fill  = amr_tier,
      color = amr_tier)
) +
  ggdist::stat_halfeye(
    adjust = 0.85, width = 0.6, .width = c(0.5, 0.95),
    point_colour = NA, alpha = 0.70
  ) +
  geom_point(position = position_jitter(height = 0.07, seed = 42),
             size = 1.2, alpha = 0.35) +
  geom_vline(xintercept = 0.020, linetype = "dashed",
             color = OI$orange, linewidth = 0.7) +
  geom_vline(xintercept = 0.100, linetype = "dashed",
             color = OI$red,    linewidth = 0.7) +
  scale_fill_manual(name  = "Colonization tier", values = tier_colors) +
  scale_color_manual(name = "Colonization tier", values = tier_colors) +
  scale_x_continuous(
    labels = percent_format(accuracy = 0.1),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  common_theme +
  theme(
    legend.position = "top",
    legend.title    = element_text(face = "bold", size = 11),
    axis.text.x     = element_text(size = 9),
    axis.text.y     = element_text(size = 9, family = "mono")
  ) +
  labs(
    x        = "Final simulated prevalence",
    y        = "β value",
    title    = "Prevalence Distribution per β — Coloured by Colonization Tier",
    subtitle = "Density curve + individual replicates | Intervals = 50% and 95% | Dashed lines = tier boundaries (2%, 10%)"
  )

print(plot_raincloud)
save_ppt(plot_raincloud, out_file("plot_03_prevalence_distribution", "png"))

# ============================================================
# 16. PLOT 4a — Beta distribution per tier
#     Density curve with mean ± 95% CI annotated
# ============================================================

plot_agnostic_rain <- ggplot(
  beta_final_tiered,
  aes(x = beta_within, y = fct_rev(amr_tier),
      fill = amr_tier, color = amr_tier)
) +
  ggdist::stat_halfeye(
    adjust = 0.9, width = 0.55, .width = c(0.50, 0.95),
    point_colour = NA, alpha = 0.75
  ) +
  geom_point(position = position_jitter(height = 0.07, seed = 42),
             size = 1.4, alpha = 0.35) +
  geom_errorbarh(
    data = beta_agnostic_summary,
    aes(xmin = beta_ci95_lo, xmax = beta_ci95_hi, y = fct_rev(amr_tier)),
    height = 0.18, linewidth = 1.1,
    color  = OI$black, inherit.aes = FALSE
  ) +
  geom_point(
    data  = beta_agnostic_summary,
    aes(x = beta_mean, y = fct_rev(amr_tier)),
    shape = 23, size = 5, fill = OI$black,
    color = "white", stroke = 1.3, inherit.aes = FALSE
  ) +
  geom_text(
    data = beta_agnostic_summary,
    aes(x     = beta_mean,
        y     = as.numeric(fct_rev(amr_tier)) + 0.40,
        label = sprintf(
          "Mean = %.4f  [95%% CI: %.4f – %.4f]\nMedian = %.4f  IQR: %.4f – %.4f",
          beta_mean, beta_ci95_lo, beta_ci95_hi,
          beta_median, beta_q25, beta_q75
        )),
    size = 3.4, hjust = 0.5, color = OI$black, inherit.aes = FALSE
  ) +
  scale_fill_manual(values  = tier_colors, guide = "none") +
  scale_color_manual(values = tier_colors, guide = "none") +
  scale_x_continuous(
    labels = function(x) sprintf("%.4f", x),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  common_theme +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14, face = "bold")
  ) +
  labs(
    x        = "β (within-hospital transmission rate)",
    y        = "Colonization tier",
    title    = "Beta Distribution by Colonization Tier — Mean ± 95% CI",
    subtitle = "◆ = mean | bar = 95% CI | density curve | European carriage/screening benchmarks"
  )

print(plot_agnostic_rain)
save_ppt(plot_agnostic_rain, out_file("plot_04a_agnostic_beta_density_mean_ci", "png"))

# ============================================================
# 17. PLOT 4b — Beta distribution per tier — MEDIAN + IQR version
#     Same structure but highlighting median and IQR instead
# ============================================================

plot_agnostic_rain_median <- ggplot(
  beta_final_tiered,
  aes(x = beta_within, y = fct_rev(amr_tier),
      fill = amr_tier, color = amr_tier)
) +
  ggdist::stat_halfeye(
    adjust = 0.9, width = 0.55, .width = c(0.25, 0.75),  # IQR intervals
    point_colour = NA, alpha = 0.75
  ) +
  geom_point(position = position_jitter(height = 0.07, seed = 42),
             size = 1.4, alpha = 0.35) +
  # IQR bar
  geom_errorbarh(
    data = beta_agnostic_summary,
    aes(xmin = beta_q25, xmax = beta_q75, y = fct_rev(amr_tier)),
    height = 0.22, linewidth = 2.8, alpha = 0.45,
    color  = OI$black, inherit.aes = FALSE
  ) +
  # Median diamond
  geom_point(
    data  = beta_agnostic_summary,
    aes(x = beta_median, y = fct_rev(amr_tier)),
    shape = 23, size = 5, fill = "white",
    color = OI$black, stroke = 1.8, inherit.aes = FALSE
  ) +
  geom_text(
    data = beta_agnostic_summary,
    aes(x     = beta_median,
        y     = as.numeric(fct_rev(amr_tier)) + 0.40,
        label = sprintf(
          "Median = %.4f  [IQR: %.4f – %.4f]\nMean = %.4f  [95%% CI: %.4f – %.4f]",
          beta_median, beta_q25, beta_q75,
          beta_mean, beta_ci95_lo, beta_ci95_hi
        )),
    size = 3.4, hjust = 0.5, color = OI$black, inherit.aes = FALSE
  ) +
  scale_fill_manual(values  = tier_colors, guide = "none") +
  scale_color_manual(values = tier_colors, guide = "none") +
  scale_x_continuous(
    labels = function(x) sprintf("%.4f", x),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  common_theme +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14, face = "bold")
  ) +
  labs(
    x        = "β (within-hospital transmission rate)",
    y        = "Colonization tier",
    title    = "Beta Distribution by Colonization Tier — Median + IQR",
    subtitle = "◇ = median | thick bar = IQR | density curve | European carriage/screening benchmarks"
  )

print(plot_agnostic_rain_median)
save_ppt(plot_agnostic_rain_median, out_file("plot_04b_agnostic_beta_density_median_iqr", "png"))

# ============================================================
# 18. PLOT 5a — Forest plot: MEAN ± 95% CI per tier
# ============================================================

plot_forest_mean_ci <- ggplot(
  beta_agnostic_summary,
  aes(x = beta_mean, y = fct_rev(amr_tier), color = amr_tier)
) +
  # 95% CI thin bar
  geom_errorbarh(aes(xmin = beta_ci95_lo, xmax = beta_ci95_hi),
                 height = 0.12, linewidth = 1.3) +
  # Mean dot
  geom_point(size = 5.5, shape = 19) +
  # Right stats label
  geom_text(
    aes(x     = beta_ci95_hi,
        label = sprintf(
          "  Mean %.4f\n  [95%% CI: %.4f – %.4f]",
          beta_mean, beta_ci95_lo, beta_ci95_hi
        )),
    hjust = 0, size = 3.8, color = OI$black
  ) +
  # Left tier label
  geom_text(
    aes(x     = beta_ci95_lo,
        label = sprintf("%s\n(%s)", amr_tier, description)),
    hjust = 1.05, size = 3.5, fontface = "italic", color = OI$black
  ) +
  # n label
  geom_text(
    aes(x     = beta_mean,
        y     = as.numeric(fct_rev(amr_tier)) + 0.30,
        label = paste0("n = ", n_reps, " reps")),
    size = 3.2, color = OI$grey, inherit.aes = FALSE
  ) +
  scale_color_manual(values = tier_colors, guide = "none") +
  scale_x_continuous(
    labels = function(x) sprintf("%.4f", x),
    expand = expansion(mult = c(0.45, 0.45))
  ) +
  common_theme +
  theme(
    axis.text.x        = element_text(size = 10),
    axis.text.y        = element_blank(),
    axis.title.y       = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    x        = "β (within-hospital transmission rate)",
    title    = "Plausible β — Mean ± 95% CI by Colonization Tier",
    subtitle = "● = mean | thin bar = 95% CI | Pathogen-agnostic | European screening benchmarks"
  )

print(plot_forest_mean_ci)
save_ppt(plot_forest_mean_ci, out_file("plot_05a_forest_mean_ci", "png"))

# ============================================================
# 19. PLOT 5b — Forest plot: MEDIAN + IQR per tier
# ============================================================

plot_forest_median_iqr <- ggplot(
  beta_agnostic_summary,
  aes(x = beta_median, y = fct_rev(amr_tier), color = amr_tier)
) +
  # IQR thick bar
  geom_errorbarh(aes(xmin = beta_q25, xmax = beta_q75),
                 height = 0.25, linewidth = 4.5, alpha = 0.40) +
  # Median tick
  geom_point(aes(x = beta_median), shape = 124, size = 10, stroke = 1.5) +
  # Median dot (open diamond)
  geom_point(size = 5.5, shape = 23, fill = "white", stroke = 1.8) +
  # Right stats label
  geom_text(
    aes(x     = beta_q75,
        label = sprintf(
          "  Median %.4f\n  [IQR: %.4f – %.4f]",
          beta_median, beta_q25, beta_q75
        )),
    hjust = 0, size = 3.8, color = OI$black
  ) +
  # Left tier label
  geom_text(
    aes(x     = beta_q25,
        label = sprintf("%s\n(%s)", amr_tier, description)),
    hjust = 1.05, size = 3.5, fontface = "italic", color = OI$black
  ) +
  # n label
  geom_text(
    aes(x     = beta_median,
        y     = as.numeric(fct_rev(amr_tier)) + 0.30,
        label = paste0("n = ", n_reps, " reps")),
    size = 3.2, color = OI$grey, inherit.aes = FALSE
  ) +
  scale_color_manual(values = tier_colors, guide = "none") +
  scale_x_continuous(
    labels = function(x) sprintf("%.4f", x),
    expand = expansion(mult = c(0.45, 0.45))
  ) +
  common_theme +
  theme(
    axis.text.x        = element_text(size = 10),
    axis.text.y        = element_blank(),
    axis.title.y       = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    x        = "β (within-hospital transmission rate)",
    title    = "Plausible β — Median + IQR by Colonization Tier",
    subtitle = "◇ = median | | = median line | thick bar = IQR | Pathogen-agnostic | European screening benchmarks"
  )

print(plot_forest_median_iqr)
save_ppt(plot_forest_median_iqr, out_file("plot_05b_forest_median_iqr", "png"))

# ============================================================
# 20. PLOT 6 — Violin + Boxplot: hospital prevalence
#     at 6 months, 1 year, 2 years, 3 years
#     Uses overall_prevalence from beta_trajectory_long
#     faceted by colonization tier
# ============================================================

# Define timepoints relative to simulation start 2024-01-01
timepoints <- tibble(
  label       = factor(
    c("6 months", "1 year", "2 years", "3 years"),
    levels = c("6 months", "1 year", "2 years", "3 years")
  ),
  target_date = as.Date(c("2024-07-01", "2025-01-01", "2026-01-01", "2026-12-31"))
)

# For each timepoint, snap to the nearest available date in the data
available_dates <- sort(unique(beta_trajectory_long$date))

snap_date <- function(target) {
  available_dates[which.min(abs(available_dates - target))]
}

timepoints <- timepoints %>%
  mutate(snapped_date = as.Date(sapply(target_date, snap_date), origin = "1970-01-01"))

# Filter trajectory to only the snapped timepoint dates
# then tag each rep with its tier
beta_timepoint_data <- beta_trajectory_long %>%
  filter(date %in% timepoints$snapped_date) %>%
  left_join(timepoints %>% select(label, snapped_date),
            by = c("date" = "snapped_date")) %>%
  left_join(
    beta_final_tiered %>%
      select(beta_within, rep_id, sim_seed, seed_hospital_iter, amr_tier) %>%
      distinct(),
    by = c("beta_within", "rep_id", "sim_seed", "seed_hospital_iter")
  ) %>%
  filter(!is.na(amr_tier))

# ---- 20a. Violin + boxplot faceted by tier ----

plot_timepoint_violin <- ggplot(
  beta_timepoint_data,
  aes(x = label, y = overall_prevalence,
      fill = amr_tier, color = amr_tier)
) +
  geom_violin(alpha = 0.45, linewidth = 0.5, scale = "width", trim = TRUE) +
  geom_boxplot(
    width = 0.18, outlier.size = 0.8, outlier.alpha = 0.4,
    alpha = 0.85, color = OI$black, linewidth = 0.5
  ) +
  facet_wrap(~ amr_tier, ncol = 3, scales = "free_y") +
  scale_fill_manual(values  = tier_colors, guide = "none") +
  scale_color_manual(values = tier_colors, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  common_theme +
  theme(
    strip.text       = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    axis.text.x      = element_text(size = 11),
    axis.text.y      = element_text(size = 9)
  ) +
  labs(
    x        = "Time since start of simulation",
    y        = "Overall network prevalence",
    title    = "Hospital Network Prevalence at Key Timepoints by Colonization Tier",
    subtitle = "Violin = full distribution | Box = median + IQR | Whiskers = 1.5×IQR | Faceted by tier"
  )

print(plot_timepoint_violin)
save_ppt(plot_timepoint_violin, out_file("plot_06a_timepoint_violin_by_tier", "png"))

# ---- 20b. Same but faceted by beta value (for diagnostic use) ----

plot_timepoint_violin_beta <- ggplot(
  beta_timepoint_data,
  aes(x = label, y = overall_prevalence,
      fill = amr_tier, color = amr_tier)
) +
  geom_violin(alpha = 0.45, linewidth = 0.4, scale = "width", trim = TRUE) +
  geom_boxplot(
    width = 0.2, outlier.size = 0.6, outlier.alpha = 0.3,
    alpha = 0.85, color = OI$black, linewidth = 0.4
  ) +
  facet_wrap(~ beta_label, scales = "free_y", ncol = 5) +
  scale_fill_manual(name  = "Colonization tier", values = tier_colors) +
  scale_color_manual(name = "Colonization tier", values = tier_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  common_theme +
  theme(
    legend.position  = "top",
    legend.title     = element_text(face = "bold", size = 11),
    strip.text       = element_text(size = 8, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    axis.text.x      = element_text(size = 7, angle = 35, hjust = 1),
    axis.text.y      = element_text(size = 7)
  ) +
  labs(
    x        = "Time since start of simulation",
    y        = "Overall network prevalence",
    title    = "Hospital Network Prevalence at Key Timepoints by β",
    subtitle = "Violin = full distribution | Box = median + IQR | Colour = colonization tier"
  )

print(plot_timepoint_violin_beta)
save_ppt(plot_timepoint_violin_beta, out_file("plot_06b_timepoint_violin_by_beta", "png"))

# ---- 20c. Median trajectory with timepoint markers highlighted ----

plot_trajectory_timepoints <- ggplot(
  beta_trajectory_summary %>%
    left_join(
      beta_final_tiered %>%
        group_by(beta_within, amr_tier) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(beta_within) %>%
        slice_max(n, n = 1, with_ties = FALSE) %>%
        select(beta_within, amr_tier),
      by = "beta_within"
    ),
  aes(x = date, y = prevalence_median,
      color = amr_tier, group = beta_within)
) +
  geom_ribbon(aes(ymin = prevalence_q25, ymax = prevalence_q75,
                  fill = amr_tier),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  # Timepoint vertical lines
  geom_vline(data = timepoints,
             aes(xintercept = snapped_date),
             linetype = "dotted", color = OI$grey, linewidth = 0.8) +
  geom_text(data = timepoints,
            aes(x = snapped_date, y = Inf, label = label),
            inherit.aes = FALSE,
            vjust = 1.3, hjust = -0.05, size = 3.2, color = OI$grey) +
  scale_color_manual(name  = "Colonization tier", values = tier_colors) +
  scale_fill_manual(name   = "Colonization tier", values = tier_colors) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  common_theme +
  theme(
    legend.position = "top",
    legend.title    = element_text(face = "bold", size = 11),
    axis.text.x     = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y     = element_text(size = 9)
  ) +
  labs(
    x        = "Date",
    y        = "Median overall prevalence",
    title    = "Prevalence Trajectories with Analysis Timepoints Marked",
    subtitle = "Each line = one β value | Ribbon = IQR | Colour = dominant colonization tier | Dotted = analysis timepoints"
  )

print(plot_trajectory_timepoints)
save_ppt(plot_trajectory_timepoints, out_file("plot_06c_trajectory_with_timepoints", "png"))

# ============================================================
# 21. COMBINED DASHBOARD
# ============================================================

dashboard <- (plot_beta_curve        | plot_forest_mean_ci)   /
  (plot_forest_median_iqr | plot_agnostic_rain)    +
  plot_annotation(
    title    = "ARCANE — Pathogen-Agnostic Beta Calibration | European AMR Colonization Tiers",
    subtitle = "SIS metapopulation model | 1,432 French hospitals | Colonization/carriage prevalence benchmarks",
    theme    = theme(
      plot.title      = element_text(size = 18, face = "bold"),
      plot.subtitle   = element_text(size = 13, color = "grey40"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

print(dashboard)
save_ppt(dashboard, out_file("plot_00_dashboard", "png"), w = 5120, h = 2880)

# ============================================================
# 22. RESTRICTED HIGH TIER — steady-state prevalence 10%–50%
#     Sensitivity analysis: relaxes the 40% upper cap to 50%.
#     Low and Mid tiers are identical to the main analysis.
# ============================================================

message("\n--- Restricted High tier: 10%–50% ---")

beta_final_tiered_50 <- beta_calibration_final %>%
  mutate(
    amr_tier = case_when(
      final_overall_prevalence <  0.020                                       ~ "Low",
      final_overall_prevalence >= 0.020 & final_overall_prevalence <  0.100  ~ "Mid",
      final_overall_prevalence >= 0.100 & final_overall_prevalence <= 0.500  ~ "High",
      TRUE ~ NA_character_
    ),
    amr_tier = factor(amr_tier, levels = c("Low", "Mid", "High"))
  ) %>%
  filter(!is.na(amr_tier))

n_added <- nrow(beta_final_tiered_50) - nrow(beta_final_tiered)
message("Reps in High tier: full range vs 10-50% restricted, difference: ", n_added)
beta_final_tiered_50 %>% count(amr_tier) %>% print()

beta_agnostic_summary_50 <- beta_final_tiered_50 %>%
  group_by(amr_tier) %>%
  summarise(
    n_reps        = n(),
    n_beta_values = n_distinct(beta_within),
    beta_mean     = mean(beta_within),
    beta_sd       = sd(beta_within),
    beta_se       = beta_sd / sqrt(n_reps),
    beta_ci95_lo  = beta_mean - qt(0.975, df = n_reps - 1) * beta_se,
    beta_ci95_hi  = beta_mean + qt(0.975, df = n_reps - 1) * beta_se,
    beta_median   = median(beta_within),
    beta_q25      = quantile(beta_within, 0.25),
    beta_q75      = quantile(beta_within, 0.75),
    beta_iqr      = IQR(beta_within),
    prev_mean     = mean(final_overall_prevalence),
    prev_median   = median(final_overall_prevalence),
    prev_q25      = quantile(final_overall_prevalence, 0.25),
    prev_q75      = quantile(final_overall_prevalence, 0.75),
    .groups = "drop"
  ) %>%
  left_join(amr_tiers, by = "amr_tier")

message("Beta summary (High capped at 50%):")
beta_agnostic_summary_50 %>%
  select(amr_tier, n_reps, beta_median, beta_q25, beta_q75,
         beta_mean, beta_ci95_lo, beta_ci95_hi, prev_median) %>%
  print()

comparison_caps <- bind_rows(
  beta_agnostic_summary    %>% filter(amr_tier == "High") %>% mutate(cap = "Full range (main)"),
  beta_agnostic_summary_50 %>% filter(amr_tier == "High") %>% mutate(cap = "10-50% restricted (sensitivity)")
) %>%
  select(cap, n_reps, beta_median, beta_q25, beta_q75,
         beta_ci95_lo, beta_ci95_hi, prev_median)

message("\n--- High tier: full range vs 10-50% restricted ---")
print(comparison_caps)

saveRDS(beta_agnostic_summary_50,
        file.path(output_dir, out_file("beta_agnostic_summary_50pct", "rds")))
write_csv(
  beta_agnostic_summary_50 %>% mutate(across(where(is.numeric), ~ round(.x, 6))),
  file.path(output_dir, out_file("beta_agnostic_summary_50pct", "csv"))
)
write_csv(
  comparison_caps %>% mutate(across(where(is.numeric), ~ round(.x, 6))),
  file.path(output_dir, out_file("beta_high_tier_cap_comparison", "csv"))
)

# ---- Plot 22a: density + median/IQR — 50% cap ---------------
plot_rain_50 <- ggplot(
  beta_final_tiered_50,
  aes(x = beta_within, y = fct_rev(amr_tier),
      fill = amr_tier, color = amr_tier)
) +
  ggdist::stat_halfeye(
    adjust = 0.9, width = 0.55, .width = c(0.25, 0.75),
    point_colour = NA, alpha = 0.75
  ) +
  geom_point(position = position_jitter(height = 0.07, seed = 42),
             size = 1.4, alpha = 0.35) +
  geom_errorbarh(
    data   = beta_agnostic_summary_50,
    aes(xmin = beta_q25, xmax = beta_q75, y = fct_rev(amr_tier)),
    height = 0.22, linewidth = 2.8, alpha = 0.45,
    color  = OI$black, inherit.aes = FALSE
  ) +
  geom_point(
    data  = beta_agnostic_summary_50,
    aes(x = beta_median, y = fct_rev(amr_tier)),
    shape = 23, size = 5, fill = "white",
    color = OI$black, stroke = 1.8, inherit.aes = FALSE
  ) +
  geom_text(
    data = beta_agnostic_summary_50,
    aes(x     = beta_median,
        y     = as.numeric(fct_rev(amr_tier)) + 0.40,
        label = sprintf("Median = %.4f  [IQR: %.4f - %.4f]",
                        beta_median, beta_q25, beta_q75)),
    size = 3.4, hjust = 0.5, color = OI$black, inherit.aes = FALSE
  ) +
  scale_fill_manual(values  = tier_colors, guide = "none") +
  scale_color_manual(values = tier_colors, guide = "none") +
  scale_x_continuous(labels = function(x) sprintf("%.4f", x),
                     expand = expansion(mult = c(0.02, 0.05))) +
  common_theme +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14, face = "bold")) +
  labs(
    x        = "beta (within-hospital transmission rate)",
    y        = "Colonization tier",
    title    = "Beta Distribution by Tier — Median + IQR  (High tier: 10-50%)",
    subtitle = "Diamond = median | thick bar = IQR | density curve | High tier upper cap relaxed to 50%"
  )

print(plot_rain_50)
save_ppt(plot_rain_50, out_file("plot_22a_rain_50pct_cap", "png"))

# ---- Plot 22b: forest plot — 40% vs 50% cap comparison ------
plot_forest_50_compare <- ggplot(
  bind_rows(
    beta_agnostic_summary    %>% filter(amr_tier == "High") %>% mutate(cap = "Full range (main)"),
    beta_agnostic_summary_50 %>% filter(amr_tier == "High") %>% mutate(cap = "10-50% restricted (sensitivity)")
  ),
  aes(x = beta_median, y = fct_rev(cap), color = cap)
) +
  geom_errorbarh(aes(xmin = beta_q25, xmax = beta_q75),
                 height = 0.20, linewidth = 4, alpha = 0.35) +
  geom_point(aes(x = beta_median), shape = 124, size = 8, stroke = 1.5) +
  geom_point(size = 5, shape = 23, fill = "white", stroke = 1.8) +
  geom_text(
    aes(x     = beta_q75,
        label = sprintf("  Median %.4f  [IQR: %.4f - %.4f]",
                        beta_median, beta_q25, beta_q75)),
    hjust = 0, size = 3.8, color = OI$black
  ) +
  scale_color_manual(
    values = c("Full range (main)"        = OI$red,
               "10-50% restricted (sensitivity)" = OI$pink),
    guide  = "none"
  ) +
  scale_x_continuous(labels = function(x) sprintf("%.4f", x),
                     expand = expansion(mult = c(0.05, 0.50))) +
  common_theme +
  theme(axis.text.x        = element_text(size = 10),
        axis.text.y        = element_text(size = 12, face = "bold"),
        panel.grid.major.y = element_blank()) +
  labs(
    x        = "beta (within-hospital transmission rate)",
    y        = NULL,
    title    = "High Tier Sensitivity: Full Range vs 10-50% Restricted",
    subtitle = "Diamond = median | thick bar = IQR | main = full range (>=10%) | sensitivity = 10-50% cap"
  )

print(plot_forest_50_compare)
save_ppt(plot_forest_50_compare, out_file("plot_22b_forest_50pct_compare", "png"))

# ============================================================
# 23. HOSPITALS WITH >=1 CASE — DISTRIBUTION AT TIMEPOINTS
#     At 6, 12, 18, 24, 30, 36 months: how many hospitals
#     have at least one case across all beta values and reps?
#     Uses n_hospitals_with_case from beta_trajectory_long.
# ============================================================

message("\n--- Hospital case count distribution at timepoints ---")

hosp_timepoints <- tibble(
  label       = factor(
    c("6 months", "12 months", "18 months",
      "24 months", "30 months", "36 months"),
    levels = c("6 months", "12 months", "18 months",
               "24 months", "30 months", "36 months")
  ),
  target_date = as.Date(c(
    "2024-07-01", "2025-01-01", "2025-07-01",
    "2026-01-01", "2026-07-01", "2026-12-31"
  ))
) %>%
  mutate(snapped_date = as.Date(
    sapply(target_date, function(d)
      available_dates[which.min(abs(available_dates - d))]),
    origin = "1970-01-01"))

hosp_case_data <- beta_trajectory_long %>%
  filter(date %in% hosp_timepoints$snapped_date) %>%
  left_join(hosp_timepoints %>% select(label, snapped_date),
            by = c("date" = "snapped_date")) %>%
  left_join(
    beta_final_tiered %>%
      select(beta_within, rep_id, sim_seed, seed_hospital_iter, amr_tier) %>%
      distinct(),
    by = c("beta_within", "rep_id", "sim_seed", "seed_hospital_iter")
  ) %>%
  filter(!is.na(amr_tier))

message("Rows for hospital case distribution: ", nrow(hosp_case_data))

# ---- Plot 23a: violin + box, faceted by tier ----------------
plot_hosp_violin_tier <- ggplot(
  hosp_case_data,
  aes(x = label, y = n_hospitals_with_case,
      fill = amr_tier, color = amr_tier)
) +
  geom_violin(alpha = 0.45, linewidth = 0.5, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.18, outlier.size = 0.8, outlier.alpha = 0.4,
               alpha = 0.85, color = OI$black, linewidth = 0.5) +
  facet_wrap(~ amr_tier, ncol = 3, scales = "free_y") +
  scale_fill_manual(values  = tier_colors, guide = "none") +
  scale_color_manual(values = tier_colors, guide = "none") +
  scale_y_continuous(labels = comma) +
  common_theme +
  theme(
    strip.text       = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    axis.text.x      = element_text(size = 10, angle = 30, hjust = 1),
    axis.text.y      = element_text(size = 9)
  ) +
  labs(
    x        = "Time since simulation start",
    y        = "Number of hospitals with >=1 case",
    title    = "Distribution of Hospitals with Active Cases at Key Timepoints",
    subtitle = "Violin = full distribution | Box = median + IQR | Faceted by colonization tier"
  )

print(plot_hosp_violin_tier)
save_ppt(plot_hosp_violin_tier, out_file("plot_23a_hosp_cases_by_tier", "png"))

# ---- Plot 23b: faceted by beta value -------------------------
plot_hosp_violin_beta <- ggplot(
  hosp_case_data,
  aes(x = label, y = n_hospitals_with_case,
      fill = amr_tier, color = amr_tier)
) +
  geom_violin(alpha = 0.40, linewidth = 0.4, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.2, outlier.size = 0.5, outlier.alpha = 0.3,
               alpha = 0.80, color = OI$black, linewidth = 0.4) +
  facet_wrap(~ beta_label, scales = "free_y", ncol = 5) +
  scale_fill_manual(name  = "Colonization tier", values = tier_colors) +
  scale_color_manual(name = "Colonization tier", values = tier_colors) +
  scale_y_continuous(labels = comma) +
  common_theme +
  theme(
    legend.position  = "top",
    legend.title     = element_text(face = "bold", size = 11),
    strip.text       = element_text(size = 8, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    axis.text.x      = element_text(size = 7, angle = 35, hjust = 1),
    axis.text.y      = element_text(size = 7)
  ) +
  labs(
    x        = "Time since simulation start",
    y        = "Number of hospitals with >=1 case",
    title    = "Hospitals with Active Cases at Key Timepoints by beta",
    subtitle = "Violin = full distribution | Box = median + IQR | Colour = colonization tier"
  )

print(plot_hosp_violin_beta)
save_ppt(plot_hosp_violin_beta, out_file("plot_23b_hosp_cases_by_beta", "png"))

# ---- Plot 23c: median trajectory with timepoint markers ------
hosp_case_summary <- beta_trajectory_long %>%
  left_join(
    beta_final_tiered %>%
      group_by(beta_within, amr_tier) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(beta_within) %>%
      slice_max(n, n = 1, with_ties = FALSE) %>%
      select(beta_within, amr_tier),
    by = "beta_within"
  ) %>%
  filter(!is.na(amr_tier)) %>%
  group_by(amr_tier, beta_within, beta_label, date) %>%
  summarise(
    hosp_median = median(n_hospitals_with_case, na.rm = TRUE),
    hosp_q25    = quantile(n_hospitals_with_case, 0.25, na.rm = TRUE),
    hosp_q75    = quantile(n_hospitals_with_case, 0.75, na.rm = TRUE),
    .groups     = "drop"
  )

plot_hosp_trajectory <- ggplot(
  hosp_case_summary,
  aes(x = date, y = hosp_median, color = amr_tier, group = beta_within)
) +
  geom_ribbon(aes(ymin = hosp_q25, ymax = hosp_q75, fill = amr_tier),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.6, alpha = 0.75) +
  geom_vline(data = hosp_timepoints,
             aes(xintercept = snapped_date),
             linetype = "dotted", color = OI$grey, linewidth = 0.7,
             inherit.aes = FALSE) +
  geom_text(data = hosp_timepoints,
            aes(x = snapped_date, y = Inf, label = label),
            vjust = 1.4, hjust = -0.05, size = 3.0,
            color = OI$grey, inherit.aes = FALSE) +
  scale_color_manual(name  = "Colonization tier", values = tier_colors) +
  scale_fill_manual(name   = "Colonization tier", values = tier_colors) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
  scale_y_continuous(labels = comma) +
  common_theme +
  theme(
    legend.position = "top",
    legend.title    = element_text(face = "bold", size = 11),
    axis.text.x     = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y     = element_text(size = 9)
  ) +
  labs(
    x        = "Date",
    y        = "Median hospitals with >=1 case",
    title    = "Epidemic Spread: Hospitals with Active Cases Over Time",
    subtitle = "Each line = one beta value | Ribbon = IQR | Colour = dominant tier | Dotted = analysis timepoints"
  )

print(plot_hosp_trajectory)
save_ppt(plot_hosp_trajectory, out_file("plot_23c_hosp_cases_trajectory", "png"))

# Summary table
hosp_case_table <- hosp_case_data %>%
  group_by(amr_tier, label) %>%
  summarise(
    n_reps       = n(),
    hosp_median  = median(n_hospitals_with_case, na.rm = TRUE),
    hosp_q25     = quantile(n_hospitals_with_case, 0.25, na.rm = TRUE),
    hosp_q75     = quantile(n_hospitals_with_case, 0.75, na.rm = TRUE),
    hosp_mean    = mean(n_hospitals_with_case, na.rm = TRUE),
    hosp_ci95_lo = hosp_mean - qt(0.975, df = n_reps - 1) *
      (sd(n_hospitals_with_case, na.rm = TRUE) / sqrt(n_reps)),
    hosp_ci95_hi = hosp_mean + qt(0.975, df = n_reps - 1) *
      (sd(n_hospitals_with_case, na.rm = TRUE) / sqrt(n_reps)),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

message("\n--- Hospitals with cases: median [IQR] per tier per timepoint ---")
print(hosp_case_table, n = Inf)

write_csv(hosp_case_table,
          file.path(output_dir, out_file("hosp_case_count_by_tier_timepoint", "csv")))
saveRDS(hosp_case_data,
        file.path(output_dir, out_file("hosp_case_data_timepoints", "rds")))

message("\n All done. Outputs saved to:\n  ", output_dir)

#Look at overall prevalence on the final dayof the sim

 final_overall_prev =beta_trajectory_long %>%
  filter(date == as.Date("2026-12-31")) %>%
  select(beta_within, rep_id, overall_prevalence)
#