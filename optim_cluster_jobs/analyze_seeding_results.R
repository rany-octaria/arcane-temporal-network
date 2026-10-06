# =============================================================================
# analyze_seeding_results.R
# Visualises and summarises the seeding scenario simulation output.
#
# Run after arcane_seeding_local.R has completed.
# Loads: seeding_timeseries_YYYYMMDD.rds + seeding_summary_YYYYMMDD.rds
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# ── Paths ─────────────────────────────────────────────────────────────────────
OUT_DIR <- file.path(
  "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/optim_cluster_jobs",
  "Outputs", "seeding"
)

# Load most recent run
ts_file  <- tail(sort(list.files(OUT_DIR, "seeding_timeseries_.*\\.rds$",
                                  full.names = TRUE)), 1)
sum_file <- tail(sort(list.files(OUT_DIR, "seeding_summary_.*\\.rds$",
                                  full.names = TRUE)), 1)

if (length(ts_file)  == 0) stop("No timeseries file found in ", OUT_DIR)
if (length(sum_file) == 0) stop("No summary file found in ", OUT_DIR)

message("Loading: ", basename(ts_file))
ts  <- readRDS(ts_file)
sdf <- readRDS(sum_file)

TOTAL_BEDS <- sum(unique(sdf %>% distinct(seed_hospital, .keep_all = FALSE)) %>%
                    nrow())  # approximate
N_HOSPITALS <- length(unique(c(ts$seed_hospital)))

# Colour palette per seed rule
seed_colours <- c(
  "highest_in_degree"   = "#0057B8",
  "highest_out_degree"  = "#F5A623",
  "highest_betweenness" = "#9B1D8A",
  "largest_beds"        = "#C1392B",
  "largest_outgoing"    = "#2ECC71",
  "random_MCO"          = "#00B4D8",
  "random_SSR"          = "#E67E22",
  "random_MCO_SSR"      = "#888888"
)

theme_arcane <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(plot.title       = element_text(face = "bold", colour = "#0D3B66"),
          plot.subtitle    = element_text(colour = "grey40", size = base_size - 1),
          panel.grid.minor = element_blank(),
          legend.position  = "right",
          strip.background = element_rect(fill = "grey93"),
          strip.text       = element_text(face = "bold"))
}

plot_dir <- file.path(OUT_DIR, "analysis")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. OUTBREAK TRAJECTORY — median + IQR of total infected over time
# =============================================================================

traj <- ts %>%
  group_by(seed_rule, day) %>%
  summarise(
    med_infected  = median(total_infected),
    q25_infected  = quantile(total_infected, 0.25),
    q75_infected  = quantile(total_infected, 0.75),
    med_hosp      = median(n_hospitals_infected),
    med_prev      = median(overall_prevalence),
    .groups = "drop"
  )

p1 <- ggplot(traj, aes(x = day, colour = seed_rule, fill = seed_rule)) +
  geom_ribbon(aes(ymin = q25_infected, ymax = q75_infected), alpha = 0.15,
              colour = NA) +
  geom_line(aes(y = med_infected), linewidth = 0.9) +
  scale_colour_manual(values = seed_colours, name = "Seed rule") +
  scale_fill_manual(values = seed_colours, name = "Seed rule") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Outbreak trajectory — total infected patients over time",
    subtitle = "Median across replicates ± IQR  |  All seed rules",
    x = "Day", y = "Total infected patients"
  ) +
  theme_arcane()

print(p1)
ggsave(file.path(plot_dir, "01_outbreak_trajectory.png"),
       p1, width = 10, height = 5.5, dpi = 150)

# =============================================================================
# 2. HOSPITALS REACHED OVER TIME
# =============================================================================

p2 <- ggplot(traj, aes(x = day, y = med_hosp,
                         colour = seed_rule)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = seed_colours, name = "Seed rule") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Hospitals with ≥ 1 infected patient over time",
    subtitle = "Median across replicates",
    x = "Day", y = "Number of hospitals"
  ) +
  theme_arcane()

print(p2)
ggsave(file.path(plot_dir, "02_hospitals_reached.png"),
       p2, width = 10, height = 5.5, dpi = 150)

# =============================================================================
# 3. OVERALL PREVALENCE OVER TIME
# =============================================================================

p3 <- ggplot(traj, aes(x = day, y = med_prev * 1000,
                         colour = seed_rule)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = seed_colours, name = "Seed rule") +
  labs(
    title    = "Overall network prevalence over time",
    subtitle = "Median across replicates  |  per 1,000 beds",
    x = "Day", y = "Prevalence (per 1,000 beds)"
  ) +
  theme_arcane()

print(p3)
ggsave(file.path(plot_dir, "03_network_prevalence.png"),
       p3, width = 10, height = 5.5, dpi = 150)

# =============================================================================
# 4. FINAL NATIONAL INCIDENCE BY SEED RULE
# Incidence = total infected at Tmax / total beds × 1,000
# =============================================================================

final_inc <- sdf %>%
  group_by(seed_rule) %>%
  summarise(
    n_reps          = n(),
    mean_final_inf  = mean(final_infected, na.rm = TRUE),
    sd_final_inf    = sd(final_infected, na.rm = TRUE),
    med_final_inf   = median(final_infected, na.rm = TRUE),
    q25_final_inf   = quantile(final_infected, 0.25, na.rm = TRUE),
    q75_final_inf   = quantile(final_infected, 0.75, na.rm = TRUE),
    mean_prevalence = mean(final_prevalence * 1000, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== FINAL NATIONAL INCIDENCE BY SEED RULE ===\n")
print(final_inc)

p4 <- ggplot(sdf, aes(x = reorder(seed_rule, final_prevalence, FUN = median),
                       y = final_prevalence * 1000,
                       fill = seed_rule)) +
  geom_boxplot(width = 0.6, outlier.shape = 21, outlier.size = 1.5,
               alpha = 0.8) +
  scale_fill_manual(values = seed_colours, guide = "none") +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  labs(
    title    = "Final network prevalence by seed rule",
    subtitle = "Distribution across replicates at end of simulation (per 1,000 beds)",
    x = NULL, y = "Final prevalence (per 1,000 beds)"
  ) +
  theme_arcane()

print(p4)
ggsave(file.path(plot_dir, "04_final_prevalence_by_rule.png"),
       p4, width = 9, height = 5, dpi = 150)

# =============================================================================
# 5. AVERAGE HOSPITAL-LEVEL INCIDENCE AT END OF SIMULATION
# =============================================================================

hosp_inc <- ts %>%
  filter(day == max(day)) %>%
  group_by(seed_rule, rep_id) %>%
  summarise(
    avg_hosp_prev = mean(overall_prevalence * 1000, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(seed_rule) %>%
  summarise(
    mean_avg = mean(avg_hosp_prev),
    sd_avg   = sd(avg_hosp_prev),
    med_avg  = median(avg_hosp_prev),
    .groups  = "drop"
  )

cat("\n=== AVERAGE HOSPITAL INCIDENCE AT SIMULATION END (per 1,000 beds) ===\n")
print(hosp_inc)

# =============================================================================
# 6. PROPORTION OF EXTINCTION BY SEED RULE
# =============================================================================

extinction <- sdf %>%
  group_by(seed_rule) %>%
  summarise(
    n_reps        = n(),
    n_extinct     = sum(ever_extinct, na.rm = TRUE),
    pct_extinct   = round(100 * mean(ever_extinct, na.rm = TRUE), 1),
    med_day_ext   = median(day_extinct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_extinct))

cat("\n=== EXTINCTION PROBABILITY BY SEED RULE ===\n")
print(extinction)

p5 <- ggplot(extinction,
             aes(x = reorder(seed_rule, pct_extinct), y = pct_extinct,
                 fill = seed_rule)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(pct_extinct, "%")), hjust = -0.1, size = 3.8) +
  scale_fill_manual(values = seed_colours, guide = "none") +
  scale_y_continuous(limits = c(0, 110), expand = expansion(mult = c(0, 0))) +
  coord_flip() +
  labs(
    title    = "Proportion of simulations ending in pathogen extinction",
    subtitle = "Higher % = seeding from this node more likely to self-limit",
    x = NULL, y = "% replicates extinct by end of simulation"
  ) +
  theme_arcane()

print(p5)
ggsave(file.path(plot_dir, "05_extinction_probability.png"),
       p5, width = 9, height = 5, dpi = 150)

# =============================================================================
# 7. PEAK INFECTED BY SEED RULE
# =============================================================================

p6 <- ggplot(sdf, aes(x = reorder(seed_rule, peak_infected, FUN = median),
                       y = peak_infected,
                       fill = seed_rule)) +
  geom_boxplot(width = 0.6, outlier.shape = 21, outlier.size = 1.5,
               alpha = 0.8) +
  scale_fill_manual(values = seed_colours, guide = "none") +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  labs(
    title    = "Peak number of infected patients by seed rule",
    subtitle = "Distribution across replicates",
    x = NULL, y = "Peak infected patients"
  ) +
  theme_arcane()

print(p6)
ggsave(file.path(plot_dir, "06_peak_infected_by_rule.png"),
       p6, width = 9, height = 5, dpi = 150)

# =============================================================================
# 8. DAY OF PEAK INFECTED
# =============================================================================

p7 <- ggplot(sdf, aes(x = reorder(seed_rule, day_of_peak, FUN = median),
                       y = day_of_peak,
                       fill = seed_rule)) +
  geom_boxplot(width = 0.6, outlier.shape = 21, outlier.size = 1.5,
               alpha = 0.8) +
  scale_fill_manual(values = seed_colours, guide = "none") +
  coord_flip() +
  labs(
    title    = "Day of peak infection by seed rule",
    subtitle = "Earlier peak = faster initial spread from seed node",
    x = NULL, y = "Day of peak"
  ) +
  theme_arcane()


print(p7)
ggsave(file.path(plot_dir, "07_day_of_peak_by_rule.png"),
       p7, width = 9, height = 5, dpi = 150)

# =============================================================================
# 9. PEAK HOSPITALS REACHED BY SEED RULE
# =============================================================================

p8 <- ggplot(sdf, aes(x = reorder(seed_rule, peak_hospitals, FUN = median),
                       y = peak_hospitals,
                       fill = seed_rule)) +
  geom_boxplot(width = 0.6, outlier.shape = 21, outlier.size = 1.5,
               alpha = 0.8) +
  scale_fill_manual(values = seed_colours, guide = "none") +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  labs(
    title    = "Maximum hospitals reached by seed rule",
    subtitle = "Distribution across replicates",
    x = NULL, y = "Peak hospitals with ≥ 1 infected patient"
  ) +
  theme_arcane()

print(p8)
ggsave(file.path(plot_dir, "08_peak_hospitals_by_rule.png"),
       p8, width = 9, height = 5, dpi = 150)

# =============================================================================
# 10. COMPREHENSIVE SUMMARY TABLE
# =============================================================================

summary_table <- sdf %>%
  group_by(seed_rule, seed_type) %>%
  summarise(
    n_reps           = n(),
    # Extinction
    pct_extinct      = round(100 * mean(ever_extinct, na.rm = TRUE), 1),
    # Peak
    med_peak_inf     = round(median(peak_infected)),
    med_day_peak     = round(median(day_of_peak)),
    med_peak_hosp    = round(median(peak_hospitals)),
    # Final state
    med_final_inf    = round(median(final_infected)),
    med_final_prev   = round(median(final_prevalence * 1000), 3),
    .groups          = "drop"
  ) %>%
  arrange(desc(med_peak_inf))

cat("\n=== COMPREHENSIVE SUMMARY TABLE ===\n")
print(summary_table, n = Inf)


write.csv2(summary_table,
           file.path(plot_dir, "summary_table.csv"),
           row.names = FALSE)
write.csv2(extinction,
           file.path(plot_dir, "extinction_by_rule.csv"),
           row.names = FALSE)
write.csv2(final_inc,
           file.path(plot_dir, "final_incidence_by_rule.csv"),
           row.names = FALSE)


message("\nAll plots and tables saved to: ", plot_dir)

