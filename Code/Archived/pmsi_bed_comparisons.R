# ══════════════════════════════════════════════════════════════════════════════
# Beds Comparison Report – PMSI vs CAPACT vs Census Max
# ══════════════════════════════════════════════════════════════════════════════

library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(here)

# If you don't have these yet:
# install.packages(c("ggplot2", "patchwork", "tidyr", "here", "gridExtra",
#                    "scales", "ggtext", "knitr", "rmarkdown", "tinytex"))
# tinytex::install_tinytex()  # only needed once for PDF rendering



library(tidyverse)
library(readxl)
library(httr)
library(here)



# =============================================================================
# PART 7: COMPARING BED COUNTS TO CENSUS NUMBERS
# =============================================================================

#getting all the stays data
stays = read_delim(here("Datasets", "MCO_SSR_HBN_2024", "MCO_SSR_HBN_IP_Direct_2024" ,
                                           "WORK_QUERY_FOR_BEFORE_DIRECT_SSR_MCO_HBN.csv"),   
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)


node_attributes_enriched = read.csv (here("Datasets", "Output Data", 
                                          "Enriched Facility Data","node_attributes_enriched.csv"))
str(stays)
# Now parse
stays$date_entree <- as.Date(stays$date_entree, format = "%d/%m/%Y")
stays$date_sortie <- as.Date(stays$date_sortie, format = "%d/%m/%Y")

str(stays)

#Removing same-day stays
stays = filter(stays, LOS_days >0)

# Define your date range 
# Including only February until three weeks of December to remove outliers

date_seq <- seq(as.Date("2024-02-01"), as.Date("2024-12-23"), by = "day")

# Count census for one day across all hospitals
count_day <- function(d) {
  stays %>%
    filter(date_entree <= d & date_sortie >= d) %>%
    group_by(FinessGeo) %>%
    summarise(n_patients = n(), .groups = "drop") %>%
    mutate(day = d)
}


# Apply over all days
daily_census <- map_dfr(date_seq, count_day)

# Fill in 0s for hospital-days with no patients
daily_census <- daily_census %>%
  complete(FinessGeo, day = date_seq, fill = list(n_patients = 0))

hospital_stats <- daily_census %>%
  group_by(FinessGeo) %>%
  summarise(
    min      = min(n_patients),
    max      = max(n_patients),
    mean     = round(mean(n_patients), 2),
    median   = median(n_patients),
    ci95_low  =  quantile(n_patients, probs = 0.05, na.rm = TRUE),
    ci95_high =  quantile(n_patients, probs = 0.95, na.rm = TRUE),
    .groups = "drop"
  )%>% 
  rename(finess_geo =FinessGeo) 

print(hospital_stats)


#Now merge it with the nodes attribute enriched data 
stats_vs_beds = left_join(node_attributes_enriched, hospital_stats, 
                          by ="finess_geo")
#Show only the variables we need to see

comparisons_mco = stats_vs_beds %>% 
  select(finess_geo, hospital_type, 
         facility_name, facility_type_capact, total_beds_mco,  no_beds, 
         lit_MCO, lit_SSR, min, max, mean, median, ci95_low, ci95_high)  %>% 
  filter(hospital_type != "SSR") %>% 
  rename (
    pmsi_total_beds_mco  =  total_beds_mco ,
    capact_total_beds = no_beds, 
    capact_beds_mco = lit_MCO,
    capact_beds_ssr = lit_SSR,
    census_min = min,
    census_max = max, 
    census_mean = mean, 
    census_median = median, 
    census_95ci_low = ci95_low,
    census_95ci_high = ci95_high)  

comparison_mco = comparisons_mco %>% 
  select(
    finess_geo, facility_name, hospital_type,facility_type_capact, 
    pmsi_total_beds_mco, capact_total_beds, capact_beds_mco, capact_beds_ssr,
    census_max, census_95ci_high, census_median, everything())


#Adding differences between census mean and p95

comparison_mco = comparison_mco %>% 
  mutate(
    diff_max_mean_prop = (census_max - census_mean)/census_max,
    diff_max_95cihi_prop = (census_max - census_95ci_high)/census_max
  )

#Create the histogram of proportion of differences 

hist(comparison_mco$diff_max_mean_prop)
hist(comparison_mco$diff_max_95cihi_prop)

library(ggplot2)
library(scales)

# Histogram 1: difference in max mean proportion
ggplot(comparison_mco, aes(x = diff_max_mean_prop)) +
  geom_histogram(bins = 30, fill = "#4C9F8F", color = "white", alpha = 0.9) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Difference between Census Maximum and Mean",
    subtitle = "Distribution across facilities",
    x = "Difference (%)",
    y = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid.minor = element_blank()
  )

# Histogram 2: difference in max 95% CI upper bound proportion
ggplot(comparison_mco, aes(x = diff_max_95cihi_prop)) +
  geom_histogram(bins = 30, fill = "#E07A5F", color = "white", alpha = 0.9) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Difference between  Census Maximum vs 95% CI Upper Bound",
    subtitle = "Distribution across facilities",
    x = "Difference (%)",
    y = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid.minor = element_blank()
  )

head(comparison_mco)
write.csv(comparison_final, 
          file =here("Datasets", "Output Data", "Enriched Facility Data",
                     "beds_comparisons.csv"))

# ── Load data ──────────────────────────────────────────────────────────────────
df_raw <- read.csv(here("Datasets", "Output Data","Enriched Facility Data",  "beds_comparisons.csv"))


df <- df_raw %>%
  filter(!is.na(pmsi_total_beds_mco),
         !is.na(capact_total_beds),
         !is.na(census_max),
         census_max > 0)

total <- nrow(df)

# ── Compute directional differences ───────────────────────────────────────────
df <- df %>%
  mutate(
    diff_pmsi        = pmsi_total_beds_mco - census_max,
    diff_capact      = capact_total_beds   - census_max,
    absdiff_pmsi     = abs(diff_pmsi),
    absdiff_capact   = abs(diff_capact),
    pmsi_adequate    = diff_pmsi   >= 0,
    capact_adequate  = diff_capact >= 0,
    coverage_cat     = case_when(
      pmsi_adequate &  capact_adequate ~ "Both cover census",
      pmsi_adequate & !capact_adequate ~ "PMSI only",
      !pmsi_adequate &  capact_adequate ~ "CAPACT only",
      TRUE                              ~ "Neither covers"
    )
  )

# ── Summary counts ─────────────────────────────────────────────────────────────
n_pmsi_ok     <- sum(df$pmsi_adequate)
n_capact_ok   <- sum(df$capact_adequate)
n_both_ok     <- sum(df$pmsi_adequate  & df$capact_adequate)
n_neither     <- sum(!df$pmsi_adequate & !df$capact_adequate)
n_pmsi_only   <- sum(df$pmsi_adequate  & !df$capact_adequate)
n_capact_only <- sum(!df$pmsi_adequate & df$capact_adequate)

# ── Hospital-type summary (types with >= 5 hospitals) ─────────────────────────
type_summary <- df %>%
  group_by(hospital_type) %>%
  summarise(
    n              = n(),
    mae_pmsi       = mean(absdiff_pmsi,   na.rm = TRUE),
    mae_capact     = mean(absdiff_capact, na.rm = TRUE),
    pct_pmsi_ok    = mean(pmsi_adequate,  na.rm = TRUE) * 100,
    pct_capact_ok  = mean(capact_adequate,na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 5)

# ══════════════════════════════════════════════════════════════════════════════
# THEME
# ══════════════════════════════════════════════════════════════════════════════
theme_report <- theme_minimal(base_size = 10) +
  theme(
    plot.background  = element_rect(fill = "#F8F9FA", color = NA),
    panel.background = element_rect(fill = "white",   color = NA),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 11, color = "#1F3864"),
    axis.title       = element_text(size = 9),
    legend.position  = "bottom",
    legend.title     = element_blank()
  )

pal <- c("Source >= Census" = "#4472C4",
         "Source < Census"      = "#E74C3C",
         "PMSI"                 = "#4472C4",
         "CAPACT"               = "#ED7D31",
         "Census Max"           = "#70AD47",
         "Both cover census"    = "#70AD47",
         "PMSI only"            = "#4472C4",
         "CAPACT only"          = "#ED7D31",
         "Neither covers"       = "#E74C3C")

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 1 – Scatter plots
# ══════════════════════════════════════════════════════════════════════════════
make_scatter <- function(data, xcol, x_label, dot_color) {
  data <- data %>%
    mutate(adequate = .data[[xcol]] >= census_max,
           label    = ifelse(adequate, "Source >= Census", "Source < Census"))
  lim <- max(data[[xcol]], data$census_max, na.rm = TRUE) * 1.05
  r   <- cor(data[[xcol]], data$census_max, use = "complete.obs")
  
  ggplot(data, aes(x = .data[[xcol]], y = census_max, color = label)) +
    geom_point(alpha = 0.45, size = 1.5) +
    geom_abline(linetype = "dashed", color = "black", linewidth = 0.6) +
    scale_color_manual(values = c("Source >= Census" = dot_color,
                                  "Source < Census"      = "#E74C3C")) +
    annotate("text", x = lim * 0.05, y = lim * 0.94,
             label = paste0("r = ", round(r, 3)),
             color = dot_color, fontface = "bold", size = 3.2, hjust = 0) +
    coord_fixed(xlim = c(0, lim), ylim = c(0, lim)) +
    labs(title = paste(x_label, "vs Census Max"),
         x = x_label, y = "Census Max") +
    theme_report +
    theme(legend.position = "bottom")
}

p_scatter <- make_scatter(df, "pmsi_total_beds_mco",  "PMSI Total Beds",   "#4472C4") +
  make_scatter(df, "capact_total_beds",     "CAPACT Total Beds", "#ED7D31") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 2 – Distribution of differences
# ══════════════════════════════════════════════════════════════════════════════
diff_long <- df %>%
  select(diff_pmsi, diff_capact) %>%
  pivot_longer(everything(),
               names_to  = "source",
               values_to = "diff") %>%
  mutate(
    source    = recode(source,
                       diff_pmsi    = "PMSI \u2212 Census Max",
                       diff_capact  = "CAPACT \u2212 Census Max"),
    diff_clip = pmax(pmin(diff, 800), -800),
    sign      = ifelse(diff_clip >= 0, "positive", "negative")
  )

means_diff <- diff_long %>%
  group_by(source) %>%
  summarise(m = mean(diff_clip, na.rm = TRUE), .groups = "drop")

pct_labels <- data.frame(
  source = c("PMSI \u2212 Census Max", "CAPACT \u2212 Census Max"),
  pct    = c(n_pmsi_ok / total * 100, n_capact_ok / total * 100)
) %>%
  mutate(label = paste0(round(pct, 1), "% cover census\n(source >= census)"))

p_dist <- ggplot(diff_long, aes(x = diff_clip, fill = sign)) +
  geom_histogram(bins = 60, color = NA, alpha = 0.78) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(data = means_diff, aes(xintercept = m),
             color = "red", linewidth = 0.9) +
  geom_text(data = means_diff,
            aes(x = m, y = Inf, label = paste0("Mean=", round(m))),
            vjust = 1.5, hjust = -0.1, size = 2.8, color = "red", inherit.aes = FALSE) +
  geom_label(data = pct_labels,
             aes(label = label), x = Inf, y = Inf,
             hjust = 1.05, vjust = 1.1, size = 2.6,
             fill = "#F0F4FF", color = "#2E5196", label.size = 0.3, inherit.aes = FALSE) +
  scale_fill_manual(values = c("positive" = "#4472C4", "negative" = "#E74C3C"),
                    labels  = c("positive" = "Source  >= Census", "negative" = "Source < Census")) +
  facet_wrap(~ source, scales = "free_y") +
  labs(title = "Distribution of Differences (Source \u2212 Census Max)",
       x = "Difference (beds \u2212 census max, clipped at \u00b1800)",
       y = "Number of hospitals") +
  theme_report

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 3a – MAE by hospital type
# ══════════════════════════════════════════════════════════════════════════════
mae_long <- type_summary %>%
  select(hospital_type, mae_pmsi, mae_capact) %>%
  pivot_longer(c(mae_pmsi, mae_capact),
               names_to  = "source",
               values_to = "mae") %>%
  mutate(source = recode(source, mae_pmsi = "PMSI", mae_capact = "CAPACT"))

p_mae <- ggplot(mae_long, aes(x = hospital_type, y = mae, fill = source)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
  geom_text(aes(label = round(mae)),
            position = position_dodge(0.8), vjust = -0.4, size = 2.5) +
  scale_fill_manual(values = c("PMSI" = "#4472C4", "CAPACT" = "#ED7D31")) +
  labs(title = "Mean Absolute Error vs Census Max \u2014 by Hospital Type",
       x = NULL, y = "MAE (beds)") +
  theme_report +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 3b – % covering census by hospital type
# ══════════════════════════════════════════════════════════════════════════════
pct_long <- type_summary %>%
  select(hospital_type, pct_pmsi_ok, pct_capact_ok) %>%
  pivot_longer(c(pct_pmsi_ok, pct_capact_ok),
               names_to  = "source",
               values_to = "pct") %>%
  mutate(source = recode(source, pct_pmsi_ok = "PMSI", pct_capact_ok = "CAPACT"))

p_pct <- ggplot(pct_long, aes(x = hospital_type, y = pct, fill = source)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "grey50") +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_dodge(0.8), vjust = -0.4, size = 2.5) +
  scale_fill_manual(values = c("PMSI" = "#4472C4", "CAPACT" = "#ED7D31")) +
  scale_y_continuous(limits = c(0, 110), labels = scales::label_percent(scale = 1)) +
  labs(title = "% Hospitals Where Source Covers Census Max \u2014 by Hospital Type",
       x = NULL, y = "% with source >= census max") +
  theme_report +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 4 – Pie chart
# ══════════════════════════════════════════════════════════════════════════════
pie_df <- data.frame(
  category = c("Both cover census", "PMSI only", "CAPACT only", "Neither covers"),
  n        = c(n_both_ok, n_pmsi_only, n_capact_only, n_neither)
) %>%
  mutate(
    pct   = n / total * 100,
    label = paste0(category, "\n(", n, " | ", round(pct, 1), "%)")
  )

p_pie <- ggplot(pie_df, aes(x = "", y = n, fill = category)) +
  geom_col(width = 1, color = "white", linewidth = 0.8) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = pal) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.5, color = "white",
            fontface = "bold") +
  labs(title = "Coverage of Census Max\n(Source >= Census)") +
  theme_void() +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5,
                                     size = 11, color = "#1F3864"),
        legend.title  = element_blank(),
        legend.text   = element_text(size = 8),
        plot.background = element_rect(fill = "#F8F9FA", color = NA))

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 5 – Top 30 hospitals bar chart
# ══════════════════════════════════════════════════════════════════════════════
top30 <- df %>%
  slice_max(census_max, n = 30) %>%
  arrange(census_max) %>%
  mutate(
    short_name = stringr::str_trunc(facility_name, 38),
    short_name = factor(short_name, levels = short_name)
  ) %>%
  select(short_name, pmsi_total_beds_mco, capact_total_beds, census_max) %>%
  pivot_longer(c(pmsi_total_beds_mco, capact_total_beds, census_max),
               names_to  = "source",
               values_to = "beds") %>%
  mutate(source = recode(source,
                         pmsi_total_beds_mco = "PMSI",
                         capact_total_beds   = "CAPACT",
                         census_max          = "Census Max"))

p_top30 <- ggplot(top30, aes(x = beds, y = short_name, fill = source)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.88) +
  scale_fill_manual(values = c("PMSI" = "#4472C4", "CAPACT" = "#ED7D31",
                               "Census Max" = "#70AD47")) +
  labs(title = "Top 30 Hospitals: PMSI vs CAPACT vs Census Max",
       x = "Beds / patients", y = NULL) +
  theme_report +
  theme(axis.text.y = element_text(size = 7))

# ══════════════════════════════════════════════════════════════════════════════
# SAVE ALL PLOTS AS PNGs (for embedding in PDF via rmarkdown or cowplot)
# Or save directly as a multi-page PDF using ggplot + pdf()
# ══════════════════════════════════════════════════════════════════════════════
out_dir <- here("outputs")
dir.create(out_dir, showWarnings = FALSE)
out_pdf  <- file.path(out_dir, "beds_comparison_report_R.pdf")

# Open multi-page PDF device
pdf(out_pdf, width = 11, height = 8.5)

# ── Cover / key findings text page ────────────────────────────────────────────
plot.new()
title_txt <- "Bed Capacity Comparison Report\nPMSI · CAPACT · Census Max"
text(0.5, 0.85, title_txt,   cex = 1.8, font = 2, col = "#1F3864", adj = 0.5)
text(0.5, 0.65,
     paste0("Hospitals analysed: ", total),
     cex = 1.1, col = "#444444", adj = 0.5)
findings <- paste0(
  "PMSI covers census:    ", n_pmsi_ok,   "  (", round(n_pmsi_ok/total*100,1),   "%)\n",
  "CAPACT covers census:  ", n_capact_ok, "  (", round(n_capact_ok/total*100,1), "%)\n",
  "Both cover census:     ", n_both_ok,   "  (", round(n_both_ok/total*100,1),   "%)\n",
  "Neither covers census: ", n_neither,   "  (", round(n_neither/total*100,1),   "%)"
)
text(0.5, 0.42, findings, cex = 1.05, col = "#1F3864", adj = 0.5, family = "mono")

# ── Coverage summary table ─────────────────────────────────────────────────────
cov_df <- data.frame(
  Category         = c("PMSI >= Census", "CAPACT >= Census", "Both >= Census",
                       "PMSI only", "CAPACT only", "Neither"),
  N                = c(n_pmsi_ok, n_capact_ok, n_both_ok,
                       n_pmsi_only, n_capact_only, n_neither),
  Pct              = paste0(round(c(n_pmsi_ok, n_capact_ok, n_both_ok,
                                    n_pmsi_only, n_capact_only, n_neither) / total * 100, 1), "%"),
  Mean_excess      = c(
    round(mean(df$diff_pmsi[df$pmsi_adequate],   na.rm = TRUE), 1),
    round(mean(df$diff_capact[df$capact_adequate],na.rm = TRUE), 1),
    round(mean(df$diff_pmsi[ df$pmsi_adequate &  df$capact_adequate], na.rm = TRUE), 1),
    round(mean(df$diff_pmsi[ df$pmsi_adequate & !df$capact_adequate], na.rm = TRUE), 1),
    round(mean(df$diff_capact[!df$pmsi_adequate & df$capact_adequate],na.rm = TRUE), 1),
    NA
  )
)
grid::grid.newpage()
gridExtra::grid.table(cov_df, rows = NULL,
                      theme = gridExtra::ttheme_default(base_size = 10))

# ── Figures ────────────────────────────────────────────────────────────────────
print(p_pie)
print(p_scatter)
print(p_dist)
print(p_mae)
print(p_pct)
print(p_top30)

dev.off()

message("✅ Report saved to: ", out_pdf)
message("   N hospitals: ", total)
message("   PMSI covers census: ",   n_pmsi_ok,   " (", round(n_pmsi_ok/total*100,1),   "%)")
message("   CAPACT covers census: ", n_capact_ok, " (", round(n_capact_ok/total*100,1), "%)")
message("   Both cover: ",           n_both_ok,   " (", round(n_both_ok/total*100,1),   "%)")
message("   Neither covers: ",       n_neither,   " (", round(n_neither/total*100,1),   "%)")


print(p_pie)
print(p_scatter)
print(p_dist)
print(p_mae)
print(p_pct)
print(p_top30)
