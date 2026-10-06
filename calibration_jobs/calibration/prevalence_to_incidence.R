# =============================================================================
# prevalence_to_incidence.R
# =============================================================================
# Run this ONCE before the calibration.
# Converts ECDC carriage prevalence tiers to hospital-acquired incidence
# targets using the network-weighted LOS from facility_level_final.RDS.
#
# SAVES TO data/:
#   inc_targets.rds   — main calibration input (inc_targets, params)
#   inc_targets.csv   — human-readable version
#
# FORMULA:
#   incidence = (gamma + 1/LOS) x (P_total - pi_vec) x 1,000
#
# ECDC TIER DEFINITIONS (EARS-Net):
#   Low      : >1-10%  carriage  (CRE/VRE low-burden, MRSA Northern EU)
#   Moderate : >10-20% carriage  (MRSA moderate, ESBL-E Eastern EU)
#   High     : >20-50% carriage  (MRSA Southern/Eastern EU, ESBL-E high-burden)
# =============================================================================

library(dplyr)

# =============================================================================
# 0. PATHS — auto-detects cluster vs local
# =============================================================================

ARCANE_ROOT <- Sys.getenv("ARCANE_ROOT", "")
if (nchar(ARCANE_ROOT) == 0)
  ARCANE_ROOT <- "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/calibration_jobs"

DATA_DIR <- file.path(ARCANE_ROOT, "data")

# =============================================================================
# 1. DISEASE PARAMETERS
# =============================================================================

gamma      <- 1 / 387   # daily decolonisation rate (ESBL-E mean carriage ~387d)
pi_vec_val <- 0.001     # community carriage on admission (0.1%)
# conservative ECDC estimate for novel/rare ARB

# =============================================================================
# 2. LOAD LOS FROM DATA
# =============================================================================

facility_level <- readRDS(file.path(DATA_DIR, "facility_level_final.RDS")) %>%
  mutate(finess_geo = as.character(finess_geo)) %>%
  rename(incidence_esbl_all = incidence_region_type_ESBL_all)

los_by_type <- facility_level %>%
  filter(!is.na(hospital_type),
         !is.na(pt_days_total), !is.na(patient_total),
         patient_total > 0) %>%
  group_by(hospital_type) %>%
  summarise(
    n_hospitals  = n(),
    pt_days_sum  = sum(pt_days_total, na.rm = TRUE),
    patients_sum = sum(patient_total,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    los        = pt_days_sum / patients_sum,
    factor     = gamma + 1 / los,
    patient_wt = patients_sum / sum(patients_sum)
  )

los_network    <- sum(los_by_type$los    * los_by_type$patient_wt)
factor_network <- sum(los_by_type$factor * los_by_type$patient_wt)

message("=== PREVALENCE TO INCIDENCE ===")
message(sprintf("gamma      = %.6f  (mean carriage = 387 days)", gamma))
message(sprintf("pi_vec_val = %.4f  (community carriage = %.1f%%)",
                pi_vec_val, pi_vec_val * 100))
message(sprintf("LOS network-weighted = %.2f days", los_network))
message(sprintf("Factor (gamma + 1/LOS) = %.5f", factor_network))

# =============================================================================
# 3. ECDC CARRIAGE PREVALENCE -> INCIDENCE TARGETS
# =============================================================================

ecdc_prevalence <- data.frame(
  tier           = c("Low",      "Moderate", "High"),
  ecdc_category  = c("low (>1-10%)", "moderate (>10-20%)", "high (>20-30%)"),
  prev_low_pct   = c(1.0,   10.0,  20.0),
  prev_high_pct  = c(10.0,  20.0,  30.0),
  ecdc_examples  = c(
    "CRE France/Germany, VRE Scandinavia, MRSA low-burden N.EU",
    "MRSA moderate settings, ESBL-E Eastern EU",
    "MRSA Greece/Romania, ESBL-E Southern/Eastern EU"
  ),
  stringsAsFactors = FALSE
)

inc_targets <- ecdc_prevalence %>%
  mutate(
    tier          = factor(tier, levels = c("Low","Moderate","High")),
    hosp_acq_low  = pmax((prev_low_pct  / 100) - pi_vec_val, 0),
    hosp_acq_high = pmax((prev_high_pct / 100) - pi_vec_val, 0),
    inc_low       = round(factor_network * hosp_acq_low  * 1000, 3),
    inc_high      = round(factor_network * hosp_acq_high * 1000, 3),
    los_network   = round(los_network,    2),
    factor_used   = round(factor_network, 5),
    gamma         = gamma,
    pi_vec_val    = pi_vec_val
  ) %>%
  arrange(tier)

message("\nIncidence targets:")
for (i in seq_len(nrow(inc_targets))) {
  r <- inc_targets[i, ]
  message(sprintf("  %-10s [%s]  ECDC prev %5.1f-%5.1f%%  ->  inc %.3f-%.3f /1k pd",
                  as.character(r$tier), r$ecdc_category,
                  r$prev_low_pct, r$prev_high_pct,
                  r$inc_low, r$inc_high))
}

# Validation: SPARES France ESBL-E (~0.48 /1k pd)
spares_check <- 0.48 / (factor_network * 1000) + pi_vec_val
message(sprintf("\nSPARES ESBL-E France (~0.48 /1k pd) => total carriage ~%.1f%%",
                spares_check * 100))
low_row <- inc_targets[inc_targets$tier == "Low", ]
message(sprintf("  Low tier inc range: %.3f-%.3f -> ESBL-E France is %s the Low tier",
                low_row$inc_low, low_row$inc_high,
                if (0.48 <= low_row$inc_high) "WITHIN" else "BELOW"))

# =============================================================================
# 4. SAVE TO data/
# =============================================================================

out_rds <- file.path(DATA_DIR, "inc_targets.rds")
out_csv <- file.path(DATA_DIR, "inc_targets.csv")

saveRDS(list(
  inc_targets    = inc_targets,
  los_by_type    = los_by_type,
  los_network    = los_network,
  factor_network = factor_network,
  gamma          = gamma,
  pi_vec_val     = pi_vec_val,
  datetime       = Sys.time()
), out_rds)

write.csv(inc_targets %>%
            select(tier, ecdc_category, prev_low_pct, prev_high_pct,
                   inc_low, inc_high, los_network, factor_used,
                   gamma, pi_vec_val, ecdc_examples),
          out_csv, row.names = FALSE)

message("\nSaved:")
message("  ", out_rds)
message("  ", out_csv)
message("\nRun calibration_incidence.R or calibration_incidence_local.R next.")