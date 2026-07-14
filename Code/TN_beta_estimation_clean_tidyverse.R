# ------------------------------------------------------------------------------
# Estimate beta values for the Tennessee CRE model
# ------------------------------------------------------------------------------
# Purpose:
#   This script keeps only the code needed to estimate transmission parameters
#   (beta) from the original TN_contain_setup_hdds.R file.
#
# What was removed from the original script:
#   - Working-directory changes
#   - Transfer-network construction
#   - HRR demographic preprocessing
#   - Community / HRR pseudo-facility construction
#   - SIS model setup and steady-state calculations
#   - Graphics and intervention code
#   - Objects not required for beta estimation
#
# Outputs from this script:
#   1) Short-term hospital beta estimate (reg_type_los_avg < 15)
#   2) Long-term hospital beta estimate (reg_type_los_avg >= 15)
#   3) Approximate 95% confidence intervals for both estimates
#   4) Fraction detected implied by the long-term model
#
# Main modeling assumptions retained from the original code:
#   - Clearance incidence_esbl_all gamma = 1 / 387 per day
#   - Short-term hospitals are defined as reg_type_los_avg < 15 days
#   - Long-term hospitals are defined as reg_type_los_avg >= 15 days
#   - For the short-term model, facilities with zero observed incidence_esbl_all are excluded
#     automatically because log(incidence_esbl_all) is undefined
# ------------------------------------------------------------------------------

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. User-supplied file paths
# ------------------------------------------------------------------------------

# Fixed clearance parameter retained from the original model
gamma_clear <- 1 / 387

# ------------------------------------------------------------------------------
# 2. Load the hospital-level source data from the RData file
# ------------------------------------------------------------------------------
# The original script loads aim2_sel into the global environment. Here we load it
# into a temporary environment to avoid side effects.

SSR_LABEL <- "Rehabilitation hospital"

matched <- readRDS(here("Datasets", "Output Data", "Enriched Facility Data",
                        "spares_incidence_los_matched.RDS"))
str(matched)


# ------------------------------------------------------------------------------
# 5. Create the analysis dataset for beta estimation
# ------------------------------------------------------------------------------
# This is the only hospital-level dataset needed for the regressions.
# We combine:
#   - CRE counts
#   - number of admissions / hospitalizations
#   - patient-days
#   - average length of stay
#   - HRR assignment
#
# The incidence_esbl_all definitions follow the original code:
#   incidence_esbl_all    = cre_count / freq
#   incidence_esbl_allday = cre_count / IPdays


# ------------------------------------------------------------------------------
# 6. Split data into short-term and long-term facilities
# ------------------------------------------------------------------------------
# The original script used 15 days as the LOS cutoff.

table(matched$type)
sth_data <- matched %>%
  filter(type != SSR_LABEL)

lth_data <- matched %>%
  filter(type == SSR_LABEL)

# ------------------------------------------------------------------------------
# 7. Estimate beta for short-term hospitals
# ------------------------------------------------------------------------------
# Original model:
#   log(incidence_esbl_all) ~ reg_type_los_avg + factor(region)
#
# Interpretation:
#   The coefficient on reg_type_los_avg is then transformed into a beta estimate using
#   the same algebra as the original script.
#
# Important:
#   Facilities with incidence_esbl_all = 0 are omitted here because log(incidence_esbl_all) is undefined.

sth_model <- sth_data %>%
  filter(!is.na(incidence_esbl_all), incidence_esbl_all > 0) %>%
  lm(log(incidence_esbl_all) ~ reg_type_los_avg + as.factor(region), data = .)

sth_tau_hat <- sth_data %>%
  filter(!is.na(incidence_esbl_all), incidence_esbl_all > 0) %>%
  summarise(tau_hat = mean(reg_type_los_avg)) %>%
  pull(tau_hat)

sth_slope <- coef(sth_model)[["reg_type_los_avg"]]
sth_slope_se <- sqrt(vcov(sth_model)["reg_type_los_avg", "reg_type_los_avg"])

# Reproduce the same log-normal CI approximation used in the original script
sth_meanlog <- log(sth_slope)
sth_sdlog <- sth_slope_se / abs(sth_slope)

sth_slope_ci <- c(
  exp(sth_meanlog - qnorm(0.975) * sqrt(sth_sdlog)),
  exp(sth_meanlog + qnorm(0.975) * sqrt(sth_sdlog))
)

# Transform the slope estimate into beta using the original formula
sth_beta <- sth_slope / (1 + sth_slope * sth_tau_hat) + gamma_clear
sth_beta_ci <- sth_slope_ci / (1 + sth_slope_ci * sth_tau_hat) + gamma_clear

# ------------------------------------------------------------------------------
# 8. Estimate beta for long-term hospitals
# ------------------------------------------------------------------------------
# Original model:
#   cre_count / freq ~ 1 / reg_type_los_avg
#
# The resulting coefficients are transformed into the LTACH beta estimate.

lth_model <- lth_data %>%
  filter(freq > 0, reg_type_los_avg > 0) %>%
  lm(I(cre_count / freq) ~ I(1 / reg_type_los_avg), data = .)

lth_coef <- coef(lth_model)

# Original transformation for long-term beta
lth_beta <- lth_coef[[1]] / (-lth_coef[[2]]) + gamma_clear

# Delta-method style SE approximation retained from the original script
lth_grad <- c(
  1 / lth_coef[[2]],
  -lth_coef[[1]] / (lth_coef[[2]]^2)
)

lth_beta_se <- sqrt(lth_grad %*% vcov(lth_model) %*% lth_grad)

# Reproduce the same CI construction used in the original code
lth_logbeta <- log(lth_beta)
lth_logsd <- lth_beta_se / abs(lth_beta)

lth_beta_ci <- c(
  exp(lth_logbeta - 1.96 * sqrt(lth_logsd)),
  exp(lth_logbeta + 1.96 * sqrt(lth_logsd))
)

# ------------------------------------------------------------------------------
# 9. Estimate the implied fraction detected
# ------------------------------------------------------------------------------
# This quantity was computed in the original script from the long-term model.

frac_detect <- lth_coef[[1]] / (1 - gamma_clear / lth_beta)

# ------------------------------------------------------------------------------
# 10. Collect results into tidy outputs
# ------------------------------------------------------------------------------
# These tibbles are easier to inspect, print, or write to file.

model_summary <- tibble(
  model = c("short_term", "long_term"),
  formula = c(
    "log(incidence_esbl_all) ~ reg_type_los_avg + factor(region)",
    "cre_count / freq ~ 1 / reg_type_los_avg"
  ),
  n_facilities = c(nobs(sth_model), nobs(lth_model))
)

beta_estimates <- tibble(
  facility_group = c("short_term", "long_term"),
  los_definition = c("reg_type_los_avg < 15", "reg_type_los_avg >= 15"),
  beta_estimate = c(sth_beta, lth_beta),
  beta_ci_lower = c(sth_beta_ci[1], lth_beta_ci[1]),
  beta_ci_upper = c(sth_beta_ci[2], lth_beta_ci[2])
)

auxiliary_estimates <- tibble(
  parameter = c("gamma_clear", "tau_hat_short_term", "frac_detect_long_term"),
  value = c(gamma_clear, sth_tau_hat, frac_detect)
)
