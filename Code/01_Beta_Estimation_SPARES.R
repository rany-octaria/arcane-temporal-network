# =============================================================================
# SPARES_beta_incidence_pipeline.R
# ARCANE Project · MESuRS (Cnam)
# =============================================================================
# Estimates within-hospital transmissibility (beta) for ESBL-producing
# Enterobacterales, using the Paul et al. (2020, Clin Infect Dis) regression
# framework reformulated for surveillance data where we observe:
#
#   I_a   = incidence_esbl_all  — cases per 1000 patient-days  [SPARES]
#   tau_a = reg_type_los_avg    — mean length of stay (days)    [PMSI]
#   region                      — 13 French admin. regions      [SPARES]
#   type                        — facility type label           [FINESS]
#
# and do NOT have raw case counts (l_a) or patient counts (n_a) separately.
#
#
# ═══════════════════════════════════════════════════════════════════════════════
# MATHEMATICAL DERIVATION — from Paul et al. original to our regression
# ═══════════════════════════════════════════════════════════════════════════════
#
# ── Shared identity ──────────────────────────────────────────────────────────
#
#   Patient-days in stratum a  =  n_a * tau_a
#
#   Incidence per 1000 pd is defined as:
#     I_a = ( l_a / (n_a * tau_a) ) * 1000
#
#   Rearranging:
#     v_a  =  l_a / n_a  =  I_a * tau_a / 1000           ... (*)
#
#   This is the key identity: the prevalence ratio v (cases/patients) can be
#   reconstructed exactly from I and tau whenever incidence is expressed per
#   100 patient-days.  No additional assumptions are needed.
#
#
# ── Short-term hospital model (non-SSR) ─────────────────────────────
#
#   Paul et al. Eq 4.7 (original, in terms of v = l/n):
#
#     log(v_a)  =  c0  +  c_region  +  c_tau * tau_a  +  epsilon        [4.7]
#
#   We use log(I_a) directly as the response instead of log(v_a).
#   Using identity (*):  log(I_a) = log(v_a) - log(tau_a) + log(1000)
#
#   So:
#     log(I_a)  =  [c0 + log(1000)]  +  c_region  +  c_tau * tau_a
#                  - log(tau_a)  +  epsilon                              [4.7']
#
#   We fit [4.7'] omitting the structural  -log(tau_a)  term:
#
#     log(I_a)  =  c0*  +  c_region  +  c_tau * tau_a  +  epsilon       [4.7'']
#
#   The omitted -log(tau_a) term acts as a mild confounder: c_tau from [4.7'']
#   absorbs it, biasing the slope slightly.  For short-term hospitals (tau ~ 3–10 days)
#   the variation in log(tau) is modest, so the bias is small in practice.
#   The intercept c0* = c0 + log(1000) does not affect beta.
#
#   → FITTED MODEL (R):
#       lm( log(I) ~ factor(region) + tau )
#
#   → BETA (Eq 4.8, approximately valid):
#       beta_S  =  gamma  +  c_tau / (1  +  c_tau * tau_bar)
#
#       where tau_bar is the mean short-term LOS in the stratum.
#
#
# ── Long-term hospital model (SSR / Rehabilitation) ──────────────────
#
#   Paul et al. Eq 4.9 (original, in terms of v and 1/tau):
#
#     v_a  =  c0  +  c_region  +  c_tau * (1/tau_a)  +  epsilon         [4.9]
#
#   Using identity (*):  v_a = I_a * tau_a / 1000 = I_a / (1000 * inv_tau_a)
#
#   If we fit I_a directly instead of v_a:
#
#     I_a  =  c0_I  +  c_tau_I * (1/tau_a)  +  epsilon
#
#   Substituting v_a = I_a / (1000 * inv_tau_a):
#     v_a  =  c0_I / (1000 * inv_tau_a)  +  c_tau_I / 1000
#           =  c0_I * tau_a / 1000       +  c_tau_I / 1000
#
#   This gives  v ~ tau  (linear in tau), NOT  v ~ 1/tau  as Paul requires.
#   The two functional forms are not equivalent, so c_tau_I from the I-response
#   regression cannot be plugged into Eq 4.10 to recover beta_L correctly.
#
#   We therefore retain the back-substitution v_hat = I * tau / 1000  for long-term hospitals,
#   which restores the exact Eq 4.9 structure at zero cost to interpretation:
#   v_hat is simply the observed incidence rate multiplied by the observed LOS,
#   divided by 1000 — no assumptions beyond the patient-day identity (*).
#
#   → FITTED MODEL (R):
#       lm( v_hat ~ factor(region) + (1/tau) )
#       where  v_hat = I * tau / 1000
#
#   → BETA (Eq 4.10, exact):
#       beta_L  =  c0 / c_tau  +  gamma      (c_tau expected negative)
#
#
# ── Region as factor (replaces v_HRR) ────────────────────────────────────────
#
#   Paul et al. used a scalar regional background prevalence v_HRR as a
#   covariate.  We replace this with region as a categorical fixed effect.
#   Each of the 13 French administrative regions gets its own intercept,
#   absorbing regional ESBL pressure nonparametrically.  c_tau is then
#   identified from within-region variation in LOS across facility types
#   (short-term) or across years (long-term, pooled).
#
#   Long-term hospital year-by-year limitation:
#   With only one SSR type per region, each year yields at most 13 long-term rows
#   (one per region).  A model with 12 region dummies + inv_tau + intercept
#   = 14 parameters has 0 residual df — it cannot be fitted.  The region FE is
#   therefore automatically dropped for long-term year-by-year runs; the model
#   falls back to intercept + inv_tau only.  This is flagged in the output.
#
# ═══════════════════════════════════════════════════════════════════════════════


library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)


# =============================================================================
# 0.  CONFIGURATION
# =============================================================================

# ESBL colonisation clearance rate (days^-1).
# Paul et al. (2020) used 1/387 based on Israeli CRE cohort data.
# Update if an ESBL-E-specific estimate is available.
GAMMA <- 1 / 387

# Label identifying long-term (SSR) facilities in the `type` column.
# All other facility types are treated as short-term.
LONG_TERM_TYPE <- "Rehabilitation hospital"

# Minimum number of observations required to attempt a regression per stratum.
# Returns NA + diagnostic flag below this threshold.
MIN_OBS_SHORT <- 5
MIN_OBS_LONG <- 3


# =============================================================================
# 1.  LOAD DATA
# =============================================================================
# `matched` is expected to already be in the environment from the data prep
# script (00_data_prep_unified.R).  To load from disk, uncomment below:
# matched <- readRDS(here("Datasets", "Output Data", "Enriched Facility Data",
#                         "spares_incidence_los_matched.RDS"))

stopifnot(exists("matched"))

required_cols <- c("Date_year", "region", "type",
                   "incidence_esbl_all", "reg_type_los_avg",
                   "n_bed_days", "reg_type_pt_days", "reg_type_n_patients")
missing_cols <- setdiff(required_cols, names(matched))
if (length(missing_cols) > 0)
  stop("Missing columns in `matched`: ", paste(missing_cols, collapse = ", "))

message(sprintf(
  "Loaded `matched`: %d rows | years: %s | types: %s",
  nrow(matched),
  paste(sort(unique(matched$Date_year)), collapse = ", "),
  paste(sort(unique(matched$type)),      collapse = "; ")
))


# =============================================================================
# 2.  PREPARE YEAR-BY-YEAR DATASET
# =============================================================================
# One row per region x type x year — already the structure of `matched`.
# We add all derived variables used directly in the two regression models.

reg_yearly <- matched |>
  mutate(
    year    = Date_year,
    stratum = if_else(type == LONG_TERM_TYPE, "Long-term", "Short-term"),
    
    # ── Short-term response: log(I_a) ─────────────────────────────────────────────
    # We use log of the incidence rate directly as the response (cases per 1000
    # patient-days).  Per derivation [4.7''], this approximates the Paul et al.
    # log(v) response up to -log(tau), which is omitted here.  The slope c_tau
    # and hence beta_S are slightly biased, but acceptably so for short-term hospitals
    # where LOS variation across types is modest (~ 3–10 days).
    log_I      = log(incidence_esbl_all),       # short-term response
    
    # Linear LOS predictor for the short-term model (free slope c_tau).
    tau        = reg_type_los_avg,
    
    # ── Long-term response: v_hat = I_a * tau_a / 1000 ───────────────────────────
    # From derivation [4.9']: back-substituting identity (*) gives v_hat = l/n.
    # This is the response in the long-term regression (Eq 4.9 structure).
    # Dividing by 1000 converts incidence/1000pd back to a dimensionless ratio.
    v_hat      = incidence_esbl_all * reg_type_los_avg / 1000,
    
    # Reciprocal LOS: predictor in Eq 4.9.
    # Captures the admission-seeding rate: higher 1/tau = faster patient turnover.
    inv_tau    = 1 / reg_type_los_avg
  ) |>
  filter(
    incidence_esbl_all > 0,    # drops true-zero rows (2 Bretagne CLCC obs in 2021-22)
    is.finite(log_I),          # safety net for any remaining non-finite log values
    reg_type_los_avg   > 0,
    !is.na(reg_type_los_avg)   # drops 4 PACA private not-for-profit rows (missing LOS)
  )

message(sprintf(
  "Year-by-year dataset: %d rows | %d Short-term | %d Long-term | %d years",
  nrow(reg_yearly),
  sum(reg_yearly$stratum == "Short-term"),
  sum(reg_yearly$stratum == "Long-term"),
  n_distinct(reg_yearly$year)
))


# =============================================================================
# 3.  PREPARE 4-YEAR POOLED DATASET
# =============================================================================
# Aggregate across all four years within each region x type cell to produce a
# single cumulative observation per cell (one row per region x type).
#
# Pooled incidence (cases per 1000 patient-days):
#   Implied cases in year k  =  I_k * n_bed_days_k / 1000
#   Pooled I  =  sum(I_k * BD_k) / sum(BD_k)     [bed-day-weighted mean]
#             =  total implied cases / total bed-days * 1000
#   n_bed_days is the SPARES-sample bed-days for that region x type x year.
#
# Pooled LOS (days):
#   Pooled tau  =  sum(reg_type_pt_days_k) / sum(reg_type_n_patients_k)
#   reg_type_pt_days is the FULL regional patient-days (all facilities in the
#   region x type stratum, not just the SPARES sample).  This is the correct
#   denominator: reg_type_los_avg = reg_type_pt_days / reg_type_n_patients in
#   the source data.  Using n_bed_days instead would undercount patient-days
#   and inflate the pooled LOS.

reg_pooled <- matched |>
  filter(incidence_esbl_all > 0, !is.na(reg_type_los_avg)) |>
  group_by(region, type) |>
  summarise(
    year                = NA_integer_,
    n_bed_days_total    = sum(n_bed_days,            na.rm = TRUE),
    n_pt_days_total     = sum(reg_type_pt_days,      na.rm = TRUE),
    n_patients_total    = sum(reg_type_n_patients,   na.rm = TRUE),
    # Bed-day-weighted mean incidence across years
    incidence_esbl_all  = sum(incidence_esbl_all * n_bed_days, na.rm = TRUE) /
      sum(n_bed_days,                       na.rm = TRUE),
    # Correct pooled LOS: total regional patient-days / total patients
    reg_type_los_avg    = sum(reg_type_pt_days,    na.rm = TRUE) /
      sum(reg_type_n_patients, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    stratum  = if_else(type == LONG_TERM_TYPE, "Long-term", "Short-term"),
    log_I    = log(incidence_esbl_all),
    tau      = reg_type_los_avg,
    v_hat    = incidence_esbl_all * reg_type_los_avg / 1000,
    inv_tau  = 1 / reg_type_los_avg
  ) |>
  filter(
    is.finite(log_I),
    reg_type_los_avg   > 0,
    incidence_esbl_all > 0
  )

message(sprintf(
  "Pooled dataset:       %d rows | %d Short-term | %d Long-term",
  nrow(reg_pooled),
  sum(reg_pooled$stratum == "Short-term"),
  sum(reg_pooled$stratum == "Long-term")
))


# =============================================================================
# 4.  REGRESSION FUNCTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# 4a.  Short-term hospital regression  (all non-SSR / non-Rehabilitation types)
# -----------------------------------------------------------------------------
# Fitted model [4.7'']:
#
#   log(I_a)  ~  factor(region)  +  tau_a
#
# log(I_a) is the log incidence rate (cases per 1000 patient-days), used
# directly as the response.  This approximates the Paul et al. log(v_a)
# response; see derivation [4.7''] in the header for the exact relationship.
# c_tau and beta_S (Eq 4.8) are recovered from the slope on tau_a.
#
# Arguments:
#   df      : data frame pre-filtered to the current year or the pooled set
#   tau_bar : mean short-term LOS; used in the Eq 4.8 denominator.
#             Computed from df if not supplied.

run_short_term_regression <- function(df, tau_bar = NULL) {
  
  df_s <- df |>
    filter(stratum == "Short-term", is.finite(log_I), is.finite(tau))
  
  if (nrow(df_s) < MIN_OBS_SHORT) {
    return(tibble(group = "Short-term", beta = NA_real_, beta_lo = NA_real_,
                  beta_hi = NA_real_, c_tau = NA_real_, c_tau_se = NA_real_,
                  n_obs = nrow(df_s), region_fe = NA, flag = "too_few_obs"))
  }
  
  if (is.null(tau_bar)) tau_bar <- mean(df_s$tau, na.rm = TRUE)
  # Region FE: ~4 short-term types x 13 regions per year → ~52 obs, ~15 params
  # → ~37 residual df.  Region FE is always supported for short-term hospitals.
  fit <- tryCatch(
    lm(log_I ~ factor(region) + tau, data = df_s),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(tibble(group = "Short-term", beta = NA_real_, beta_lo = NA_real_,
                  beta_hi = NA_real_, c_tau = NA_real_, c_tau_se = NA_real_,
                  n_obs = nrow(df_s), region_fe = TRUE, flag = "lm_error"))
  }
  
  coefs    <- coef(fit)
  vcov_fit <- vcov(fit)
  c_tau    <- coefs["tau"]
  c_tau_se <- sqrt(vcov_fit["tau", "tau"])
  c_tau_p  <- summary(fit)$coefficients["tau", "Pr(>|t|)"]
  
  # Guard: c_tau <= 0 → LOS-prevalence slope is in the wrong direction.
  # beta_S is not identifiable; return NA with diagnostic flag.
  # NOTE: in this dataset c_tau is frequently non-positive due to case-mix
  # confounding — types with shorter LOS (private for-profit) also happen to
  # have lower testing intensity.  This is an identifiability limitation of
  # region x type aggregates, not a data error.
  if (is.na(c_tau) || !is.finite(c_tau) || c_tau <= 0) {
    return(tibble(group = "Short-term", beta = NA_real_, beta_lo = NA_real_,
                  beta_hi = NA_real_, c_tau = c_tau, c_tau_se = c_tau_se,
                  c_tau_p = c_tau_p, n_obs = nrow(df_s), region_fe = TRUE,
                  flag = "non_pos_slope"))
  }
  
  # Eq 4.8:  beta_S = gamma + c_tau / (1 + c_tau * tau_bar)
  # tau_bar (mean short-term LOS) appears in the denominator to convert the
  # log-prevalence slope into a per-day transmission probability.
  beta_s        <- GAMMA + c_tau / (1 + c_tau * tau_bar)
  
  # Delta-method 95% CI for beta_S propagated through Eq 4.8.
  # d(beta_S) / d(c_tau) = 1 / (1 + c_tau * tau_bar)^2
  d_beta_d_ctau <- 1 / (1 + c_tau * tau_bar)^2
  beta_se       <- abs(d_beta_d_ctau) * c_tau_se
  z             <- qnorm(0.975)
  
  tibble(
    group     = "Short-term",
    beta      = beta_s,
    beta_lo   = beta_s - z * beta_se,
    beta_hi   = beta_s + z * beta_se,
    c_tau     = c_tau,
    c_tau_se  = c_tau_se,
    c_tau_p   = c_tau_p,
    n_obs     = nrow(df_s),
    region_fe = TRUE,
    flag      = if_else(beta_s >= 0 & beta_s <= 1, "ok", "out_of_range")
  )
}


# -----------------------------------------------------------------------------
# 4b.  Long-term hospital regression  (SSR / Rehabilitation only)
# -----------------------------------------------------------------------------
# Fitted model [4.9']:
#
#   v_hat_a  ~  factor(region)  +  (1/tau_a)
#   where  v_hat = I_a * tau_a / 1000
#
# Structurally identical to Paul et al. Eq 4.9; no transformation needed
# beyond the back-substitution v_hat = I * tau / 1000.
# c_tau (slope on 1/tau) is expected to be NEGATIVE.
#
# Region FE caveat — year-by-year runs:
# Each year provides at most 13 long-term rows (one SSR stratum per region).
# With 12 region dummies + inv_tau + intercept = 14 parameters, residual
# df = 13 - 14 = -1: the model is saturated and singular.
# → Region FE is dropped automatically; the model uses intercept + inv_tau.
# → Flagged as region_fe = FALSE in the output.
# The pooled dataset (~52 long-term rows) always supports the region FE.
#
# Arguments:
#   df : data frame pre-filtered to the current year or the pooled set

run_long_term_regression <- function(df) {
  
  df_l <- df |>
    filter(stratum == "Long-term", is.finite(v_hat), is.finite(inv_tau))
  
  if (nrow(df_l) < MIN_OBS_LONG) {
    return(tibble(group = "Long-term", beta = NA_real_, beta_lo = NA_real_,
                  beta_hi = NA_real_, c0 = NA_real_, c_tau = NA_real_,
                  n_obs = nrow(df_l), region_fe = NA, flag = "too_few_obs"))
  }
  
  # Degrees-of-freedom check for region FE.
  # Parameters needed: (n_regions - 1) region dummies + inv_tau + intercept
  #                  = n_regions + 1
  n_regions  <- n_distinct(df_l$region)
  n_params   <- n_regions + 1
  resid_df   <- nrow(df_l) - n_params
  use_region <- resid_df >= MIN_OBS_LONG
  
  if (!use_region) {
    message(sprintf(
      "  Long-term: %d obs, %d params with region FE → dropping region FE (resid df = %d)",
      nrow(df_l), n_params, resid_df
    ))
  }
  
  formula_ltach <- if (use_region) {
    v_hat ~ factor(region) + inv_tau   # full model: region FE + 1/tau seeding
  } else {
    v_hat ~ inv_tau                    # reduced: intercept + 1/tau only
  }
  
  fit <- tryCatch(
    lm(formula_ltach, data = df_l),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(tibble(group = "Long-term", beta = NA_real_, beta_lo = NA_real_,
                  beta_hi = NA_real_, c0 = NA_real_, c_tau = NA_real_,
                  n_obs = nrow(df_l), region_fe = use_region, flag = "lm_error"))
  }
  
  coefs    <- coef(fit)
  vcov_fit <- vcov(fit)
  c0       <- coefs["(Intercept)"]   # = p * (1 - gamma/beta) at equilibrium
  c_tau    <- coefs["inv_tau"]       # = -p / beta  (expected negative)
  c_tau_se <- sqrt(vcov_fit["inv_tau", "inv_tau"])
  c_tau_p  <- summary(fit)$coefficients["inv_tau", "Pr(>|t|)"]
  
  # Guard: c_tau >= 0 is epidemiologically implausible for SSR facilities.
  if (is.na(c_tau) || c_tau >= 0) {
    return(tibble(group = "Long-term", beta = NA_real_, beta_lo = NA_real_,
                  beta_hi = NA_real_, c0 = c0, c_tau = c_tau,
                  c_tau_p = c_tau_p, n_obs = nrow(df_l),
                  region_fe = use_region, flag = "unexpected_ltach_slope"))
  }
  
  # Eq 4.10:  beta_L = c0 / c_tau + gamma
  # c0 > 0 and c_tau < 0 → c0/c_tau < 0 → beta_L = gamma - |c0/c_tau|
  # Out-of-range (beta < 0) indicates the equilibrium intercept c0 is too large
  # relative to c_tau — consistent with insufficient LOS variation to identify
  # the seeding slope against regional background differences.
  beta_l <- c0 / c_tau + GAMMA
  
  # Delta-method 95% CI for beta_L via joint uncertainty in c0 and c_tau.
  # d(beta_L)/d(c0)   = 1 / c_tau
  # d(beta_L)/d(c_tau)= -c0 / c_tau^2
  grad_c0     <- 1 / c_tau
  grad_ctau   <- -c0 / c_tau^2
  cov_c0_ctau <- vcov_fit["(Intercept)", "inv_tau"]
  var_beta    <- grad_c0^2   * vcov_fit["(Intercept)", "(Intercept)"] +
    grad_ctau^2 * vcov_fit["inv_tau",     "inv_tau"]     +
    2 * grad_c0 * grad_ctau * cov_c0_ctau
  beta_se     <- sqrt(max(var_beta, 0))
  z           <- qnorm(0.975)
  
  tibble(
    group     = "Long-term",
    beta      = beta_l,
    beta_lo   = beta_l - z * beta_se,
    beta_hi   = beta_l + z * beta_se,
    c0        = c0,
    c_tau     = c_tau,
    c_tau_p   = c_tau_p,
    n_obs     = nrow(df_l),
    region_fe = use_region,
    flag      = if_else(beta_l >= 0 & beta_l <= 1, "ok", "out_of_range")
  )
}


# Convenience wrapper: run short-term then long-term on the same data frame,
# binding results into one tibble.
run_both <- function(df, tau_bar = NULL) {
  bind_rows(
    run_short_term_regression(df, tau_bar),
    run_long_term_regression(df)
  )
}


# =============================================================================
# 5.  APPROACH A — YEAR-BY-YEAR
# =============================================================================
# Run short-term and long-term regressions separately for each calendar year.
# Produces one beta estimate per stratum per year; useful for checking
# temporal stability of the estimates.

message("\n--- Approach A: year-by-year ---")

yearly_betas <- reg_yearly |>
  group_by(year) |>
  group_modify(~ run_both(.x)) |>
  ungroup() |>
  mutate(approach = "A_year_by_year")

print(yearly_betas |>
        select(approach, year, group, beta, beta_lo, beta_hi, region_fe, flag))


# =============================================================================
# 6.  APPROACH B — 4-YEAR POOLED (CUMULATIVE)
# =============================================================================
# Run both regressions on the pooled dataset (region x type cells aggregated
# across all four years into cumulative incidence / average LOS).
# Most stable estimates: maximum observations, region FE supported in both
# strata, no year-to-year noise.

message("\n--- Approach B: 4-year pooled ---")

pooled_betas <- run_both(reg_pooled) |>
  mutate(approach = "B_pooled_4yr", year = NA_integer_)

print(pooled_betas |>
        select(approach, group, beta, beta_lo, beta_hi, region_fe, flag))


# =============================================================================
# 7.  COMBINED RESULTS TABLE
# =============================================================================

all_betas <- bind_rows(
  yearly_betas |> mutate(run = as.character(year)),
  pooled_betas |> mutate(run = "4yr_pooled")
) |>
  select(approach, run, group, beta, beta_lo, beta_hi,
         c_tau, c_tau_p, n_obs, region_fe, flag, everything()) |>
  arrange(approach, run, group)

message("\n=== All beta estimates ===")
print(all_betas |>
        select(approach, run, group, beta, beta_lo, beta_hi,
               c_tau, c_tau_p, region_fe, flag),
      n = Inf)


# =============================================================================
# 7b. IDENTIFIABILITY DIAGNOSTICS
# =============================================================================
# These checks replicate the Python validation findings and flag known
# structural limitations of using region x type aggregates with this framework.

message("\n--- Identifiability diagnostics ---")

# Short-term: how much variance does tau explain AFTER region FE?
diag_short <- reg_yearly |>
  filter(stratum == "Short-term") |>
  group_by(year) |>
  group_modify(~ {
    fe_only <- lm(log_I ~ factor(region),       data = .x)
    fe_tau  <- lm(log_I ~ factor(region) + tau, data = .x)
    tibble(
      n           = nrow(.x),
      r2_fe_only  = summary(fe_only)$r.squared,
      r2_fe_tau   = summary(fe_tau)$r.squared,
      r2_tau_gain = summary(fe_tau)$r.squared - summary(fe_only)$r.squared,
      c_tau       = coef(fe_tau)["tau"],
      c_tau_p     = summary(fe_tau)$coefficients["tau", "Pr(>|t|)"]
    )
  }) |>
  ungroup()

message("Short-term: marginal R² gain from tau after region FE")
print(diag_short)
message("  If r2_tau_gain < 0.05, tau does not independently predict log(I) within regions.")
message("  Cause: LOS co-varies with facility type for case-mix reasons, not transmission.")

# Long-term: LOS variation and correlation with v_hat
diag_long <- reg_yearly |>
  filter(stratum == "Long-term") |>
  summarise(
    n              = n(),
    tau_min        = min(tau),
    tau_max        = max(tau),
    tau_sd         = sd(tau),
    cor_vhat_invtau = cor(v_hat, inv_tau, use = "complete.obs")
  )

message("\nLong-term: LOS variation and v_hat ~ inv_tau correlation (all years pooled)")
print(diag_long)
message("  If tau_sd < 5 days, LOS variation is likely too narrow to identify beta_L.")
message("  If |cor_vhat_invtau| < 0.3, the seeding signal is weak relative to noise.")


# =============================================================================
# 8.  DIAGNOSTICS — inspect regression inputs
# =============================================================================

message("\n--- Short-term input: first 10 rows (yearly) ---")
print(
  reg_yearly |>
    filter(stratum == "Short-term") |>
    select(year, region, type, incidence_esbl_all, reg_type_los_avg,
           log_I, tau) |>
    head(10)
)

message("\n--- Long-term input: all rows (yearly) ---")
print(
  reg_yearly |>
    filter(stratum == "Long-term") |>
    select(year, region, type, incidence_esbl_all, reg_type_los_avg,
           v_hat, inv_tau)
)

message("\n--- Long-term input: pooled ---")
print(
  reg_pooled |>
    filter(stratum == "Long-term") |>
    select(region, type, incidence_esbl_all, reg_type_los_avg, v_hat, inv_tau)
)


# =============================================================================
# 9.  PLOT
# =============================================================================

plot_df <- all_betas |>
  filter(!is.na(beta)) |>
  mutate(
    run_label = case_when(
      approach == "A_year_by_year" ~ paste0("Year ", run),
      approach == "B_pooled_4yr"   ~ "4-year pooled",
      TRUE ~ run
    ),
    group_label = if_else(group == "Short-term",
                          "Short-term hospital",
                          "Long-term hospital")
  )

p <- ggplot(plot_df,
            aes(x      = reorder(run_label, beta),
                y      = beta,
                ymin   = beta_lo,
                ymax   = beta_hi,
                colour = group_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(
    linewidth = 0.5, fatten = 2,
    # Open circles mark long-term year-by-year estimates where region FE was dropped
    shape = if_else(plot_df$region_fe == FALSE, 21L, 19L)
  ) +
  facet_wrap(~ approach, scales = "free_y", ncol = 1,
             labeller = labeller(approach = c(
               A_year_by_year = "A  —  Year-by-year",
               B_pooled_4yr   = "B  —  4-year pooled (cumulative)"
             ))) +
  scale_colour_manual(
    values = c("Short-term hospital" = "#0057B8",
               "Long-term hospital"      = "#C1392B")
  ) +
  coord_flip() +
  labs(
    title    = "ESBL-E within-hospital transmissibility (\u03b2)",
    subtitle = paste0(
      "Paul et al. (2020) reformulation | I per 1000 pt-days + LOS | region FE\n",
      "Open circles = long-term year-by-year without region FE (insufficient df)"
    ),
    x      = NULL,
    y      = expression(beta ~ "(per day)"),
    colour = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "#F5F7FA"),
    strip.text       = element_text(face = "bold")
  )

print(p)


# =============================================================================
# 10. SAVE OUTPUTS
# =============================================================================

output_dirs <- list(
  cluster  = here("Cluster Jobs"),
  seeding  = here("Seeding Jobs"),
  datasets = here("Datasets", "Output Data")
)

for (d in output_dirs) {
  if (dir.exists(d)) {
    saveRDS(all_betas,
            file.path(d, "beta_incidence_pipeline_all.rds"))
    write.csv(all_betas,
              file.path(d, "beta_incidence_pipeline_all.csv"),
              row.names = FALSE)
    ggsave(file.path(d, "beta_incidence_pipeline_plot.png"),
           plot = p, width = 8, height = 9, dpi = 150)
    message("Saved to: ", d)
  }
}