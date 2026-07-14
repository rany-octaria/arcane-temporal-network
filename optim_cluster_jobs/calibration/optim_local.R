# =============================================================================
# optim_local.R  —  LOCAL TEST DRIVER
# -----------------------------------------------------------------------------
# Runs a fast, lightweight calibration cycle on a local workstation.
# Edit only the KNOBS section below; all model logic lives in:
#   optim_cluster_jobs/shared/optim_core.R  (sourced at the end of this script)
#
# FOLDER LAYOUT (everything lives under one project-root folder):
#   <project_root>/
#   └── optim_cluster_jobs/
#       ├── shared/
#       │   └── optim_core.R
#       ├── calibration/
#       │   └── optim_local.R         ← this file
#       ├── data/
#       │   ├── weekly.RDS                  — transfer network
#       │   └── facility_level_final.RDS    — facility type/region/beds/LOS/incidence
#       └── Outputs/                  ← results land here automatically
#
# DESIGNED FOR:
#   • Confirming that data loading and model functions are error-free
#   • Checking the optimizer returns sensible betas on a small network
#   • Typical runtime: 2–10 minutes depending on machine
#
# HOW TO RUN:
#   1. Set your working directory to the project root (the folder that
#      CONTAINS optim_cluster_jobs/), e.g. in R:
#        setwd("C:/path/to/your_project")
#   2. Rscript optim_cluster_jobs/calibration/optim_local.R
#      — or open in RStudio (with the project root as working directory)
#        and press Ctrl+Shift+Enter (Run All)
#
# OUTPUT: optimal beta PER FACILITY TYPE (France-wide — pooled over all
# regions of that type), saved under OUT_DIR with a validation scatter plot.
# =============================================================================

library(tidyverse)

# =============================================================================
# 0.  KNOBS  —  LOCAL SETTINGS  (edit freely)
# =============================================================================

# ── Optimisation ─────────────────────────────────────────────────────────────
N_CORES         <- 2     # parallel workers — keep <= physical cores on laptop
N_REP_OBJ       <- 5     # replicates per objective call (noisy but fast)
N_REP_VALID     <- 10    # replicates for final validation
N_RANDOM_STARTS <- 1     # random starts in addition to the 3 structured ones
MAXIT_NM        <- 15    # Nelder-Mead iteration cap

# ── RNG seeds ────────────────────────────────────────────────────────────────
SEED_OBJ    <- 1000
SEED_VALID  <- 50000
SEED_STARTS <- 123

# ── Search space ─────────────────────────────────────────────────────────────
LOWER_BETA <- 0.0001
UPPER_BETA <- 0.10

# ── Simulation ───────────────────────────────────────────────────────────────
N_YEARS        <- 2
INIT_PREV      <- 0.02   # small endemic seed to shorten burn-in
ADMISSION_PREV <- 0.05  #Set 
ALPHA          <- 0      # no isolation effect on transfers
GAMMA_CLEAR    <- 1 / 387

# Used only as a fallback where facility_level_final.RDS$los_mean is NA
DEFAULT_LOS <- c(
  "MCO" = 5.5, "SSR" = 32.0, "MCO/SSR" = 7.0, "PSY" = 60.0,
  "HAD" = 22.0, "CLCC" = 6.0, "Other" = 8.0, "Unknown" = 7.0
)
MAX_P_TR <- 0.60

# ── Local test: restrict to a small hospital subset ───────────────────────────
# Setting LOCAL_TEST = TRUE limits the run to N_HOSP_TEST hospitals so one
# full optimisation cycle completes quickly.  Set to FALSE for a full local run
# (expect several hours with N_CORES = 2 and the full network).
LOCAL_TEST    <- TRUE
N_HOSP_TEST   <- 300     # hospitals to keep (includes ≥1 per type)

# ── Parallel backend ─────────────────────────────────────────────────────────
# PSOCK creates separate R processes — works on Windows, macOS, and Linux.
# It requires exporting all large objects to each worker, which is slower than
# FORK but is the only option on Windows.
PARALLEL_TYPE <- "PSOCK"

# ── Paths ────────────────────────────────────────────────────────────────────
# Everything lives under optim_cluster_jobs/ — one input data folder holding
# BOTH weekly.RDS and facility_level_final.RDS, plus an Outputs folder.
PROJECT_ROOT <- getwd()
JOB_DIR      <- file.path(PROJECT_ROOT, "optim_cluster_jobs")

DATA_DIR <- file.path(JOB_DIR, "data")     # weekly.RDS + facility_level_final.RDS
OUT_DIR  <- file.path(JOB_DIR, "Outputs")

# Optional warm start from a prior calibration result; safe to leave as-is —
# the optimiser falls back to a mid-range starting guess if this file is absent.
V3_RESULT_FILE <- file.path(JOB_DIR, "warmup_beta.RDS")

BASE_TEXT <- 12  # slightly smaller text for local screen viewing

# ── Verbose diagnostic flag ───────────────────────────────────────────────────
# Prints data-loading shapes and key intermediate values for debugging.
LOCAL_VERBOSE <- TRUE


# =============================================================================
# PRE-FLIGHT DIAGNOSTICS  (run before sourcing core to catch data issues early)
# =============================================================================
if (LOCAL_VERBOSE) {
  message("\n=== LOCAL PRE-FLIGHT ===")
  message("Working directory : ", getwd())
  message("JOB_DIR           : ", JOB_DIR)
  message("DATA_DIR          : ", DATA_DIR)
  message("OUT_DIR           : ", OUT_DIR)

  if (!dir.exists(JOB_DIR)) {
    stop(
      "optim_cluster_jobs/ folder not found under the current working directory.\n",
      "  Current working directory : ", getwd(), "\n",
      "  Expected folder           : ", JOB_DIR, "\n",
      "  Fix: setwd() to the folder that CONTAINS optim_cluster_jobs/."
    )
  }

  # Confirm the two required input files exist before doing anything else
  required_files <- c(
    file.path(DATA_DIR, "weekly.RDS"),
    file.path(DATA_DIR, "facility_level_final.RDS")
  )
  for (f in required_files) {
    ok <- file.exists(f)
    message(if (ok) "  [OK]  " else "  [MISSING] ", f)
    if (!ok) stop("Required file not found: ", f)
  }

  # Quick column check on facility_level_final.RDS so a missing column fails
  # fast with a clear message, rather than deep inside the simulation later.
  fl_check <- readRDS(file.path(DATA_DIR, "facility_level_final.RDS"))
  #Rany Fixing column names
  fl_check = fl_check %>% 
    rename(facility_type = type_spares,
           incidence_esbl_all = incidence_region_type_ESBL_all)
  needed_cols <- c("finess_geo", "facility_type", "region",
                   "census_max", "los_mean", "incidence_esbl_all")
  missing_cols <- setdiff(needed_cols, names(fl_check))
  if (length(missing_cols) > 0) {
    stop("facility_level_final.RDS is missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }
  message("  [OK]  facility_level_final.RDS has all required columns: ",
          paste(needed_cols, collapse = ", "))
  message("  facility_level_final.RDS rows: ", nrow(fl_check))
  rm(fl_check)

  message("LOCAL_TEST        : ", LOCAL_TEST,
          if (LOCAL_TEST) paste0("  (N_HOSP_TEST = ", N_HOSP_TEST, ")") else "")
  message("PARALLEL_TYPE     : PSOCK  (Windows-compatible)")
  message("N_CORES           : ", N_CORES, "  N_REP_OBJ = ", N_REP_OBJ)
  message("=========================\n")
}

# Ensure the output directory exists before the core creates run subfolders
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# SOURCE SHARED CORE (all model logic, data prep, optimisation loop)
# =============================================================================
SHARED_DIR <- file.path(JOB_DIR, "shared")

if (!file.exists(file.path(SHARED_DIR, "optim_core.R"))) {
  stop(
    "Cannot find optim_cluster_jobs/shared/optim_core.R.\n",
    "  Current working directory : ", getwd(), "\n",
    "  Expected file at          : ", file.path(SHARED_DIR, "optim_core.R")
  )
}

source(file.path(SHARED_DIR, "optim_core.R"))
