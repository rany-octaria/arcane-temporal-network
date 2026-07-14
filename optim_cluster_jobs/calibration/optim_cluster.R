# =============================================================================
# optim_cluster.R  —  CLUSTER PRODUCTION DRIVER  (PBS / Torque)
# -----------------------------------------------------------------------------
# Full-scale calibration run for the PBS cluster at
#   /media/kevinNFS2/rany  (queue: mem128G)
# Edit only the KNOBS section below; all model logic lives in:
#   optim_cluster_jobs/shared/optim_core.R  (sourced at the end of this script)
#
# CONSOLIDATED LAYOUT (identical structure to the local driver — only the
# KNOBS differ between local and cluster):
#   /media/kevinNFS2/rany/                      ← PROJECT_ROOT (= ARCANE_ROOT)
#   └── optim_cluster_jobs/
#       ├── shared/
#       │   └── optim_core.R          ← model + optimisation logic (one copy,
#       │                                shared by both local and cluster runs)
#       ├── calibration/
#       │   ├── optim_local.R         ← local test driver
#       │   └── optim_cluster.R       ← this file
#       ├── data/
#       │   ├── weekly.RDS                  — transfer network
#       │   └── facility_level_final.RDS    — facility type/region/census_max/
#       │                                      los_mean/incidence_esbl_all
#       ├── warmup_beta.RDS            (optional) prior best-beta for warm start
#       ├── logs/                      PBS stdout / stderr
#       └── Outputs/                   run_YYYYMMDD_HHMMSS/ result folders
#
# DATA SOURCE: facility_level_final.RDS (same file, same folder, same columns
# as the local version) supplies facility_type, region, census_max (treated
# as full-occupancy bed capacity), los_mean (length of stay), and
# incidence_esbl_all (SPARES target).  There is no separate Datasets/ folder
# and no node_attributes_full.RDS / coords_beds_active.RDS anywhere in this
# pipeline anymore.
#
# HOW TO SUBMIT:
#   1. Place input data as shown above
#   2. cd /media/kevinNFS2/rany
#   3. qsub optim_cluster_jobs/submit_optim.sh
#   — or run directly for interactive testing:
#   4. Rscript --vanilla optim_cluster_jobs/calibration/optim_cluster.R
#
# ENVIRONMENT VARIABLES (set by submit_optim.sh or manually):
#   ARCANE_ROOT — absolute path to the project root on the cluster
#                 (defaults to /media/kevinNFS2/rany if not set)
#   NCPUS       — overrides N_CORES if set.  submit_optim.sh computes this
#                 from the PBS node allocation ($PBS_NODEFILE) and exports it,
#                 so this script never needs to know about PBS internals directly.
#
# CLUSTER-SPECIFIC CHOICES:
#   • PARALLEL_TYPE = "FORK": children inherit parent's memory via copy-on-write
#     — no serialisation of the ~500 MB transfer object to N_CORES workers.
#     FORK is only available on Linux/macOS; never use it on Windows.
#   • options(future.globals.maxSize) not needed with FORK (no furrr used here)
#     but is set defensively in case future is loaded elsewhere.
# =============================================================================


# =============================================================================
# 0.  KNOBS  —  CLUSTER / PRODUCTION SETTINGS
# =============================================================================

# ── Parallelism ───────────────────────────────────────────────────────────────
# NCPUS is exported by submit_optim.sh after counting lines in $PBS_NODEFILE
# (the standard way to learn how many cores PBS actually granted this job).
# Falls back to 30 for interactive / manual runs where NCPUS is unset.
N_CORES <- {
  env_val <- suppressWarnings(as.integer(Sys.getenv("NCPUS")))
  if (!is.na(env_val) && env_val > 0) max(1L, env_val - 1L) else 30L
  # Reserve 1 CPU for the main R process; workers get the rest
}

# ── Optimisation ─────────────────────────────────────────────────────────────
N_REP_OBJ       <- 100   # replicates per objective evaluation
N_REP_VALID     <- 300   # replicates for final validation
N_RANDOM_STARTS <- 6     # random starts on top of 3 structured ones (9 total)
MAXIT_NM        <- 150   # Nelder-Mead iteration cap per start

# ── RNG seeds ────────────────────────────────────────────────────────────────
SEED_OBJ    <- 1000
SEED_VALID  <- 50000
SEED_STARTS <- 123

# ── Search space ─────────────────────────────────────────────────────────────
LOWER_BETA <- 1e-5
UPPER_BETA <- 0.10

# ── Simulation ───────────────────────────────────────────────────────────────
N_YEARS        <- 2
INIT_PREV      <- 0.02   # small endemic seed to shorten burn-in
ADMISSION_PREV <- 0.05
ALPHA          <- 0
GAMMA_CLEAR    <- 1 / 387

# Used only as a fallback where facility_level_final.RDS$los_mean is NA
DEFAULT_LOS <- c(
  "MCO" = 5.5, "SSR" = 32.0, "MCO/SSR" = 7.0, "PSY" = 60.0,
  "HAD" = 22.0, "CLCC" = 6.0, "Other" = 8.0, "Unknown" = 7.0
)
MAX_P_TR <- 0.60

# ── No subsetting on cluster ──────────────────────────────────────────────────
LOCAL_TEST    <- FALSE   # use full hospital network
N_HOSP_TEST   <- NULL    # unused when LOCAL_TEST = FALSE

# ── Parallel backend ─────────────────────────────────────────────────────────
# FORK: workers inherit the parent process memory via copy-on-write.
# This avoids serialising the ~500 MB transfer object to each worker,
# saving both time and ~N_CORES × 500 MB of RAM.
# FORK is Linux-only; it will crash on Windows.
PARALLEL_TYPE <- "FORK"

# ── Paths (resolved from ARCANE_ROOT environment variable) ───────────────────
# Defaults to the path your existing working script.sh already uses.
PROJECT_ROOT <- Sys.getenv("ARCANE_ROOT", unset = "/media/kevinNFS2/rany")
JOB_DIR      <- file.path(PROJECT_ROOT, "optim_cluster_jobs")

# ONE data folder holds BOTH weekly.RDS and facility_level_final.RDS — same
# contract as the local driver, so optim_core.R needs no cluster-specific logic.
DATA_DIR <- file.path(JOB_DIR, "data")
OUT_DIR  <- file.path(JOB_DIR, "Outputs")

# Warm start: OPTIONAL.  If you have a prior best-beta estimate, copy that
# result file here and rename it warmup_beta.RDS.  It must contain an object
# with $beta_by_type — a data frame with columns facility_type and best_beta.
# If absent, the optimiser starts from a mid-range guess instead — slower
# convergence but not required.
V3_RESULT_FILE <- file.path(JOB_DIR, "warmup_beta.RDS")

BASE_TEXT <- 14

# A single JOB_ID variable that resolves whether the scheduler is PBS or SLURM
# (kept generic in case this script is ever run on a different cluster type).
JOB_ID <- Sys.getenv("PBS_JOBID",
                     unset = Sys.getenv("SLURM_JOB_ID", unset = "(interactive)"))


# =============================================================================
# CLUSTER INITIALISATION
# =============================================================================
message("=== ARCANE OPTIM CLUSTER RUN ===")
message("PROJECT_ROOT  : ", PROJECT_ROOT)
message("JOB_DIR       : ", JOB_DIR)
message("DATA_DIR      : ", DATA_DIR, "  (weekly.RDS + facility_level_final.RDS)")
message("OUT_DIR       : ", OUT_DIR)
message("PARALLEL_TYPE : FORK  (", N_CORES, " workers)")
message("N_REP_OBJ     : ", N_REP_OBJ)
message("MAXIT_NM      : ", MAXIT_NM, "  ×  ", 3 + N_RANDOM_STARTS, " starts")
message("JOB_ID        : ", JOB_ID)
message("Node          : ", Sys.info()[["nodename"]])
message("Start time    : ", format(Sys.time()))
message("================================\n")

# Sanity check the two required input files BEFORE sourcing the core, so a
# missing file fails fast with a clear message rather than mid-simulation.
required_files <- c(
  file.path(DATA_DIR, "weekly.RDS"),
  file.path(DATA_DIR, "facility_level_final.RDS")
)
for (f in required_files) {
  if (!file.exists(f)) {
    stop("Required file not found: ", f,
         "\nBoth weekly.RDS and facility_level_final.RDS must be in DATA_DIR.")
  }
}
message("Input data check passed: both required files found in DATA_DIR.\n")

# Defensive memory cap for any future-aware code loaded transitively
options(future.globals.maxSize = 4 * 1024^3)  # 4 GB

# Ensure output directory exists before the core creates the checkpoint subdir
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# SOURCE SHARED CORE
# =============================================================================
# optim_core.R lives in optim_cluster_jobs/shared/ — the SAME file used by
# optim_local.R, so any model change made for the local check automatically
# applies here with zero duplication.
SHARED_DIR <- file.path(JOB_DIR, "shared")

if (!file.exists(file.path(SHARED_DIR, "optim_core.R"))) {
  stop("Cannot find optim_cluster_jobs/shared/optim_core.R under PROJECT_ROOT = ",
       PROJECT_ROOT,
       "\nSet ARCANE_ROOT correctly before running this script.")
}

source(file.path(SHARED_DIR, "optim_core.R"))

# ── Post-run timing log ───────────────────────────────────────────────────────
message("\nEnd time  : ", format(Sys.time()))
message("JOB_ID = ", JOB_ID)
