# =============================================================================
# compile_seeding_novel.R  —  Pool local + cluster seeding_novel results
# =============================================================================
# LOCAL:   Outputs/seeding_novel/seeding_results_*.rds  (single file)
# CLUSTER: Outputs/seeding_novel_cluster/job_XX/seeding_results_*.rds
#
# Switch INCLUDE_LOCAL / INCLUDE_CLUSTER as needed.
# Each source is tagged before pooling — sim_id alone is NOT unique across
# jobs, so global_sim_id = paste0(source, "_sim", sim_id) is the true key.
# =============================================================================

library(dplyr)

LOCAL_ROOT   <- "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/calibration_jobs"
LOCAL_DIR    <- file.path(LOCAL_ROOT, "Outputs", "seeding_novel")
CLUSTER_DIR  <- file.path(LOCAL_ROOT, "Outputs", "seeding_novel_cluster")
COMPILED_DIR <- file.path(LOCAL_ROOT, "Outputs", "seeding_novel", "compiled")
dir.create(COMPILED_DIR, recursive=TRUE, showWarnings=FALSE)

INCLUDE_LOCAL   <- TRUE    # set FALSE if local run not available
INCLUDE_CLUSTER <- TRUE    # set FALSE if cluster results not yet copied

# =============================================================================
# HELPER: load RDS or CSV from a folder, return tagged sim_summary + traj
# =============================================================================

load_result <- function(folder, source_label) {
  rds_f <- list.files(folder, "seeding_results.*\\.rds$", full.names=TRUE)
  csv_f <- list.files(folder, "seeding_summary.*\\.csv$",  full.names=TRUE)
  
  if (length(rds_f) == 0 && length(csv_f) == 0) {
    message("  ", source_label, " — no results found in: ", folder)
    return(NULL)
  }
  
  if (length(rds_f) > 0) {
    res <- readRDS(tail(sort(rds_f), 1))
    sc  <- res$sim_summary %>%
      mutate(source = source_label,
             global_sim_id = paste0(source_label, "_sim", sim_id))
    # Tag traj with source BEFORE returning so global_sim_id is correct
    tr  <- if (!is.null(res$traj_df))
      res$traj_df %>%
      mutate(source        = source_label,
             global_sim_id = paste0(source_label, "_sim", sim_id))    else NULL
    params <- res$params
  } else {
    sc  <- read.csv(tail(sort(csv_f), 1), stringsAsFactors=FALSE) %>%
      mutate(source = source_label,
             global_sim_id = paste0(source_label, "_sim", sim_id))
    tr  <- NULL
    params <- NULL
  }
  
  n_ext <- sum(sc$ever_extinct)
  message(sprintf("  %-35s  %d sims | extinct %d (%.0f%%)",
                  source_label, nrow(sc), n_ext, n_ext/nrow(sc)*100))
  list(sc=sc, traj=tr, params=params)
}

# =============================================================================
# 1. LOCAL
# =============================================================================

all_sc   <- list()
all_traj <- list()
params_ref <- NULL

if (INCLUDE_LOCAL) {
  message("=== Local result ===")
  res <- load_result(LOCAL_DIR, "local")
  if (!is.null(res)) {
    all_sc[["local"]]   <- res$sc
    if (!is.null(res$traj)) all_traj[["local"]] <- res$traj
    params_ref <- res$params
  }
}

# =============================================================================
# 2. CLUSTER (job_XX subfolders)
# =============================================================================

if (INCLUDE_CLUSTER) {
  message("\n=== Cluster results ===")
  job_dirs <- list.dirs(CLUSTER_DIR, full.names=TRUE, recursive=FALSE)
  job_dirs <- sort(job_dirs[grepl("job_\\d+$", basename(job_dirs))])
  
  if (length(job_dirs) == 0) {
    message("  No job_XX folders found in: ", CLUSTER_DIR)
  } else {
    message("  Found ", length(job_dirs), " job folder(s)")
    for (jdir in job_dirs) {
      key <- paste0("cluster_", basename(jdir))
      res <- load_result(jdir, key)
      if (!is.null(res)) {
        all_sc[[key]]   <- res$sc
        if (!is.null(res$traj)) all_traj[[key]] <- res$traj
        if (is.null(params_ref)) params_ref <- res$params
      }
    }
  }
}

# =============================================================================
# 3. POOL — join on global_sim_id so trajectories stay correctly matched
# =============================================================================

if (length(all_sc) == 0)
  stop("No results loaded from any source.")

sim_summary <- bind_rows(all_sc)   # global_sim_id is already in each chunk

traj_df <- if (length(all_traj) > 0)
  bind_rows(all_traj)  else NULL

message("\n=== POOLED SUMMARY ===")
message("Sources: ", paste(names(all_sc), collapse=", "))
message("Total sims     : ", nrow(sim_summary))
message("Extinct        : ", sum(sim_summary$ever_extinct),
        " (", round(mean(sim_summary$ever_extinct)*100,1), "%)")
message("Non-extinct    : ", sum(!sim_summary$ever_extinct))

cat("\nOutcomes by source x tier (non-extinct):\n")
print(sim_summary %>%
        filter(!ever_extinct) %>%
        group_by(source, tier) %>%
        summarise(n=n(),
                  net_ss_inc = round(mean(net_ss_inc),3),
                  fac_ss_inc = round(mean(fac_ss_inc_mean),3),
                  .groups="drop"), n=Inf)

# =============================================================================
# 4. SAVE
# =============================================================================

run_date <- format(Sys.Date(), "%Y%m%d")

saveRDS(list(
  sim_summary = sim_summary,
  traj_df     = traj_df,
  sources     = names(all_sc),
  total_sims  = nrow(sim_summary),
  datetime    = Sys.time(),
  params      = params_ref
), file.path(COMPILED_DIR,
             paste0("compiled_seeding_novel_", run_date, ".rds")))

write.csv(sim_summary,
          file.path(COMPILED_DIR, "seeding_summary_compiled.csv"),
          row.names=FALSE)

cat("\n=== DONE ===\n")
cat("Sources    :", paste(names(all_sc), collapse=", "), "\n")
cat("Total sims :", nrow(sim_summary), "\n")
cat("Saved to   :", COMPILED_DIR, "\n")