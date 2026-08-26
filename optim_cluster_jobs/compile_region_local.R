# =============================================================================
# compile_region.R
# Scans all region result folders, picks the best run per region (lowest SSE),
# and saves one compiled dataset for the Rmd.
#
# HOW TO USE:
#   1. Set REGION_DIR to wherever you copied the cluster results
#   2. source("compile_region.R")
#   3. Produces Outputs/region_cluster/compiled/compiled_region.rds
# =============================================================================

library(dplyr)
library(tidyr)

# ── Point this to the cluster results folder ──────────────────────────────────
REGION_DIR <- file.path(
  "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/optim_cluster_jobs",
  "Outputs", "region" #region_cluster"
)

###############################################################################
# 1. DISCOVER ALL REGION FOLDERS
###############################################################################

region_folders <- list.dirs(REGION_DIR, recursive = FALSE, full.names = TRUE)
region_folders <- region_folders[!grepl("compiled|analysis", region_folders)]

if (length(region_folders) == 0)
  stop("No region folders found under: ", REGION_DIR)

cat("Region folders found:", length(region_folders), "\n")
cat(paste(" ", basename(region_folders)), sep = "\n")

###############################################################################
# 2. LOAD ALL RUNS PER REGION — PICK BEST (LOWEST SSE)
###############################################################################

load_best_for_region <- function(region_folder) {
  
  region_name <- basename(region_folder)
  
  # Results nested: run_XX/run_nm_REGION_YYYYMMDD/final_validation.rds
  # Also handle older job_XX structure
  run_dirs <- list.dirs(region_folder, recursive = FALSE, full.names = TRUE)
  run_dirs <- run_dirs[grepl("run_\\d+|job_\\d+", basename(run_dirs))]
  run_dirs <- run_dirs[!grepl("compiled|analysis", run_dirs)]
  
  # From each run folder take the most recent final_validation.rds
  rds_files <- unlist(lapply(run_dirs, function(d) {
    f <- list.files(d, pattern = "final_validation\\.rds$",
                    recursive = TRUE, full.names = TRUE)
    if (length(f) == 0) return(NULL)
    sort(f, decreasing = TRUE)[1]
  }))
  
  # Fallback: look directly in region folder (single-run results)
  if (length(rds_files) == 0) {
    rds_files <- list.files(region_folder, pattern = "final_validation\\.rds$",
                            recursive = TRUE, full.names = TRUE)
    rds_files <- rds_files[!grepl("compiled|analysis", rds_files)]
  }
  
  if (length(rds_files) == 0) {
    cat("  WARNING: no final_validation.rds for", region_name, "\n")
    return(NULL)
  }
  
  # Load all runs for this region
  all_runs  <- lapply(rds_files, readRDS)
  sse_vals  <- sapply(all_runs, function(r) r$sse_final)
  best_run  <- all_runs[[which.min(sse_vals)]]
  
  cat(sprintf("  %-35s  %d runs  |  best SSE = %.6f\n",
              region_name, length(rds_files), min(sse_vals)))
  
  # Add run-level summary
  best_run$all_run_sse <- data.frame(
    region   = best_run$region,
    run_file = basename(dirname(rds_files)),
    sse      = round(sse_vals, 6),
    is_best  = sse_vals == min(sse_vals),
    stringsAsFactors = FALSE
  )
  
  best_run
}

cat("\nLoading results...\n")
results <- lapply(region_folders, load_best_for_region)
results <- Filter(Negate(is.null), results)

if (length(results) == 0) stop("No valid results found.")
cat("\nRegions successfully loaded:", length(results), "\n")

###############################################################################
# 3. COMBINED DATASETS
###############################################################################

# Validation summary: one row per type × region (best run per region)
all_summaries <- bind_rows(lapply(results, function(r) {
  r$validation_summary %>%
    mutate(region     = r$region,
           H_region   = r$H_region,
           sse_total  = r$sse_final,
           n_rep      = r$n_rep_valid)
}))

# Run-level SSE: all runs per region (for stability assessment)
all_run_sse <- bind_rows(lapply(results, `[[`, "all_run_sse"))

# Beta wide: one row per region, one column per facility type
beta_wide <- all_summaries %>%
  select(region, type, beta) %>%
  pivot_wider(names_from = type, values_from = beta) %>%
  left_join(
    all_summaries %>% distinct(region, sse_total, H_region),
    by = "region"
  ) %>%
  arrange(region)

# SSE summary by region
sse_summary <- all_summaries %>%
  distinct(region, sse_total, H_region) %>%
  arrange(sse_total)

cat("\n=== SSE BY REGION (sorted) ===\n")
print(sse_summary)

cat("\n=== BETA BY TYPE × REGION ===\n")
print(beta_wide)

###############################################################################
# 4. BETA STATS ACROSS REGIONS (for each type)
###############################################################################

beta_type_stats <- all_summaries %>%
  group_by(type) %>%
  summarise(
    n_regions = n(),
    beta_min  = round(min(beta, na.rm = TRUE), 6),
    beta_med  = round(median(beta, na.rm = TRUE), 6),
    beta_max  = round(max(beta, na.rm = TRUE), 6),
    beta_cv   = round(100 * sd(beta, na.rm = TRUE) / mean(beta, na.rm = TRUE), 1),
    .groups   = "drop"
  )

cat("\n=== BETA VARIABILITY ACROSS REGIONS ===\n")
print(beta_type_stats)

###############################################################################
# 5. SAVE COMPILED OBJECT
###############################################################################

compiled_dir <- file.path(REGION_DIR, "compiled")
dir.create(compiled_dir, recursive = TRUE, showWarnings = FALSE)

compiled <- list(
  datetime        = Sys.time(),
  n_regions       = length(results),
  regions         = sapply(results, `[[`, "region"),
  results         = setNames(results, sapply(results, `[[`, "region")),
  all_summaries   = all_summaries,
  all_run_sse     = all_run_sse,
  beta_wide       = beta_wide,
  sse_summary     = sse_summary,
  beta_type_stats = beta_type_stats
)

save_path <- file.path(compiled_dir, "compiled_region.rds")
saveRDS(compiled, save_path)

write.csv2(all_summaries,   file.path(compiled_dir, "all_regions_summary.csv"),    row.names = FALSE)
write.csv2(beta_wide,       file.path(compiled_dir, "beta_by_type_region.csv"),     row.names = FALSE)
write.csv2(sse_summary,     file.path(compiled_dir, "sse_by_region.csv"),           row.names = FALSE)
write.csv2(beta_type_stats, file.path(compiled_dir, "beta_variability_by_type.csv"), row.names = FALSE)

cat("\n=== DONE ===\n")
cat("Compiled object saved to:\n  ", save_path, "\n")
cat("CSV tables saved to:", compiled_dir, "\n")
cat("Regions included:", paste(sapply(results, `[[`, "region"), collapse = ", "), "\n")