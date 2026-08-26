# =============================================================================
# compile_france.R
# Loads France-wide calibration results, filters to a specific date,
# picks the best run (lowest SSE), and saves a compiled object for the Rmd.
#
# HOW TO USE:
#   1. Set TARGET_DATE to the date you want to compile (format: YYYYMMDD)
#   2. Run: source("compile_france.R")
#   3. Opens compiled_france.rds in Outputs/france/compiled/
# =============================================================================

library(dplyr)

# ── Set the date you want to compile ─────────────────────────────────────────
TARGET_DATE <- "20260821"   # August 21 2026 — change as needed

OUT_DIR <- file.path(
  "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/optim_cluster_jobs",
  "Outputs", "france"
)

###############################################################################
# 1. FIND ALL RESULT FILES
###############################################################################

# Results are nested: job_XX / run_nm_france_YYYYMMDD_HHMMSS / final_validation.rds
job_dirs <- list.dirs(OUT_DIR, recursive = FALSE, full.names = TRUE)
job_dirs <- job_dirs[grepl("job_\\d+", basename(job_dirs))]
job_dirs <- job_dirs[!grepl("compiled|analysis", job_dirs)]

if (length(job_dirs) == 0) stop("No job_XX folders found under: ", OUT_DIR)

# From each job folder find all result files, then take the most recent per job
all_files <- unlist(lapply(job_dirs, function(d) {
  list.files(d, pattern = "final_validation\\.rds$",
             recursive = TRUE, full.names = TRUE)
}))
all_files <- all_files[!is.na(all_files)]

cat("Total final_validation.rds files found:", length(all_files), "\n")

###############################################################################
# 2. FILTER TO TARGET DATE
# The date appears in the run folder name: run_nm_france_YYYYMMDD_HHMMSS
###############################################################################

date_files <- all_files[grepl(TARGET_DATE, all_files)]

cat("Files matching", TARGET_DATE, ":", length(date_files), "\n")

if (length(date_files) == 0) {
  # Show what dates ARE available so the user can choose
  available <- unique(gsub(".*run_nm_france_(\\d{8}).*", "\\1", all_files))
  available <- available[grepl("^\\d{8}$", available)]
  cat("No files found for date:", TARGET_DATE, "\n")
  cat("Available dates in your results:\n")
  print(sort(available))
  stop("Update TARGET_DATE above to one of the available dates.")
}

# If a job ran twice on the same date, take the most recent time
# (sort descending so the latest HHMMSS comes first per job)
date_files_sorted <- sort(date_files, decreasing = TRUE)

# Keep only one file per job folder (the most recent on that date)
job_of_file <- gsub(".*(job_\\d+).*", "\\1", date_files_sorted)
keep        <- !duplicated(job_of_file)
date_files  <- date_files_sorted[keep]

cat("Files after deduplication (one per job):", length(date_files), "\n")
cat("\nFiles to be compiled:\n")
cat(paste(" ", date_files), sep = "\n")

###############################################################################
# 3. LOAD ALL SELECTED RESULTS
###############################################################################

results  <- lapply(date_files, readRDS)
n_jobs   <- length(results)

# Extract key metrics
job_ids  <- sapply(results, function(r) r$job_index)
sse_vals <- sapply(results, function(r) r$sse_final)

###############################################################################
# 4. RANK AND PICK BEST
###############################################################################

sse_ranking <- data.frame(
  job_index  = job_ids,
  sse        = round(sse_vals, 6),
  file       = basename(dirname(date_files)),
  stringsAsFactors = FALSE
) %>% arrange(sse)

cat("\n=== SSE RANKING (", TARGET_DATE, ") ===\n")
print(sse_ranking)

best_idx    <- which.min(sse_vals)
best_result <- results[[best_idx]]

cat("\nBest job  :", best_result$job_index, "\n")
cat("Best SSE  :", round(best_result$sse_final, 6), "\n")
cat("Best beta :\n")
print(round(best_result$beta_type_opt, 6))

###############################################################################
# 5. COMBINED VALIDATION SUMMARY (all selected runs)
###############################################################################

all_summaries <- bind_rows(lapply(results, function(r) {
  r$validation_summary %>%
    mutate(job_index = r$job_index, sse_total = r$sse_final,
           date = TARGET_DATE)
}))

###############################################################################
# 6. BETA STABILITY TABLE
###############################################################################

beta_stats <- all_summaries %>%
  group_by(type) %>%
  summarise(
    n_runs   = n(),
    beta_min = round(min(beta, na.rm = TRUE), 6),
    beta_med = round(median(beta, na.rm = TRUE), 6),
    beta_max = round(max(beta, na.rm = TRUE), 6),
    beta_cv  = round(100 * sd(beta, na.rm = TRUE) /
                       mean(beta, na.rm = TRUE), 1),
    beta_best = round(
      all_summaries$beta[all_summaries$type == type[1] &
                           all_summaries$job_index == best_result$job_index][1],
      6),
    .groups  = "drop"
  )

cat("\n=== BETA STABILITY ===\n")
print(beta_stats)

###############################################################################
# 7. SAVE COMPILED OBJECT
###############################################################################

compiled_dir <- file.path(OUT_DIR, "compiled")
dir.create(compiled_dir, recursive = TRUE, showWarnings = FALSE)

compiled <- list(
  target_date    = TARGET_DATE,
  datetime       = Sys.time(),
  n_jobs         = n_jobs,
  sse_ranking    = sse_ranking,
  best_job       = best_result$job_index,
  best_sse       = best_result$sse_final,
  best_result    = best_result,
  all_summaries  = all_summaries,
  beta_stats     = beta_stats,
  files_used     = date_files
)

save_path <- file.path(compiled_dir,
                       paste0("compiled_france_", TARGET_DATE, ".rds"))
saveRDS(compiled, save_path)

# Also save as the default (for the Rmd to pick up without knowing the date)
saveRDS(compiled, file.path(compiled_dir, "compiled_france_latest.rds"))

# Save warm start for region jobs
warm <- list(
  beta_type_opt   = setNames(best_result$validation_summary$beta,
                             best_result$validation_summary$type),
  objective_value = best_result$sse_final,
  job_index       = best_result$job_index,
  date            = TARGET_DATE,
  datetime        = Sys.time()
)
saveRDS(warm, file.path(dirname(OUT_DIR), "warm_start_france.rds"))

# Save CSV summaries
write.csv2(sse_ranking,
           file.path(compiled_dir,
                     paste0("sse_ranking_", TARGET_DATE, ".csv")),
           row.names = FALSE)
write.csv2(all_summaries,
           file.path(compiled_dir,
                     paste0("all_jobs_summary_", TARGET_DATE, ".csv")),
           row.names = FALSE)
write.csv2(beta_stats,
           file.path(compiled_dir,
                     paste0("beta_stability_", TARGET_DATE, ".csv")),
           row.names = FALSE)

cat("\n=== DONE ===\n")
cat("Compiled object saved to:\n  ", save_path, "\n")
cat("warm_start_france.rds updated.\n")
cat("CSV tables saved to:", compiled_dir, "\n")