# =============================================================================
# recover_all_france.R
# Scans all France job output folders and recovers whatever results exist,
# regardless of whether jobs ran to completion.
#
# Collects three types of evidence (in order of reliability):
#   1. final_validation.rds  — job completed fully (best)
#   2. checkpoint_best_beta.rds — optimisation done, validation timed out
#   3. checkpoint_last_eval.rds — partial optimisation (least reliable)
#
# Picks the overall best beta (lowest SSE), saves warm_start_france.rds,
# and prints a full comparison table.
# =============================================================================

library(dplyr)

OUT_DIR <- file.path(
  "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/optim_cluster_jobs",
  "Outputs", "france_latest"
)

if (!dir.exists(OUT_DIR)) stop("Output folder not found: ", OUT_DIR)

# =============================================================================
# 1. SCAN FOR ALL RESULT FILES
# =============================================================================

cat("Scanning:", OUT_DIR, "\n\n")

# Type 1: fully completed jobs
final_files <- list.files(OUT_DIR,
                           pattern    = "final_validation\\.rds$",
                           recursive  = TRUE,
                           full.names = TRUE)
final_files <- final_files[!grepl("compiled|analysis|recovered", final_files)]

# Type 2: optimisation done, validation timed out
ckpt_best_files <- list.files(OUT_DIR,
                               pattern    = "checkpoint_best_beta\\.rds$",
                               recursive  = TRUE,
                               full.names = TRUE)

# Type 3: partial optimisation (last eval seen)
ckpt_last_files <- list.files(OUT_DIR,
                               pattern    = "checkpoint_last_eval\\.rds$",
                               recursive  = TRUE,
                               full.names = TRUE)

cat("Found:\n")
cat("  final_validation.rds     :", length(final_files), "\n")
cat("  checkpoint_best_beta.rds :", length(ckpt_best_files), "\n")
cat("  checkpoint_last_eval.rds :", length(ckpt_last_files), "\n\n")

# =============================================================================
# 2. LOAD AND STANDARDISE ALL RESULTS
# =============================================================================

all_results <- list()

# ── Type 1: final validation ──────────────────────────────────────────────────
if (length(final_files) > 0) {
  for (f in final_files) {
    r <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(r)) next
    all_results[[length(all_results) + 1]] <- list(
      source      = "final_validation",
      job_index   = r$job_index,
      sse         = r$sse_final,
      beta        = r$beta_type_opt,
      incidence_obs  = r$incidence_obs,
      incidence_sim  = r$incidence_final,
      n_rep       = r$n_rep_valid,
      file        = f,
      datetime    = r$datetime
    )
  }
}

# ── Type 2: checkpoint best beta ─────────────────────────────────────────────
if (length(ckpt_best_files) > 0) {
  for (f in ckpt_best_files) {
    # Skip if already covered by a final_validation from the same run folder
    run_dir <- dirname(f)
    if (any(grepl(run_dir, final_files, fixed = TRUE))) next
    r <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(r)) next
    all_results[[length(all_results) + 1]] <- list(
      source      = "checkpoint_best",
      job_index   = NA_integer_,
      sse         = r$objective_value,
      beta        = r$beta_type,
      incidence_obs  = r$incidence_obs,
      incidence_sim  = r$incidence_sim,
      n_rep       = r$n_rep_obj,
      file        = f,
      datetime    = r$datetime
    )
  }
}

# ── Type 3: last eval checkpoint (only if nothing better found) ───────────────
if (length(ckpt_last_files) > 0) {
  for (f in ckpt_last_files) {
    run_dir <- dirname(f)
    already_covered <- any(grepl(run_dir, c(final_files, ckpt_best_files),
                                  fixed = TRUE))
    if (already_covered) next
    r <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(r) || !r$is_best) next
    all_results[[length(all_results) + 1]] <- list(
      source      = "checkpoint_last",
      job_index   = NA_integer_,
      sse         = r$objective_value,
      beta        = r$beta_type,
      incidence_obs  = r$incidence_obs,
      incidence_sim  = r$incidence_sim,
      n_rep       = r$n_rep_obj,
      file        = f,
      datetime    = r$datetime
    )
  }
}

if (length(all_results) == 0)
  stop("No usable results found in ", OUT_DIR)

cat("Total usable results loaded:", length(all_results), "\n\n")

# =============================================================================
# 3. COMPARISON TABLE
# =============================================================================

results_df <- bind_rows(lapply(all_results, function(r) {
  data.frame(
    source    = r$source,
    job_index = if (is.null(r$job_index)) NA_integer_ else as.integer(r$job_index),
    sse       = round(r$sse, 6),
    n_types   = length(r$beta),
    n_rep     = r$n_rep,
    file      = basename(dirname(r$file)),
    datetime  = format(r$datetime, "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE
  )
})) %>% arrange(sse)

cat("=== ALL RESULTS (sorted by SSE) ===\n")
print(results_df, row.names = FALSE)

# =============================================================================
# 4. BEST BETA
# =============================================================================

best_idx <- which.min(sapply(all_results, `[[`, "sse"))
best     <- all_results[[best_idx]]

cat("\n=== BEST RESULT ===\n")
cat("Source  :", best$source, "\n")
cat("SSE     :", round(best$sse, 6), "\n")
cat("File    :", best$file, "\n")
cat("Date    :", format(best$datetime, "%Y-%m-%d %H:%M"), "\n")

cat("\nOptimal beta per type:\n")
print(round(best$beta, 6))

if (!is.null(best$incidence_obs) && !is.null(best$incidence_sim)) {
  cat("\nObserved vs simulated incidence:\n")
  comp <- data.frame(
    type          = names(best$incidence_obs),
    observed      = round(as.numeric(best$incidence_obs), 3),
    simulated     = round(as.numeric(best$incidence_sim), 3),
    diff          = round(as.numeric(best$incidence_sim) -
                            as.numeric(best$incidence_obs), 3),
    pct_error     = round(100 * (as.numeric(best$incidence_sim) -
                                   as.numeric(best$incidence_obs)) /
                            as.numeric(best$incidence_obs), 1),
    stringsAsFactors = FALSE
  )
  print(comp, row.names = FALSE)
}

# =============================================================================
# 5. BETA COMPARISON ACROSS ALL RESULTS
# =============================================================================

all_types <- unique(unlist(lapply(all_results, function(r) names(r$beta))))

cat("\n=== BETA BY TYPE ACROSS ALL RESULTS ===\n")
beta_table <- data.frame(
  result = seq_along(all_results),
  source = sapply(all_results, `[[`, "source"),
  sse    = round(sapply(all_results, `[[`, "sse"), 6)
)
for (typ in all_types) {
  beta_table[[typ]] <- round(sapply(all_results, function(r) {
    b <- r$beta
    if (typ %in% names(b)) b[[typ]] else NA_real_
  }), 6)
}
print(beta_table %>% arrange(sse), row.names = FALSE)

# =============================================================================
# 6. SAVE BEST BETA AS WARM START FOR NEXT RUN
# =============================================================================

warm_start <- list(
  beta_type_opt   = best$beta,
  objective_value = best$sse,
  source          = best$source,
  source_file     = best$file,
  datetime        = Sys.time()
)

warm_path <- file.path(
  "C:/Users/octariar/OneDrive - LECNAM/Documents/GitHub/arcane-temporal-network-new/optim_cluster_jobs",
  "warm_start_france.rds"
)
saveRDS(warm_start, warm_path)
cat("\nWarm start saved to:\n ", warm_path, "\n")

# Also save summary CSV
compiled_dir <- file.path(OUT_DIR, "compiled")
dir.create(compiled_dir, recursive = TRUE, showWarnings = FALSE)
write.csv2(results_df,
           file.path(compiled_dir, "all_recovered_results.csv"),
           row.names = FALSE)
write.csv2(beta_table %>% arrange(sse),
           file.path(compiled_dir, "beta_comparison.csv"),
           row.names = FALSE)

cat("Summary CSVs saved to:", compiled_dir, "\n")
cat("\n=== DONE ===\n")
