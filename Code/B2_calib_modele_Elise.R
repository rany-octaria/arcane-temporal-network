###############################################################################
###### PATHOGÈNE #####
###############################################################################

pathogen <- "BLSE"
#pathogen <- "EPC"

###############################################################################
###### IMPORTS #####
###############################################################################

source("scripts/0_import.R")
source("scripts/B2_0_params.R")

library(parallel)

###############################################################################
###### PARAMÈTRES #####
###############################################################################

n_cores <- 30

n_rep_obj <- 100
n_rep_valid <- 300

seed_objective <- 1000
seed_validation <- 50000
seed_random_starts <- 123

lower_beta <- 1e-5
upper_beta <- 0.10

n_random_starts <- 8
maxit_nm <- 100

calibration_out_dir <- "save_simulations/calibration"
dir.create(calibration_out_dir, recursive = TRUE, showWarnings = FALSE)

last_year_start <- Tmax - 364
last_year_len <- 365

###############################################################################
###### INITIALISATION À L'ÉQUILIBRE #####
###############################################################################

load("save_simulations/init/prev_eq_100years_to_keep_for_init.RData")

prev_init_etab <- pmax(prev_init_etab, 1 / beds)

###############################################################################
###### FICHIERS BETA INITIAUX ; SORTIES GLOBALES #####
###############################################################################

warm_start_file <- file.path(
  calibration_out_dir,
  paste0("warm_start_beta_", pathogen, ".rds")
)

final_recovered_beta_file <- file.path(
  calibration_out_dir,
  paste0("recovered_best_beta_", pathogen, ".rds")
)

final_validation_summary_file <- file.path(
  calibration_out_dir,
  paste0("validation_summary_latest_", pathogen, ".csv")
)

###############################################################################
###### BETA INITIAL (trouvé en précalibration) #####
###############################################################################

warm_start_object <- readRDS(warm_start_file)

beta_start <- warm_start_object$beta_type
beta_start <- beta_start[names(incidence_obs)]
beta_start <- pmin(pmax(beta_start, lower_beta), upper_beta)

###############################################################################
###### FONCTION DE SIMULATION (résumée pour que ça aille plus vite) #####
###############################################################################

run_simulation_summary <- function(beta_vec, alpha, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  p_rec <- 1 - exp(-gamma)
  
  ###########################################################################
  ###### INITIALISATION #####
  ###########################################################################
  
  I_loc <- rbinom(H, beds, prev_init_etab)
  S_loc <- beds - I_loc
  
  inc_sum_last <- numeric(H)
  
  for (t in seq_len(Tmax)) {
    
    #########################################################################
    ###### SIS #####
    #########################################################################
    
    for (i in seq_len(H)) {
      
      N <- S_loc[i] + I_loc[i]
      if (N <= 0) next
      
      p_inf <- 1 - exp(-beta_vec[i] * I_loc[i] / N)
      
      new_inf <- rbinom(1, S_loc[i], p_inf)
      recov   <- rbinom(1, I_loc[i], p_rec)
      
      if (t >= last_year_start) {
        inc_sum_last[i] <- inc_sum_last[i] + new_inf
      }
      
      S_loc[i] <- S_loc[i] - new_inf + recov
      I_loc[i] <- I_loc[i] + new_inf - recov
    }
    
    #########################################################################
    ###### SORTIES + TRANSFERTS #####
    #########################################################################
    
    S_stay <- S_loc
    I_stay <- I_loc
    
    S_tr <- numeric(H)
    I_tr <- numeric(H)
    
    handle_exit <- function(h) {
      
      n_exit_S <- rbinom(1, S_loc[h], p_exit[h])
      n_exit_I <- rbinom(1, I_loc[h], p_exit[h])
      
      if ((n_exit_S + n_exit_I) == 0) return()
      
      S_stay[h] <<- S_stay[h] - n_exit_S
      I_stay[h] <<- I_stay[h] - n_exit_I
      
      p_tr_h <- p_tr[h]
      
      n_tr_S <- rbinom(1, n_exit_S, p_tr_h)
      n_tr_I <- rbinom(
        1,
        n_exit_I,
        pmin(pmax((1 - alpha) * p_tr_h, 0), 1)
      )
      
      if ((n_tr_S + n_tr_I) > 0) {
        
        probs_dest <- P_tr[, h]
        
        if (!all(is.finite(probs_dest))) return()
        
        s <- sum(probs_dest)
        if (s <= 0) return()
        
        probs_dest <- probs_dest / s
        
        if (n_tr_S > 0) {
          dest_S <- rmultinom(1, n_tr_S, probs_dest)
          S_tr <<- S_tr + dest_S[, 1]
        }
        
        if (n_tr_I > 0) {
          dest_I <- rmultinom(1, n_tr_I, probs_dest)
          I_tr <<- I_tr + dest_I[, 1]
        }
      }
    }
    
    for (h in seq_len(H)) handle_exit(h)
    
    #########################################################################
    ###### ADMISSIONS #####
    #########################################################################
    
    occ <- S_stay + I_stay + S_tr + I_tr
    A <- pmax(0, beds - occ)
    
    A_I <- rbinom(H, A, pi_vec)
    A_S <- A - A_I
    
    S_loc <- S_stay + S_tr + A_S
    I_loc <- I_stay + I_tr + A_I
  }
  
  ###########################################################################
  ###### INCIDENCE PAR ÉTABLISSEMENT PUIS PAR TYPE #####
  ###########################################################################
  
  inc_etab <- 1000 * inc_sum_last / (beds * last_year_len)
  
  incidence_type <- tapply(
    inc_etab,
    type_etab_calib,
    mean,
    na.rm = TRUE
  )
  
  incidence_type <- incidence_type[names(incidence_obs)]
  
  incidence_type
}

###############################################################################
###### CLUSTER #####
###############################################################################

cl <- makeCluster(n_cores)

on.exit(stopCluster(cl), add = TRUE)

clusterExport(cl, varlist = c(
  "run_simulation_summary",
  "alpha",
  "gamma",
  "pi_vec",
  "beds",
  "H",
  "Tmax",
  "p_exit",
  "p_tr",
  "P_tr",
  "prev_init_etab",
  "type_etab_calib",
  "incidence_obs",
  "last_year_start",
  "last_year_len"
))

###############################################################################
###### RÉPÉTITIONS PARALLÈLES #####
###############################################################################

rep_chunks <- split(
  seq_len(n_rep_obj),
  rep(seq_len(n_cores), length.out = n_rep_obj)
)

rep_chunks_valid <- split(
  seq_len(n_rep_valid),
  rep(seq_len(n_cores), length.out = n_rep_valid)
)

###############################################################################
###### DOSSIER DE SAUVEGARDE #####
###############################################################################

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")

checkpoint_dir <- file.path(
  calibration_out_dir,
  pathogen,
  paste0("run_nm_", pathogen, "_", run_id)
)

dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

checkpoint_best_file <- file.path(checkpoint_dir, paste0("checkpoint_best_beta_", pathogen, ".rds"))
checkpoint_last_file <- file.path(checkpoint_dir, paste0("checkpoint_last_eval_", pathogen, ".rds"))
history_file <- file.path(checkpoint_dir, paste0("history_objective_", pathogen, ".csv"))
starts_file <- file.path(checkpoint_dir, paste0("starts_used_", pathogen, ".rds"))
fits_file <- file.path(checkpoint_dir, paste0("fits_nm_", pathogen, ".rds"))
final_file <- file.path(checkpoint_dir, paste0("final_validation_", pathogen, ".rds"))
validation_summary_run_file <- file.path(checkpoint_dir, paste0("validation_summary_", pathogen, ".csv"))

###############################################################################
###### SAUVEGARDE des dossiers #####
###############################################################################

eval_counter <- 0L
best_value <- Inf

safe_saveRDS <- function(object, file) {
  
  tmp_file <- paste0(file, ".tmp")
  saveRDS(object, tmp_file)
  
  if (file.exists(file)) file.remove(file)
  file.rename(tmp_file, file)
}

save_objective_state <- function(
    beta_type_log,
    beta_type,
    incidence_sim,
    objective_value,
    sse_by_type,
    is_best,
    start_id = NA_integer_
) {
  
  state <- list(
    datetime = Sys.time(),
    pathogen = pathogen,
    eval_counter = eval_counter,
    start_id = start_id,
    objective_value = objective_value,
    sse_by_type = sse_by_type,
    is_best = is_best,
    n_rep_obj = n_rep_obj,
    n_cores = n_cores,
    seed_objective = seed_objective,
    beta_type_log = beta_type_log,
    beta_type = beta_type,
    incidence_sim = incidence_sim,
    incidence_obs = incidence_obs,
    lower_beta = lower_beta,
    upper_beta = upper_beta,
    beta_start = beta_start,
    checkpoint_dir = checkpoint_dir
  )
  
  safe_saveRDS(state, checkpoint_last_file)
  
  if (is_best) {
    safe_saveRDS(state, checkpoint_best_file)
  }
  
  hist_row <- data.frame(
    eval_counter = eval_counter,
    datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    pathogen = pathogen,
    start_id = start_id,
    objective_value = objective_value,
    best_value = best_value,
    is_best = is_best,
    n_rep_obj = n_rep_obj,
    stringsAsFactors = FALSE
  )
  
  beta_cols <- as.data.frame(as.list(beta_type), check.names = FALSE)
  names(beta_cols) <- paste0("beta_", names(beta_type))
  
  inc_cols <- as.data.frame(as.list(incidence_sim), check.names = FALSE)
  names(inc_cols) <- paste0("inc_sim_", names(incidence_sim))
  
  sse_cols <- as.data.frame(as.list(sse_by_type), check.names = FALSE)
  names(sse_cols) <- paste0("sse_", names(sse_by_type))
  
  hist_row <- cbind(hist_row, beta_cols, inc_cols, sse_cols)
  
  write.table(
    hist_row,
    file = history_file,
    sep = ";",
    dec = ".",
    row.names = FALSE,
    col.names = !file.exists(history_file),
    append = file.exists(history_file)
  )
}

###############################################################################
###### FONCTION OBJECTIF #####
###############################################################################

current_start_id <- NA_integer_

objective_fn <- function(beta_type_log) {
  
  eval_counter <<- eval_counter + 1L
  
  beta_type <- exp(beta_type_log)
  names(beta_type) <- names(incidence_obs)
  
  beta_type <- beta_type[names(incidence_obs)]
  
  beta_vec <- beta_type[as.character(type_etab_calib)]
  
  if (anyNA(beta_vec)) {
    stop("NA dans beta_vec")
  }
  
  results <- parLapply(
    cl,
    X = rep_chunks,
    fun = function(rs, beta_vec, seed_objective) {
      
      out <- lapply(rs, function(r) {
        run_simulation_summary(
          beta_vec = beta_vec,
          alpha = alpha,
          seed = seed_objective + r
        )
      })
      
      do.call(rbind, out)
    },
    beta_vec = beta_vec,
    seed_objective = seed_objective
  )
  
  inc_mat <- do.call(rbind, results)
  
  incidence_sim <- colMeans(inc_mat, na.rm = TRUE)
  incidence_sim <- incidence_sim[names(incidence_obs)]
  
  sse_by_type <- (incidence_sim - incidence_obs)^2
  sse_by_type <- sse_by_type[names(incidence_obs)]
  
  objective_value <- sum(sse_by_type, na.rm = TRUE)
  
  is_best <- objective_value < best_value
  
  if (is_best) {
    best_value <<- objective_value
  }
  
  save_objective_state(
    beta_type_log = beta_type_log,
    beta_type = beta_type,
    incidence_sim = incidence_sim,
    objective_value = objective_value,
    sse_by_type = sse_by_type,
    is_best = is_best,
    start_id = current_start_id
  )
  
  print(data.frame(
    eval_counter = eval_counter,
    pathogen = pathogen,
    start_id = current_start_id,
    objective_value = objective_value,
    best_value = best_value,
    is_best = is_best
  ))
  
  print("Incidence simulée :")
  print(incidence_sim)
  
  print("Incidence observée :")
  print(incidence_obs)
  
  print("SSE par type :")
  print(sse_by_type)
  
  objective_value
}

###############################################################################
###### OBJECTIF BORNÉ POUR NELDER-MEAD #####
###############################################################################

lower_log <- log(lower_beta)
upper_log <- log(upper_beta)

objective_bounded <- function(beta_type_log) {
  
  if (any(!is.finite(beta_type_log))) {
    return(1e12)
  }
  
  if (any(beta_type_log < lower_log) || any(beta_type_log > upper_log)) {
    
    penalty <- sum(
      pmax(beta_type_log - upper_log, 0)^2 +
        pmax(lower_log - beta_type_log, 0)^2
    )
    
    return(1e9 + 1e9 * penalty)
  }
  
  objective_fn(beta_type_log)
}

###############################################################################
###### CONSTRUCTION DES POINTS DE DÉPART #####
###############################################################################

starts <- list()

starts[[length(starts) + 1]] <- log(beta_start)

beta_start_UH <- beta_start
beta_start_UH["University hospital"] <- 0.017
beta_start_UH <- pmin(pmax(beta_start_UH, lower_beta), upper_beta)
beta_start_UH <- beta_start_UH[names(incidence_obs)]
starts[[length(starts) + 1]] <- log(beta_start_UH)

beta_start_all_up <- beta_start * 1.5
beta_start_all_up <- pmin(pmax(beta_start_all_up, lower_beta), upper_beta)
beta_start_all_up <- beta_start_all_up[names(incidence_obs)]
starts[[length(starts) + 1]] <- log(beta_start_all_up)

beta_start_UH_high <- beta_start
beta_start_UH_high["University hospital"] <- 0.025
beta_start_UH_high <- pmin(pmax(beta_start_UH_high, lower_beta), upper_beta)
beta_start_UH_high <- beta_start_UH_high[names(incidence_obs)]
starts[[length(starts) + 1]] <- log(beta_start_UH_high)

set.seed(seed_random_starts)

for (s in seq_len(n_random_starts)) {
  
  beta_random <- beta_start * exp(
    runif(
      length(beta_start),
      min = log(0.25),
      max = log(4)
    )
  )
  
  beta_random <- pmin(pmax(beta_random, lower_beta), upper_beta)
  beta_random <- beta_random[names(incidence_obs)]
  
  starts[[length(starts) + 1]] <- log(beta_random)
}

starts <- lapply(starts, function(x) {
  x <- x[names(incidence_obs)]
  x <- pmin(pmax(x, lower_log), upper_log)
  x
})

names(starts) <- paste0("start_", seq_along(starts))

safe_saveRDS(
  list(
    datetime = Sys.time(),
    pathogen = pathogen,
    starts_log = starts,
    starts_beta = lapply(starts, exp),
    beta_start = beta_start,
    beta_start_UH = beta_start_UH,
    beta_start_all_up = beta_start_all_up,
    beta_start_UH_high = beta_start_UH_high,
    n_random_starts = n_random_starts,
    seed_random_starts = seed_random_starts,
    lower_beta = lower_beta,
    upper_beta = upper_beta,
    incidence_obs = incidence_obs
  ),
  starts_file
)

print("============================================================")
print("POINTS DE DÉPART UTILISÉS")
print("============================================================")
print(lapply(starts, exp))

###############################################################################
###### OPTIMISATION MULTI-START NELDER-MEAD #####
###############################################################################

fits <- vector("list", length(starts))
names(fits) <- names(starts)

for (i in seq_along(starts)) {
  
  current_start_id <<- i
  
  print("############################################################")
  print(paste("Début optimisation", names(starts)[i], "pour", pathogen))
  print("Beta de départ :")
  print(exp(starts[[i]]))
  print("############################################################")
  
  fit_i <- tryCatch(
    {
      optim(
        par = starts[[i]],
        fn = objective_bounded,
        method = "Nelder-Mead",
        control = list(
          maxit = maxit_nm,
          trace = 1,
          REPORT = 1,
          reltol = 1e-4,
          parscale = rep(1, length(starts[[i]]))
        )
      )
    },
    error = function(e) {
      list(
        par = starts[[i]],
        value = Inf,
        convergence = NA_integer_,
        message = conditionMessage(e)
      )
    }
  )
  
  fits[[i]] <- fit_i
  
  safe_saveRDS(
    list(
      datetime = Sys.time(),
      pathogen = pathogen,
      fits = fits,
      eval_counter = eval_counter,
      best_value = best_value,
      checkpoint_best_file = checkpoint_best_file,
      checkpoint_last_file = checkpoint_last_file,
      history_file = history_file,
      starts_file = starts_file,
      beta_start = beta_start
    ),
    fits_file
  )
  
  print("Fin optimisation du start :")
  print(names(starts)[i])
  print(fit_i$value)
  print(fit_i$convergence)
  print(fit_i$message)
}

current_start_id <<- NA_integer_

###############################################################################
###### RÉSULTATS OPTIMISATION #####
###############################################################################

fit_values <- sapply(fits, function(x) x$value)

print("============================================================")
print("VALEURS FINALES PAR START")
print("============================================================")
print(fit_values)

best_fit_id <- which.min(fit_values)

print("============================================================")
print("MEILLEUR START SELON OPTIM")
print("============================================================")
print(best_fit_id)
print(fits[[best_fit_id]]$value)

if (file.exists(checkpoint_best_file)) {
  
  best_checkpoint <- readRDS(checkpoint_best_file)
  
  beta_type_opt <- best_checkpoint$beta_type
  beta_type_log_opt <- best_checkpoint$beta_type_log
  
  print("============================================================")
  print("MEILLEUR CHECKPOINT UTILISÉ")
  print("============================================================")
  print(best_checkpoint$objective_value)
  print(best_checkpoint$incidence_sim)
  print(best_checkpoint$sse_by_type)
  
} else {
  
  beta_type_opt <- exp(fits[[best_fit_id]]$par)
  names(beta_type_opt) <- names(incidence_obs)
  
  beta_type_log_opt <- fits[[best_fit_id]]$par
}

beta_type_opt <- beta_type_opt[names(incidence_obs)]
beta_type_log_opt <- log(beta_type_opt)

beta_opt <- beta_type_opt[as.character(type_etab_calib)]

if (anyNA(beta_opt)) {
  stop("NA dans beta_opt : problème entre type_etab_calib et names(beta_type_opt)")
}

print("============================================================")
print("BETA OPTIMAL RETENU")
print("============================================================")
print(beta_type_opt)

###############################################################################
###### VALIDATION FINALE #####
###############################################################################

res_final <- parLapply(
  cl,
  X = rep_chunks_valid,
  fun = function(rs, beta_opt, seed_validation) {
    
    out <- lapply(rs, function(r) {
      run_simulation_summary(
        beta_vec = beta_opt,
        alpha = alpha,
        seed = seed_validation + r
      )
    })
    
    do.call(rbind, out)
  },
  beta_opt = beta_opt,
  seed_validation = seed_validation
)

inc_final_mat <- do.call(rbind, res_final)

inc_final <- colMeans(inc_final_mat, na.rm = TRUE)
inc_final <- inc_final[names(incidence_obs)]

inc_final_sd <- apply(inc_final_mat, 2, sd, na.rm = TRUE)
inc_final_sd <- inc_final_sd[names(incidence_obs)]

inc_final_se <- inc_final_sd / sqrt(n_rep_valid)
inc_final_se <- inc_final_se[names(incidence_obs)]

diff_final <- inc_final - incidence_obs
diff_final <- diff_final[names(incidence_obs)]

sse_final_by_type <- diff_final^2
sse_final_by_type <- sse_final_by_type[names(incidence_obs)]

sse_final <- sum(sse_final_by_type, na.rm = TRUE)

validation_summary <- data.frame(
  pathogen = pathogen,
  type = names(incidence_obs),
  beta = as.numeric(beta_type_opt[names(incidence_obs)]),
  incidence_obs = as.numeric(incidence_obs),
  incidence_sim_mean = as.numeric(inc_final),
  incidence_sim_sd = as.numeric(inc_final_sd),
  incidence_sim_se = as.numeric(inc_final_se),
  diff = as.numeric(diff_final),
  sse = as.numeric(sse_final_by_type),
  stringsAsFactors = FALSE
)

final_object <- list(
  datetime = Sys.time(),
  pathogen = pathogen,
  beta_type_opt = beta_type_opt,
  beta_type_log_opt = beta_type_log_opt,
  beta_type = beta_type_opt,
  beta_type_log = beta_type_log_opt,
  beta_opt = beta_opt,
  incidence_obs = incidence_obs,
  incidence_final = inc_final,
  incidence_final_sd = inc_final_sd,
  incidence_final_se = inc_final_se,
  diff_final = diff_final,
  sse_final_by_type = sse_final_by_type,
  sse_final = sse_final,
  n_rep_obj = n_rep_obj,
  n_rep_valid = n_rep_valid,
  n_cores = n_cores,
  seed_objective = seed_objective,
  seed_validation = seed_validation,
  seed_random_starts = seed_random_starts,
  lower_beta = lower_beta,
  upper_beta = upper_beta,
  n_random_starts = n_random_starts,
  maxit_nm = maxit_nm,
  fits = fits,
  fit_values = fit_values,
  best_fit_id = best_fit_id,
  validation_summary = validation_summary,
  checkpoint_dir = checkpoint_dir,
  checkpoint_best_file = checkpoint_best_file,
  checkpoint_last_file = checkpoint_last_file,
  history_file = history_file,
  starts_file = starts_file,
  fits_file = fits_file,
  final_file = final_file,
  beta_start = beta_start,
  warm_start_file = warm_start_file
)

safe_saveRDS(final_object, final_file)
safe_saveRDS(final_object, final_recovered_beta_file)

write.csv2(
  validation_summary,
  file = validation_summary_run_file,
  row.names = FALSE
)

write.csv2(
  validation_summary,
  file = final_validation_summary_file,
  row.names = FALSE
)

###############################################################################
###### OUTPUT #####
###############################################################################

print("============================================================")
print("PATHOGÈNE")
print("============================================================")
print(pathogen)

print("============================================================")
print("BETA INITIAL UTILISÉ")
print("============================================================")
print(beta_start)

print("============================================================")
print("BETA OPTIMAL")
print("============================================================")
print(beta_type_opt)

print("============================================================")
print("INCIDENCE SIMULÉE FINALE")
print("============================================================")
print(inc_final)

print("============================================================")
print("SD VALIDATION")
print("============================================================")
print(inc_final_sd)

print("============================================================")
print("SE VALIDATION")
print("============================================================")
print(inc_final_se)

print("============================================================")
print("INCIDENCE OBSERVÉE")
print("============================================================")
print(incidence_obs)

print("============================================================")
print("DIFFÉRENCE SIM - OBS")
print("============================================================")
print(diff_final)

print("============================================================")
print("SSE FINALE PAR TYPE")
print("============================================================")
print(sse_final_by_type)

print("============================================================")
print("SSE FINALE TOTALE")
print("============================================================")
print(sse_final)

print("============================================================")
print("TABLEAU VALIDATION")
print("============================================================")
print(validation_summary)

print("============================================================")
print("DOSSIER DE SORTIE")
print("============================================================")
print(checkpoint_dir)

print("============================================================")
print("FICHIERS SAUVEGARDÉS")
print("============================================================")
print(final_file)
print(final_recovered_beta_file)
print(final_recovered_beta_legacy_file)
print(validation_summary_run_file)
print(final_validation_summary_file)
