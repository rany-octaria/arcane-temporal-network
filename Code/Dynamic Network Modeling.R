#Dynamic Temporal Network Model Code

source(here("Code", "data-cleaning-network-model.R"))
# If one dataset already has finess_geo, rename() will fail.
# In that case, use this safer version instead:

# Start the Modeling Now!
#Setup
#1. Model assumption 1 : Full Occupancy. the number of discharges each day is the 
# same as the number of admissions


# -------------------------------
# Clean daily admissions dataset
# -------------------------------
adm_daily <- daily_admission %>%
  clean_names() %>%
  mutate(
    date = as.Date(date_entree),
    daily_admissions = as.integer(no_admissions)
  ) %>%
  select(finess_geo, date, daily_admissions)

# -------------------------------
# Clean weekly transfer dataset
# -------------------------------
#Getting the date start as the last date
transfers <- weekly %>%
  clean_names() %>%
  mutate(
    finess_geo_origin = as.character(finess_geo_origin),
    finess_geo_target = as.character(finess_geo_target),
    transfer_date = as.Date(window_end),
    weight = as.numeric(weight)
  ) 
# --------------------------------------------------------
# Draw integer LOS from a bounded normal-like distribution
# --------------------------------------------------------
sample_los <- function(n, mean_los = 7, sd_los = 2, min_los = 1, max_los = 15) {
  x <- round(rnorm(n, mean = mean_los, sd = sd_los))
  x <- pmax(min_los, pmin(max_los, x))
  as.integer(x)
}
#this will change according to the availability of the data


## INITIALIZING PATIENT POPULATION

# -----------------------------------------------------------
# Seed hospital = largest total outgoing transfers
# -----------------------------------------------------------
seed_hospital <- transfers %>%
  group_by(finess_geo_origin) %>%
  summarise(total_outgoing = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  slice_max(order_by = total_outgoing, n = 1, with_ties = FALSE) %>%
  pull(finess_geo_origin)

seed_hospital
# -----------------------------------------------------------
# Build hospital universe from all origins and targets
# then attach no_beds
# -----------------------------------------------------------
all_transfer_hospitals <- bind_rows(
  transfers %>% transmute(finess_geo = finess_geo_origin),
  transfers %>% transmute(finess_geo = finess_geo_target)
) %>%
  distinct()

hospitals <- all_transfer_hospitals %>%
  left_join(
    coords_beds_active %>%
      clean_names() %>%
      transmute(
        finess_geo = as.character(finess_geo),
        no_beds = readr::parse_number(as.character(no_beds))
      ) %>%
      distinct(),
    by = "finess_geo"
  ) %>%
  mutate(
    mean_beds = round(mean(no_beds, na.rm = TRUE)),
    no_beds = if_else(is.na(no_beds), mean_beds, no_beds),
    no_beds = as.integer(round(no_beds))
  ) %>%
  select(finess_geo, no_beds)

# -------------------------------------------------------------------
# Create one occupied bed = one patient at time 0
# Seed infection into one hospital
# -------------------------------------------------------------------
# -----------------------------------------------------------
# Initialize hospital-level state
# -----------------------------------------------------------
initialize_hospital_state <- function(hospitals,
                                      seed_hospital,
                                      n_seed_infected = 1) {
  
  hospitals %>%
    mutate(
      n_infected = if_else(
        finess_geo == seed_hospital,
        pmin(n_seed_infected, no_beds),
        0L
      ),
      prevalence = n_infected / no_beds
    )
}

# -----------------------------------------------------------
# For one day of transfers:
# infected transferred on each edge = round(weight * prevalence_origin)
#
# Then cap total infected outgoing from each hospital so we do not
# transfer out more infected than currently exist in that hospital.
# -----------------------------------------------------------
compute_daily_infected_transfers <- function(state, transfers_day) {
  
  if (nrow(transfers_day) == 0) {
    return(tibble(
      finess_geo_origin = character(),
      finess_geo_target = character(),
      weight = numeric(),
      prev_origin = numeric(),
      infected_transfer = integer()
    ))
  }
  
  # attach origin prevalence and infected count
  transfers_aug <- transfers_day %>%
    left_join(
      state %>%
        select(
          finess_geo,
          no_beds,
          n_infected,
          prevalence
        ),
      by = c("finess_geo_origin" = "finess_geo")
    ) %>%
    rename(
      n_infected_origin = n_infected,
      prev_origin = prevalence
    ) %>%
    mutate(
      prev_origin = replace_na(prev_origin, 0),
      n_infected_origin = replace_na(n_infected_origin, 0L),
      infected_transfer_raw = round(weight * prev_origin),
      infected_transfer_raw = pmax(0, infected_transfer_raw),
      infected_transfer_raw = pmin(infected_transfer_raw, weight)
    )
  
  # cap total outgoing infected at n_infected_origin
  transfers_capped <- transfers_aug %>%
    group_by(finess_geo_origin) %>%
    group_modify(~{
      dat <- .x
      
      infected_available <- dat$n_infected_origin[1]
      proposed_total <- sum(dat$infected_transfer_raw, na.rm = TRUE)
      
      # if proposal is already feasible, keep it
      if (proposed_total <= infected_available) {
        return(
          dat %>%
            mutate(infected_transfer = as.integer(infected_transfer_raw))
        )
      }
      
      # otherwise scale down proportionally
      if (proposed_total == 0 || infected_available == 0) {
        return(
          dat %>%
            mutate(infected_transfer = 0L)
        )
      }
      
      scaled <- dat$infected_transfer_raw * infected_available / proposed_total
      base_alloc <- floor(scaled)
      remainder <- scaled - base_alloc
      
      left_to_assign <- infected_available - sum(base_alloc)
      
      if (left_to_assign > 0) {
        add_one_idx <- order(remainder, decreasing = TRUE)[seq_len(left_to_assign)]
        base_alloc[add_one_idx] <- base_alloc[add_one_idx] + 1L
      }
      
      dat %>%
        mutate(
          infected_transfer = as.integer(base_alloc)
        )
    }) %>%
    ungroup() %>%
    select(
      finess_geo_origin,
      finess_geo_target,
      weight,
      prev_origin,
      infected_transfer
    )
  
  transfers_capped
}

# -----------------------------------------------------------
# Simulate one day at hospital level
# -----------------------------------------------------------
simulate_one_day_agg <- function(state,
                                 transfers,
                                 current_date,
                                 beta_within = 0.05,
                                 gamma_clear = 1 / 387,
                                 admission_prev = 0) {
  
  # ---------------------------------------------------------
  # A. Transfers occurring today
  # ---------------------------------------------------------
  transfers_day <- transfers %>%
    filter(transfer_date == current_date) %>%
    select(finess_geo_origin, finess_geo_target, weight)
  
  infected_transfers <- compute_daily_infected_transfers(
    state = state,
    transfers_day = transfers_day
  )
  
  # total infected leaving each origin
  infected_out <- infected_transfers %>%
    group_by(finess_geo_origin) %>%
    summarise(
      infected_out = sum(infected_transfer, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(finess_geo = finess_geo_origin)
  
  # total infected arriving at each destination
  infected_in <- infected_transfers %>%
    group_by(finess_geo_target) %>%
    summarise(
      infected_in = sum(infected_transfer, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(finess_geo = finess_geo_target)
  
  # ---------------------------------------------------------
  # B. Update hospital infected counts after transfers
  #
  # infected patients leaving origin are replaced by susceptible
  # community admissions, since admission_prev defaults to 0.
  #
  # infected patients arriving at destination replace susceptibles.
  # ---------------------------------------------------------
  state <- state %>%
    left_join(infected_out, by = "finess_geo") %>%
    left_join(infected_in, by = "finess_geo") %>%
    mutate(
      infected_out = replace_na(infected_out, 0L),
      infected_in = replace_na(infected_in, 0L),
      
      # if admission_prev > 0, some replacements at origin could be infected
      infected_replaced_from_community = rbinom(
        n(),
        size = infected_out,
        prob = admission_prev
      ),
      
      n_infected = n_infected - infected_out + infected_in + infected_replaced_from_community,
      n_infected = pmax(0L, pmin(n_infected, no_beds)),
      prevalence = n_infected / no_beds
    )
  
  # ---------------------------------------------------------
  # C. Clearance
  # ---------------------------------------------------------
  state <- state %>%
    mutate(
      n_cleared = rbinom(n(), size = n_infected, prob = gamma_clear),
      n_infected_after_clear = n_infected - n_cleared
    )
  
  # ---------------------------------------------------------
  # D. Within-hospital SIS transmission
  #
  # susceptible -> infected with:
  # 1 - exp(-beta * I / N)
  # ---------------------------------------------------------
  state <- state %>%
    mutate(
      n_susceptible = no_beds - n_infected_after_clear,
      p_infection = 1 - exp(-beta_within * (n_infected_after_clear / no_beds)),
      n_new_infected = rbinom(n(), size = n_susceptible, prob = p_infection),
      n_infected = n_infected_after_clear + n_new_infected,
      n_infected = pmax(0L, pmin(n_infected, no_beds)),
      prevalence = n_infected / no_beds
    ) %>%
    select(finess_geo, no_beds, n_infected, prevalence)
  
  # ---------------------------------------------------------
  # E. Summaries
  # ---------------------------------------------------------
  daily_summary <- state %>%
    transmute(
      date = current_date,
      finess_geo,
      no_beds,
      n_infected,
      prevalence
    )
  
  overall_summary <- state %>%
    summarise(
      date = current_date,
      total_patients = sum(no_beds),
      total_infected = sum(n_infected),
      overall_prevalence = total_infected / total_patients,
      n_hospitals_with_case = sum(n_infected > 0)
    )
  
  list(
    state = state,
    daily_summary = daily_summary,
    overall_summary = overall_summary,
    infected_transfers = infected_transfers
  )
}

# -----------------------------------------------------------
# Run the simplified aggregate SIS model
# -----------------------------------------------------------
run_sis_simulation_agg <- function(hospitals,
                                   transfers,
                                   start_date,
                                   end_date,
                                   seed_hospital = NULL,
                                   n_seed_infected = 1,
                                   beta_within = 0.05,
                                   gamma_clear = 1 / 387,
                                   admission_prev = 0,
                                   seed = 123) {
  
  set.seed(seed)
  
  # default seed hospital = largest outgoing transfers
  if (is.null(seed_hospital)) {
    seed_hospital <- transfers %>%
      group_by(finess_geo_origin) %>%
      summarise(total_outgoing = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      slice_max(order_by = total_outgoing, n = 1, with_ties = FALSE) %>%
      pull(finess_geo_origin)
  }
  
  state <- initialize_hospital_state(
    hospitals = hospitals,
    seed_hospital = seed_hospital,
    n_seed_infected = n_seed_infected
  )
  
  sim_dates <- seq.Date(
    from = as.Date(start_date),
    to = as.Date(end_date),
    by = "day"
  )
  
  daily_results <- vector("list", length(sim_dates))
  overall_results <- vector("list", length(sim_dates))
  transfer_results <- vector("list", length(sim_dates))
  
  for (i in seq_along(sim_dates)) {
    current_date <- sim_dates[i]
    
    out <- simulate_one_day_agg(
      state = state,
      transfers = transfers,
      current_date = current_date,
      beta_within = beta_within,
      gamma_clear = gamma_clear,
      admission_prev = admission_prev
    )
    
    state <- out$state
    daily_results[[i]] <- out$daily_summary
    overall_results[[i]] <- out$overall_summary
    transfer_results[[i]] <- out$infected_transfers %>%
      mutate(date = current_date)
  }
  
  list(
    seed_hospital = seed_hospital,
    state_final = state,
    daily_results = bind_rows(daily_results),
    overall_results = bind_rows(overall_results),
    infected_transfer_results = bind_rows(transfer_results)
  )
}

sim_out <- run_sis_simulation_agg(
  hospitals = hospitals,
  transfers = transfers,
  start_date = "2024-01-07",
  end_date   = "2025-01-05",
  seed_hospital = NULL,      # automatically uses largest outgoing hospital
  n_seed_infected = 5,
  beta_within = 0.20,
  gamma_clear = 1 / 387,
  admission_prev = 0,
  seed = 101
)

sim_out$seed_hospital




head(sim_out$hospitals)
head(sim_out$daily_results)
head(sim_out$overall_results)
head(sim_out$patients_final)

view(sim_out$daily_results)
view(sim_out$overall_results)


sim_out$overall_results %>%
  ggplot(aes(x = date, y = total_infected)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  labs(
    title = "Total number of infected patients over 365 days",
    x = "Date",
    y = "Number of infected patients"
  ) +
  theme_minimal()


hospitals_with_cases <- sim_out$daily_results %>%
  group_by(date) %>%
  summarise(
    n_hospitals_with_case = sum(n_infected > 0, na.rm = TRUE),
    .groups = "drop"
  )


#Plotting the number of infected hospitals
hospitals_with_cases %>%
  ggplot(aes(x = date, y = n_hospitals_with_case)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Number of hospitals with at least one infected patient over time",
    x = "Date",
    y = "Hospitals with at least one case"
  ) +
  theme_minimal()



#Write the saved runs
todaysdate = as.character(today())
write.csv(sim_out$daily_results, file = here("Outputs", "Saved Results", 
                                             paste0("daily_initial_run_", todaysdate,".csv")))
write.csv(sim_out$overall_results, file = here("Outputs", "Saved Results", 
                                             paste0("overall_initial_run_", todaysdate,".csv")))



#With this simplification, all patients in the same hospital turn over on the 
# same LOS clock unless they entered on different days. 
# That is acceptable for a first version, but it may 
# create somewhat artificial departure patterns compared with individual-level LOS draws.
# 
# The next thing I would refine is the departure mechanism,
# because right now it still forces departures to equal dail
# even when LOS-ready patients are fewer.