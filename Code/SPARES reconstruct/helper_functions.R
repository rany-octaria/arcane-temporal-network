

# Get the number of repeated events per patient
getRepeatedEvents = function(nosocomial, idsite) {
  diff_site_same_origin = 0
  same_site_diff_origin = 0
  diff_site_diff_origin = 0
  
  tab_rep = table(nosocomial, idsite)
  for (r in 2:length(nosocomial)) {
    if (idsite[1] != idsite[r] & nosocomial[1] == nosocomial[r]) diff_site_same_origin = diff_site_same_origin+1
    if (idsite[1] != idsite[r] & nosocomial[1] != nosocomial[r]) diff_site_diff_origin = diff_site_diff_origin+1
    if (idsite[1] == idsite[r] & nosocomial[1] != nosocomial[r]) same_site_diff_origin = same_site_diff_origin+1
  }
  
  out = tibble(
    diff_site_same_origin = diff_site_same_origin,
    same_site_diff_origin = same_site_diff_origin,
    diff_site_diff_origin = diff_site_diff_origin,
    n_samples = length(nosocomial)
  )
  
  return(out)
}


# Get the number of repeated events per patient
getConsecutiveSamples = function(samp_date, idetab) {
  diff_date_same_etab = 0
  same_date_diff_etab = 0
  diff_date_diff_etab = 0
  
  tab_rep = table(samp_date, idetab)
  for (r in 1:(length(idetab)-1)) {
    if (samp_date[r] != samp_date[r+1] & idetab[r] == idetab[r+1]) diff_date_same_etab = diff_date_same_etab+1
    if (samp_date[r] != samp_date[r+1] & idetab[r] != idetab[r+1]) diff_date_diff_etab = diff_date_diff_etab+1
    if (samp_date[r] == samp_date[r+1] & idetab[r] != idetab[r+1]) same_date_diff_etab = same_date_diff_etab+1
  }
  
  out = tibble(
    diff_date_same_etab = diff_date_same_etab,
    diff_date_diff_etab = diff_date_diff_etab,
    same_date_diff_etab = same_date_diff_etab,
    n_samples = length(samp_date)
  )
  return(out)
}



# Get the number of repeated samples with different nosocomial 
# status 
getSamplesWithDifferentOnset = function(samp_date, nosocomial) {
  tab_rep = table(samp_date, nosocomial)
  
  out = tibble(
    n_0 = ifelse(is.na(colSums(tab_rep)["0"]), 0, colSums(tab_rep)["0"]),
    n_1 = ifelse(is.na(colSums(tab_rep)["1"]), 0, colSums(tab_rep)["1"]),
    n_9 = ifelse(is.na(colSums(tab_rep)["9"]), 0, colSums(tab_rep)["9"])
    )
  return(out)
}


# Transform yearly and quaterly data to monthly data 
transform_to_monthly = function(df, startDate = "2019-01-01", endDate = "2020-06-01") {
  df_temp = df %>%
    arrange(Date) %>%
    mutate(Date = as.Date(Date)) %>%
    as.data.table(.)
  
  df_temp = df_temp[.(seq(as.IDate(startDate), as.IDate(endDate), by="month")),
                    on=.(Date), roll=T]
  
  return(as.data.frame(df_temp))
}

# Transform yearly and quaterly data to weekly data 
transform_to_weekly= function(df, startDate = "2019-01-01", endDate = "2020-06-30") {
  df_temp = df %>%
    arrange(Date) %>%
    mutate(Date = as.Date(Date)) %>%
    as.data.table(.)
  
  df_temp = df_temp[.(seq(as.IDate(startDate), as.IDate(endDate), by="week")),
                    on=.(Date), roll=T]
  
  out = as.data.frame(df_temp) %>%
    #group_by(Date) %>%
    mutate(#vol = vol/ n(), 
           Date = floor_date(as.Date(Date), "weeks", week_start = 1))
  return(out)
}


# Select unique outcome when multiple resistance profiles for 
# the same isolate is found 
selectUniqueOutcome = function(result_vec) {
  out = "problem"
  
  if (all(result_vec %in% c("S", "R", "I"))) {
    if ("R" %in% result_vec) out = result_vec[result_vec != "R"]
    else if ("I" %in% result_vec) out = result_vec[result_vec != "I"]
  } 
  
  if (all(result_vec %in% c("O", "N"))) {
    if ("O" %in% result_vec) out = result_vec[result_vec != "O"]
  }
  
  if (any(c("R", "S", "I") %in% result_vec) & any(c("O", "N") %in% result_vec)) out = NULL
  
  return(out) 
}


# Longitudinal data 
getLongitudinalData = function(df) {
  
  consecutive_days = sort(unique(df$Date))
  
  if (length(consecutive_days) == 1) {
    return(NULL)
    
  } else {
    # Keep consecutive days separated by less than 30 days 
    nDays = difftime(consecutive_days, lag(consecutive_days), units = "days")
    days_to_keep = mapply(function(x, y) {
     if(y <= 30) {
       consecutive_days[c(x-1,x)]
     }else {
       NULL
     } 
    },
    2:length(consecutive_days), 
    as.numeric(nDays)[-1], 
    SIMPLIFY = FALSE
    )
    
    # Keep first occurrence if less than 2 variations in antibiotic profile
    diags_to_remove = c()
    for (d in days_to_keep) {
      if (!is.null(d)) {
        rename_date = c("d1", "d2")
        names(rename_date) = d
        
        tot_diff = df %>% 
          filter(Date %in% d) %>%
          mutate(Date = recode(as.character(Date), !!!rename_date)) %>%
          pivot_wider(names_from = Date, values_from = Resultat) %>%
          filter(!is.na(d1), !is.na(d2)) %>%
          summarise(tot_differences = sum(d1!=2)) %>%
          .$tot_differences
        
        if (tot_diff <= 2) diags_to_remove = c(diags_to_remove, d[2]) 
      }
    }
    
    if (is.null(diags_to_remove)) {
      return(NULL)
    } else {
     return(filter(df, Date %in% diags_to_remove)) 
    }
  }
}


# Calculate lagged prevalence 
laggedCovariable = function(df, dateVar, covar) {
  out = df %>% 
    arrange( !!sym(dateVar) ) %>% 
    mutate(lagged = lag( !!sym(covar) )) %>% 
    mutate(lagged = ifelse(is.na(lagged), 0, lagged)) %>%
    .$lagged
  
  return(out)
}


# Test over dispersion in GLMM model
# Function by Bockel et al., 2011 
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ#overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  out = c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
  return(out)
}



# Merge phenotype profiles when multiple isolates were
# collected within the same individual on the same day
# and these phenotypes are identical for the antibiotics 
# in common
sample_fusion = function(d) {
  nlignes = length(unique(d$ligne))
  if (nlignes < 1) stop("Error in the number of lignes")
  
  if (nlignes == 1) {
    return(d)
  } else {
    
    ligne_ids = sort(unique(d$ligne)) 
    out = data.frame()
    
    # Keep phenotypes that are tested only once
    tested_once = d %>%
      group_by(molecule, molecule_class) %>%
      mutate(n = n()) %>%
      filter(n == 1) %>%
      select(-n) %>%
      mutate(ligne = ligne_ids[1])
    
    # Keep most "severe" phenotypes when tested multiple times 
    tested_multiple_times = d %>%
      group_by(molecule, molecule_class) %>%
      mutate(
        n = length(unique(Resultat)),
        Resultat = case_when(
          "N" %in% Resultat & !"O" %in% Resultat ~ "N",
          "N" %in% Resultat & "O" %in% Resultat ~ "O",
          !"N" %in% Resultat & "O" %in% Resultat ~ "O",
          "S" %in% Resultat & !"I" %in% Resultat & !"R" %in% Resultat ~ "S",
          "S" %in% Resultat & "I" %in% Resultat & !"R" %in% Resultat ~ "I",
          "S" %in% Resultat & "I" %in% Resultat & "R" %in% Resultat ~ "R",
          "S" %in% Resultat & !"I" %in% Resultat & "R" %in% Resultat ~ "R",
          !"S" %in% Resultat & "I" %in% Resultat & !"R" %in% Resultat ~ "I",
          !"S" %in% Resultat & "I" %in% Resultat & "R" %in% Resultat ~ "R",
          !"S" %in% Resultat & !"I" %in% Resultat & "R" %in% Resultat ~ "R"
        )
      ) %>%
      select(-c(n, ligne)) %>%
      distinct() %>%
      mutate(ligne = ligne_ids[1])
    
    out = bind_rows(tested_once, tested_multiple_times)
    
    return(out)
  }
} 

# Exclude isolates that are collected less than 30 days 
# away and with less than 2 differences 
sample_exclusion = function(d) {
  ndates = length(unique(d$prelevement))
  if (ndates < 1) stop("Error in the number of sampling dates")
  
  if (ndates == 1) {
    return(d)
    
  } else {
    # All dates
    all_dates = format(sort(unique(as.Date(d$prelevement, "%d/%m/%Y"))), "%d/%m/%Y")
    all_time_diff = as.numeric(difftime(as.Date(all_dates,"%d/%m/%Y"), as.Date(lag(all_dates), "%d/%m/%Y"), units = "days"))
    all_time_diff[is.na(all_time_diff)] = 0
    time_diff_recode = setNames(all_time_diff, all_dates)
    
    # Isolates from different days
    d %>%
      mutate(
        date = as.Date(prelevement, "%d/%m/%Y"),
        time_diff = recode(prelevement, !!!time_diff_recode)
      ) %>%
      arrange(date) %>%
      group_by(molecule_class, molecule) %>%
      mutate(
        Resultat_diff = case_when(
          lag(Resultat) == Resultat ~ 0,
          lag(Resultat) == "S" & Resultat == "I" ~ 1,
          lag(Resultat) == "I" & Resultat == "R" ~ 1,
          lag(Resultat) == "R" & Resultat == "I" ~ 1,
          lag(Resultat) == "I" & Resultat == "S" ~ 1,
          lag(Resultat) == "O" & Resultat == "N" ~ 2,
          lag(Resultat) == "N" & Resultat == "O" ~ 2,
          lag(Resultat) == "R" & Resultat == "S" ~ 2,
          lag(Resultat) == "S" & Resultat == "R" ~ 2,
          .default = NA
        )
      ) %>%
      ungroup() %>%
      group_by(prelevement, secteur, ligne) %>%
      mutate(to_keep = case_when(
        sum(Resultat_diff %in% 1, na.rm = T) <= 2 & sum(Resultat_diff %in% 2, na.rm = T) == 0 & unique(time_diff) <= 30 ~ 0,
        .default = 1
      )) %>%
      filter(to_keep > 0) %>%
      select(prelevement, ligne, Resultat, molecule, molecule_class, secteur)
  }
}


# Identification of samples to exclude because they are part of 
# sequence of highly similar samples 
sample_exclusion_identification = function(d) {
  ndates = length(unique(d$prelevement))
  if (ndates < 1) stop("Error in the number of sampling dates")
    
  if (ndates == 1) {
    return(d)
    
  } else {
    # All dates
    all_dates = format(sort(unique(as.Date(d$prelevement, "%d/%m/%Y"))), "%d/%m/%Y")
    all_time_diff = as.numeric(difftime(as.Date(all_dates,"%d/%m/%Y"), as.Date(lag(all_dates), "%d/%m/%Y"), units = "days"))
    all_time_diff[is.na(all_time_diff)] = 0
    time_diff_recode = setNames(all_time_diff, all_dates)
    
    # Isolates from different days
    d %>%
      mutate(
        date = as.Date(prelevement, "%d/%m/%Y"),
        time_diff = recode(prelevement, !!!time_diff_recode)
      ) %>%
      arrange(date) %>%
      group_by(molecule_class, molecule) %>%
      mutate(
        Resultat_diff = case_when(
          lag(Resultat) == Resultat ~ 0,
          lag(Resultat) == "S" & Resultat == "I" ~ 1,
          lag(Resultat) == "I" & Resultat == "R" ~ 1,
          lag(Resultat) == "R" & Resultat == "I" ~ 1,
          lag(Resultat) == "I" & Resultat == "S" ~ 1,
          lag(Resultat) == "O" & Resultat == "N" ~ 2,
          lag(Resultat) == "N" & Resultat == "O" ~ 2,
          lag(Resultat) == "R" & Resultat == "S" ~ 2,
          lag(Resultat) == "S" & Resultat == "R" ~ 2,
          .default = NA
        )
      ) %>%
      ungroup() %>%
      group_by(prelevement, secteur, ligne) %>%
      mutate(excluded = case_when(
        sum(Resultat_diff %in% 1, na.rm = T) <= 2 & sum(Resultat_diff %in% 2, na.rm = T) == 0 & unique(time_diff) <= 30 ~ 1,
        .default = 0
      ))
  }
}


# Identification of samples to exclude because they are part of 
# sequence of samples with less than 2 phenotypic differences
selection_30days_sequence = function(df) {
  dates_seq = sort(unique(df$prelevement))
  nmax = length(dates_seq)
  
  to_keep = dates_seq[1]
  for (i in 1:(nmax-1)) {
    if (i > 1 & !dates_seq[i] %in% to_keep) {
      dates_seq = dates_seq[-i]
      nmax = length(dates_seq)  
      i = i-1
    }
    
    diff_results = df %>%
      filter(prelevement %in% dates_seq[i:(i+1)]) %>%
      arrange(molecule, prelevement) %>%
      group_by(molecule) %>%
      filter(n() == 2) %>%
      mutate(resultat_diff = case_when(
        Resultat == lag(Resultat) ~ 0,
        Resultat != lag(Resultat) & ( 
          (Resultat == "S" & lag(Resultat) == "I") |
            (Resultat == "I" & lag(Resultat) == "R") | 
            (Resultat == "R" & lag(Resultat) == "I") |
            (Resultat == "I" & lag(Resultat) == "S")
        ) ~ 1,
        Resultat != lag(Resultat) & ( 
          (Resultat == "S" & lag(Resultat) == "R") |
            (Resultat == "R" & lag(Resultat) == "S") | 
            (Resultat == "O" & lag(Resultat) == "N") |
            (Resultat == "N" & lag(Resultat) == "O")
        ) ~ 2,
        .default = NA
      )
      ) %>%
      filter(!is.na(resultat_diff))
    
    if (sum(diff_results$resultat_diff == 1) > 2 | any(diff_results$resultat_diff == 2)) 
      to_keep = c(to_keep, dates_seq[i+1])
  }
  
  to_remove = dates_seq[!dates_seq %in% to_keep]
  df %>%
    dplyr::select(ligne, prelevement) %>%
    filter(prelevement %in% to_remove) %>%
    distinct()
}

# Get percentage estimate and its 95% CI from a data frame 
# with one row 
getBinomCI = function(d, sides, method) {
  out = BinomCI(d$n_res, d$n_tot, sides = sides, method = method)
  out = data.frame(out)
  colnames(out) = c("res_rate", "res_rate_lwr", "res_rate_upr")
  if (d$n_tot == 0) out = data.frame(res_rate = 0, res_rate_lwr = 0, res_rate_upr = 0)
  return(out)
}

# Function to plot antibiotic consumption per year and region
atbplot = function(df, atbclass, level = "regional", facet_type = "geographic") {
  
  # Color gradient of the significance level for the Friedman test
  colfunc <- colorRampPalette(c("floralwhite", "deepskyblue4"))
  
  if (level == "regional") {
    # Consumption per antibiotic class
    df_final = df %>%
      group_by(code, Date_year, region) %>%
      summarise(consumption = sum(molDDD)/unique(Nbhosp)*1000, .groups = "drop")
    m = max(df_final$consumption)
    
    # P value of Friedman test
    pvals = df_final %>%
      group_by(region) %>%
      friedman_test(consumption ~ Date_year | code) %>%
      mutate(p_sign = factor(case_when(
        p > 0.05 ~ "n.s.",
        p <= 0.05 & p > 0.01 ~ "*",
        p <= 0.01 & p > 0.001 ~ "**",
        p <= 0.001 & p > 0.0001 ~ "***",
        p <= 0.0001 ~ "****"
      ), 
      levels = c("n.s.", "*", "**", "***", "****"),
      labels = c("n.s.", "*", "**", "***", "****")
      ))
    
    # Mean consumption per region and year
    df_mean = df_final %>%
      group_by(Date_year, region) %>%
      summarise(consumption = mean(consumption), .groups = "drop") %>%
      left_join(., pvals %>% dplyr::select(region, p_sign), by = "region")
    
    # Significance levels of multiple comparison tests
    df_comp = df_final %>%
      arrange(code, Date_year) %>%
      group_by(region) %>%
      wilcox_test(
        consumption ~ Date_year, 
        paired = T, 
        p.adjust.method = "BH", 
        alternative = "two.sided",
        conf.level = 1-0.05,
        detailed = T
      ) %>%
      mutate(group1 = factor(group1), group2 = factor(group2)) %>%
      filter(p.adj <= 0.05, group1 == "2019") %>%
      mutate(y = case_when(group2 == "2020" ~ m/2, group2 == "2021" ~ m*2/3, group2 == "2022" ~ m*5/6)) %>%
      mutate(p.adj = ifelse(p.adj<0.0001, "<0.0001", format(round(p.adj, 4), nsmall = 4)))
    
    # Plot
    out = ggplot(df_final, aes(x = as.factor(Date_year))) +
      geom_line(alpha = 0.1, aes(y = consumption, group = code)) +
      geom_point(data = df_mean, aes(x = as.factor(Date_year), y = consumption, fill = p_sign), 
                 col = "black", pch=21, size = 3, stroke = 0.2) +
      geom_signif(data = df_comp, aes(xmin = group1, xmax = group2, annotations = p.adj,
                                      y_position = y),
                  manual = T, textsize = 3, vjust = -0.2) +
      theme_bw() +
      scale_fill_manual(
        limits = c("n.s.", "*", "**", "***", "****"),
        values = colfunc(5),
        name = "Friedman test p-value") +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title = element_text(hjust=0.5)) +
      guides(fill = guide_legend(title.position = "top")) +
      labs(x = "", y = "Annual antibiotic consumption\n(DDD/1,000 bed days)")
    
    if (facet_type == "geographic") out = out + facet_geo(~ region, grid = my_regional_grid)
    if (facet_type == "alphabetical") out = out + facet_wrap(facets = vars(region), ncol = 4)
    return(out)
  }
  
  if (level == "type") {
    # Consumption per antibiotic class
    df = df %>%
      mutate(consumption = molDDD/Nbhosp*1000)
    df_max = df %>%
      group_by(type) %>%
      summarise(m = max(consumption), .groups = "drop")
    
    # P value of Friedman test
    pvals = df %>%
      group_by(type) %>%
      friedman_test(consumption ~ Date_year | code) %>%
      mutate(p_sign = factor(case_when(
        p > 0.05 ~ "n.s.",
        p <= 0.05 & p > 0.01 ~ "*",
        p <= 0.01 & p > 0.001 ~ "**",
        p <= 0.001 & p > 0.0001 ~ "***",
        p <= 0.0001 ~ "****"
      ), 
      levels = c("n.s.", "*", "**", "***", "****"),
      labels = c("n.s.", "*", "**", "***", "****")
      ))
    
    # Mean consumption per type and year
    df_mean = df %>%
      group_by(Date_year, type) %>%
      summarise(consumption = mean(consumption), .groups = "drop") %>%
      left_join(., pvals %>% dplyr::select(type, p_sign), by = "type")
    
    # Significance levels of multiple comparison tests
    df_comp = df %>%
      arrange(code, Date_year) %>%
      group_by(type) %>%
      wilcox_test(
        consumption ~ Date_year, 
        paired = T, 
        p.adjust.method = "BH", 
        alternative = "two.sided",
        conf.level = 1-0.05,
        detailed = T
      ) %>%
      mutate(group1 = factor(group1), group2 = factor(group2)) %>%
      filter(p.adj <= 0.05, group1 == "2019") %>%
      left_join(., df_max, by = "type") %>%
      mutate(y = case_when(group2 == "2020" ~ m/2, group2 == "2021" ~ m*2/3, group2 == "2022" ~ m*5/6))
    
    # Plot
    out = ggplot(df, aes(x = as.factor(Date_year))) +
      geom_line(alpha = 0.1, aes(y = consumption, group = code)) +
      geom_point(data = df_mean, aes(x = as.factor(Date_year), y = consumption, fill = p_sign), 
                 col = "black", pch=21, size = 2, stroke = 0.2) +
      geom_signif(data = df_comp, aes(xmin = group1, xmax = group2, annotations = format(round(p.adj, 4), nsmall = 4),
                                      y_position = y),
                  manual = T, textsize = 3, vjust = -0.2) +
      facet_wrap(facets = vars(type), ncol = 3, scales = "free_y") +
      theme_bw() +
      scale_fill_manual(
        limits = c("n.s.", "*", "**", "***", "****"),
        values = colfunc(5),
        name = "Friedman test p-value") +
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(hjust = 0.5), legend.position = "bottom") +
      guides(col = guide_legend(title.position = "top")) +
      labs(x = "", y = "Annual antibiotic consumption\n(DDD/1,000 bed days)", 
           title = atbclass)
    return(out)
  }
  
  if (level == "national") {
    df_final = df %>%
      mutate(consumption = molDDD / Nbhosp * 1000) %>%
      arrange(code, Date_year) %>%
      group_by(code) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      filter(n == 4)
    m = max(df$consumption)
    
    df_mean = df %>%
      group_by(Date_year) %>%
      summarise(consumption = mean(consumption), .groups = "drop")
    
    df_comp = df %>%
      arrange(code, Date_year) %>%
      wilcox_test(
        consumption ~ Date_year, 
        paired = T, 
        p.adjust.method = "BH", 
        alternative = "two.sided",
        conf.level = 1-0.05,
        detailed = T
      ) %>%
      mutate(group1 = factor(group1), group2 = factor(group2)) %>%
      filter(p.adj <= 0.05, group1 == "2019") %>%
      mutate(y = case_when(group2 == "2020" ~ m/2, group2 == "2021" ~ m*2/3, group2 == "2022" ~ m*5/6))
    
    out = ggplot() +
      geom_line(data = df_final, aes(x = factor(Date_year), y = consumption, group = code), alpha = 0.1) +
      geom_point(data = df_mean, aes(x = factor(Date_year), y = consumption), col = "red") +
      geom_signif(data = df_comp, aes(xmin = group1, xmax = group2, annotations = p.adj, y_position = y), 
                  manual = T) +
      theme_bw() +
      labs(x = "", y = "Annual antibiotic consumption\n(DDD/1,000 hospitalisation days)", 
           title = atb_class)
    return(out)
  }
}

# Get color for the corresponding level of anti-COVID-19 intervention
col_interventions <- function(i) { 
  paste0(colorRampPalette(c("steelblue4", "ivory"))(4)[i], "99")
}



# Function to get the p value of the Friedman test 
# for the comparison of multiple distributions with 
# repeated measures
atbpval = function(df, level = "regional") {
  
  if (level == "regional") {
    # Consumption per antibiotic class
    out = df %>%
      group_by(code, Date_year, region) %>%
      summarise(consumption = sum(molDDD)/unique(Nbhosp)*1000, .groups = "drop") %>%
      group_by(region) %>%
      friedman_test(consumption ~ Date_year | code) %>%
      mutate(p_friedman = round(p, 5)) %>%
      dplyr::select(region, p_friedman)   
    return(out)
  }
  
  if (level == "national") {
    # Consumption per antibiotic class
    out = df %>%
      mutate(consumption = molDDD/Nbhosp*1000) %>%
      arrange(code, Date_year) %>%
      group_by(code) %>%
      mutate(n=n()) %>%
      ungroup() %>%
      filter(n ==4) %>%
      friedman_test(consumption ~ Date_year | code) %>%
      mutate(p_friedman = round(p, 5)) %>%
      .$p_friedman    
    return(out)
  }
  
  if (level == "type") {
    # Consumption per antibiotic class
    out = df %>%
      mutate(consumption = molDDD/Nbhosp*1000) %>%
      arrange(code, Date_year, type) %>%
      group_by(type) %>%
      friedman_test(consumption ~ Date_year | code) %>%
      mutate(p_friedman = round(p, 5)) %>%
      dplyr::select(type, p_friedman)    
    return(out)
  }
}



# Function to get the p value of the Wilcoxon 
# multiple comparison tests
atbmulcomp = function(df, adjustment = "BH", cohort = T) {
  
  out = NA
  if (adjustment == "BH") adjustment_conf_level = 0.95
  if (adjustment == "bonferroni") adjustment_conf_level = 1-0.05/6
  
  if (cohort) {
    out = df %>%
      mutate(consumption = molDDD/Nbhosp*1000) %>%
      group_by(code) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      filter(n == 4) %>%
      arrange(code, Date_year) %>%
      wilcox_test(
        consumption ~ Date_year, 
        paired = T, 
        p.adjust.method = adjustment,
        comparisons = list(c("2019", "2020"), c("2019", "2021"), c("2019", "2022")),
        alternative = "two.sided",
        conf.level = adjustment_conf_level,
        detailed = T, 
        exact = T
      ) %>%
      dplyr::select(group1, group2, p.adj, estimate, conf.low, conf.high)
  }
  
  if (!cohort) {
    out = df %>%
      mutate(consumption = molDDD/Nbhosp*1000) %>%
      arrange(code, Date_year) %>%
      wilcox_test(
        consumption ~ Date_year, 
        paired = F, 
        p.adjust.method = adjustment,
        comparisons = list(c("2019", "2020"), c("2019", "2021"), c("2019", "2022")),
        alternative = "two.sided",
        conf.level = adjustment_conf_level,
        detailed = T, 
        exact = T
      ) %>%
      dplyr::select(group1, group2, p.adj, estimate, conf.low, conf.high)
  }
  
  return(out)
  
}

# Function to get the p value of the Wilcoxon 
# multiple comparison tests
atbunicomp = function(df) {
  df_final = df %>%
    mutate(consumption = molDDD/Nbhosp*1000) %>%
    filter(Date_year %in% c(2019,2020)) 
  
  change_percentage = df_final %>% 
    dplyr::select(code, Date_year, consumption) %>%
    pivot_wider(names_from = Date_year, values_from = consumption) %>%
    filter(`2019` > 0) %>%
    summarise(cp = median((`2020`-`2019`)/`2019`)) %>%
    .$cp
  
  df_final %>%
    arrange(code, Date_year) %>%
    wilcox_test(
        consumption ~ Date_year, 
        paired = T, 
        alternative = "two.sided",
        conf.level = 0.95,
        detailed = T
      ) %>%
    dplyr::select(group1, group2, p, estimate, conf.low, conf.high) %>%
    mutate(diff = change_percentage)
}

# Function to perform Box-Ljung tests
box_test = function(df) {
  out = Box.test(df$residuals, lag = 51, type = "Ljung-Box") 
  data.frame(values=unlist(out)) %>%
    rownames_to_column(var = "box_vars") %>%
    filter(!box_vars %in% c("method", "data.name")) %>%
    pivot_wider(names_from = box_vars, values_from = values) %>%
    rename(Chi2 = `statistic.X-squared`, df = parameter.df) %>%
    mutate(Chi2 = round(as.numeric(Chi2), 2), p.value = round(as.numeric(p.value), 3))
}

