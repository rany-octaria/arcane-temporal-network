library(tidyverse)
library(readxl)
library(janitor)
library(psych)

bhre <- read_excel("Datasets/Sante Publique/e-SIN_BHRe_2017_2025_envoi.xlsx")
bmr = read_excel("Datasets/Sante Publique/e-SIN_BMR_2017_2025_envoi.xlsx")
cd = read_excel("Datasets/Sante Publique/e-SIN_Cd_2017_2025_envoi.xlsx")


#Initial Data Checks for BHRE

head(bhre)
str(bhre)




# 1) Rename columns -------------------------------------------------
# NOTE: this renames by POSITION and assumes the 37 columns are in the
# same order as in your str(bhre) output. The guard below stops you if
# the shape is different.
stopifnot(ncol(bhre) == 37)

names(bhre) <- c(
  "ehesp_id",                                # Identifiant EHESP
  "form_issue_date",                         # Date d'émission de la fiche
  "global_closure_date",                     # Date de clôture globale
  "facility_finess_code",                    # Code FINESS étab
  "facility_status",                         # Statut de l'établissement
  "facility_type",                           # Type de l'établissement
  "first_positive_sample_date",              # Date du 1er prelevement positif
  "sample_type",                             # Type de prélèvement
  "microorganism_1",                         # Microorganisme 1
  "resistance_mechanism_1_1",                # Mecanisme de resistance 1-1
  "resistance_mechanism_1_2",                # Mecanisme de resistance 1-2
  "microorganism_2",                         # Microorganisme 2
  "resistance_mechanism_2_1",                # Mecanisme de resistance 2-1
  "resistance_mechanism_2_2",                # Mecanisme de resistance 2-2
  "microorganism_3",                         # Microorganisme 3
  "resistance_mechanism_3_1",                # Mecanisme de resistance 3-1
  "resistance_mechanism_3_2",                # Mecanisme de resistance 3-2
  "ward_1",                                  # Service 1
  "ward_1_admission_date",                   # Date dentree service 1
  "ward_2",                                  # Service 2
  "ward_2_admission_date",                   # Date dentree service 2
  "ward_3",                                  # Service 3
  "ward_3_admission_date",                   # Date dentree service 3
  "patient_origin",                          # Provenance du patient
  "foreign_link",                            # Lien avec letranger
  "country",                                 # Pays
  "cnr_referral",                            # Envoi au CNR
  "cnr_referral_date",                       # Date denvoi au CNR
  "contact_patients_screening",              # Depistage des patients contacts
  "secondary_cases_identified",              # Cas secondaires identifies
  "n_total_if_secondary_cases",              # N tot si cas secondaire(s)
  "last_case_date",                          # Date du dernier cas
  "n_infections",                            # N infections
  "carrier_patients_transfer",               # Transfert de patients porteurs
  "carrier_transfer_downstream_informed",    # Si transfert de porteurs, ES daval informes
  "contact_patients_transfer",               # Transfert de patients contacts
  "contact_transfer_downstream_informed"     # Si transfert de contacts, ES daval informes
)

# 2) Parse dates ----------------------------------------------------
# form_issue_date is date-only ("01/09/2025").
# The columns below carry a time component ("27/08/2025 - 02:00");
# dmy_hm() parses them, then as_date() drops the (uninformative) time.
datetime_cols <- c(
  "global_closure_date",
  "first_positive_sample_date",
  "ward_1_admission_date",
  "ward_2_admission_date",
  "ward_3_admission_date",
  "cnr_referral_date",
  "last_case_date"
)

bhre_eng <- bhre |>
  mutate(
    form_issue_date = dmy(form_issue_date),
    across(all_of(datetime_cols), ~ as_date(dmy_hm(.x)))
  )

# check the result
str(bhre)

# ---------------------------------------------------------------
# OPTIONAL: keep the real closure timestamp (15:23 etc.) as datetime
# instead of a plain Date. If you want that, comment global_closure_date
# out of `datetime_cols` above and add this line inside mutate():
#
#   global_closure_date = dmy_hm(global_closure_date)   # POSIXct
# ---------------------------------------------------------------

# Doing basic descriptive statistics
bhre_eng = bhre_eng %>% 
  mutate(episode_year = year(first_positive_sample_date),
         episode_duration = as.numeric(last_case_date - first_positive_sample_date) )

#Getting some yearly statistics

#How many samples per year

options(scipen = 999)

tabyl(bhre_eng, episode_year)
tabyl(bhre_eng, microorganism_1)


#how to link the hospital finess geo number with the facilities here;
library(here)
hospitals = readRDS(here("Datasets","Cleaned Model Input Data", "node_attributes_enriched.RDS"))%>% 
  mutate(
    finess_geo = as.numeric(finess_geo)) %>% 
  filter(!is.na(finess_geo))

bhre_eng$facility_finess_code = as.numeric(bhre_eng$facility_finess_code)

# Mapping episodes and cases by hospital types, region, year
# Mechanisms involved, map
# Map of France and for each hospital circle dot showing size proportional to episodes or cases
# Color is mechanism by mechanism

#Graph of the distribution of episodes size per year and how it evolves
# Episodes size per year and how it evolves. \\\# does rate of reporting goes down by year

# Capture - Re-capture paper with eSIN and CNR

# Compare the incidence in eSIN and SPARES


bhre_hosp = left_join(bhre_eng, hospitals, by = c("facility_finess_code" = "finess_geo"))
tabyl(bhre_hosp, facility_type_capact)
fac_count = tabyl(bhre_hosp, facility_name_capact) %>% 
  arrange(-n)


str(bhre_hosp)
colnames(bhre_hosp)
fac_no_match = filter(bhre_hosp, is.na(facility_type_pmsi ))


# ============================================================================
# BHRe episode mechanism recoding for infector-candidate matching
# ----------------------------------------------------------------------------
# Dataset : bhre_2024
# Purpose : encode the paper's "incidence episode mechanism vs candidate
#           mechanism(s)" matching rule as boolean variables, so that
#           candidate-infector filtering becomes a simple column lookup.
#
# RULE (from the replicated paper):
#   KPC / NDM / VIM : the generic (un-subtyped) term is a family WILDCARD.
#                     generic  -> matches generic + every specific in family
#                     specific -> matches generic + ITSELF only
#                     => two different specifics (e.g. NDM-1 vs NDM-5) do NOT
#                        match each other, only via a generic-labelled episode.
#   OXA-48 family   : STRICT exact self-match, no generic wildcard.
#                     "OXA-48-like" is its OWN category, NOT a wildcard.
#
# The relation is symmetric (a star graph per family).
# ============================================================================

# --- 0. The six columns that may hold a resistance mechanism -----------------
mech_cols <- c(
  "resistance_mechanism_1_1", "resistance_mechanism_1_2",
  "resistance_mechanism_2_1", "resistance_mechanism_2_2",
  "resistance_mechanism_3_1", "resistance_mechanism_3_2"
)
stopifnot(all(mech_cols %in% names(bhre_hosp)))

# --- 1. Candidate rule: left column = key, right column = candidate set -------
# Exactly as in the paper's table, with clearly-marked extensions for
# mechanisms present in SPARES but absent from the paper (same family logic).
# NB: for each family generic, the candidate set lists EVERY specific known to
# occur, so if you add a new specific, also add it to the generic's vector.
candidate_rule <- list(
  # ---- OXA-48 family: STRICT exact self-match (no generic wildcard) ----
  "OXA-48"      = c("OXA-48"),
  "OXA-48-like" = c("OXA-48-like"),
  "OXA-181"     = c("OXA-181"),
  "OXA-204"     = c("OXA-204"),   # in paper, absent from SPARES -> all FALSE
  "OXA-244"     = c("OXA-244"),
  "OXA-162"     = c("OXA-162"),   # EXTENSION (SPARES): paper OXA logic
  "OXA-232"     = c("OXA-232"),   # EXTENSION (SPARES): paper OXA logic
  
  # ---- KPC family: generic = wildcard; specific = generic + itself ----
  "KPC"   = c("KPC", "KPC-2", "KPC-3"),
  "KPC-2" = c("KPC", "KPC-2"),    # in paper, absent from SPARES -> all FALSE
  "KPC-3" = c("KPC", "KPC-3"),    # in paper, absent from SPARES -> all FALSE
  
  # ---- NDM family ----
  "NDM"   = c("NDM", "NDM-1", "NDM-4", "NDM-5", "NDM-7"),
  "NDM-1" = c("NDM", "NDM-1"),
  "NDM-4" = c("NDM", "NDM-4"),    # in paper, absent from SPARES -> all FALSE
  "NDM-5" = c("NDM", "NDM-5"),
  "NDM-7" = c("NDM", "NDM-7"),
  
  # ---- VIM family ----
  "VIM"   = c("VIM", "VIM-1", "VIM-2", "VIM-4"),
  "VIM-1" = c("VIM", "VIM-1"),
  "VIM-2" = c("VIM", "VIM-2"),    # in paper, absent from SPARES -> all FALSE
  "VIM-4" = c("VIM", "VIM-4"),    # EXTENSION (SPARES): paper VIM logic
  
  # ---- other carbapenemases in SPARES, same family logic (EXTENSION) ----
  "IMI"   = c("IMI", "IMI-1"),
  "IMI-1" = c("IMI", "IMI-1"),
  "IMP"   = c("IMP")
)

mech_keys <- names(candidate_rule)

# Mechanisms deliberately OUT OF SCOPE of this carbapenemase rule.
# Episodes carrying ONLY these will match nothing here (empty sets, all FALSE).
#   - GRE / glycopeptide : "Van A", "Van B", "Suspicion de resistance aux glycopeptides",
#                          "Mecanisme de resistance aux glycopeptides non identifie ..."
#   - colistin           : "mcr-2"
#   - family-unknown     : "Carbapenemase non identifiee ...", "Suspicion de carbapenemase",
#                          "Mecanisme de resistance non identifie", "Autre mecanisme de resistance"
# -> If enterococci (GRE) are in-scope, add a SEPARATE Van rule.
# -> If family-unknown carbapenemase calls should act as wildcards, that is a
#    methodological choice the paper must specify; not assumed here.

# --- 2. Normalise the mechanism columns (trim, character) --------------------
for (col in mech_cols) bhre_hosp[[col]] <- trimws(as.character(bhre_hosp[[col]]))

# Helper: safe column-name suffix ("OXA-48-like" -> "OXA_48_like") ------------
sanitize <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

# --- 3. Per-episode raw + candidate mechanism SETS (list-columns) ------------
raw_mat <- as.matrix(bhre_hosp[mech_cols])

bhre_hosp$mech_raw <- lapply(seq_len(nrow(raw_mat)), function(i) {
  v <- unique(raw_mat[i, ])
  v[!is.na(v) & v %in% mech_keys]          # keep only recognised carbapenemases
})

bhre_hosp$mech_candidates <- lapply(bhre_hosp$mech_raw, function(v) {
  if (length(v) == 0) character(0) else unique(unlist(candidate_rule[v], use.names = FALSE))
})

# --- 4. mech_<L> : RAW-presence flags = "the grouping on the left" ------------
# TRUE if the episode actually carries mechanism L (exact match, any of 6 cols).
for (L in mech_keys) {
  bhre_hosp[[paste0("mech_", sanitize(L))]] <-
    vapply(bhre_hosp$mech_raw, function(v) L %in% v, logical(1))
}

# --- 5. cand_<L> : CANDIDATE-eligibility flags = the matching operationalised -
# cand_<L> is TRUE if this episode is a VALID candidate infector for an
# incidence episode whose mechanism is L (i.e. episode's raw mechanism is in
# Candidate(L)). This is the column you filter on during attribution.
for (L in mech_keys) {
  candset <- candidate_rule[[L]]
  bhre_hosp[[paste0("cand_", sanitize(L))]] <-
    vapply(bhre_hosp$mech_raw, function(v) length(intersect(v, candset)) > 0, logical(1))
}

# ============================================================================
# HOW TO MATCH (mechanism layer only; apply time / network / space separately)
# ----------------------------------------------------------------------------
# Candidate infectors for a single incidence episode i (row index):
compatible_candidates <- function(i, candidate_ids = seq_len(nrow(bhre_hosp))) {
  target <- bhre_hosp$mech_candidates[[i]]          # what i accepts as a source
  if (length(target) == 0) return(integer(0))       # i has no in-scope mechanism
  keep <- vapply(candidate_ids, function(c)
    length(intersect(bhre_hosp$mech_raw[[c]], target)) > 0, logical(1))
  setdiff(candidate_ids[keep], i)                   # drop self
}

# Symmetric pairwise test (TRUE if episodes a and b are mechanism-compatible):
mech_compatible <- function(a, b) {
  length(intersect(bhre_hosp$mech_raw[[a]], bhre_hosp$mech_candidates[[b]])) > 0
}

# Column-lookup shortcut: for an incidence episode of mechanism "NDM-5",
# its candidates are simply the rows where cand_NDM_5 == TRUE.
# If an incidence episode carries MULTIPLE mechanisms, OR the relevant cand_
# columns together (a candidate matching ANY of its mechanisms is retained).

# --- 6. Sanity checks --------------------------------------------------------
# (a) raw-flag totals — compare against your known frequency table
mech_flag_cols <- paste0("mech_", sanitize(mech_keys))
print(vapply(mech_flag_cols, function(f) sum(bhre_hosp[[f]]), integer(1)))

# (b) episodes with NO in-scope carbapenemase (GRE-only / unknown / mcr, etc.)
cat("Episodes with no in-scope carbapenemase mechanism: ",
    sum(lengths(bhre_hosp$mech_raw) == 0), "\n")

# (c) worked example: NDM-1 must NOT accept an NDM-5-only candidate, but MUST
#     accept a generic-NDM candidate. Uncomment to verify on your data.
# i_ndm1 <- which(vapply(bhre_hosp$mech_raw, function(v) identical(v, "NDM-1"), logical(1)))[1]
# str(bhre_hosp$mech_candidates[[i_ndm1]])   # expect c("NDM","NDM-1")
# ============================================================================

# ============================================================================
# HOW TO USE FOR LINKAGE
# ----------------------------------------------------------------------------
#   * First pass  : link episodes that share a FAMILY flag
#                   (has_KPC / has_NDM / has_VIM / has_OXA48_group).
#                   NDM-1 and NDM-5 episodes link here - correct, they can be
#                   the same plasmid-driven event.
#   * Within OXA  : refine using the per-variant flags. Treat has_OXA_48_like
#                   as compatible with ANY specific OXA-48 variant (wildcard),
#                   never as its own separate group.
#   * Clonal step : where you need a same-strain hypothesis, additionally
#                   require the matching variant flag (e.g. has_NDM_5).
# ============================================================================


#Export the dataset 

#Dataset containing all years

#Subsetting cases that were happening in 2024
bhre_2024 = filter(bhre_hosp, episode_year == 2024 | year(last_case_date) == 2024 | (year(first_positive_sample_date) < 2024 & year(last_case_date) >2024))
tabyl(bhre_2024, episode_year)

bhre_2024 %>%
  select(episode_duration, n_infections, n_total_if_secondary_cases) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~name, scales = "free")

#Looking at distribution of episodes
bhre_2024 %>% 
  select(episode_duration, n_infections, n_total_if_secondary_cases) %>% 
  summary()

#Looking at the breakdown of the facilities
fac_tab = tabyl(bhre_2024, facility_finess_code) %>% 
  arrange(-n)
colnames(bhre_2024)

# Looking at discrepancy between the number of cases and
# whether secondary cases are identified

discord_case_count = bhre_2024 %>% 
  filter( (secondary_cases_identified== 1 & n_total_if_secondary_cases <2)  | 
            (secondary_cases_identified == 0 & n_total_if_secondary_cases >1) 
  ) %>% 
  select(ehesp_id, facility_finess_code, first_positive_sample_date, 
         secondary_cases_identified, n_total_if_secondary_cases ) %>% 
  mutate(
    discord_type = ifelse(secondary_cases_identified ==1, "Unidentified secondary cases", 
                          "Identified secondary cases without coding"
    )
  )
view(discord_case_count)

#Export the dataset
bhre_for_csv = select(bhre_hosp, -mech_raw, -mech_candidates)
write.csv(bhre_for_csv, file= here("Datasets" , "Sante Publique","Cleaned", "bhre_hosp_clean.csv"))
saveRDS(bhre_for_csv, file= here("Datasets" , "Sante Publique","Cleaned", "bhre_hosp_clean.RDS"))

#Saving 2024 file only
bhre_for_csv_2024 = select(bhre_2024, -mech_raw, -mech_candidates)
write.csv(bhre_for_csv_2024, file= here("Datasets" , "Sante Publique","Cleaned", "bhre_hosp_clean_2024.csv"))
saveRDS(bhre_for_csv_2024, file= here("Datasets" , "Sante Publique","Cleaned", "bhre_hosp_clean_2024.RDS"))
