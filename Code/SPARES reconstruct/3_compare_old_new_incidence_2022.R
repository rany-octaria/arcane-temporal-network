# ============================================================
# Compare OLD vs NEW ESBL incidence, 2022
#
# OLD: Datasets/SPARES/incidence_esble.txt - prior SPARES-derived
#      incidence, 6-category ownership-based facility type scheme
# NEW: Datasets/Output Data/esbl_incidence_by_region_type_2022.csv -
#      output of esbl_incidence_2022.R (BMR/ATB-based, groupe-based
#      facility type scheme, ESBL-E species scope: E. coli,
#      K. pneumoniae, Enterobacter cloacae complex)
#
# The two files use different region spellings and different facility-
# type taxonomies, so this script reconciles both before comparing:
#   - region: BMR's Nouvelle_Region spelling -> old file's canonical
#     spelling (Corse and the overseas regions have no old-file
#     counterpart and are dropped from the comparison)
#   - type: BMR's groupe -> old file's type, only where there's a
#     defensible correspondence (CH/CHU/ESSR map 1:1; MCO is compared
#     against the SUM of old's "Private for profit" + "Private
#     not-for-profit", since groupe can't distinguish ownership within
#     MCO). CLCC/ESLD/HIA/LOC/PSY have no old-file counterpart and are
#     dropped from the comparison.
# ============================================================

library(here)
library(tidyverse)

# ------------------------------------------------------------
# 0. Load both files
# ------------------------------------------------------------
new_incidence <- read.csv(
  here("Datasets", "Output Data", "esbl_incidence_by_region_type_2022.csv")
)

old_incidence <- read.delim(
  here("Datasets", "SPARES", "incidence_eblse.txt")
) %>%
  filter(Date_year == 2022)

# ------------------------------------------------------------
# 1. Region mapping: BMR's Nouvelle_Region -> old file's spelling
# ------------------------------------------------------------
region_map <- c(
  "Auvergne - Rhône Alpes"      = "Auvergne-Rhône-Alpes",
  "Bourgogne - Franche Comté"   = "Bourgogne-Franche-Comté",
  "Bretagne"                    = "Bretagne",
  "Centre-Val de Loire"         = "Centre-Val de Loire",
  "Grand Est"                   = "Grand-Est",
  "Hauts de France"             = "Hauts-de-France",
  "Ile de France"               = "Île-de-France",
  "Normandie"                   = "Normandie",
  "Nouvelle Aquitaine"          = "Nouvelle-Aquitaine",
  "Occitanie"                   = "Occitanie",
  "Pays de Loire"               = "Pays de la Loire",
  "Provence Alpes Côte d'Azur"  = "Provence-Alpes-Côte d'Azur"
  # Corse, Guadeloupe, Martinique, Reunion - Mayotte, Nouvelle Calédonie
  # have no counterpart in the old file - left unmapped (become NA below
  # and get dropped)
)

# ------------------------------------------------------------
# 2. Facility-type mapping: BMR's groupe -> old file's type
# ------------------------------------------------------------
type_map <- c(
  "CH"   = "General public hospital",
  "CHU"  = "University hospital",
  "ESSR" = "Rehabilitation hospital",
  "MCO"  = "Private (combined)"
  # CLCC, ESLD, HIA, LOC, PSY have no old-file counterpart - left
  # unmapped (become NA below and get dropped)
)

# ------------------------------------------------------------
# 3. Reshape NEW: one row per region x type, columns per species
# ------------------------------------------------------------
new_wide <- new_incidence %>%
  filter(species_group != "All ESBL-E") %>%   # old file has no "all" total
  mutate(
    region       = unname(region_map[Nouvelle_Region]),
    type_compare = unname(type_map[groupe]),
    species = case_when(
      species_group == "Escherichia coli"             ~ "ecoli",
      species_group == "Klebsiella pneumoniae"         ~ "kpneumoniae",
      species_group == "Enterobacter cloacae complex"  ~ "ecloacaecomplex"
    )
  ) %>%
  filter(!is.na(region), !is.na(type_compare)) %>%
  group_by(region, type_compare, species) %>%
  summarise(
    n_esbl_positive = sum(n_esbl_positive),
    patient_days    = sum(patient_days),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(region, type_compare),
    names_from = species,
    values_from = c(n_esbl_positive, patient_days)
  ) %>%
  mutate(
    incidence_new_ecoli           = round(n_esbl_positive_ecoli / patient_days_ecoli * 1000, 3),
    incidence_new_kpneumoniae     = round(n_esbl_positive_kpneumoniae / patient_days_kpneumoniae * 1000, 3),
    incidence_new_ecloacaecomplex = round(n_esbl_positive_ecloacaecomplex / patient_days_ecloacaecomplex * 1000, 3)
  )

# ------------------------------------------------------------
# 4. Reshape OLD: combine Private for-profit + non-profit into the
#    single "MCO (combined)" bucket, recompute incidence from summed
#    counts (NOT averaged rates)
# ------------------------------------------------------------
old_type_map <- c(
  "General public hospital"         = "General public hospital",
  "University hospital"             = "University hospital",
  "Rehabilitation hospital"         = "Rehabilitation hospital",
  "Private for profit hospital"     = "Private (combined)",
  "Private not-for-profit hospital" = "Private (combined)"
)

old_wide <- old_incidence %>%
  mutate(type_compare = unname(old_type_map[type])) %>%
  filter(!is.na(type_compare)) %>%
  group_by(region, type_compare) %>%
  summarise(
    n_esbl_ecoli           = sum(n_esbl_ecoli),
    n_esbl_kpneumoniae     = sum(n_esbl_kpneumoniae),
    n_esbl_ecloacaecomplex = sum(n_esbl_ecloacaecomplex),
    n_bed_days             = sum(n_bed_days),
    .groups = "drop"
  ) %>%
  mutate(
    incidence_old_ecoli           = round(n_esbl_ecoli / n_bed_days * 1000, 3),
    incidence_old_kpneumoniae     = round(n_esbl_kpneumoniae / n_bed_days * 1000, 3),
    incidence_old_ecloacaecomplex = round(n_esbl_ecloacaecomplex / n_bed_days * 1000, 3)
  )

# ------------------------------------------------------------
# 5. Merge and compare
# ------------------------------------------------------------
comparison <- inner_join(new_wide, old_wide, by = c("region", "type_compare"))

cat("Comparable region x type cells:", nrow(comparison), "\n")
cat("New-only cells (no old-file match):",
    nrow(anti_join(new_wide, old_wide, by = c("region", "type_compare"))), "\n")
cat("Old-only cells (no new-data match):",
    nrow(anti_join(old_wide, new_wide, by = c("region", "type_compare"))), "\n\n")

species <- c("ecoli", "kpneumoniae", "ecloacaecomplex")

for (sp in species) {
  new_col <- paste0("incidence_new_", sp)
  old_col <- paste0("incidence_old_", sp)

  diff     <- comparison[[new_col]] - comparison[[old_col]]
  pct_diff <- diff / comparison[[old_col]] * 100
  corr     <- cor(comparison[[new_col]], comparison[[old_col]])

  # Pooled (sum counts across all matched cells, then compute one rate)
  n_new_col <- paste0("n_esbl_positive_", sp)
  d_new_col <- paste0("patient_days_", sp)
  n_old_col <- paste0("n_esbl_", sp)
  inc_new_pooled <- sum(comparison[[n_new_col]]) / sum(comparison[[d_new_col]]) * 1000
  inc_old_pooled <- sum(comparison[[n_old_col]]) / sum(comparison$n_bed_days) * 1000

  cat("--", sp, "--\n")
  cat(sprintf("  pooled incidence   new: %.3f   old: %.3f   ratio: %.2f\n",
              inc_new_pooled, inc_old_pooled, inc_new_pooled / inc_old_pooled))
  cat(sprintf("  mean abs diff: %.3f   mean pct diff: %.1f%%   correlation: %.2f\n\n",
              mean(abs(diff), na.rm = TRUE), mean(pct_diff, na.rm = TRUE), corr))
}

# ------------------------------------------------------------
# 6. Save the full comparison table
# ------------------------------------------------------------
write.csv(
  comparison,
  here("Datasets", "Output Data", "esbl_incidence_old_vs_new_comparison_2022.csv"),
  row.names = FALSE
)

print(comparison, n = 60)
