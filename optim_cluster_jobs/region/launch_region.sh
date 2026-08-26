#!/bin/bash
# =============================================================================
# Check the actual number of regions BEFORE running this:
#   Rscript --vanilla -e "
#     fl <- readRDS('optim_cluster_jobs/data/facility_level_final.RDS')
#     cat(length(sort(unique(fl\$region[!is.na(fl\$region) & fl\$region != 'Unknown']))), '\n')
#   "
# Then set N_REGIONS below to match.
# =============================================================================
N_REGIONS=13   # ← update from the command above

cd /media/kevinNFS2/rany || exit 1
for num in $(seq 1 $N_REGIONS)
do
  echo "Submitting region job $num/$N_REGIONS"
  qsub -q mem128G -l nodes=bioclustnew04:ppn=16 -l walltime=48:00:00 \
       -v jobindex=$num optim_cluster_jobs/region/script.sh
done
echo "All $N_REGIONS region jobs submitted. Monitor: qstat -u kevin"
