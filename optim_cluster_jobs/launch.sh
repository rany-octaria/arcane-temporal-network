#!/bin/bash
# =============================================================================
# launch.sh  —  submit the ARCANE beta optimisation job to PBS
# =============================================================================
# Mirrors the structure of the original launch.sh.
# Run from /media/kevinNFS2/rany:
#   bash optim_cluster_jobs/launch.sh
#
# Adjust ppn= to however many cores you want the optimisation to use.
# The R code automatically sets N_CORES = NCPUS - 1 (leaving 1 for the main
# R process), so ppn=16 → 15 parallel workers, ppn=4 → 3 workers, etc.
# Check available cores on bioclustnew04 with:
#   pbsnodes bioclustnew04
# =============================================================================

cd /media/kevinNFS2/rany || exit 1

echo "Submitting ARCANE optim job on bioclustnew04 (ppn=16)"

qsub \
  -q mem128G \
  -l nodes=bioclustnew04:ppn=16 \
  -l walltime=48:00:00 \
  optim_cluster_jobs/script.sh

echo "Job submitted. Monitor with: qstat -u kevin"
