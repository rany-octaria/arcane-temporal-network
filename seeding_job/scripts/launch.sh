#!/bin/bash
# ============================================================
# ARCANE — Seeding Job: Submit all 24 PBS jobs
# Run from login node:
#   bash /media/kevinNFS2/rany/seeding_job/scripts/launch.sh
# ============================================================

cd /media/kevinNFS2/rany/seeding_job || exit 1

N_JOBS=24   # must match N_JOBS in R/arcane_seeding_scenarios_cluster.R

for num in $(seq 1 $N_JOBS)
do
  echo "Submitting job indexcode=$num ..."
  qsub \
    -q mem128G \
    -l nodes=bioclustnew04:ppn=4 \
    -l walltime=12:00:00 \
    -v indexcode=$num \
    scripts/script.sh
done

echo ""
echo "All $N_JOBS jobs submitted."
echo "Monitor with: qstat -u kevin"
echo "Check logs:   ls -lh /media/kevinNFS2/rany/seeding_job/logs/"
echo "Check output: ls -lh /media/kevinNFS2/rany/seeding_job/outputs/"
