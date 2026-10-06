#!/bin/bash
# launch.sh — submit 10 independent incidence calibration jobs
# Each job runs N_REP=3 reps per beta, giving 30 total reps per beta.

ARCANE_ROOT=/media/kevinNFS2/rany/calibration_jobs
SCRIPT=${ARCANE_ROOT}/calibration/script.sh

# Create output folders for each job
for i in $(seq 1 10); do
  mkdir -p ${ARCANE_ROOT}/Outputs/prevalence/job_$(printf '%02d' ${i})
done

echo "Submitting 10 incidence calibration jobs..."
for i in $(seq 1 10); do
  qsub -v jobindex=${i} ${SCRIPT}
  echo "  Submitted job ${i}/10"
  sleep 1
done

echo "Done. Monitor with: qstat -u kevin"
