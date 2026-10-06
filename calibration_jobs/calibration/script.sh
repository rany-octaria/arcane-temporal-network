#!/bin/bash
#PBS -N calib_incidence
#PBS -q mem128G
#PBS -l nodes=1:ppn=4
#PBS -l walltime=48:00:00
#PBS -j oe

# Redirect all output to a named log file — ${jobindex} is available at runtime
exec > /media/kevinNFS2/rany/calibration_jobs/logs/job_${jobindex}.log 2>&1

echo "======================================================="
echo "ARCANE — Incidence Calibration"
echo "Job index : ${jobindex}"
echo "Node      : $(hostname)"
echo "Started   : $(date)"
echo "======================================================="

ARCANE_ROOT=/media/kevinNFS2/rany/calibration_jobs
export ARCANE_ROOT
export jobindex

cd ${ARCANE_ROOT}

Rscript --vanilla calibration/calibration_incidence.R

EXIT_CODE=$?

echo "======================================================="
echo "Finished  : $(date)"
echo "Exit code : ${EXIT_CODE}"
echo "======================================================="

exit $EXIT_CODE
