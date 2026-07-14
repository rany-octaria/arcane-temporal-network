#!/bin/bash
#PBS -N arcane_seeding
#PBS -q mem128G
#PBS -l walltime=12:00:00
#PBS -o /media/kevinNFS2/rany/seeding_job/logs/${PBS_JOBID}.out
#PBS -e /media/kevinNFS2/rany/seeding_job/logs/${PBS_JOBID}.err

set -euo pipefail

echo "=========================================================="
echo "Starting on  : $(date)"
echo "Node         : $(hostname)"
echo "Job ID       : ${PBS_JOBID:-NA}"
echo "Indexcode    : ${indexcode:-NA}"
echo "=========================================================="

cd /media/kevinNFS2/rany/seeding_job || exit 1

Rscript --vanilla R/arcane_seeding_scenarios_cluster.R "${indexcode:-1}"

echo "=========================================================="
echo "Finished on  : $(date)"
echo "=========================================================="
