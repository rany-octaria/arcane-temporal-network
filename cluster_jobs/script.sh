#!/bin/bash
#PBS -N arcane_job
#PBS -q mem128G
#PBS -l walltime=08:00:00
#PBS -o /media/kevinNFS2/rany/cluster_jobs/logs/${PBS_JOBID}.out
#PBS -e /media/kevinNFS2/rany/cluster_jobs/logs/${PBS_JOBID}.err

set -euo pipefail

echo "=========================================================="
echo "Starting on       : $(date)"
echo "Running on node   : $(hostname)"
echo "PBS job ID        : ${PBS_JOBID:-NA}"
echo "PBS workdir       : ${PBS_O_WORKDIR:-NA}"
echo "Indexcode         : ${indexcode:-NA}"
echo "=========================================================="

cd "${PBS_O_WORKDIR:-/media/kevinNFS2/rany}" || exit 1

mkdir -p cluster_jobs/data cluster_jobs/Outputs cluster_jobs/logs

# If your cluster uses environment modules, uncomment:
# module load R

Rscript --vanilla cluster_jobs/arcane_code_beta_estimation.R "${indexcode:-1}"

echo "=========================================================="
echo "Finished on       : $(date)"
echo "=========================================================="
