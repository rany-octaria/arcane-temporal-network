#!/bin/bash
#PBS -N arcane_optim
#PBS -q mem128G
#PBS -l walltime=48:00:00
#PBS -o /media/kevinNFS2/rany/optim_cluster_jobs/logs/${PBS_JOBID}.out
#PBS -e /media/kevinNFS2/rany/optim_cluster_jobs/logs/${PBS_JOBID}.err

set -euo pipefail

echo "=========================================================="
echo "Starting on       : $(date)"
echo "Running on node   : $(hostname)"
echo "PBS job ID        : ${PBS_JOBID:-NA}"
echo "PBS workdir       : ${PBS_O_WORKDIR:-NA}"
echo "=========================================================="

cd "${PBS_O_WORKDIR:-/media/kevinNFS2/rany}" || exit 1

mkdir -p optim_cluster_jobs/data \
         optim_cluster_jobs/Outputs \
         optim_cluster_jobs/logs

# Detect how many cores PBS actually gave this job, so optim_cluster.R
# can set N_CORES without hard-coding it here.
# wc -l on PBS_NODEFILE counts one line per allocated core.
if [ -n "${PBS_NODEFILE:-}" ] && [ -f "$PBS_NODEFILE" ]; then
  export NCPUS=$(wc -l < "$PBS_NODEFILE")
else
  export NCPUS=4
fi

export ARCANE_ROOT="${PBS_O_WORKDIR:-/media/kevinNFS2/rany}"

echo "NCPUS             : $NCPUS"
echo "ARCANE_ROOT       : $ARCANE_ROOT"
echo "=========================================================="

# If your cluster uses environment modules, uncomment:
# module load R

Rscript --vanilla optim_cluster_jobs/calibration/optim_cluster.R

echo "=========================================================="
echo "Finished on       : $(date)"
echo "=========================================================="
