#!/bin/bash
#PBS -N arcane_france
#PBS -q mem128G
#PBS -o /media/kevinNFS2/rany/optim_cluster_jobs/logs/${PBS_JOBID}.out
#PBS -e /media/kevinNFS2/rany/optim_cluster_jobs/logs/${PBS_JOBID}.err

set -euo pipefail

echo "=========================================================="
echo "Starting on  : $(date)"
echo "Running on   : $(hostname)"
echo "PBS job ID   : ${PBS_JOBID:-NA}"
echo "jobindex     : ${jobindex:-1}"
echo "=========================================================="

cd "${PBS_O_WORKDIR:-/media/kevinNFS2/rany}" || exit 1

mkdir -p optim_cluster_jobs/Outputs/france \
         optim_cluster_jobs/logs

if [ -n "${PBS_NODEFILE:-}" ] && [ -f "$PBS_NODEFILE" ]; then
  export NCPUS=$(wc -l < "$PBS_NODEFILE")
else
  export NCPUS=4
fi

export ARCANE_ROOT="${PBS_O_WORKDIR:-/media/kevinNFS2/rany}"

echo "NCPUS        : $NCPUS"
echo "ARCANE_ROOT  : $ARCANE_ROOT"
echo "=========================================================="

# module load R

Rscript --vanilla optim_cluster_jobs/france/optim_france.R

echo "=========================================================="
echo "Finished on  : $(date)"
echo "=========================================================="
