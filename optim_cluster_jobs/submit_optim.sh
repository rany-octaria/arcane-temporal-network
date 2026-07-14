#!/bin/bash
#PBS -N arcane_optim
#PBS -q mem128G
#PBS -l nodes=1:ppn=30
#PBS -l walltime=48:00:00
#PBS -o /media/kevinNFS2/rany/optim_cluster_jobs/logs/${PBS_JOBID}.out
#PBS -e /media/kevinNFS2/rany/optim_cluster_jobs/logs/${PBS_JOBID}.err

# =============================================================================
# submit_optim.sh  —  ARCANE beta-calibration optimisation (Nelder-Mead)
# -----------------------------------------------------------------------------
# Modelled on the original working arcane_job script (same PBS conventions:
# queue, log paths, cd/mkdir pattern).  Everything this job needs lives under
# the consolidated optim_cluster_jobs/ layout:
#   optim_cluster_jobs/
#   ├── shared/optim_core.R           ← model logic (shared with local driver)
#   ├── calibration/optim_cluster.R   ← driver this script calls
#   ├── data/weekly.RDS
#   ├── data/facility_level_final.RDS
#   ├── logs/
#   └── Outputs/
#
# IMPORTANT — check before first submission:
#   `nodes=1:ppn=30` requests 30 cores on the SAME physical node (required —
#   FORK parallelism only works within one node).  Run `pbsnodes -a` or ask
#   your admin to confirm the mem128G queue's nodes have >= 30 cores and
#   128 GB RAM.  If a node has fewer cores, lower ppn accordingly.
#
# Submit with:   qsub optim_cluster_jobs/submit_optim.sh
# Monitor with:  qstat -u $USER
# Cancel with:   qdel <job_id>
# =============================================================================

set -euo pipefail

echo "=========================================================="
echo "Starting on       : $(date)"
echo "Running on node   : $(hostname)"
echo "PBS job ID        : ${PBS_JOBID:-NA}"
echo "PBS workdir       : ${PBS_O_WORKDIR:-NA}"
echo "=========================================================="

cd "${PBS_O_WORKDIR:-/media/kevinNFS2/rany}" || exit 1

# ── Make ARCANE_ROOT explicit for the R driver script ─────────────────────────
export ARCANE_ROOT="${PBS_O_WORKDIR:-/media/kevinNFS2/rany}"
echo "ARCANE_ROOT       : $ARCANE_ROOT"

# ── Determine how many cores PBS actually granted this job ────────────────────
if [ -n "${PBS_NODEFILE:-}" ] && [ -f "$PBS_NODEFILE" ]; then
  export NCPUS=$(wc -l < "$PBS_NODEFILE")
else
  export NCPUS=30   # fallback for interactive / manual runs
fi
echo "NCPUS (allocated) : $NCPUS"

# ── Create required output / log directories (consolidated layout) ───────────
mkdir -p optim_cluster_jobs/data
mkdir -p optim_cluster_jobs/logs
mkdir -p optim_cluster_jobs/Outputs

# If your cluster uses environment modules, uncomment and adjust:
# module load R

echo "R version         : $(Rscript --version 2>&1)"
echo "=========================================================="

Rscript --vanilla optim_cluster_jobs/calibration/optim_cluster.R

echo "=========================================================="
echo "Finished on       : $(date)"
echo "=========================================================="
