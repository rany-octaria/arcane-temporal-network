#!/bin/bash
# =============================================================================
# launch_novel.sh — Submit novel pathogen seeding simulation (single job)
# =============================================================================
# 8 seed rules x 2 tiers x 30 reps = 480 simulations on 16 cores.
# Outputs to: calibration_jobs/Outputs/seeding_novel/
# =============================================================================

ARCANE_ROOT=/media/kevinNFS2/rany/calibration_jobs
SCRIPT=${ARCANE_ROOT}/seeding/script_novel.sh

echo "=== ARCANE Novel Pathogen Seeding — Job Submission ==="
echo "Root: ${ARCANE_ROOT}"
echo ""

echo "Pre-flight checks..."
ABORT=0

[ -f "${ARCANE_ROOT}/seeding/arcane_seeding_novel_cluster.R" ] \
  && echo "  [OK] arcane_seeding_novel_cluster.R" \
  || { echo "  [MISSING] arcane_seeding_novel_cluster.R"; ABORT=1; }

[ -f "${ARCANE_ROOT}/data/weekly.RDS" ] \
  && echo "  [OK] weekly.RDS" \
  || { echo "  [MISSING] weekly.RDS"; ABORT=1; }

[ -f "${ARCANE_ROOT}/data/facility_level_final.RDS" ] \
  && echo "  [OK] facility_level_final.RDS" \
  || { echo "  [MISSING] facility_level_final.RDS"; ABORT=1; }

if [ ${ABORT} -eq 1 ]; then
  echo ""
  echo "Aborting — fix missing files before submitting."
  exit 1
fi

echo ""
echo "Creating output and log folders..."
mkdir -p ${ARCANE_ROOT}/seeding/logs
mkdir -p ${ARCANE_ROOT}/Outputs/seeding_novel
chmod -R 775 ${ARCANE_ROOT}/seeding/logs
chmod -R 775 ${ARCANE_ROOT}/Outputs/seeding_novel
echo "  Folders OK"

echo ""
echo "Submitting job..."
JOB_ID=$(qsub ${SCRIPT})
echo "  Submitted — PBS ID: ${JOB_ID}"

echo ""
echo "Monitor with:"
echo "  qstat -u kevin"
echo "  tail -f ${ARCANE_ROOT}/seeding/logs/seeding_novel.log"
echo "  ls ${ARCANE_ROOT}/Outputs/seeding_novel/"
