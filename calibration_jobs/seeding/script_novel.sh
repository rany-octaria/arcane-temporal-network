#!/bin/bash
#PBS -N seeding_novel
#PBS -q mem128G
#PBS -l nodes=1:ppn=16
#PBS -l walltime=48:00:00
#PBS -j oe
#PBS -o /media/kevinNFS2/rany/calibration_jobs/seeding/logs/

# =============================================================================
# script_novel.sh — Novel pathogen seeding PBS job
# =============================================================================

ARCANE_ROOT=/media/kevinNFS2/rany/calibration_jobs
LOG_FILE=${ARCANE_ROOT}/seeding/logs/seeding_novel.log

mkdir -p ${ARCANE_ROOT}/seeding/logs
mkdir -p ${ARCANE_ROOT}/Outputs/seeding_novel

exec > >(tee -a ${LOG_FILE}) 2>&1

echo "======================================================="
echo "ARCANE — Novel Pathogen Seeding Simulation"
echo "Node      : $(hostname)"
echo "Started   : $(date)"
echo "Log       : ${LOG_FILE}"
echo "======================================================="

echo "Pre-flight checks..."
ls -lh ${ARCANE_ROOT}/data/weekly.RDS                  && echo "  weekly.RDS OK"          || { echo "  ERROR: weekly.RDS missing";                exit 1; }
ls -lh ${ARCANE_ROOT}/data/facility_level_final.RDS    && echo "  facility_level OK"      || { echo "  ERROR: facility_level_final.RDS missing";   exit 1; }
ls -lh ${ARCANE_ROOT}/seeding/arcane_seeding_novel_cluster.R && echo "  R script OK"      || { echo "  ERROR: arcane_seeding_novel_cluster.R missing"; exit 1; }

echo ""
echo "Running R script..."
export ARCANE_ROOT

Rscript --vanilla ${ARCANE_ROOT}/seeding/arcane_seeding_novel_cluster.R

EXIT_CODE=$?

echo ""
echo "======================================================="
echo "Finished  : $(date)"
echo "Exit code : ${EXIT_CODE}"

OUT_DIR=${ARCANE_ROOT}/Outputs/seeding_novel
if ls ${OUT_DIR}/seeding_results_*.rds 1>/dev/null 2>&1; then
  echo "Output    : results RDS written OK"
  ls -lh ${OUT_DIR}/
else
  echo "WARNING   : no results RDS found in ${OUT_DIR}"
fi
echo "======================================================="

exit ${EXIT_CODE}
