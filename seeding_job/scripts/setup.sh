#!/bin/bash
# ============================================================
# ARCANE — Seeding Job: One-time cluster setup
# Run once from the login node:
#   bash /media/kevinNFS2/rany/seeding_job/scripts/setup.sh
# ============================================================

set -euo pipefail

PROJECT=/media/kevinNFS2/rany/seeding_job
CLUSTER_JOBS=/media/kevinNFS2/rany/cluster_jobs

echo "Creating project folder structure under $PROJECT ..."

mkdir -p "$PROJECT/R"
mkdir -p "$PROJECT/data/raw"
mkdir -p "$PROJECT/data/calibration"
mkdir -p "$PROJECT/outputs"
mkdir -p "$PROJECT/logs"
mkdir -p "$PROJECT/scripts"

echo "Copying raw data files..."
cp "$CLUSTER_JOBS/data/HBN_weekly_sliding_edgelist_2024.csv" \
   "$PROJECT/data/raw/"
cp "$CLUSTER_JOBS/data/finessgeo_metadata_2024.csv" \
   "$PROJECT/data/raw/"

echo "Copying most recent calibrated beta parameters..."
# Find the most recently modified calibration RDS and copy it
CALIB=$(find "$CLUSTER_JOBS/Outputs" \
         -name "beta_calibrated_params_SERVER.rds" \
         -type f | sort | tail -1)

if [ -z "$CALIB" ]; then
  echo "  WARNING: beta_calibrated_params_SERVER.rds not found."
  echo "  The R script will fall back to hard-coded defaults."
else
  cp "$CALIB" "$PROJECT/data/calibration/beta_calibrated_params_SERVER.rds"
  echo "  Copied: $CALIB"
fi

echo "Making scripts executable..."
chmod +x "$PROJECT/scripts/script.sh"
chmod +x "$PROJECT/scripts/launch.sh"

echo ""
echo "Setup complete. Project structure:"
find "$PROJECT" -not -path "*/outputs/*" -not -path "*/logs/*" | sort
