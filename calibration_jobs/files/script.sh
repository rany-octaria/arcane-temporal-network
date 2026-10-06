#!/bin/bash
#PBS -N prev_calib
#PBS -q mem128G
#PBS -l nodes=1:ppn=4
#PBS -l walltime=48:00:00
#PBS -j oe
#PBS -o /media/kevinNFS2/rany/prev_calib_jobs/logs/job_${jobindex}.log

echo "======================================================="
echo "ARCANE — Prevalence Calibration"
echo "Job index : ${jobindex}"
echo "Node      : $(hostname)"
echo "Started   : $(date)"
echo "======================================================="

ARCANE_ROOT=/media/kevinNFS2/rany/prev_calib_jobs
export ARCANE_ROOT
export jobindex

cd ${ARCANE_ROOT}

Rscript --vanilla calibration/optim_prevalence.R

EXIT_CODE=$?
echo "======================================================="
echo "Finished : $(date)"
echo "Exit code: ${EXIT_CODE}"
echo "======================================================="

# Telegram notification
TG_TOKEN="YOUR_TOKEN_HERE"
TG_CHAT="YOUR_CHAT_ID_HERE"
if [ $EXIT_CODE -eq 0 ]; then
  curl -s -X POST "https://api.telegram.org/bot${TG_TOKEN}/sendMessage" \
    -d chat_id="${TG_CHAT}" \
    -d text="✅ Prev calib job ${jobindex}/10 DONE
Node: $(hostname)
Time: $(date '+%Y-%m-%d %H:%M')" > /dev/null 2>&1
else
  curl -s -X POST "https://api.telegram.org/bot${TG_TOKEN}/sendMessage" \
    -d chat_id="${TG_CHAT}" \
    -d text="❌ Prev calib job ${jobindex}/10 FAILED (exit ${EXIT_CODE})
Node: $(hostname)
Time: $(date '+%Y-%m-%d %H:%M')" > /dev/null 2>&1
fi

exit $EXIT_CODE
