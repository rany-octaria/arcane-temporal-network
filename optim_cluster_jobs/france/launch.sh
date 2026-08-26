#!/bin/bash
cd /media/kevinNFS2/rany || exit 1

for num in $(seq 1 10)
do
  echo "Submitting France job $num/10"
  qsub \
    -q mem128G \
    -l nodes=bioclustnew04:ppn=16 \
    -l walltime=48:00:00 \
    -v jobindex=$num \
    optim_cluster_jobs/france/script.sh
done

echo "All 10 France jobs submitted. Monitor: qstat -u kevin"
