#!/bin/bash
cd /media/kevinNFS2/rany || exit 1

for num in $(seq 1 10)
do
  echo "Submitting job for indexcode=$num on bioclustnew04 (ppn=4)"
  qsub \
    -q mem128G \
    -l nodes=bioclustnew04:ppn=4 \
    -l walltime=08:00:00 \
    -v indexcode=$num \
    cluster_jobs/script.sh
done
