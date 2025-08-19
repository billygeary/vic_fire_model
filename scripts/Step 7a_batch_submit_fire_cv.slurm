#!/bin/bash

for fold in {1..2}
do

JOB_NAME="Fold_${fold}"
OUT_NAME="/data/gpfs/projects/punim2310/fire/outputs/cv_fold_${fold}.out"

sbatch --job-name=$JOB_NAME --output=$OUT_NAME /data/gpfs/projects/punim2310/fire/submit_fire_cv_workflow.slurm $fold

done