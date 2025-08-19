#!/bin/bash
#
# Set required nodes.
#SBATCH --nodes=1
#
# Set up nodes for job. 
#SBATCH --ntasks=1
#
# Set partition
#SBATCH -p "sapphire"
#
# Set number of CPUs
#SBATCH --cpus-per-task=1
#
# Set memory limit
#SBATCH --mem=150000
#
# Set wall time
#SBATCH --time=5-00
#
# Set email notifications
#SBATCH --mail-user=billy.geary@unimelb.edu.au
#SBATCH --mail-type=FAIL,REQUEUE,INVALID_DEPEND,STAGE_OUT,TIME_LIMIT_90

fold=$1

module purge 

module load GCC/11.3.0
module load OpenMPI/4.1.4
module load GDAL/3.5.0
module load rgdal/1.6-6
module load R/4.4.0
module load unimelb-mf-clients

cd /data/gpfs/projects/punim2310

Rscript --vanilla /data/gpfs/projects/punim2310/fire/Step_8_fire_model_cv_workflow.R $fold