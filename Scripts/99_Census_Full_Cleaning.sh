#!/bin/sh
#99_Census_Full_Cleaning.sh
#Slurm script to run R Census Cleaning

#Slurm directives
#
#SBATCH -A sscc                  # The account name for the job.
#SBATCH -J Census Full Cleaning  # The job name.
#SBATCH -c 1                     # The number of cpu cores to use.
#SBATCH -t 3:00:00               # The time the job will take to run.
#SBATCH --mem-per-cpu 2gb        # The memory the job will use per cpu core.

module load R

#Command to execute R code
R CMD BATCH --no-save --vanilla 99_Census_Full_Cleaning.R routput

# End of script