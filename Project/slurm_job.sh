#!/bin/sh

#SBATCH --job-name=BayesST
#SBATCH --time=6:00:00
#SBATCH --mail-user=wangmk@umich.edu
#SBATCH --mail-type=END,FAIL,BEGIN
#SBATCH --mem=10g
#SBATCH --array=1-9
#SBATCH --cpus-per-task=12
#SBATCH --output=/home/wangmk/BIOSTAT830/slurm-%j.out

module load R/4.1.2-gcc8.3.0
Rscript --vanilla /home/wangmk/BIOSTAT830/model_fitting.R

