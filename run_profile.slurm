#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --account=courseq
#SBATCH --job-name=matrices
#SBATCH --time=00:10:00
#SBATCH --export=NONE

aprun -n 1 matrix.O3+pat

