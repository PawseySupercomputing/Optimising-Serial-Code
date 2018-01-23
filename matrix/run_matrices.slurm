#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --reservation=courseq
#SBATCH --account=courses01
#SBATCH --job-name=matrices
#SBATCH --time=00:10:00
#SBATCH --export=NONE

echo matrices.O0
srun --export=all -n 1 matrix.O0

echo matrix.O1
srun --export=all -n 1 matrix.O1

echo matrix.O2
srun --export=all -n 1 matrix.O2

echo matrix.O3
srun --export=all -n 1 matrix.O3

