#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --account=courseq
#SBATCH --job-name=matrices
#SBATCH --time=00:10:00
#SBATCH --export=NONE

echo matrices.O0
aprun -n 1 matrix.O0

echo matrix.O1
aprun -n 1 matrix.O1

echo matrix.O2
aprun -n 1 matrix.O2

echo matrix.O3
aprun -n 1 matrix.O3

