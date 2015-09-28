#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --reservation=courseq
#SBATCH --account=courses01
#SBATCH --job-name=matrices
#SBATCH --time=00:10:00
#SBATCH --export=NONE

aprun -n 1 matrix+pat
#aprun -n 1 matrix+apa

