#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --reservation=courseq
#SBATCH --account=courses01
#SBATCH --job-name=iobench
#SBATCH --time=00:10:00
#SBATCH --export=NONE

aprun -n 1 iobench_hdf5

