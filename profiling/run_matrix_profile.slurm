#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --reservation=courseq
#SBATCH --account=courses01
#SBATCH --job-name=matrices
#SBATCH --time=00:10:00
#SBATCH --export=NONE

export PAT_RT_PERFCTR=1
export PAT_RT_SAMPLING_INTERVAL=1000
srun --export=all -n 1 matrix+pat
#srun --export=all -n 1 matrix+apa

