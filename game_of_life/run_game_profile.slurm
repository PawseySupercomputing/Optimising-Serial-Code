#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --reservation=courseq
#SBATCH --account=courses01
#SBATCH --job-name=game_of_life
#SBATCH --time=00:10:00
#SBATCH --export=NONE

module load perftools

OPTFLAG=O3

export PAT_RT_PERFCTR=1
export PAT_RT_SAMPLING_INTERVAL=1000

srun --export=all -n 1 game_of_life.${OPTFLAG}.cray+pat
#srun --export=all -n 1 game_of_life.${OPTFLAG}.cray+apa

