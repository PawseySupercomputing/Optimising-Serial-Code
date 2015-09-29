#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --reservation=courseq
#SBATCH --account=courses01
#SBATCH --job-name=game_of_life
#SBATCH --time=00:10:00
#SBATCH --export=NONE

aprun -n 1 game_of_life+pat

