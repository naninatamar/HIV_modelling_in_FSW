#!/bin/bash
#SBATCH --job-name=mod3b_approx
#SBATCH --output=output_%j.txt
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=1GB
#SBATCH --time=96:00:00

# compiel the C++ program
g++ -o mod3b_calib THEMBISA.cpp mersenne.cpp StatFunctions.cpp

# Run the compiled executable
./mod3b_calib
