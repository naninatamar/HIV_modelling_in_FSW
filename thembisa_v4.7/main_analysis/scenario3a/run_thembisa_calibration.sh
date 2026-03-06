#!/bin/bash
#SBATCH --job-name=mod3a
#SBATCH --output=output_%j.txt
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=1GB
#SBATCH --time=48:00:00

# compiel the C++ program
g++ -o mod3a_calib THEMBISA.cpp mersenne.cpp StatFunctions.cpp

# Run the compiled executable
./mod3a_calib
