#!/bin/bash
#SBATCH --job-name=mod1a_IRR
#SBATCH --output=output_%j.txt
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=1GB
#SBATCH --time=04:00:00

# compiel the C++ program
g++ -o mod1a_generation THEMBISA.cpp mersenne.cpp StatFunctions.cpp

# Run the compiled executable
./mod1a_generation
