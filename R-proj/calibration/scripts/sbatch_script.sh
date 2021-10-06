#!/bin/sh
# script required to launch ABC calibration on Gizmo (use with sbatch: ex. sbatch -n 10 -t 24:00:00 ./sbatch_script.sh)
# logging is done to the slurm<xxx>.out file int the current directory
# output goes to the file out_mcmc.rdata in the R-proj directory
date
R --vanilla < run_calibration_abc.R
date
