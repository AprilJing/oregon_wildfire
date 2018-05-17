#!/bin/bash
#PBS -N casecross_job
#PBS -l nodes=6:ppn=1
#PBS -W group_list=pierce_group
#PBS -q batch
#PBS -M rgan@colostate.edu
#PBS -m abe

# set memory to unlimited
ulimit -s unlimited

# set command directory
cd /home/ryangan/local_git_repo/oregon_wildfire/

# export R library
export R_LIB=~/R/x86_64-pc-linux-gnu-library/3.3

# run rscript
Rscript --vanilla ./r_scripts/data_wrangle/asthma_pos_timestrat.R

