#!/bin/bash
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
cp $HOME/fc420_HPC_2021_main.R $TMPDIR
echo "R is about to run"
R --vanilla < $HOME/fc420_HPC_2021_cluster.R
mv *.rda $HOME
echo "R has finished running"
