#!/bin/bash 
echo "enter Matric dimesion "
read -i MatDim

echo "enter Number of runs "
read -i  NumRun

echo "enter percentage cover "
read -i PerCov


Rscript CreateLandscapeCover.R $MatDim $NumRun $PerCov

echo "Enter starting seed"
read seed1


echo "Enter last seed"
read seed2


g++ TNM_New_Template.cpp



for i in $(seq $seed1 $seed2)
do
./a.out $i 1 landscape1_0.5.txt &
process_id=$!
done