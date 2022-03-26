#!/bin/bash 

echo "Enter starting seed"
read seed1

echo "Enter last seed"
read seed2

echo "Enter Dispersal distance"
read DispDist

echo "Enter Interaction distance"
read InteractDist

echo "Enter Intraspecific competition"
read IntraComp

echo "Enter Immigration rate"
read Immigrat

g++ TNM_CommandLine_Template.cpp

for i in $(seq $seed1 $seed2)
do
    
    ./a.out $i $DispDist $InteractDist $IntraComp $Immigrat &
    process_id=$!

done
