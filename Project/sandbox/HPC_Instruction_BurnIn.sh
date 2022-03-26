#!/bin/bash 

# PBS Array 1 
# Call args 0-2 via PBS * 4 -1 -2 -3 -4
# use args to Run C++

# Array of parameters

SeedStart=( 100 200 300 400 500 )
DispDist=( 0 1 1.5 2.5 )
InteractDist=( 0 )
IntraComp=( 0 -0.05 -0.1 )
Immigrat=(0.005 0.0005 0.00005)
#fragmentNames=("Landscape1_1.txt" "Landscape1_0.txt" "AmalgamationLandscape1_0.1.txt" "AmalgamationLandscape1_0.2.txt" "AmalgamationLandscape1_0.3.txt" "AmalgamationLandscape1_0.4.txt" "AmalgamationLandscape1_0.5.txt" "AmalgamationLandscape1_0.6.txt" "AmalgamationLandscape1_0.7.txt" "AmalgamationLandscape1_0.8.txt" "AmalgamationLandscape1_0.9.txt")


# array of every combination

args=()
for b in "${SeedStart[@]}";
do
    for i in "${DispDist[@]}"; 
    do
        for j in "${InteractDist[@]}"; 
        do
            for k in "${IntraComp[@]}";
            do
                for l in "${Immigrat[@]}";
                do
                    args+=("$b" "$i" "$j" "$k" "$l") 
                done
            done
        done
    done
done
echo "${args[@]}"

# Splitting up each combination
g=5
#no for loop use PBS array
for((i=0; i < ${#args[@]}; i+=g))
do
    part=( "${args[@]:i:g}" )
    echo "Elements in this group: ${part[*]}"
done


PBS=1
echo " ${args[$( expr "$PBS" '*' 5 - 3)]}"
echo "${args[$( expr "$PBS" '*' 5 - 5)]} ${args[$( expr "$PBS" '*' 5 - 4)]} ${args[$( expr "$PBS" '*' 5 - 3)]} ${args[$( expr "$PBS" '*' 5 - 2)]} ${args[$( expr "$PBS" '*' 5 - 1)]}"
g++ TNM_Disperal_Template.cpp

./a.out ${args[$( expr "$PBS" '*' 5 - 5)]} ${args[$( expr "$PBS" '*' 5 - 4)]} ${args[$( expr "$PBS" '*' 5 - 3)]} ${args[$( expr "$PBS" '*' 5 - 2)]} ${args[$( expr "$PBS" '*' 5 - 1)]} &
    process_id=$!

# notes for next step

#PBS 

#PBS -l walltime=12:30:00

#PBS -l select=1:ncpus=1:mem=800mb

#module load intel-suite

#$WORK/IOFC_May12/IOFC_V1.o ${PBS_ARRAY_INDEX}

#mv Raw* $WORK/IOFC_May12

#mv Summary* $WORK/IOFC_May12