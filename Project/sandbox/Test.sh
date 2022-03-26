#!/bin/bash 


# Array of parameters

newSeed1=11
newSeed2=15

DispDist=( 0 1 1.5 2.5 )
IntraComp=( 0 -0.05 -0.1 )
Immigrat=(0.005 0.0005 0.00005)

# array of every combination

args=()
for i in "${DispDist[@]}"; 
do
    for j in "${IntraComp[@]}";
    do
        for k in "${Immigrat[@]}";
        do
            args+=("$i" "$j" "$k")
        done
    done
done
echo "${args[@]}"

# Splitting up each combination
g=3
for((i=0; i < ${#args[@]}; i+=g))
do
    part=( "${args[@]:i:g}" )
    echo "Elements in this group: ${part[*]}"
done



# notes for next step

# PBS 

#PBS -l walltime=12:30:00

#PBS -l select=1:ncpus=1:mem=800mb

#module load intel-suite

#$WORK/IOFC_May12/IOFC_V1.o ${PBS_ARRAY_INDEX}

#mv Raw* $WORK/IOFC_May12

#mv Summary* $WORK/IOFC_May12