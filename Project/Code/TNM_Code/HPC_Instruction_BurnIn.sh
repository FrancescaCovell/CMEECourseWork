#!/bin/bash 

# PBS Array 1 
# Call args 0-2 via PBS * 4 -1 -2 -3 -4
# use args to Run C++

# Array of parameters

SeedStart=( 100 )
DispDist=( 0 1 1.5 2.5 )
InteractDist=( 0 )
IntraComp=( 0 -0.05 -0.1 )
Immigrat=(0.005 0.0005 0.00005)


# Array of every combination

args=()
m=$SeedStart
for i in "${DispDist[@]}"; 
do
    for j in "${InteractDist[@]}"; 
    do
        for k in "${IntraComp[@]}";
        do
            for l in "${Immigrat[@]}";
            do
                ((m=m+10)) #increase burnin start seed by 10
                args+=("$m" "$i" "$j" "$k" "$l") 
            done
        done
    done    
done

echo "${args[@]}"



PBS_ARRAY_INDEX=30
echo "Test PBS_Array Call"
echo "${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 5)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 4)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 3)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 2)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 1)]}"

g++ TNM_CommandLine_Template.cpp

./a.out ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 5)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 4)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 3)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 2)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 1)]} &
process_id=$!

#g=4
#no for loop use PBS array
#for((i=0; i < ${#args[@]}; i+=g))
#do
#    part=( "${args[@]:i:g}" )
#    echo "Elements in this group: ${part[*]}"
#done

# notes for next step

#PBS 

#PBS -l walltime=12:30:00

#PBS -l select=1:ncpus=1:mem=800mb

#module load intel-suite

#$WORK/IOFC_May12/IOFC_V1.o ${PBS_ARRAY_INDEX}

#mv Raw* $WORK/IOFC_May12

#mv Summary* $WORK/IOFC_May12