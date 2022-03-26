#!/bin/bash 

# PBS Array 1 
# Call args 0-2 via PBS * 4 -1 -2 -3 -4 etc
# use args to Run C++

# Array of parameters
SeedStart=( 100 )
DispDist=( 0 1 1.5 2.5 )
InteractDist=( 0 )
IntraComp=( 0 -0.05 -0.1 )
Immigrat=(0.005 0.0005 0.00005)
fragmentNames=("Landscape1_1.txt" "Landscape1_0.txt" "AmalgamationLandscape1_0.1.txt" "AmalgamationLandscape1_0.2.txt" "AmalgamationLandscape1_0.3.txt" "AmalgamationLandscape1_0.4.txt" "AmalgamationLandscape1_0.5.txt" "AmalgamationLandscape1_0.6.txt" "AmalgamationLandscape1_0.7.txt" "AmalgamationLandscape1_0.8.txt" "AmalgamationLandscape1_0.9.txt")


# array of every combination

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
                for n in "${fragmentNames[@]}";
                do
                    ((m=m+10)) #increase start seed by 10 
                    t=$(($m+4)) # number of fragmentation itterations
                    b=$(($m))
                    until [ $b -gt $t ]
                    do
                        ((b=b+1)) #increase fragmentation seed by 1 until reach iteration max
                        args+=("$m" "$b" "$i" "$j" "$k" "$l" 1 "$n") 
                    done
                done
            done
        done
    done
done   

echo "${args[@]}"

# Test PBS_ Array_Index
PBS_ARRAY_INDEX=50
echo "Test PBS_Array Call"
echo "${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 8)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 7)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 6)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 5)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 4)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 3)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 2)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 1)]}"

# Runninf TMN Via PBS_Array_Index
g++ TNM_CommandLine_Template.cpp
./a.out ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 8)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 7)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 6)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 5)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 4)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 3)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 2)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 1)]} &
process_id=$!

#g=7
#no for loop use PBS array
#for((i=0; i < ${#args[@]}; i+=g))
#do
#    part=( "${args[@]:i:g}" )
#    echo "Elements in this group: ${part[*]}"
#done