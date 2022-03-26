#!/bin/bash

echo "Enter the starting seed of the original model runs"
read seed1

echo "Enter the end seed of the original model runs"
read seed2

echo "Enter the starting seed of the fragmented runs"
read newSeed1

echo "Enter the end seed of the fragmented runs"
read newSeed2

echo "Enter Dispersal distance"
read DispDist

echo "Enter Interaction distance"
read InteractDist

echo "Enter Intraspecific competition"
read IntraComp

echo "Enter Immigration rate"
read Immigrat



fragmentNames=("Landscape1_1.txt" "Landscape1_0.txt" "AmalgamationLandscape1_0.1.txt" "AmalgamationLandscape1_0.2.txt" "AmalgamationLandscape1_0.3.txt" "AmalgamationLandscape1_0.4.txt" "AmalgamationLandscape1_0.5.txt" "AmalgamationLandscape1_0.6.txt" "AmalgamationLandscape1_0.7.txt" "AmalgamationLandscape1_0.8.txt" "AmalgamationLandscape1_0.9.txt")
#fragmentNames=("RasterLandscapePID0006_0.3.txt" "RasterLandscapePID0091_0.8.txt" "RasterLandscapePID0095_0.6.txt" "RasterLandscapePID0117_0.4.txt" "RasterLandscapePID0131_0.7.txt" "RasterLandscapePID1005_0.5.txt") 
#fragmentNames=("RandomLandscape1_0.1.txt" "RandomLandscape1_0.2.txt" "RandomLandscape1_0.3.txt" "RandomLandscape1_0.4.txt" "RandomLandscape1_0.5.txt" "RandomLandscape1_0.6.txt" "RandomLandscape1_0.7.txt" "RandomLandscape1_0.8.txt" "RandomLandscape1_0.9.txt")

#"AmalgamationLandscape2_0.1.txt" "AmalgamationLandscape2_0.2.txt" "AmalgamationLandscape2_0.3.txt" "AmalgamationLandscape2_0.4.txt" "AmalgamationLandscape2_0.5.txt" "AmalgamationLandscape2_0.6.txt" "AmalgamationLandscape2_0.7.txt" "AmalgamationLandscape2_0.8.txt" "AmalgamationLandscape2_0.9.txt"
#"AmalgamationLandscape3_0.1.txt" "AmalgamationLandscape3_0.2.txt" "AmalgamationLandscape3_0.3.txt" "AmalgamationLandscape3_0.4.txt" "AmalgamationLandscape3_0.5.txt" "AmalgamationLandscape3_0.6.txt" "AmalgamationLandscape3_0.7.txt" "AmalgamationLandscape3_0.8.txt" "AmalgamationLandscape3_0.9.txt"
#"AmalgamationLandscape4_0.1.txt" "AmalgamationLandscape4_0.2.txt" "AmalgamationLandscape4_0.3.txt" "AmalgamationLandscape4_0.4.txt" "AmalgamationLandscape4_0.5.txt" "AmalgamationLandscape4_0.6.txt" "AmalgamationLandscape4_0.7.txt" "AmalgamationLandscape4_0.8.txt" "AmalgamationLandscape4_0.9.txt"
#"AmalgamationLandscape5_0.1.txt" "AmalgamationLandscape5_0.2.txt" "AmalgamationLandscape5_0.3.txt" "AmalgamationLandscape5_0.4.txt" "AmalgamationLandscape5_0.5.txt" "AmalgamationLandscape5_0.6.txt" "AmalgamationLandscape5_0.7.txt" "AmalgamationLandscape5_0.8.txt" "AmalgamationLandscape5_0.9.txt")

#g++ TNM_New_Template.cpp
g++ TNM_CommandLine_Template.cpp


for i in $(seq $seed1 $seed2)
do
    for j in $(seq $newSeed1 $newSeed2)
    do
        for k in "${fragmentNames[@]}";
        do
            ./a.out $i $j $DispDist $InteractDist $IntraComp $Immigrat 1 $k &
            process_id=$!
        done
    done
done