#!/bin/bash

# imagemagick must be installed 

for f in *.tif;
    do 
        echo "Converting $f";
        convert "$f" "$(basename "$f" .tif).png";
    done
    