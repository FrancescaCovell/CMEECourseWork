#!/usr/bin/env python3
"""Import data and explore"""
__author__ = 'Francesca Covell (f.covell21@imperial.ac.uk)'
__version__ = '0.0.1'
#__license__ = "License for this code/program"
## imports ##

import pandas as pd
import scipy as sc
import matplotlib.pylab as pl
import seaborn as sns 
import sys
import numpy as np


## Data ##
# Contains measuement of chang in biomass of cells of microbes over time.
# Collected around the world
# Two main interests PopBio(abundance) and Time
# Single population growth rate = curve unique temperature-species-medium-citation-replicate combinations
# Concatenate them to get a new string variable that identifies unique growth curves

def main(argv):
    """ Imports  LogisticGrowthData and MetaData from data directory.
    Converts to data frame, adds ID collumn and removes anomelies
    exports new data as ModGrowthData.csv
    """
    ## Load data
    data = pd.read_csv("../data/LogisticGrowthData.csv")
    Meta = pd.read_csv("../data/LogisticGrowthMetaData.csv")

    ## Data wrangle and make into df
    df = pd.DataFrame(data)
    df.drop(df[df["Time"] < 0].index, inplace=True)
    df.drop(df[df["PopBio"] <= 0].index, inplace=True) 
    df.insert(0,'ID',  df.groupby(['Species','Temp','Medium','Citation']).ngroup())
    #df['ID'] = df['ID'].replace(0, df['ID'].max()+1) 
    test = df.ID.value_counts() < 5
    test = test[test] 
    df.drop(df.loc[df.ID.isin(test.index)].index, inplace=True) 
    df.drop('ID',1, inplace=True)
    df.insert(0,'ID',  df.groupby(['Species','Temp','Medium','Citation']).ngroup())
    df['ID'] = df['ID'].replace(0, df['ID'].max()+1) 
    #df['log_PopBio'] = np. log(df['PopBio'])
    df.insert(4, 'log_PopBio', np. log(df['PopBio']))
    ## Export
    df.to_csv('../data/ModGrowthData.csv')

    return 0

if __name__ == "__main__":
    status = main(sys.argv)
    sys.exit(status)
    