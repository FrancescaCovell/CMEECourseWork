#!/usr/bin/env python3
"""run R script unsing subprocess"""
__author__ = 'Francesca Covell (francesca.covell21@imperial.ac.uk'
__version__ = '0.0.1'
#__license__ = "License for this code/program"
## imports ##
import subprocess


subprocess.Popen("Rscript --verbose TestR.R > ../results/TestR.Rout 2> ../results/TestR_errFile.Rout", shell=True).wait()