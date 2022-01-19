#practical 
# Make a function that ask for r a z e,  t, R0C0
# run main function which also plots
# 

#!/usr/bin/env python3
"""recteating Lotka-Volterra model"""
__author__ = 'Francesca Covell (francesca.covell21@imperial.ac.uk'
__version__ = '0.0.1'
#__license__ = "License for this code/program"
## imports ##
import numpy as np
import scipy as sc
from scipy import stats
from scipy import integrate
import matplotlib.pylab as p

#### Lotka-Volterra model ####

#Define function
def dCR_dt(pops, t=0):
""" Returns the growth rate of consumer and resource population at any given time step"""
    R = pops[0]
    C = pops[1]
    dRdt = r * R - a * R * C
    dCdt = -z * C + e * a * R * C

    return np.array([dRdt, dCdt])


type(dCR_dt)

# assign parameter
r = 1.
a = 0.1
z = 1.5
e = 0.75

#define time
#intergrate from time 0 - 15 with 1000 sub-division
t = np.linspace(0, 15, 1000) #linspace Return evenly spaced numbers over a specified interval.


# set intial condition 
R0 = 10
C0 = 5
RC0 = np.array([R0, C0])

#numerically intergrate
pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)
pops

#extra information
type(infodict)
infodict.keys()
infodict['message']

#create an empty figure object
f1 = p.figure()


p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1] , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')
p.show()

f1.savefig('../results/LV_model.pdf')


f2 = p.figure()
p.plot(pops[:,0], pops[:,1], 'r-')
p.grid()
p.xlabel('Resource density')
p.ylabel('Consumer density')
p.title('Consumer-Resource population dynamics')
p.show()
f2.savefig('../results/LV_model2.pdf')
