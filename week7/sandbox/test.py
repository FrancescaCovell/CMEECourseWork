import sys
def main(argv):
       
    r = None
    a = None
    z = None
    e = None
    Tstart = None
    Tstop = None
    Tdiv = None
    R0 = None
    C0 = None
    # R = pops[0]
   # C = pops[1]

    """simulate""" 
    if (r, a, z, e) == None:
        r=1.
        a=0.1 
        z=1.5
        e=0.75
        dRdt = r * R - a * R * C
        dCdt = -z * C + e * a * R * C
    else:
        r, a, z, e
        dRdt = r * R - a * R * C
        dCdt = -z * C + e * a * R * C 
    return np.array([dRdt, dCdt])

    """time wise"""
    #define time
    #intergrate from time 0 - 15 with 1000 sub-division
    #t = 0
    if (Tstart, Tstop, Tdiv) == None:
        t = np.linspace(0, 15, 1000)
    else:
        t = np.linspace(Tstart, Tstop, Tdiv)
    return 0
# set intial condition 
    if ( R0, C0) == None:
        R0 = 10
        C0 = 5
        RC0 = np.array([R0, C0])
    else:
        RC0 = np.array([R0, C0])
    return RC0

#numerically intergrate
    pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)
    pops

    """plot"""
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
    print('Done!')

if __name__ == "__main__":
    main()