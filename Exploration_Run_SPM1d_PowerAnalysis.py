# -*- coding: utf-8 -*-
"""
Created on Mon Feb  7 12:20:47 2022

@author: Kate.Harrison
"""

import power1d
import numpy as np
from matplotlib import pyplot as plt

#(0) Create geometry and noise models:
J         = 10    # sample size
Q         = 101  # continuum size
q         = 10   # signal location

## power analysis based on hip adduction and knee abduction trajectories in novice runners study. 
## Finding a difference of 2 degrees across stride given group SD of 4. 
g0   = power1d.geom.GaussianPulse(Q=Q, q=q, amp=7, sigma=10)
g1= power1d.geom.GaussianPulse(Q=Q, q = 45, amp = 2, sigma = 10)
g2 = power1d.geom.GaussianPulse(Q=Q, q = 65, amp = -9, sigma = 10)
g3 = power1d.geom.Constant( Q, amp = 6)
baseline  = g0 + g1 + g2 + g3

signal0   = power1d.geom.Null(Q)
#signal1   = power1d.geom.Constant(Q=101, amp=1)
signal1= power1d.geom.GaussianPulse(Q=Q, q = 10, amp = 4, sigma = 10)
noise     = power1d.noise.SmoothGaussian(J=8, Q=101, mu = 0, sigma=4, fwhm = 20)

#(1) Create data sample models:
model0    = power1d.models.DataSample(baseline, signal0, noise, J=J)  #null
model1    = power1d.models.DataSample(baseline, signal1, noise, J=J)  #alternative

plt.figure()
model0.plot( color='k' )
model1.plot( color='r' )

#(2) Iteratively simulate for a range of sample sizes:
np.random.seed(0)    #seed the random number generator
JJ         = range(5, 51, 1) #sample sizes
tstat      = power1d.stats.t_1sample  #test statistic function
emodel0    = power1d.models.Experiment(model0, tstat) # null
emodel1    = power1d.models.Experiment(model1, tstat) # alternative
sim        = power1d.ExperimentSimulator(emodel0, emodel1)
### loop through the different sample sizes:
power_omni = []
power_coi  = []
coir       = 3
for J in JJ:
        emodel0.set_sample_size( J )
        emodel1.set_sample_size( J )
        results = sim.simulate( 1000 )
        results.set_coi( ( q , coir ) )  #create a COI at the signal location
        power_omni.append( results.p_reject1 )  #omnibus power
        power_coi.append( results.p_coi1[0] )   #coi power

#(3) Plot the results:
plt.figure()
ax = plt.axes()
ax.plot(JJ, power_omni, 'o-', label='Omnibus')
ax.plot(JJ, power_coi,  'o-', label='COI (radius=%d)' %coir)
ax.axhline(0.8, color='k', linestyle='--')
ax.set_xlabel('Sample size', size=14)
ax.set_ylabel('Power', size=14)
ax.legend()
plt.show()

### Sample size to achieve 80% power is 20. Since these are based on modeled and not actual data might increase to ~25 for slight variations in sample size. 