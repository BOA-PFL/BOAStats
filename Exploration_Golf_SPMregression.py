# -*- coding: utf-8 -*-
"""
Created on Fri Dec 10 12:40:52 2021

@author: Kate.Harrison
"""

import numpy as np
from matplotlib import pyplot as plt
import os
import spm1d
import pandas as pd

os.chdir('C:/Users/kate.harrison/Boa Technology Inc/PFL - Documents/General/PowerPerformance/Ecco_Nov2021/KineticsKinematics/')

var = 'GRF_Rear_Z'

data = pd.read_excel(('Compiled_' + var+ '.xlsx'))
data.dropna(inplace = True)

Y = data.iloc[:,3:]
Y.columns = range(Y.shape[1])

TMdata = pd.read_excel('C:/Users/kate.harrison/Boa Technology Inc/PFL - Documents/General/PowerPerformance/Ecco_Nov2021/TrackMan/CompiledTrackManData_SPM.xlsx')

allDat = pd.merge(TMdata, data, on = ['Subject', 'DriveNo', 'Config'])

Y = allDat.iloc[:, 5:]
Y.columns = range(Y.shape[1])

driveDist = allDat.iloc[:,3]
driveDir = allDat.iloc[:,4]

nSubj = 10

### drive distance
BETA = []

for subj in range(1,nSubj+1):
    Ytmp = Y[allDat['Subject'] == subj]
    DDtmp = driveDist[allDat['Subject'] == subj]
    t = spm1d.stats.regress(Ytmp, DDtmp)
    BETA.append( t.beta[0])
    
BETA = np.array(BETA)

alpha = 0.05
t = spm1d.stats.ttest(BETA)
ti = t.inference(alpha, two_tailed = True)

plt.figure()
ti.plot()
    

### drive direction

BETA = []

for subj in range(1,nSubj+1):
    Ytmp = Y[allDat['Subject'] == subj]
    DDtmp = driveDir[allDat['Subject'] == subj]
    t = spm1d.stats.regress(Ytmp, DDtmp)
    BETA.append( t.beta[0])
    
BETA = np.array(BETA)

alpha = 0.05
t = spm1d.stats.ttest(BETA)
ti = t.inference(alpha, two_tailed = True)

plt.figure()
ti.plot()

