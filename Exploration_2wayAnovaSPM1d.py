# -*- coding: utf-8 -*-
"""
Created on Thu Feb  3 08:15:19 2022

@author: Kate.Harrison
"""

import numpy as np
from matplotlib import pyplot as plt
import os
import pandas as pd

import spm1d

# Change directory to the one with your data
os.chdir('C:/Users/kate.harrison/Documents/NoviceRunnersTrainingStudy/KinematicData')

joint = 'HX'

data = pd.read_excel('compiled'+joint+'.xlsx')
data = pd.DataFrame(data)
data8 = data.groupby(by = ['SUBJECT', 'TIME']).head(8)

kinDat = data8.iloc[:,3:104]
subDat = data8.iloc[:,0]
groupDat = data8.iloc[:,1]
timeDat = data8.iloc[:,2]

FF = spm1d.stats.anova2onerm(kinDat, groupDat, timeDat, subDat )

FFi = [F.inference(alpha=0.050) for F in FF]
# plt.figure()
# FFi[0].plot() # group main effect
# plt.xlabel('% Stride time')
# plt.figure()
# FFi[1].plot() # time main effect
# plt.xlabel('% Stride time')
# plt.figure()
# FFi[2].plot() # interaction effect
# plt.xlabel('% Stride time')

time1dat = data8[data8['TIME'] == 1]
time2dat = data8[data8['TIME'] == 2]
time3dat = data8[data8['TIME'] == 3]

label1 = 'T1'
label2 = 'T2'
label3 = 'T3'

plt.figure()
spm1d.plot.plot_mean_sd(time1dat.iloc[:,3:104], linecolor='y', facecolor='whitesmoke', edgecolor='y', label=label1)
spm1d.plot.plot_mean_sd(time2dat.iloc[:,3:104], linecolor='g', facecolor='whitesmoke', edgecolor='g', label=label2)
spm1d.plot.plot_mean_sd(time3dat.iloc[:,3:104], linecolor='b', facecolor='whitesmoke', edgecolor='b', label=label3)
plt.legend()
plt.xlabel('% Stride time')
plt.ylabel('Joint angle (\N{DEGREE SIGN})')

    
shadeRegionStarts = []
shadeRegionEnds = []

for i in range(len(FF[1].z)-1):
    if i ==0 and FF[1].z[i] >= FFi[1].zstar:
        shadeRegionStarts.append(i)
    if i == len(FF[1].z)-2 and FF[1].z[i+1] >= FFi[1].zstar:
        shadeRegionEnds.append(i+1)
        
    if FF[1].z[i] < FFi[1].zstar and FF[1].z[i+1] >= FFi[1].zstar:
        shadeRegionStarts.append(i)
    if FF[1].z[i] > FFi[1].zstar and FF[1].z[i+1] <= FFi[1].zstar:
        shadeRegionEnds.append(i)
   
for i in range(len(shadeRegionStarts)):

    plt.axvspan(shadeRegionStarts[i], shadeRegionEnds[i], color = 'lightgray', alpha = 0.5)

## Post-hoc

# Time 1 vs. 2 

topY = 57
groups12dat = data8[data8['GROUP'] != 3]
F = spm1d.stats.anova1rm(groups12dat.iloc[:,3:104], groups12dat['TIME'], groups12dat['SUBJECT'])
Fi = F.inference(alpha=0.05)

shadeRegionStarts = []
shadeRegionEnds = []

for i in range(len(F.z)-1):
    if i ==0 and F.z[i] >= Fi.zstar:
        shadeRegionStarts.append(i)
    if i == len(F.z)-2 and F.z[i+1] >= Fi.zstar:
        shadeRegionEnds.append(i+1)
        
    if F.z[i] < Fi.zstar and F.z[i+1] >= Fi.zstar:
        shadeRegionStarts.append(i)
    if F.z[i] > Fi.zstar and F.z[i+1] <= Fi.zstar:
        shadeRegionEnds.append(i)
   
for i in range(len(shadeRegionStarts)):
    #i = 1
    x = list(range(shadeRegionStarts[i],shadeRegionEnds[i]+1))
    y = np.repeat(topY, len(range(shadeRegionStarts[i],shadeRegionEnds[i]+1)))
    plt.plot(x, y, 'g*')
    
# plt.figure()
# Fi.plot()
# plt.xlabel('% Stride time')

topY = 61
groups23dat = data8[data8['GROUP'] != 1]
F = spm1d.stats.anova1rm(groups23dat.iloc[:,3:104], groups23dat['TIME'], groups23dat['SUBJECT'])
Fi = F.inference(alpha=0.05)
# plt.figure()
# Fi.plot()
# plt.xlabel('% Stride time')

shadeRegionStarts = []
shadeRegionEnds = []

for i in range(len(F.z)-1):
    if i ==0 and F.z[i] >= Fi.zstar:
        shadeRegionStarts.append(i)
    if i == len(F.z)-2 and F.z[i+1] >= Fi.zstar:
        shadeRegionEnds.append(i+1)
        
    if F.z[i] < Fi.zstar and F.z[i+1] >= Fi.zstar:
        shadeRegionStarts.append(i)
    if F.z[i] > Fi.zstar and F.z[i+1] <= Fi.zstar:
        shadeRegionEnds.append(i)
   
for i in range(len(shadeRegionStarts)):
    #i = 1
    x = list(range(shadeRegionStarts[i],shadeRegionEnds[i]+1))
    y = np.repeat(topY, len(range(shadeRegionStarts[i],shadeRegionEnds[i]+1)))
    plt.plot(x, y, 'b*')

topY = 7
groups13dat = data8[data8['GROUP'] != 2]
F = spm1d.stats.anova1rm(groups13dat.iloc[:,3:104], groups13dat['TIME'], groups13dat['SUBJECT'])
Fi = F.inference(alpha=0.05)
# plt.figure()
# Fi.plot()
# plt.xlabel('% Stride time')

shadeRegionStarts = []
shadeRegionEnds = []

for i in range(len(F.z)-1):
    if i ==0 and F.z[i] >= Fi.zstar:
        shadeRegionStarts.append(i)
    if i == len(F.z)-2 and F.z[i+1] >= Fi.zstar:
        shadeRegionEnds.append(i+1)
        
    if F.z[i] < Fi.zstar and F.z[i+1] >= Fi.zstar:
        shadeRegionStarts.append(i)
    if F.z[i] > Fi.zstar and F.z[i+1] <= Fi.zstar:
        shadeRegionEnds.append(i)
   
for i in range(len(shadeRegionStarts)):
    #i = 1
    x = list(range(shadeRegionStarts[i],shadeRegionEnds[i]+1))
    y = np.repeat(topY, len(range(shadeRegionStarts[i],shadeRegionEnds[i]+1)))
    plt.plot(x, y, 'k*')