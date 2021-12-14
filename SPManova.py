# -*- coding: utf-8 -*-
"""
Created on Fri Nov 19 11:09:40 2021

@author: Kate.Harrison
"""

import numpy as np
from matplotlib import pyplot as plt
import os
import spm1d
import pandas as pd

os.chdir('C:/Users/kate.harrison/Documents/NoviceRunnersTrainingStudy/KinematicData/')

var = 'KX'

data = pd.DataFrame(np.loadtxt('compiled' + var+ '.txt'))

data = data.groupby([0, 1, 2]).head(8)


SUBJ = data[0]

A = data[1]

B = data[2]

Y = data.iloc[:,3:]
Y.columns = range(Y.shape[1])
FF = spm1d.stats.anova2onerm(Y, A, B, SUBJ)

FFi = [F.inference(alpha=0.05) for F in FF]

plt.figure()
FFi[2].plot() # interaction

plt.figure()
FFi[0].plot() # group

plt.figure()
FFi[1].plot() # time

print(FFi)
print(FFi[1].clusters)

T1 = Y[B==1]
T2 = Y[B==2]
T3 = Y[B==3]

plt.figure()
spm1d.plot.plot_mean_sd(T1, linecolor='y', alpha = 0.5,facecolor=(0.7,0.7,1), edgecolor='y',  label='Timepoint 1')
spm1d.plot.plot_mean_sd(T2, linecolor='r', alpha = 0.5, facecolor=(0.7,0.7,1), edgecolor='r', label='Timepoint 2')
spm1d.plot.plot_mean_sd(T3, linecolor='g', alpha = 0.5, facecolor=(0.7,0.7,1), edgecolor='g', label='Timepoint 3')


##### post hoc one way anova

dataT1T2 = data[data[2] < 3]

SUBJ_T1T2 = dataT1T2[0]

A_T1T2 = dataT1T2[1]

B_T1T2 = dataT1T2[2]

Y_T1T2 = dataT1T2.iloc[:, 3:-1]

F = spm1d.stats.anova1rm(Y_T1T2, B_T1T2, SUBJ_T1T2, equal_var = True)

Fi = F.inference(alpha = 0.05)

plt.figure()
Fi.plot()
print(Fi)

dataT2T3 = data[data[2] > 1]

SUBJ_T2T3 = dataT2T3[0]

A_T2T3 = dataT2T3[1]

B_T2T3 = dataT2T3[2]

Y_T2T3 = dataT2T3.iloc[:, 3:-1]

F = spm1d.stats.anova1rm(Y_T2T3, B_T2T3, SUBJ_T2T3, equal_var = True)

Fi = F.inference(alpha = 0.05)

plt.figure()
Fi.plot()
print(Fi)


# One way ANOVA


F = spm1d.stats.anova1(Y, A, equal_var = True)

Fi = F.inference(alpha = 0.05)

plt.figure()
Fi.plot()
print(Fi)