# -*- coding: utf-8 -*-
"""
Created on Thu Jun 16 11:37:37 2022

@author: Milena.Singletary
"""

import matplotlib.pyplot as plt
import matplotlib.image as img
import matplotlib as mpl
import matplotlib.patches as mpatches
import numpy as np
import pandas as pd

from matplotlib import cm


im = img.imread(r'C:\Users\milena.singletary\OneDrive - Boa Technology Inc\Pictures\Foot.PNG')

# initiate foot regions
# input the number of regions
# RegionsList = []
# plt.imshow(im)
# regions = 23
# for i in range(regions):
#     print('Select region' + str(i+1) + '.')
#     RegionsList.append(plt.ginput(1))
    
# print('select region1')
# rr1 = plt.ginput(n=1)
# print('select region2')
# rr2 = plt.ginput(n=1)



rr1 = [66, 90]
rr2 = [138, 84]
rr3 = [158, 85.5]
rr4 = [203.5, 60]
rr5 = [200, 83]
rr6 = [303, 107]
rr7 = [305, 94]
rr8 = [323, 102.5]
rr9 = [340, 94]
rr10 = [384, 19]
rr11 = [383.5, 46]
rr12 = [405, 25]
rr13 = [410, 48]

RegionsList = [rr1, rr2, rr3, rr4, rr5, rr6, rr7, rr8, rr9, rr10, rr11, rr12, rr13]

datt = pd.read_excel(r'C:\Users\milena.singletary\Boa Technology Inc\PFL Team - Documents\General\Testing Segments\EndurancePerformance\TrailRun_2022\TrailRunQualFootRegions.xlsx', 'Sheet4')


# using datt DataFrame
PFS = pd.DataFrame(datt[datt['Config'] == 'PFS'])
Lace = pd.DataFrame(datt[datt['Config'] == 'Lace'])
count= pd.DataFrame(PFS.sum(0)[2:25])
count.columns = ['PFS']
count['Lace'] = pd.DataFrame(Lace.sum(0)[2:25])
count['Count'] = count.sum(1)
count = count.T

count['Total'] = count.sum(1)
count2 = np.array(count)
count3 = count.drop('Total', axis=1)
# plot frequency
count3.T.plot.bar(use_index = True)

diff = count.diff(periods = - 1)


# positive is PFS, negative Lace
diffList = []
for ii in range(len(diff.axes[1])):
    diffList.append(diff.iloc[0,ii])

#removing the total column
diffList.pop(-1)


# create circles
def createCirc (center, radius, n):
    ang = np.linspace(0,2*np.pi,n)
    x = radius*np.cos(ang) + center[0]
    y = radius*np.sin(ang) + center[1] 
    return x, y

# xx , yy = createCirc(region1, 10, 20)

# setting alpha with freq
length = np.shape(count)[1]
alph_PFS = []
alph_L = []
for i in range(length-1):
    alpha = count2[0,i]/ count2[0,length-1]
    alph_PFS.append(alpha)
    alpha = count2[1,i]/ count2[1,length-1]
    alph_L.append(alpha)


# determining face color based on difference
fc = []
alph = []
for ii, val in enumerate(diffList):
    if val > 0:
        fc.append('b')
        alph.append(abs(val)/((max(diffList)-min(diffList))/0.8))
    if val < 0:
        fc.append('r')
        alph.append(abs(val)/((max(diffList)-min(diffList))/0.8))
    if val == 0:
        fc.append('purple')
        alph.append(0)


# Difference plot PFS blue, Lace Red
plt.figure(2)
ax = plt.gca()
ax.axes.xaxis.set_visible(False)
ax.axes.yaxis.set_visible(False)
for ind, reg in enumerate(RegionsList):
    xx, yy = createCirc(reg, 10, 20)
    plt.fill_between(xx,yy,facecolor = fc[ind], alpha = alph[ind])
    plt.title('Qualitative Feedback')
    laceleg = mpatches.Patch(color = 'red' , label = 'Lace')
    PFSleg = mpatches.Patch(color = 'blue' , label = 'PFS')
    plt.legend(handles = [laceleg, PFSleg], loc = 2)
plt.imshow(im)




# PFS
plt.figure(3)
ax = plt.gca()
# ax.axis('off')
ax.axes.xaxis.set_visible(False)
ax.axes.yaxis.set_visible(False)
for ind, reg in enumerate(RegionsList):
    xx, yy = createCirc(reg, 10, 20)
    alpha = alph_PFS[ind]
    plt.fill_between(xx,yy,facecolor = 'b', alpha = alpha * (1/(max(alph_PFS)/0.8)))
    plt.title('PFS')
plt.imshow(im)

# Lace
plt.figure(4)
ax = plt.gca()
# ax.axis('off')
ax.axes.xaxis.set_visible(False)
ax.axes.yaxis.set_visible(False)
for ind, reg in enumerate(RegionsList):
    xx, yy = createCirc(reg, 10, 20)
    alpha = alph_L[ind]
    plt.fill_between(xx,yy,facecolor = 'r', alpha = alpha * (1/(max(alph_L)/0.8)))
    plt.title('Lace')
plt.imshow(im)










