# import required packages

import numpy as np
from matplotlib import pyplot as plt
import os
import spm1d



# Change directory to the one with your data
os.chdir('C:/Users/kate.harrison/Documents/NoviceRunnersTrainingStudy/SPM')


grp1 = 'COMPLETE' # change to name for grp1
grp2 = 'DROPOUT' # chagne to name for grp2
# Load data for spm analysis (subject x time)

grp1_A_X = np.loadtxt('a_x'+grp1+'.txt')
grp1_A_Y = np.loadtxt('a_y'+grp1+'.txt')
grp1_A_Z = np.loadtxt('a_z'+grp1+'.txt')

grp1_K_X = np.loadtxt('k_x'+grp1+'.txt')
grp1_K_Y = np.loadtxt('k_y'+grp1+'.txt')
grp1_K_Z = np.loadtxt('k_z'+grp1+'.txt')

grp1_H_X = np.loadtxt('h_x'+grp1+'.txt')
grp1_H_Y = np.loadtxt('h_y'+grp1+'.txt')
grp1_H_Z = np.loadtxt('h_z'+grp1+'.txt')

grp2_A_X = np.loadtxt('a_x'+grp2+'.txt')
grp2_A_Y = np.loadtxt('a_y'+grp2+'.txt')
grp2_A_Z = np.loadtxt('a_z'+grp2+'.txt')

grp2_K_X = np.loadtxt('k_x'+grp2+'.txt')
grp2_K_Y = np.loadtxt('k_y'+grp2+'.txt')
grp2_K_Z = np.loadtxt('k_z'+grp2+'.txt')

grp2_H_X = np.loadtxt('h_x'+grp2+'.txt')
grp2_H_Y = np.loadtxt('h_y'+grp2+'.txt')
grp2_H_Z = np.loadtxt('h_z'+grp2+'.txt')

#Plot group means and SD
plt.figure()
spm1d.plot.plot_mean_sd(grp1_A_X, linecolor='y', facecolor=(0.7,0.7,1), edgecolor='y', label=grp1)
spm1d.plot.plot_mean_sd(grp2_A_X, linecolor='r', facecolor=(0.7,0.7,1), edgecolor='r', label=grp2)

plt.figure()
spm1d.plot.plot_mean_sd(grp1_A_Y, linecolor='y', facecolor=(0.7,0.7,1), edgecolor='y', label=grp1)
spm1d.plot.plot_mean_sd(grp2_A_Y, linecolor='r', facecolor=(0.7,0.7,1), edgecolor='r', label=grp2)

plt.figure()
spm1d.plot.plot_mean_sd(grp1_A_Z, linecolor='y', facecolor=(0.7,0.7,1), edgecolor='y', label=grp1)
spm1d.plot.plot_mean_sd(grp2_A_Z, linecolor='r', facecolor=(0.7,0.7,1), edgecolor='r', label=grp2)


plt.figure()
spm1d.plot.plot_mean_sd(grp1_K_X, linecolor='y', facecolor=(0.7,0.7,1), edgecolor='y', label=grp1)
spm1d.plot.plot_mean_sd(grp2_K_X, linecolor='r', facecolor=(0.7,0.7,1), edgecolor='r', label=grp2)

plt.figure()
spm1d.plot.plot_mean_sd(grp1_K_Y, linecolor='y', facecolor=(0.7,0.7,1), edgecolor='y', label=grp1)
spm1d.plot.plot_mean_sd(grp2_K_Y, linecolor='r', facecolor=(0.7,0.7,1), edgecolor='r', label=grp2)

plt.figure()
spm1d.plot.plot_mean_sd(grp1_K_Z, linecolor='y', facecolor=(0.7,0.7,1), edgecolor='y', label=grp1)
spm1d.plot.plot_mean_sd(grp2_K_Z, linecolor='r', facecolor=(0.7,0.7,1), edgecolor='r', label=grp2)


plt.figure()
spm1d.plot.plot_mean_sd(grp1_H_X, linecolor='y', facecolor=(0.7,0.7,1), edgecolor='y', label=grp1)
spm1d.plot.plot_mean_sd(grp2_H_X, linecolor='r', facecolor=(0.7,0.7,1), edgecolor='r', label=grp2)

plt.figure()
spm1d.plot.plot_mean_sd(grp1_H_Y, linecolor='y', facecolor=(0.7,0.7,1), edgecolor='y', label=grp1)
spm1d.plot.plot_mean_sd(grp2_H_Y, linecolor='r', facecolor=(0.7,0.7,1), edgecolor='r', label=grp2)

plt.figure()
spm1d.plot.plot_mean_sd(grp1_H_Z, linecolor='y', facecolor=(0.7,0.7,1), edgecolor='y', label=grp1)
spm1d.plot.plot_mean_sd(grp2_H_Z, linecolor='r', facecolor=(0.7,0.7,1), edgecolor='r', label=grp2)


# Stack data for multivariate tests

GRP1 = np.dstack((grp1_A_X, grp1_A_Y, grp1_A_Z, grp1_K_X, grp1_K_Y, grp1_K_Z, grp1_H_X, grp1_H_Y, grp1_H_Z))
GRP2 = np.dstack((grp2_A_X, grp2_A_Y, grp2_A_Z, grp2_K_X, grp2_K_Y, grp2_K_Z, grp2_H_X, grp2_H_Y, grp2_H_Z))

GRP1_X = np.dstack((grp1_A_X, grp1_K_X, grp1_H_X))
GRP2_X = np.dstack((grp2_A_X, grp2_K_X, grp2_H_X))

GRP1_Y = np.dstack((grp1_A_Y, grp1_K_Y, grp1_H_Y))
GRP2_Y = np.dstack((grp2_A_Y, grp2_K_Y, grp2_H_Y))

GRP1_AH_Y = np.dstack((grp1_A_Y, grp1_H_Y))
GRP2_AH_Y = np.dstack((grp2_A_Y, grp2_H_Y))

GRP1_Z = np.dstack((grp1_A_Z, grp1_K_Z, grp1_H_Z))
GRP2_Z = np.dstack((grp2_A_Z, grp2_K_Z, grp2_H_Z))

GRP1_AH_Z = np.dstack((grp1_A_Z, grp1_H_Z))
GRP2_AH_Z = np.dstack((grp2_A_Z, grp2_H_Z))

GRP1_A = np.dstack((grp1_A_X, grp1_A_Y, grp1_A_Z))
GRP2_A = np.dstack((grp2_A_X, grp2_A_X, grp2_A_Z))

GRP1_K = np.dstack((grp1_K_X, grp1_K_Y, grp1_K_Z))
GRP2_K = np.dstack((grp2_K_X, grp2_K_X, grp2_K_Z))

GRP1_H = np.dstack((grp1_H_X, grp1_H_Y, grp1_H_Z))
GRP2_H = np.dstack((grp2_H_X, grp2_H_X, grp2_H_Z))


# SPM comparisons 2 sample
test = spm1d.stats.hotellings2(GRP1, GRP2)
testi = test.inference(0.05)
plt.figure()
testi.plot()
print(testi)


# post-hoc

# by plane

xtest = spm1d.stats.hotellings2(GRP1_X, GRP2_X)
xtesti = xtest.inference(0.05)
plt.figure()
xtesti.plot()
print(xtesti)

ytest = spm1d.stats.hotellings2(GRP1_Y, GRP2_Y)
ytesti = ytest.inference(0.05)
plt.figure()
ytesti.plot()
print(ytesti)

ztest = spm1d.stats.hotellings2(GRP1_Z, GRP2_Z)
ztesti = ztest.inference(0.05)
plt.figure()
ztesti.plot()
print(ztesti)

# by plane ankle hip only

ahytest = spm1d.stats.hotellings2(GRP1_AH_Y, GRP2_AH_Y)
ahytesti = ahytest.inference(0.05)
plt.figure()
ahytesti.plot()
print(ahytesti)

ahztest = spm1d.stats.hotellings2(GRP1_AH_Z, GRP2_AH_Z)
ahztesti = ahztest.inference(0.05)
plt.figure()
ahztesti.plot()
print(ahztesti)

# by joint

atest = spm1d.stats.hotellings2(GRP1_A, GRP2_A)
atesti = atest.inference(0.05)
plt.figure()
atesti.plot()
print(atesti)

ktest = spm1d.stats.hotellings2(GRP1_K, GRP2_K)
ktesti = ktest.inference(0.05)
plt.figure()
ktesti.plot()
print(ktesti)

htest = spm1d.stats.hotellings2(GRP1_H, GRP2_H)
htesti = htest.inference(0.05)
plt.figure()
htesti.plot()
print(htesti)


# by joint and plane

A_Xtest = spm1d.stats.ttest2(grp1_A_X, grp2_A_X, equal_var=False)
A_Xtesti = A_Xtest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
A_Xtesti.plot()
print(A_Xtesti)

A_Ytest = spm1d.stats.ttest2(grp1_A_Y, grp2_A_Y, equal_var=False)
A_Ytesti = A_Ytest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
A_Ytesti.plot()
print(A_Ytesti)

A_Ztest = spm1d.stats.ttest2(grp1_A_Z, grp2_A_Z, equal_var=False)
A_Ztesti = A_Ztest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
A_Ztesti.plot()
print(A_Ztesti)


K_Xtest = spm1d.stats.ttest2(grp1_K_X, grp2_K_X, equal_var=False)
K_Xtesti = K_Xtest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
K_Xtesti.plot()
print(K_Xtesti)

K_Ytest = spm1d.stats.ttest2(grp1_K_Y, grp2_K_Y, equal_var=False)
K_Ytesti = K_Ytest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
K_Ytesti.plot()
print(K_Ytesti)

K_Ztest = spm1d.stats.ttest2(grp1_K_Z, grp2_K_Z, equal_var=False)
K_Ztesti = K_Ztest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
K_Ztesti.plot()
print(K_Ztesti)

H_Xtest = spm1d.stats.ttest2(grp1_H_X, grp2_H_X, equal_var=False)
H_Xtesti = H_Xtest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
H_Xtesti.plot()
print(H_Xtesti)

H_Ytest = spm1d.stats.ttest2(grp1_H_Y, grp2_H_Y, equal_var=False)
H_Ytesti = H_Ytest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
H_Ytesti.plot()
print(H_Ytesti)

H_Ztest = spm1d.stats.ttest2(grp1_H_Z, grp2_H_Z, equal_var=False)
H_Ztesti = H_Ztest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
H_Ztesti.plot()
print(H_Ztesti)


# SPM comparisons paired
test = spm1d.stats.hotellings_paired(GRP1, GRP2)
testi = test.inference(0.05)
plt.figure()
testi.plot()
print(testi)


# post-hoc

# by plane

xtest = spm1d.stats.hotellings_paired(GRP1_X, GRP2_X)
xtesti = xtest.inference(0.05)
plt.figure()
xtesti.plot()
print(xtesti)

ytest = spm1d.stats.hotellings_paired(GRP1_Y, GRP2_Y)
ytesti = ytest.inference(0.05)
plt.figure()
ytesti.plot()
print(ytesti)

ztest = spm1d.stats.hotellings_paired(GRP1_Z, GRP2_Z)
ztesti = ztest.inference(0.05)
plt.figure()
ztesti.plot()
print(ztesti)

# by plane ankle hip only

ahytest = spm1d.stats.hotellings_paired(GRP1_AH_Y, GRP2_AH_Y)
ahytesti = ahytest.inference(0.05)
plt.figure()
ahytesti.plot()
print(ahytesti)

ahztest = spm1d.stats.hotellings_paired(GRP1_AH_Z, GRP2_AH_Z)
ahztesti = ahztest.inference(0.05)
plt.figure()
ahztesti.plot()
print(ahztesti)

# by joint

atest = spm1d.stats.hotellings_paired(GRP1_A, GRP2_A)
atesti = atest.inference(0.05)
plt.figure()
atesti.plot()
print(atesti)

ktest = spm1d.stats.hotellings_paired(GRP1_K, GRP2_K)
ktesti = ktest.inference(0.05)
plt.figure()
ktesti.plot()
print(ktesti)

htest = spm1d.stats.hotellings_paired(GRP1_H, GRP2_H)
htesti = htest.inference(0.05)
plt.figure()
htesti.plot()
print(htesti)


# by joint and plane

A_Xtest = spm1d.stats.ttest_paired(grp1_A_X, grp2_A_X, equal_var=False)
A_Xtesti = A_Xtest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
A_Xtesti.plot()
print(A_Xtesti)

A_Ytest = spm1d.stats.ttest_paired(grp1_A_Y, grp2_A_Y, equal_var=False)
A_Ytesti = A_Ytest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
A_Ytesti.plot()
print(A_Ytesti)

A_Ztest = spm1d.stats.ttest_paired(grp1_A_Z, grp2_A_Z, equal_var=False)
A_Ztesti = A_Ztest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
A_Ztesti.plot()
print(A_Ztesti)


K_Xtest = spm1d.stats.ttest_paired(grp1_K_X, grp2_K_X, equal_var=False)
K_Xtesti = K_Xtest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
K_Xtesti.plot()
print(K_Xtesti)

K_Ytest = spm1d.stats.ttest_paired(grp1_K_Y, grp2_K_Y, equal_var=False)
K_Ytesti = K_Ytest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
K_Ytesti.plot()
print(K_Ytesti)

K_Ztest = spm1d.stats.ttest_paired(grp1_K_Z, grp2_K_Z, equal_var=False)
K_Ztesti = K_Ztest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
K_Ztesti.plot()
print(K_Ztesti)

H_Xtest = spm1d.stats.ttest_paired(grp1_H_X, grp2_H_X, equal_var=False)
H_Xtesti = H_Xtest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
H_Xtesti.plot()
print(H_Xtesti)

H_Ytest = spm1d.stats.ttest_paired(grp1_H_Y, grp2_H_Y, equal_var=False)
H_Ytesti = H_Ytest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
H_Ytesti.plot()
print(H_Ytesti)

H_Ztest = spm1d.stats.ttest_paired(grp1_H_Z, grp2_H_Z, equal_var=False)
H_Ztesti = H_Ztest.inference(alpha=0.05, two_tailed=True, interp=True)
plt.figure()
H_Ztesti.plot()
print(H_Ztesti)