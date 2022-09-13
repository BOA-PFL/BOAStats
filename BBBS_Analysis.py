# -*- coding: utf-8 -*-
"""
Created on Thu Jul 21 10:15:30 2022

@author: Kate.Harrison
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os

# Define constants and options
fThresh = 80 #below this value will be set to 0.

# Read in balance file


fPath = 'C:/Users/kate.harrison/Boa Technology Inc/PFL - Documents/General/Testing Segments/AgilityPerformanceData/CPDMech_PanelLength_June2022/Overground/'
fileExt = r".txt"
entries = [fName for fName in os.listdir(fPath) if fName.endswith(fileExt)]



def findLandings(force, fThresh):
    """
    This function finds the landings from force plate data
    it uses a heuristic to determine landings from when the smoothed force is
    0 and then breaches a threshold
    
    Parameters
    ----------
    force : Pandas Series
        Vertical force from force plate.
    fThresh: integer
        Value force has to be greater than to count as a takeoff/landing

    Returns
    -------
    lic : list
        Indices of landings.

    """
    lic = [] 
    
    for step in range(len(force)-1):
    
            
            if force[step] == 0 and force[step + 1] >= fThresh :
                lic.append(step)
        
    return lic



def findTakeoffs(force, fThresh):
    """
    This function calculates the takeoffs using a heuristic 

    Parameters
    ----------
    force : Pandas Series
        vertical force from force plate.
    
    fThresh: integer
        Value force has to be greater than to count as a takeoff/landing
    Returns
    -------
    lto : list
        indices of takeoffs obtained from force data. Takeoffs here mean
        the moment a force signal was > a threshold and then goes to 0

    # """
    lto = []
    for step in range(len(force)-1):
        if force[step] >= fThresh and force[step + 1] == 0 :
            lto.append(step + 1)
    
    return lto 

    # lto = []
    # for step in range(len(force)-1):
    #     if force[step] >= fThresh and force[step + 1] == 0 and force[step + 5] == 0 and force[step + 10] == 0:
    #         lto.append(step + 1)
    # return lto 


def delimitTrial(inputDF):

    """
      This function uses ginput to delimit the start and end of a trial
    You will need to indicate on the plot when to start/end the trial. 
    You must use tkinter or plotting outside the console to use this function
    Parameters
    ----------
    inputDF : Pandas DataFrame
        DF containing all desired output variables.
    zForce : numpy array 
        of force data from which we will subset the dataframe

    Returns
    -------
    outputDat: dataframe subset to the beginning and end of jumps.

    """

    # generic function to plot and start/end trial #
    fig, ax = plt.subplots()
    totForce = dat.FP1_GRF_Z + dat.FP2_GRF_Z + dat.FP3_GRF_Z + dat.FP4_GRF_Z
    ax.plot(totForce, label = 'Total Force')
    fig.legend()
    pts = np.asarray(plt.ginput(2, timeout=-1))
    plt.close()
    outputDat = dat.iloc[int(np.floor(pts[0,0])) : int(np.floor(pts[1,0])),:]
    outputDat = outputDat.reset_index()
    return(outputDat)



CT = []
peakGRFz = []
peakGRFx = []

subName = []
config = []
movements = []


## save configuration names from files
for fName in entries:
    try:
        
        #fName = entries[1]

        
        config1 = fName.split('_')[1]
        tmpMove = fName.split('_')[2]

        dat = pd.read_csv(fPath+fName,sep='\t', skiprows = 8, header = 0)
        dat = dat.fillna(0)
        
        
        landings = [] # erase landings and takeoffs from last loop
        takeoffs = []
        if (tmpMove == 'Skater') or (tmpMove == 'skater'):
            
            dat = delimitTrial(dat)
            # create vector of force from vertical signal from each file and make low values 0
            if np.max(abs(dat.FP3_GRF_Z)) > np.max(abs(dat.FP4_GRF_Z)):

                ZForce = dat.FP3_GRF_Z
                XForce = dat.FP3_GRF_X
                
            else:
                ZForce = dat.FP4_GRF_Z
                XForce = dat.FP4_GRF_X 

                
            if abs(np.min(XForce)) > abs(np.max(XForce)):
                XForce = XForce * -1
            
            #dat = delimitTrialSkate(dat)
            ZForce[ZForce<fThresh] = 0
            
            #find the landings from function above
            landings = findLandings(ZForce)
            takeoffs = findTakeoffs(ZForce)
            
            landings[:] = [x for x in landings if x < takeoffs[-1]]
            takeoffs[:] = [x for x in takeoffs if x > landings[0]]
            
         
        elif (tmpMove == 'CMJ') or (tmpMove == 'cmj'):
            
            dat = delimitTrial(dat)
        
            ZForce = dat.FP2_GRF_Z
            ZForce[ZForce<fThresh] = 0
            
            XForce = dat.FP2_GRF_X 
          
            landings = findLandings(ZForce, fThresh )
            takeoffs = findTakeoffs(ZForce, fThresh)
           
            landings[:] = [x for x in landings if x < takeoffs[-1]]
            takeoffs[:] = [x for x in takeoffs if x > landings[0]] 
             
            
        else:
            print('this movement is not included in Performance Test Analysis')
        
        
        for i in range(len(landings)):

            try:
                
                #i = 0
                CT.append((takeoffs[i] - landings[i])/200)
                
                peakGRFz.append(np.max(ZForce[landings[i]:takeoffs[i]]))
                peakGRFx.append(np.max(XForce[landings[i]:takeoffs[i]]))
        
                subName.append(fName.split('_')[0])
                config.append( config1 )
                movements.append( tmpMove )
            
            except:

                print(fName + str(i))

    except:
        print(fName)


outcomes = pd.DataFrame({'Subject':list(subName), 'Config': list(config), 'Movement':list(movements),'CT':list(CT), 'peakGRF_Z':list(peakGRFz), 'peakGRF_X':list(peakGRFx) })                  

outfileName = fPath + 'CompiledAgilityData.csv'
outcomes.to_csv(outfileName, index = False)





