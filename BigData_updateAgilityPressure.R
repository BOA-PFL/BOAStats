### Load libraries

library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)

rm(list=ls())

### Import Parent Data

ParentDat <- read.csv('C:/Users/kate.harrison/Boa Technology Inc/PFL Team - General/BigData/DB_V2/AgilityPressureDB.csv',nrows=1)

name_order = colnames(ParentDat)

### Import Dynamic Pressure data

ChildDat <- read.csv(file.choose())

ChildDat <- subset(ChildDat, ChildDat$Movement == ('cmj')|ChildDat$Movement == ('skater'))

ChildDat <- ChildDat %>%
  group_by(Subject, Config, Movement) %>%
  summarise(ContactTime = mean(ContactTime), PeakToePressure = mean(maxmaxToes), HeelContactArea = mean(heelAreaP), LateralProportionForce = mean(latPropMid), 
            DorsalVariation = mean(dorsalVar), PeakDorsalPressure = mean(maxDorsalP), MeanFFpressure = mean(ffDorsalMidP), MeanMFpressure = mean(mfDorsalMidP), 
            meanInsteppressure = mean(instepMidP), maxFFpressure = mean(ffDorsalMax), maxMFpressure = mean(mfDorsalMax), maxInsteppressure = mean(instepMax))

#staticDat$Subject[stati
ChildDat$Year <- rep(2023, dim(ChildDat)[1])
ChildDat$Month <- rep('January', dim(ChildDat)[1])
ChildDat$Brand <- rep('NoBull', dim(ChildDat)[1])
ChildDat$Model <- rep('Trainer', dim(ChildDat)[1]) 
#childDat$Subject == 'WesWebber'] <- 'WesWeber'

ChildDat <- ChildDat[,name_order]


a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'C:/Users/kate.harrison/Boa Technology Inc/PFL Team - General/BigData/DB_V2/AgilityPressureDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}




