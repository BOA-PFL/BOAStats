### Load libraries

library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)

rm(list=ls())

## Update Agility Pressure 


### Import Parent Data
ParentDat <- read.csv('Z:/BigData/DB_V2/AgilityPressureDB.csv',nrows=1)

name_order = colnames(ParentDat)

### Import Dynamic Pressure data

ChildDat <- read.csv('Z:\\Testing Segments\\AgilityPerformanceData\\2025_Mechanistic_WomensPanelStiffnessII_Adidas\\Xsensor\\cropped\\1_CompiledResults.csv')

ChildDat <- subset(ChildDat, ChildDat$Movement == ('cmj')|ChildDat$Movement == ('skater'))

ChildDat <- ChildDat %>%
  group_by(Subject, Config, Order, Movement) %>%
  summarise_all(mean)

#staticDat$Subject[stati
noSub <- length(ChildDat$Subject)
ChildDat$Year <- rep(2025, dim(ChildDat)[1])
ChildDat$Month <- rep('May', dim(ChildDat)[1])
ChildDat$Brand <- rep('Adidas', dim(ChildDat)[1])
ChildDat$Model <- rep('Barricade', dim(ChildDat)[1]) 
ChildDat$toeArea_mid <- rep('NA', each = noSub)
ChildDat$ffP_late <- rep('NA', each = noSub)
ChildDat$ffArea_late <- rep('NA', each = noSub)
ChildDat$ffP_Mid <- rep('NA', each = noSub)
ChildDat$ffArea_Mid <- rep('NA', each = noSub)
ChildDat$ffPMax_late <- rep('NA', each = noSub)

ChildDat$mfP_late <- rep('NA', each = noSub)
ChildDat$mfArea_late <- rep('NA', each = noSub)
ChildDat$mfP_Mid <- rep('NA', each = noSub)
ChildDat$mfArea_Mid <- rep('NA', each = noSub)
ChildDat$latP_late <- rep('NA', each = noSub)
ChildDat$latArea_late <- rep('NA', each = noSub)


ChildDat$medP_late <- rep('NA', each = noSub)
ChildDat$medArea_late <- rep('NA', each = noSub)

ChildDat$ffDorsalEarlyP <- rep('NA', each = noSub)
ChildDat$ffDorsalMidP <- rep('NA', each = noSub)
ChildDat$ffDorsalLateP <- rep('NA', each = noSub)
ChildDat$mfDorsalEarlyP <- rep('NA', each = noSub)
ChildDat$mfDorsalMidP <- rep('NA', each = noSub)
ChildDat$mfDorsalLateP <- rep('NA', each = noSub)
ChildDat$instepEarlyP <- rep('NA', each = noSub)
ChildDat$instepMidP <- rep('NA', each = noSub)
ChildDat$instepLateP <- rep('NA', each = noSub)
ChildDat$ffDorsalMax <- rep('NA', each = noSub)
ChildDat$mfDorsalMax <- rep('NA', each = noSub)
ChildDat$instepMax <- rep('NA', each = noSub)

ChildDat <- ChildDat[,name_order]


a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'Z:/BigData/DB_V2/AgilityPressureDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}

rm(ParentDat,ChildDat,name_order,a)


