### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/AgilitySpeedDB.csv',nrows=1)
ParentDat <- ParentDat %>%
  rename('Subject' = ï..Subject)
name_order = colnames(ParentDat)

# Read and summarize the overground data:
AgilityDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/AgilityPerformanceData/CPDMech_ForefootFit_July2022/Overground/CompiledAgilityData.csv')

CMJDat <- AgilityDat %>%
  filter(Movement == 'CMJ') %>%
  group_by(Subject, Config, Movement) %>%
  summarise(ContactTime = mean(CT), PeakAnklePFMoment = mean(peakPFmom), PropForce = mean(peakGRF_Z),
            PeakAnkleInMoment = mean(peakINVmom), KneeAbAdROM = mean(kneeABDrom), COMEccWork = mean(eccWork))

SKTDat <- AgilityDat %>%
  filter(Movement == 'Skater') %>%
  group_by(Subject, Config, Movement) %>%
  summarise(ContactTime = mean(CT), PeakAnklePFMoment = mean(peakPFmom), PropForce = mean(peakGRF_X),
            PeakAnkleInMoment = mean(peakINVmom), KneeAbAdROM = mean(kneeABDrom), COMEccWork = mean(eccWork))

ChildDat <- merge(x=CMJDat,y=SKTDat,all=TRUE)
ChildDat$Year <- rep(2022, dim(ChildDat)[1])
ChildDat$Month <- rep('May', dim(ChildDat)[1])
ChildDat$Brand <- rep('nobull', dim(ChildDat)[1])
ChildDat$Model <- rep('Trainer', dim(ChildDat)[1])

#_______________________________________________________________________________
# Place NaNs for missing data
ChildDat$COMConWork <- rep('NA', dim(ChildDat)[1])
ChildDat$PeakKneeAbMoment <- rep('NA', dim(ChildDat)[1])
# ChildDat$PeakAnklePFMoment <- rep('NA', dim(ChildDat)[1])
# ChildDat$PropForce <- rep('NA', dim(ChildDat)[1])
# ChildDat$PeakAnkleInMoment <- rep('NA', dim(ChildDat)[1])
# ChildDat$KneeAbAdROM <- rep('NA', dim(ChildDat)[1])
# ChildDat$COMEccWork <- rep('NA', dim(ChildDat)[1])
#_______________________________________________________________________________

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/AgilitySpeedDB.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
}

