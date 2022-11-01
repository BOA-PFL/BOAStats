### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/LandingDB.csv',nrows=1)
ParentDat <- ParentDat %>%
  rename('Subject' = ï..Subject)
name_order = colnames(ParentDat)

# Read and summarize the overground data:
LandingDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/FocusAnkleDualDial_Midcut_Sept2022/TrailStabilize.csv')

# Single Leg Landing (SLL) data only
ChildDat <- LandingDat %>%
  group_by(Subject, Config) %>%
  summarize(Time2Stabilize = mean(StabalizeTime, na.rm = TRUE))

ChildDat$Movement <- rep('SLLt', dim(ChildDat)[1])
ChildDat$Year <- rep(2022, dim(ChildDat)[1])
ChildDat$Month <- rep('September', dim(ChildDat)[1])
ChildDat$Brand <- rep('Tecnica', dim(ChildDat)[1])
ChildDat$Model <- rep('Forge GTX', dim(ChildDat)[1])

#_______________________________________________________________________________
# Place NaNs for missing biomechanical data
ChildDat$KneeAbAdROM <- rep('NA', dim(ChildDat)[1])
ChildDat$PeakKneeAbMoment <- rep('NA', dim(ChildDat)[1])
#_______________________________________________________________________________
# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/LandingDB.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
}
