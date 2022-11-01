### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/LandingDB.csv',nrows=1)
ParentDat <- ParentDat %>%
  rename('Subject' = ï..Subject)
name_order = colnames(ParentDat)

# Read and summarize the overground data:
LandingDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/ZonalFit_Midcut_Aug2022/Overground/CompiledSLLData_KneeMoments.csv')
LandingDat <- LandingDat %>%
  mutate(Config = replace(Config, Config == 'BD','LZ')) %>%
  mutate(Config = replace(Config, Config == 'TD','UZ')) %>%
  mutate(Config = replace(Config, Config == '2D','NoFA'))
#   mutate(Movement= replace(Movement, Movement == 'SLhops','SLH'))
  # rename('Movement' = Task)

# Single Leg Landing (SLL) data only
SLLDat <- LandingDat %>%
  filter(Movement == 'SLL') %>%
  group_by(Subject, Config, Movement) %>%
  summarize(Time2Stabilize = mean(StabTime/200, na.rm = TRUE), PeakKneeAbMoment = mean(LkneeABDMom))

# Single Leg Hop (SLH) data only
SLHDat <- LandingDat %>%
  filter(Movement == 'SLH') %>%
  group_by(Subject, Config, Movement) %>%
  summarize(Time2Stabilize = mean(StabTime/100, na.rm = TRUE))#, PeakKneeAbMoment = mean(KneeABDmoment))

ChildDat <- SLLDat
ChildDat <- merge(x=SLLDat,y=SLHDat,all=TRUE)
ChildDat$Year <- rep(2022, dim(ChildDat)[1])
ChildDat$Month <- rep('August', dim(ChildDat)[1])
ChildDat$Brand <- rep('Tecnica', dim(ChildDat)[1])
ChildDat$Model <- rep('Forge GTX', dim(ChildDat)[1])

#_______________________________________________________________________________
# Place NaNs for missing data
ChildDat$KneeAbAdROM <- rep('NA', dim(ChildDat)[1])
# ChildDat$PeakKneeAbMoment <- rep('NA', dim(ChildDat)[1])
#_______________________________________________________________________________
# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/LandingDB.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
}
