### Appending new biomech data to big data ###
library(tidyverse)
library(readxl)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/TrailDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ï..Subject)
name_order = colnames(ParentDat)

# Read and summarize the IMU Data:
IMUDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/ZonalFit_Midcut_Aug2022/IMU/CompiledIMUResults_trail.csv')

IMUDat <- IMUDat %>%
  group_by(Subject, Config, Movement) %>%
  summarize(IMUSpeed = mean(imuSpeed), PeakEvVel = mean(pGyr), PeakJerk = mean(pJerk),
            PeakAcc = mean(pAcc),RangeMLAcc = mean(rMLacc)) 

# Read and summarize the pressure data
PressDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/ZonalFit_Midcut_Aug2022/XSENSOR/CompiledPressureDataHeelArea_Treadmill_Trail.csv')

PressDat <- PressDat %>%
  filter(Movement == 'Trail') %>%
  group_by(Subject, Config) %>%
  summarize(HeelContact = mean(heelArea/31.4*100), PeakToePress = mean(maxmaxToes)) %>%
  mutate(Config= replace(Config, Config == '2D','DD')) %>%
  mutate(Config= replace(Config, Config == 'BD','LZ')) %>%
  mutate(Config= replace(Config, Config == 'TD','UZ'))

# Combine IMU and pressure data
ChildDat <- merge(x=IMUDat,y=PressDat,all=TRUE)

#_______________________________________________________________________________
# Append other necessary information to the DataFrame
ChildDat$Year <- rep(2022, dim(ChildDat)[1])
ChildDat$Month <- rep('September', dim(ChildDat)[1])
ChildDat$Brand <- rep('Tecnica', dim(ChildDat)[1])
ChildDat$Model <- rep('Forge GTX', dim(ChildDat)[1])

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

write.table(ChildDat, file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/TrailDB.csv', sep = ',',
           append = TRUE,col.names = FALSE, row.names = FALSE)

