### Appending new biomech data to big data ###
library(tidyverse)
library(readxl)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:\\Users\\adam.luftglass\\OneDrive - Boa Technology Inc\\General/BigData/DB_V2/TrailDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read and summarize the IMU Data:
IMUDat <- read.csv('C:\\Users\\adam.luftglass\\OneDrive - Boa Technology Inc\\General\\Testing Segments\\Material Testing\\UpperStiffnessE&H\\IMU\\IMUmetrics.csv')

IMUDat <- IMUDat %>%
  filter(Label > 0) %>%
  group_by(Subject, Config, Sesh, Label) %>%
  summarize(IMUSpeed = mean(imuSpeed), PeakEvVel = mean(pIEgyro), PeakJerk = mean(pJerk),
            PeakAcc = mean(pAcc),RangeMLAcc = mean(rMLacc)) 

# Read and summarize the pressure data
PressDat <- read.csv('C:\\Users\\adam.luftglass\\OneDrive - Boa Technology Inc\\General\\Testing Segments\\Material Testing\\UpperStiffnessE&H\\XSensor/PressureOutcomes.csv')

PressDat <- PressDat %>%
  filter(Label > 0) %>%
  group_by(Subject, Config, Sesh, Label) %>%
  summarize(HeelContact = mean(heelAreaP), PeakToePress = mean(ffPMax_late)) 

# Combine IMU and pressure data
ChildDat <- merge(x=IMUDat,y=PressDat,all=TRUE) %>%
  mutate(Label= replace(Label, Label == 1,'UHR')) %>%
  mutate(Label= replace(Label, Label == 2,'TR')) %>%
  mutate(Label= replace(Label, Label == 3,'DHR'))

# Create correct column name for the Moment portion
colnames(ChildDat)[which(names(ChildDat) == 'Label')] <- 'Movement'
#_______________________________________________________________________________
# Only need this step for replacing subject names
# SubNames <- read_xlsx('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/MasterListOutdoor.xlsx')
# SubNames <- SubNames %>% filter(Name != 'NA')
# SubNames <-  subset(SubNames,select = c('Subject Number','Name'))
# SubNames <- setNames(SubNames, c('Subject','Name'))
# ChildDat <- merge(x=ChildDat,y=SubNames, all = TRUE)
# ChildDat$Subject <- ChildDat$Name
# ChildDat <- subset(ChildDat,select = -c(Name, Sesh))
#_______________________________________________________________________________
# Append other necessary information to the DataFrame
ChildDat$Year <- rep(2023, dim(ChildDat)[1])
ChildDat$Month <- rep('September', dim(ChildDat)[1])
ChildDat$Brand <- rep('Speedland', dim(ChildDat)[1])
ChildDat$Model <- rep('GS Tam', dim(ChildDat)[1])

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

write.table(ChildDat, file = 'C:\\Users\\adam.luftglass\\OneDrive - Boa Technology Inc\\General/BigData/DB_V2/TrailDB.csv', sep = ',',
            append = TRUE,col.names = FALSE, row.names = FALSE)

