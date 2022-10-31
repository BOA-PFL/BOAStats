### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/CyclingPowerDB_V2.csv',nrows=1)
ParentDat <- ParentDat %>%
  rename('Subject' = ï..Subject)
name_order = colnames(ParentDat)

# Read the Watt Bike Data:
CycleDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Cycling Performance Tests/CyclingDD_Jan2022/WattBike/CompiledPowerData.csv')
# If any configurations need to be renamed
CycleDat <- CycleDat
  # mutate(Subject= replace(Subject, Subject == 'RobinFassett','RobinFassettCarman'))

# Read and summarize the steady-state:
PressSteadyDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Cycling Performance Tests/CyclingDD_Jan2022/XSENSOR Data/TestDataSteadyPressureData_DDJan22.csv')

PressSteadyDat <- PressSteadyDat %>%
  group_by(Subject, Config, Trial) %>%
  summarize(HeelContact_steady = mean(Steady_HeelContactArea, na.rm = TRUE), PeakToePress_steady = mean(PeakToePressure, na.rm = TRUE))
  # mutate(Subject= replace(Subject, Subject == 'EricHoner','EricHonert')) %>%
  # mutate(Subject= replace(Subject, Subject == 'AmeliaShea','AmeliaShae')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Matt Kjowski','MattKijowski')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Matt Kijowski','MattKijowski')) %>%
  # mutate(Subject= replace(Subject, Subject == 'RobinFerrettCarman','RobinFassettCarman')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Nick Roosen','NickRoosen'))

#   mutate(Config= replace(Config, Config == 'LRHL','HL'))  %>%

# Read and summarize the sprint:
PressSprintDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Cycling Performance Tests/CyclingDD_Jan2022/XSENSOR Data/TestDataSprintPressureData_DDJan22.csv')

PressSprintDat <- PressSprintDat %>%
  group_by(Subject, Config, Trial) %>%
  summarize(HeelContact_sprint = mean(Sprint_HeelContactArea, na.rm = TRUE), PeakToePress_sprint = mean(PeakToePressure, na.rm = TRUE))
  # mutate(Subject= replace(Subject, Subject == 'EricHoner','EricHonert')) %>%
  # mutate(Subject= replace(Subject, Subject == 'AmeliaShea','AmeliaShae')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Matt Kjowski','MattKijowski')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Matt Kijowski','MattKijowski')) %>%
  # mutate(Subject= replace(Subject, Subject == 'RobinFerrettCarman','RobinFassettCarman')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Nick Roosen','NickRoosen'))


#   mutate(Config= replace(Config, Config == 'LRHL','HL')) %>%
#   mutate(Subject= replace(Subject, Subject == 'Brendan','BrendanLee')) %>%
#   mutate(Subject= replace(Subject, Subject == 'Bredan','BrendanLee')) %>%
#   mutate(Subject= replace(Subject, Subject == 'RobinFassetCarman','RobinFassettCarman')) %>%
#   mutate(Subject= replace(Subject, Subject == 'RobinFassetCarman','RobinFassettCarman')) %>%
#   mutate(Subject= replace(Subject, Subject == 'Conrad','ConradMcCarthy'))

# Combine IMU and pressure data
ChildDat <- list(CycleDat,PressSteadyDat,PressSprintDat) %>%
  reduce(full_join)

ChildDat$Year <- rep(2022, dim(ChildDat)[1])
ChildDat$Month <- rep('January', dim(ChildDat)[1])
ChildDat$Brand <- rep('Giro', dim(ChildDat)[1])
ChildDat$Model <- rep('Regime', dim(ChildDat)[1])
ChildDat <- subset(ChildDat,select = -c(Trial))

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]


# Check the Child Data before!!
write.table(ChildDat, file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/CyclingPowerDB_V2.csv', sep = ',',
            append = TRUE,col.names = FALSE, row.names = FALSE)
