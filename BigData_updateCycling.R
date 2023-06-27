### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:/Users/kate.harrison/Boa Technology Inc/PFL Team - General/BigData/DB_V2/CyclingPowerDB_V2.csv',nrows=1)
ParentDat <- ParentDat %>%
  rename('Subject' = ï..Subject)
name_order = colnames(ParentDat)

# Read the Watt Bike Data:
CycleDat <- read.csv(file.choose())
# If any configurations need to be renamed
CycleDat <- CycleDat
  # mutate(Subject= replace(Subject, Subject == 'RobinFassett','RobinFassettCarman'))

# Read and summarize the steady-state:
PressDat <- read.csv(file.choose())

PressSteadyDat <- PressDat %>%
  filter(Movement == 'Steady') %>%
  group_by(Subject, Config) %>%
  summarize(HeelContact_steady = mean(heelAreaP, na.rm = TRUE), PeakToePress_steady = mean(maxmaxToes, na.rm = TRUE))
  # mutate(Subject= replace(Subject, Subject == 'EricHoner','EricHonert')) %>%
  # mutate(Subject= replace(Subject, Subject == 'AmeliaShea','AmeliaShae')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Matt Kjowski','MattKijowski')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Matt Kijowski','MattKijowski')) %>%
  # mutate(Subject= replace(Subject, Subject == 'RobinFerrettCarman','RobinFassettCarman')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Nick Roosen','NickRoosen'))

#   mutate(Config= replace(Config, Config == 'LRHL','HL'))  %>%


PressSprintDat <- PressDat %>%
  group_by(Subject, Config) %>%
  summarize(HeelContact_sprint = mean(heelAreaP, na.rm = TRUE), PeakToePress_sprint = mean(maxmaxToes, na.rm = TRUE))
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

ChildDat$Year <- rep(2023, dim(ChildDat)[1])
ChildDat$Month <- rep('June', dim(ChildDat)[1])
ChildDat$Brand <- rep('Giro', dim(ChildDat)[1])
ChildDat$Model <- rep('Regime', dim(ChildDat)[1])
#ChildDat <- subset(ChildDat,select = -c(Trial))

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'C:/Users/kate.harrison/Boa Technology Inc/PFL Team - General/BigData/DB_V2/CyclingPowerDB_V2.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}
