### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/WalkRunDB.csv',nrows=1)
ParentDat <- ParentDat %>%
  rename('Subject' = ï..Subject)
name_order = colnames(ParentDat)

# Read and summarize the Pressure Data:
PressDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/AgilityPerformanceData/CPD_TongueLocatedDial_Oct2022/Xsensor/CompiledPressureData_allwalking.csv')
PressDat$Subject <- gsub(" ", "", PressDat$Subject) # remove spaces in names

PressDat <- PressDat %>%
  filter(Side == 'Right') %>%
  group_by(Subject, Config) %>%
  summarize(PeakToePress = mean(maxmaxToes, na.rm = TRUE), HeelContact = mean(heelAreaP, na.rm = TRUE))
  # mutate(Subject= replace(Subject, Subject == 'Olivia','OliviaBojan'))

# Read and summarize other treadmill data:
TreadDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/AgilityPerformanceData/CPD_TongueLocatedDial_Oct2022/Treadmill/TreadmillOutcomes.csv')
TreadDat[TreadDat == Inf] <- NA
TreadDat$Slope <- as.character(TreadDat$Slope)

TreadDat <- TreadDat %>%
  group_by(Subject, Config, Speed, Slope) %>%
  summarize(LoadingRate = mean(VALR, na.rm = TRUE), PosCOMWork = mean(COMWork_pos), NegCOMWork = mean(COMWork_neg)) 
  # mutate(Subject= replace(Subject, Subject == 'Amanda','AmandaBuchholtz'))

ChildDat <- list(PressDat,TreadDat) %>%
  reduce(full_join)

# Append other necessary information to the DataFrame
ChildDat$Speed <- rep(3.0, dim(ChildDat)[1])
ChildDat$Slope <- rep(0, dim(ChildDat)[1])
ChildDat$Year <- rep(2022, dim(ChildDat)[1])
ChildDat$NegFootWork <- rep(NA, dim(ChildDat)[1])
ChildDat$PosAnkleWork <- rep(NA, dim(ChildDat)[1])
ChildDat$PeakAnkleEvVel <- rep(NA, dim(ChildDat)[1])
ChildDat$Month <- rep('October', dim(ChildDat)[1])
ChildDat$Brand <- rep('Nobull', dim(ChildDat)[1])
ChildDat$Model <- rep('Trainer', dim(ChildDat)[1])

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/WalkRunDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
} 
