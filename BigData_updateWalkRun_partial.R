### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/WalkRunDB.csv',nrows=1)
ParentDat <- ParentDat %>%
  rename('Subject' = ï..Subject)
name_order = colnames(ParentDat)

# Read and summarize the Pressure Data:
PressDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/WorkWear_Performance/Elten_Workwear_EltenLowCut_Performance_Feb22/Pressure/walk/CompiledPressureData.csv')

PressDat <- PressDat %>%
  # filter(Side == 'Left') %>%
  # rename('Slope' = Condition) %>%
  group_by(Subject, Config) %>%
  summarize(PeakToePress = mean(maxmaxToes, na.rm = TRUE)) %>% #, HeelContact = mean(meanHeelContactArea, na.rm = TRUE)) %>%
  mutate(Subject= replace(Subject, Subject == 'Dan','DanFeeney')) %>%
  mutate(Subject= replace(Subject, Subject == 'LorenBrintion','LorenBrinton')) %>%
  mutate(Subject= replace(Subject, Subject == 'Toby Beck','TobyBeck'))

  # mutate(Slope= replace(Slope, Slope == 'Walking6deg', -6)) %>%
  # mutate(Slope= replace(Slope, Slope == 'Walking', 0))

# Read and summarize the Kinematics Data:
AnkKinematics <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/WorkWear_Performance/Workwear_Jalas_Performance_Aug22/Kinematics.csv')

AnkKinematics <- AnkKinematics %>%
  rename('Slope' = Cond) %>%
  group_by(Subject, Config, Slope) %>%
  summarize(PeakAnkleEvVel = mean(pAnkEvVel)) %>%
  mutate(Slope= replace(Slope, Slope == 'Downhill', -6)) %>%
  mutate(Slope= replace(Slope, Slope == 'Level', 0)) %>%
  mutate(Subject= replace(Subject, Subject == 'Ryan','RyanRopken')) %>%
  mutate(Subject= replace(Subject, Subject == 'Marco','MarcoArmiraglio')) %>%
  mutate(Subject= replace(Subject, Subject == 'Roland','RolandRotheneder')) %>%
  mutate(Subject= replace(Subject, Subject == 'Emiliano','EmilianoFalcon'))

# Read and summarize other treadmill data:
TreadDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/WorkWear_Performance/Elten_Workwear_EltenLowCut_Performance_Feb22/Treadmill/GeneralExport/TreadmillOutcomes.csv')
TreadDat[TreadDat == Inf] <- NA
TreadDat$Slope <- as.character(TreadDat$Slope)

TreadDat <- TreadDat %>%
  group_by(Subject, Config, Speed, Slope) %>%
  summarize(LoadingRate = mean(VALR, na.rm = TRUE), PosCOMWork = mean(COMWork_pos), NegCOMWork = mean(COMWork_neg)) %>%
  mutate(Subject= replace(Subject, Subject == 'TaylorDitallo','TJDitallo'))
    
  # mutate(Subject= replace(Subject, Subject == 'Marco','MarcoArmiraglio')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Roland','RolandRotheneder')) %>%
  # mutate(Subject= replace(Subject, Subject == 'Emiliano','EmilianoFalcon'))

ChildDat <- list(PressDat,TreadDat) %>%
  reduce(full_join)

# Append other necessary information to the DataFrame
ChildDat$Speed <- rep(1.2, dim(ChildDat)[1])
ChildDat$Slope <- rep(0, dim(ChildDat)[1])
ChildDat$Year <- rep(2022, dim(ChildDat)[1])
ChildDat$HeelContact <- rep(NA, dim(ChildDat)[1])
ChildDat$NegFootWork <- rep(NA, dim(ChildDat)[1])
ChildDat$PosAnkleWork <- rep(NA, dim(ChildDat)[1])
ChildDat$PeakAnkleEvVel <- rep(NA, dim(ChildDat)[1])
ChildDat$Month <- rep('January', dim(ChildDat)[1])
ChildDat$Brand <- rep('Elten', dim(ChildDat)[1])
ChildDat$Model <- rep('Maddox', dim(ChildDat)[1])

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/WalkRunDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
} 
