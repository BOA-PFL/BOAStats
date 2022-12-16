### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/WalkRunDB.csv',nrows=1)
ParentDat <- ParentDat %>%
  rename('Subject' = ï..Subject)
name_order = colnames(ParentDat)

# Read and summarize the Pressure Data:
PressDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/ZonalFit_Midcut_Aug2022/Xsensor/CompiledPressureData_allwalking.csv')
PressDat <- PressDat %>%
  rename('Slope' = Movement) %>%
  filter(Slope == 'Uphill' | Slope == 'Dowhnhill') %>%
  group_by(Subject, Config, Slope) %>%
  summarize(HeelContact = mean(heelAreaP, na.rm = TRUE), PeakToePress = mean(maxmaxToes, na.rm = TRUE)) %>%
  mutate(Config= replace(Config, Config == 'TD','UZ')) %>%
  mutate(Config= replace(Config, Config == 'BD','LZ')) %>%
  mutate(Config= replace(Config, Config == '2D','DD'))


# Read and summarize other treadmill data:
TreadDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/FocusAnkleDualDial_Midcut_Sept2022/Treadmill/TreadmillOutcomes.csv')
TreadDat[TreadDat == Inf] <- NA 
TreadDat <- TreadDat %>%
  group_by(Subject, Config, Slope) %>%
  summarize(PosCOMWork = mean(COMWork_pos), NegCOMWork = mean(COMWork_neg),
            PeakAnkleEvVel = mean(pAnkEvVel, na.rm = TRUE), NegFootWork = mean(DisWork, na.rm = TRUE),
            PosAnkleWork = mean(AnkWork_pos, na.rm = TRUE), LoadingRate = mean(VALR, na.rm = TRUE))

ChildDat <- merge(x=PressDat,y=TreadDat,all=TRUE) 




# Read and summarize the COM Data:
COMDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/ZonalFit_Midcut_Aug2022/COMWork.csv')

COMDat <- COMDat %>%
  rename('Slope' = Cond) %>%
  group_by(Subject, Config, Slope) %>%
  summarize(PosCOMWork = mean(COMWork_pos), NegCOMWork = mean(COMWork_neg))

# Read and summarize the Kinematics Data:
AnkKinematics <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/ZonalFit_Midcut_Aug2022/Kinematics.csv')

AnkKinematics <- AnkKinematics %>%
  rename('Slope' = Cond) %>%
  group_by(Subject, Config, Slope) %>%
  summarize(PeakAnkleEvVel = mean(pAnkEvVel))

# Read and summarize the Foot Work Data:
FootDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/ZonalFit_Midcut_Aug2022/FootWork.csv')
FootDat$Slope <- rep('Downhill', dim(FootDat)[1])

FootDat <- FootDat %>%
  group_by(Subject, Config, Slope) %>%
  summarize(NegFootWork = mean(DisWork))

# Read and summarize other treadmill data:
TreadDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/ZonalFit_Midcut_Aug2022/TreadmillPerformanceMetrics.csv')
TreadDat[TreadDat == Inf] <- NA 

TreadDat <- TreadDat %>%
  rename('Slope' = Level) %>%
  group_by(Subject, Config, Slope) %>%
  summarize(PosAnkleWork = mean(AnklePosWork/200, na.rm = TRUE), LoadingRate = mean(VLR*1000/200, na.rm = TRUE)) %>%
  mutate(Config= replace(Config, Config == 'TD','UZ')) %>%
  mutate(Config= replace(Config, Config == 'BD','LZ')) %>%
  mutate(Config= replace(Config, Config == '2D','DD')) %>%
  mutate(Slope= replace(Slope, Slope == 'uphill','Uphill')) %>%
  mutate(Slope= replace(Slope, Slope == 'downhill','Downhill'))


ChildDat <- list(PressDat,COMDat,AnkKinematics,FootDat,TreadDat) %>%
  reduce(full_join) %>%
  mutate(Config= replace(Config, Config == 'DD','NoFA'))

ChildDat[ChildDat == 0] <- NA

# Append other necessary information to the DataFrame
ChildDat$Speed <- rep(1.2, dim(ChildDat)[1])
ChildDat$Year <- rep(2022, dim(ChildDat)[1])
ChildDat$Month <- rep('August', dim(ChildDat)[1])
ChildDat$Brand <- rep('Tecnica', dim(ChildDat)[1])
ChildDat$Model <- rep('Forge GTX', dim(ChildDat)[1])

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/WalkRunDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}  
  
  
