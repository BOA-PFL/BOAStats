###The Purpose of this code is to update big data from workwear and Hike data###
################################################################################

library(tidyverse)
library(readxl)
rm(list=ls())
# Create overall variables for each test
Year <- '2024'
Month <- 'April'
Brand <- 'Scarpa'
Model <- 'Rush TRK PRO GTX '
TestName <- 'EH_Hike_MidcutSD_Mech_April2024'
Benefit <- 'E/H'
TestType <- 'Mechanistic'
### Functions
cmp_strings <- function(instr1,instr2,strtype){
  out_strings = 0
  if (length(instr1) != length(instr2)){
    print(c('Warning: Unequal',strtype, 'number'))
  }
  
  for (val1 in instr1){
    count = 0
    for (val2 in instr2){
      if (val1 == val2){
        break
      } else {
        count = count + 1
      }
      if (count == length(instr2)){
        print(c(strtype,' match not found for:',val1))
        out_strings = 1
      }
    }
  }
  if (out_strings == 1) {
    print('Qual Data Sheet')
    print(instr1)
    print('Metric Data Sheet')
    print(instr2)
  }
}
################################################################################
### Updating Quantitative Big Data ###
# Read the existing database: Only to get column name order
ParentDat <- read.csv('Z:/BigData/DB_V2/QualitativeBigData_v2.csv',nrows=1)
# ParentDat <- ParentDat %>%
  # rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read the qualitative data to be added to master data
ChildDat <- read_xlsx('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/Hike/EH_Hike_MidcutSD_Mech_April2024/EH_Hike_MidcutSD_Mech_April2024.xlsx')
ChildDat <- ChildDat %>%
  rename('Overall' = OverallFit)
noSub <- length(ChildDat$Subject)
#Remove any subject spaces
ChildDat$Subject <- tolower(gsub(" ", "", ChildDat$Subject))
Subject <- unique(ChildDat$Subject)

### Set up data to append ###
# Data to be set for each study
ChildDat$Year <- rep(Year, each = noSub)
ChildDat$Month <- rep(Month, each = noSub)
ChildDat$Brand <- rep(Brand, each = noSub)
ChildDat$Model <- rep(Model, each = noSub)
# Add in the additional dial torque information
ChildDat$L_DialTorque2 <- rep('NA', each = noSub)
ChildDat$L_DialTorque2 <- rep('NA', each = noSub)
ChildDat$L_DialTorque3 <- rep('NA', each = noSub)
ChildDat$R_DialTorque3 <- rep('NA', each = noSub)
ChildDat$Dial3Closure <- rep('NA', each = noSub)

if ('Cuff' %in% colnames(ChildDat)) {
  Cuff <- ChildDat$Cuff
} else {
  ChildDat$Cuff <- rep('NA', each = noSub)
}

if ('GeneralComments' %in% colnames(ChildDat)) {
  GeneralComments <- ChildDat$GeneralComments
} else {
  ChildDat$GeneralComments <- rep('NA', each = noSub)
}

# Sort the qual data into the correct order
ChildDat <- subset(ChildDat,select = -c(Notes))
ChildDat <- ChildDat[,name_order]

# write output. add a 1 to the end if you are at all unsure of output!!!
a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "Z:/BigData/DB_V2/QualitativeBigData_v2.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}
# Remove variables from the list
rm(ParentDat,noSub,name_order)

################################################################################
### Updating Config Big Data ###
ParentDat <- read.csv('Z:/BigData/DB_V2/ConfigDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Name.of.Test' = ?..Name.of.Test)
name_order = colnames(ParentDat)

Config <- unique(ChildDat$Config)
noSub <- length(Config)
Config.Long <- c('Cuff Focus','Performance Fit Solution','Instep Focus')
ChildDat <- data.frame(Config,Config.Long)
ChildDat$Year <- rep(Year, each = noSub)
ChildDat$Month <- rep(Month, each = noSub)
ChildDat$Brand <- rep(Brand, each = noSub)
ChildDat$Model <- rep(Model, each = noSub)
ChildDat$Name.of.Test <- rep(TestName, each = noSub)

# Sort the data into the correct order
ChildDat <- ChildDat[,name_order]

# write output. add a 1 to the end if you are at all unsure of output!!!
a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "Z:/BigData/DB_V2/ConfigDB.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}
# Remove variables from the list
rm(ParentDat,ChildDat,noSub,name_order,a)
################################################################################
### Update Subject Visits ###
# Read the existing database: Only to get column name order
ParentDat <- read.csv('Z:/BigData/DB_V2/MasterSubjectVisits.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

ChildDat <- read_xlsx('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/Hike/EH_Hike_MidcutSD_Mech_April2024/EH_Hike_MidcutSD_Mech_April2024.xlsx',sheet = 'Anthro')
ChildDat <- subset(ChildDat,select = -c(FootScan,Height))
ChildDat <- ChildDat %>% rename(Speed.run. = RunSpeed)
noSub <- length(ChildDat$Subject)
ChildDat$Year <- rep(Year, each = noSub)
ChildDat$Month <- rep(Month, each = noSub)
ChildDat$Brand <- rep(Brand, each = noSub)
ChildDat$Model <- rep(Model, each = noSub)
ChildDat$Name.of.Test <- rep(TestName, each = noSub)
ChildDat$Benefit <- rep(Benefit, each = noSub)
ChildDat$Type <- rep(TestType, each = noSub)
ChildDat$Resistance <- rep('NA', each = noSub)  


# Sort the data into the correct order
ChildDat <- ChildDat[,name_order]

# write output. add a 1 to the end if you are at all unsure of output!!!
a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "Z:/BigData/DB_V2/MasterSubjectVisits.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
}


rm(ParentDat,ChildDat,noSub,name_order,a)
################################################################################
### Overground Landing ###
# Read the existing database: Only to get column name order
ParentDat <- read.csv('Z:/BigData/DB_V2/LandingDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read and summarize the overground data:
LandingDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/Hike/EH_Hike_MidcutSD_Mech_April2024/Overground/0_Stabilization.csv')

LandingDat$Subject <- tolower(gsub(" ", "", LandingDat$Subject))
# Set Up 2 Checks here: Config Names and Subject Names 
cmp_strings(Subject,unique(LandingDat$Subject),'subject')
cmp_strings(Config,unique(LandingDat$Config),'config')

# Adjust names as necessary
LandingDat <- LandingDat %>%
  mutate(Subject = replace(Subject, Subject == 'gregmullens','gregmullen'))

# Single Leg Landing (SLL) data only: Make sure to apply correct data filters
ChildDat <- LandingDat %>%
  filter(StabTime != 'NA') %>%
  group_by(Subject, Config, Movement) %>%
  summarize(Time2Stabilize = mean(StabTime, na.rm = TRUE))

ChildDat <- left_join(ChildDat,LandingDat %>%
                        filter(RkneeABDMom > -500) %>%
                        group_by(Subject, Config, Movement) %>%
                        summarize(PeakKneeAbMoment = mean(RkneeABDMom, na.rm = TRUE)), by = c('Subject','Config','Movement'))

ChildDat$Year <- rep(Year, dim(ChildDat)[1])
ChildDat$Month <- rep(Month, dim(ChildDat)[1])
ChildDat$Brand <- rep(Brand, dim(ChildDat)[1])
ChildDat$Model <- rep(Model, dim(ChildDat)[1])
#_______________________________________________________________________________
# Place NaNs for missing data
ChildDat$KneeAbAdROM <- rep('NA', dim(ChildDat)[1])
# ChildDat$PeakKneeAbMoment <- rep('NA', dim(ChildDat)[1])
#_______________________________________________________________________________
# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "Z:/BigData/DB_V2/LandingDB.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
}
rm(ParentDat,ChildDat,LandingDat,name_order,a)
################################################################################
### Uneven terrain landing ###
# Read the existing database: Only to get column name order
ParentDat <- read.csv('Z:/BigData/DB_V2/LandingDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read and summarize the overground data:
LandingDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/Hike/EH_Hike_MidcutSD_Mech_April2024/IMU/SLL/0_TrailStabilize.csv')
LandingDat$Subject <- tolower(gsub(" ", "", LandingDat$Subject))

#Check Names and Configs
cmp_strings(Subject,unique(LandingDat$Subject),'subject')
cmp_strings(Config,unique(LandingDat$Config),'config')

# Adjust names as necessary
LandingDat <- LandingDat %>%
  mutate(Subject = replace(Subject, Subject == 'tj','tjditallo'))

# Single Leg Landing (SLL) data only
ChildDat <- LandingDat %>%
  group_by(Subject, Config) %>%
  summarize(Time2Stabilize = mean(StabalizeTime, na.rm = TRUE))

ChildDat$Year <- rep(Year, dim(ChildDat)[1])
ChildDat$Month <- rep(Month, dim(ChildDat)[1])
ChildDat$Brand <- rep(Brand, dim(ChildDat)[1])
ChildDat$Model <- rep(Model, dim(ChildDat)[1])
ChildDat$Movement <- rep('SLL', dim(ChildDat)[1])
#_______________________________________________________________________________
# Place NaNs for missing biomechanical data
ChildDat$KneeAbAdROM <- rep('NA', dim(ChildDat)[1])
ChildDat$PeakKneeAbMoment <- rep('NA', dim(ChildDat)[1])
#_______________________________________________________________________________
# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "Z:/BigData/DB_V2/LandingDB.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
}
rm(ParentDat,ChildDat,LandingDat,name_order,a)
################################################################################
### Uneven terrain walking  ###
# Read the existing database: Only to get column name order
ParentDat <- read.csv('Z:/BigData/DB_V2/TrailDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read and summarize the IMU Data:
IMUDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/Hike/EH_Hike_MidcutSD_Mech_April2024/IMU/Trail/0_Trail_CompIMUmetrics.csv')
IMUDat$Subject <- tolower(gsub(" ", "", IMUDat$Subject))

# Check Subject and Config Names
cmp_strings(Subject,unique(IMUDat$Subject),'subject')
cmp_strings(Config,unique(IMUDat$Config),'config')

# Summarize all metrics: Make sure to use correct data filters
ChildDat <- IMUDat %>%
  group_by(Subject, Config) %>%
  filter(imuSpeed > 0.25) %>%
  summarize(IMUSpeed = mean(imuSpeed))

ChildDat <- left_join(ChildDat,IMUDat %>%
                        filter(pIEgyro > 0, imuSpeed > 0.25) %>%
                        group_by(Subject, Config) %>%
                        summarize(PeakEvVel = mean(pGyr)), by = c('Subject','Config'))

ChildDat <- left_join(ChildDat,IMUDat %>%
                        filter(imuSpeed > 0.25) %>%
                        group_by(Subject, Config) %>%
                        summarize(PeakJerk = mean(pJerk)), by = c('Subject','Config'))

ChildDat <- left_join(ChildDat,IMUDat %>%
                        filter(imuSpeed > 0.25) %>%
                        group_by(Subject, Config) %>%
                        summarize(PeakAcc = mean(pAcc)), by = c('Subject','Config'))

ChildDat <- left_join(ChildDat,IMUDat %>%
                        filter(imuSpeed > 0.25) %>%
                        group_by(Subject, Config) %>%
                        summarize(RangeMLAcc = mean(rMLacc)), by = c('Subject','Config'))

# Read and summarize the pressure data
PressDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/Hike/EH_Hike_MidcutSD_Mech_April2024/Xsensor/0_CompiledResults_Trail.csv')
PressDat$Subject <- tolower(gsub(" ", "", PressDat$Subject))

# Check Subject and Config Names
cmp_strings(Subject,unique(PressDat$Subject),'subject')
cmp_strings(Config,unique(PressDat$Config),'config')

# Summarize all metrics: Make sure to use correct data filters
ChildDat <- left_join(ChildDat,PressDat %>%
                        group_by(Subject, Config) %>%
                        summarize(HeelContact = mean(heelAreaP)), by = c('Subject','Config'))
ChildDat <- left_join(ChildDat,PressDat %>%
                        group_by(Subject, Config) %>%
                        summarize(PeakToePress = mean(maxmaxToes)), by = c('Subject','Config'))

#_______________________________________________________________________________
# Append other necessary information to the DataFrame
ChildDat$Movement <- rep('ILT',dim(ChildDat)[1])
ChildDat$Year <- rep(Year, dim(ChildDat)[1])
ChildDat$Month <- rep(Month, dim(ChildDat)[1])
ChildDat$Brand <- rep(Brand, dim(ChildDat)[1])
ChildDat$Model <- rep(Model, dim(ChildDat)[1])

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/TrailDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}
rm(ParentDat,ChildDat,IMUDat,PressDat,name_order,a)
################################################################################
### Static Pressure  ###
ParentDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/StaticPressureDB.csv',nrows=1)
name_order = colnames(ParentDat)

ChildDat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/WorkWear_Performance/EH_Workwear_MidCutStabilityII_CPDMech_Sept23/XSENSOR/cropped/CompiledResults_Static.csv')
ChildDat$Year <- rep(Year, dim(ChildDat)[1])
ChildDat$Month <- rep(Month, dim(ChildDat)[1])
ChildDat$Brand <- rep(Brand, dim(ChildDat)[1])
ChildDat$Model <- rep(Model, dim(ChildDat)[1])

ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/StaticPressureDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}
