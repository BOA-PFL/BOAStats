### Appending new biomech data to big data ###
library(tidyverse)
library(readxl)
rm(list=ls())






##############################
#
#
# Make sure all of the subject names match between data bases! 
# Check that the names match those who have previously been entered into the " Subject Visits" DB
#
##############################

Year <- '2024'  
Month <- 'July'
Brand <- 'Altra'
Model <- 'Timp' 
Benefit <- 'E/H' 
Type <- 'Mechanistic'# Performance, mechanistic, materials

TestName<-'EH_Trail_TrailStability3_Mech_July24'





# Read the existing database: Only to get column name order

ParentDat <- read.csv('Z:\\BigData\\DB_V2\\TrailDB.csv',nrows=1)

# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read and summarize the IMU Data:

IMUDat <- read.csv('Z:\\Testing Segments\\Material Testing\\2023\\UpperStiffnessE&H_Performance_Aug2023\\IMU\\IMUmetrics.csv')

IMUDat <- IMUDat %>%
  filter(Label > 0) %>%
  group_by(Subject, Config, Order, Label) %>%
  summarize(IMUSpeed = mean(imuSpeed), PeakEvVel = mean(pIEgyro), PeakJerk = mean(pJerk),
            PeakAcc = mean(pAcc),RangeMLAcc = mean(rMLacc)) 

# Read and summarize the pressure data

PressDat <- read.csv('C:\\Users\\bethany.kilpatrick\\Boa Technology Inc\\PFL - General\\Testing Segments\\EndurancePerformance\\EH_Trail_TrailStability3_Mech_July24\\Xsensor\\0_CompiledResults.csv')

PressDat <- PressDat %>%
  filter(Label > 0) %>%
  group_by(Subject, Config, Order, Label) %>%
  summarize(HeelContact = mean(heelAreaP), PeakToePress = mean(maxmaxToes)) 


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
ChildDat$Year <- rep(2024, dim(ChildDat)[1])
ChildDat$Month <- rep('July', dim(ChildDat)[1])
ChildDat$Brand <- rep('Altra', dim(ChildDat)[1])
ChildDat$Model <- rep('Timp', dim(ChildDat)[1])

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]


write.table(PressDat, file = 'Z:\\BigData\\DB_V2\\TrailDB.csv', sep = ',',
            append = TRUE,col.names = FALSE, row.names = FALSE)




##### Qualitative #####

qual_ParentDat <- read.csv('Z:/BigData/DB_V2/QualitativeBigData_v2.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(qual_ParentDat)

# Read the qualitative data to be added to master data
qual_ChildDat <- read_xlsx('C:\\Users\\bethany.kilpatrick\\Boa Technology Inc\\PFL - General\\Testing Segments\\EndurancePerformance\\EH_Trail_TrailStability3_Mech_July24\\Qual_EH_Trail_TrailStability3_Jul24.xlsx')
qual_ChildDat <- qual_ChildDat %>%
  rename('Overall' = OverallFit)
noSub <- length(qual_ChildDat$Subject)

### Set up data to append ###
# Data to be set for each study
qual_ChildDat$Year <- rep(Year, each = noSub)
qual_ChildDat$Month <- rep(Month, each = noSub)
qual_ChildDat$Brand <- rep(Brand, each = noSub)
qual_ChildDat$Model <- rep(Model, each = noSub) 
qual_ChildDat$Dial1Closure <- rep('Instep', each = noSub) # refer to the "READ ME" on how the closures are defined
qual_ChildDat$Dial2Closure <- rep('NA', each = noSub)
qual_ChildDat$Dial3Closure <- rep('NA', each = noSub)


if ('GeneralComments' %in% colnames(qual_ChildDat)) {
  GeneralComments <- qual_ChildDat$GeneralComments
} else {
  qual_ChildDat$GeneralComments <- rep('NA', each = noSub)
}


if ('Cuff' %in% colnames(qual_ChildDat)) {
  Cuff <- qual_ChildDat$Cuff
} else {
  qual_ChildDat$Cuff <- rep('NA', each = noSub)
}

# Sort the qual data into the correct order
qual_ChildDat <- subset(qual_ChildDat,select = -c(Notes))
qual_ChildDat <- qual_ChildDat[,name_order]

# write output. add a 1 to the end if you are at all unsure of output!!!
a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(qual_ChildDat, "Z:/BigData/DB_V2/QualitativeBigData_v2.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}


######### Sub Visits BD ############


ParentDat <- read.csv('Z:/BigData/DB_V2/MasterSubjectVisits.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)
# Read in qual sheet to reference names
ChildDat <- read_xlsx('C:\\Users\\bethany.kilpatrick\\Boa Technology Inc\\PFL - General\\Testing Segments\\EndurancePerformance\\EH_Trail_TrailStability3_Mech_July24\\Qual_EH_Trail_TrailStability3_Jul24.xlsx',sheet = 'Anthros')
# ChildDat <- subset(ChildDat,select = -c(FootScan,Compensation))
# ChildDat <- ChildDat %>% rename(Speed.run. = RunSpeed)
noSub <- length(ChildDat$Subject)
ChildDat$Year <- rep(Year, each = noSub)
ChildDat$Month <- rep(Month, each = noSub)
ChildDat$Brand <- rep(Brand, each = noSub)
ChildDat$Model <- rep(Model, each = noSub)
ChildDat$Name.of.Test <- rep(TestName, each = noSub)
ChildDat$Benefit <- rep(Benefit, each = noSub)
ChildDat$Type <- rep(Type, each = noSub) 
ChildDat$Speed.run. <- rep('NA', each = noSub)

# Sort the data into the correct order
ChildDat <- ChildDat[,name_order]

# write output. add a 1 to the end if you are at all unsure of output!!!
a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "Z:/BigData/DB_V2/MasterSubjectVisits.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}
rm(ParentDat,ChildDat,noSub,name_order,a) 



### Updating Config Big Data ###


ParentDat <- read.csv('Z:\\BigData\\DB_V2\\ConfigDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Name.of.Test' = ?..Name.of.Test)
name_order = colnames(ParentDat)

Config <- unique(ChildDat$Config)
noSub <- length(Config)
Config.Long <- c('High Stiffness 72.3','Mid Stiffness 36.8' ,'Low Stiffness 26.6')
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
  write.table(ChildDat, "Z:\\BigData\\DB_V2\\ConfigDB.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}
# Remove variables from the list
rm(ParentDat,ChildDat,noSub,name_order,a)




