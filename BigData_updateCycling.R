### Appending new biomech data to big data ###
library(tidyverse) 
library(readxl)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2) 

rm(list=ls())

##############################
#
#
# Make sure all of the subject names match between data bases! 
# Check that the names match those who have previously been entered into the " Subject Visits" DB
#
##############################

Year <- '2023'  
Month <- 'September'
Brand <- 'Giro'
Model <- 'Regime' 
Benefit <- 'P/P' 
Type <- 'Performance'# Performance, mechanistic, materials




######### Cycling power and Pressure DB ##########################
# Read the existing database: Only to get column name order
pwerParentDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/BigData/DB_V2/CyclingPowerDB_V2.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(pwerParentDat)

# Read the Watt Bike Data:
CycleDat <- read.csv(file.choose())
# If any configurations need to be renamed
# CycleDat <- CycleDat
  # mutate(Subject= replace(Subject, Subject == 'RobinFassett','RobinFassettCarman'))

# Read and summarize the steady-state:
PressDat <- read.csv(file.choose())

PressSteadyDat <- PressDat %>%
  filter(Movement == 'Steady') %>%
  group_by(Subject, Config) %>%
  summarize(HeelContact_steady = mean(heelAreaP, na.rm = TRUE), PeakToePress_steady = mean(maxmaxToes, na.rm = TRUE))



PressSprintDat <- PressDat %>%
  filter(Movement == 'Sprint') %>%
  group_by(Subject, Config) %>%
  summarize(HeelContact_sprint = mean(heelAreaP, na.rm = TRUE), PeakToePress_sprint = mean(maxmaxToes, na.rm = TRUE))

# Combine IMU and pressure data
pwer_ChildDat <- list(CycleDat,PressSteadyDat,PressSprintDat) %>%
  reduce(full_join)

pwer_ChildDat$Year <- rep(Year, dim(pwer_ChildDat)[1])
pwer_ChildDat$Month <- rep(Month, dim(pwer_ChildDat)[1])
pwer_ChildDat$Brand <- rep(Brand, dim(pwer_ChildDat)[1])
pwer_ChildDat$Model <- rep(Model, dim(pwer_ChildDat)[1])
#ChildDat <- subset(ChildDat,select = -c(Trial))

# Sort the DataFrame columns into the right order (from the Parent)
pwer_ChildDat <- pwer_ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(pwer_ChildDat, file = 'C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/BigData/DB_V2/CyclingPowerDB_V2.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
} 

######### Static pressure BD ############ 

statParentDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/BigData/DB_V2/StaticPressureDB.csv',nrows=1)

name_order = colnames(statParentDat)

### Import Static Pressure data

statChildDat <- read.csv(file.choose()) 


#staticDat$Subject[staticDat$Subject == 'WesWebber'] <- 'WesWeber'

statChildDat$Year <- rep(Year, dim(statChildDat)[1])
statChildDat$Month <- rep(Month, dim(statChildDat)[1])
statChildDat$Brand <- rep(Brand, dim(statChildDat)[1])
statChildDat$Model <- rep(Model, dim(statChildDat)[1]) 


statChildDat <- statChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(statChildDat, file = 'C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/BigData/DB_V2/StaticPressureDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}





######### Qual BD ############

qual_ParentDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/BigData/DB_V2/QualitativeBigData_v2.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(qual_ParentDat)

# Read the qualitative data to be added to master data
qual_ChildDat <- read_xlsx('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/Cycling Performance Tests/PP_Cycling_PFS-SD_DD_DD-HeelLock_Mech_Sept23/PP_Cycling_PFS-SD_DD_DD-HeelLock_Mech_Sept23_Qual.xlsx')
qual_ChildDat <- qual_ChildDat %>%
  rename('Overall' = OverallFit)
noSub <- length(qual_ChildDat$Subject)

### Set up data to append ###
# Data to be set for each study
qual_ChildDat$Year <- rep(Year, each = noSub)
qual_ChildDat$Month <- rep(Month, each = noSub)
qual_ChildDat$Brand <- rep(Brand, each = noSub)
qual_ChildDat$Model <- rep(Model, each = noSub) 
qual_ChildDat$Dial1Closure <- rep('Mid', each = noSub) # refer to the "READ ME" on how the closures are defined
qual_ChildDat$Dial2Closure <- rep('Instep', each = noSub)
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
  write.table(qual_ChildDat, "C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/BigData/DB_V2/QualitativeBigData_v2.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}

######### Sub Visits BD ############

# Set path to your directory
subVisits <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/BigData/DB_V2/MasterSubjectVisits.csv')
# subVisits <- subVisits %>%
#   rename('Subject' = ?..Subject)

subVisits %>%
  group_by(Year) %>%
  summarize(count <- n_distinct(Subject))

# Add Subject name, test name, Mass, resistence and sex

Subject <- 'TestSub'
Name.of.Test <- 'Cycling_HL_test'
Speed.run. <- NA
Mass <- 68
Resistance <- 4.5
Sex <- 'M'


dat_to_append <- data.frame(Subject, Year, Month, Benefit, Brand, Model,
                            Name.of.Test, Type, Speed.run., Mass, Resistance, Sex)

subVisits <- rbind(subVisits, dat_to_append)

# write output. add a 1 to the end if you are at all unsure of ourput!!!
a <- winDialog(type = 'yesno', message = 'about to overwrite DB')
if (a == 'YES'){
  write.table(subVisits, "C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/DB_V2/MasterSubjectVisits.csv", sep=',', row.names = FALSE)
  
}

