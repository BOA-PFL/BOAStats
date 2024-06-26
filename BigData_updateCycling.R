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

Year <- '2024'  
Month <- 'May'
Brand <- 'Giro'
Model <- 'Regime' 
Benefit <- 'P/P' 
Type <- 'Performance'# Performance, mechanistic, materials

TestName<-'PP_Perf_CyclingUpperStiffnessII_May2024'


######### Cycling power and Pressure DB ##########################
# Read the existing database: Only to get column name order
pwerParentDat <- read.csv('Z:/BigData/DB_V2/CyclingPowerDB_V2.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(pwerParentDat)

# Read the Watt Bike Data:
CycleDat <- read.csv(file.choose())
# If any configurations need to be renamed
CycleDat <- CycleDat%>% 
  group_by(Subject, Config, Order)
  # mutate(Subject= replace(Subject, Subject == 'TrapperSteinle','TrapperSteinle'))

# Read and summarize the steady-state:
PressDat <- read.csv(file.choose())

PressSteadyDat <- PressDat %>%
  filter(Movement == 'Steady') %>%
  group_by(Subject, Config, Order) %>% 
  summarize(HeelContact_steady = mean(heelArea_Up, na.rm = TRUE), PeakToePress_steady = mean(maxmaxToes, na.rm = TRUE))
  # mutate(Subject= replace(Subject, Subject == 'TrapperSteinle','TrapperSteinle'))



PressSprintDat <- PressDat %>%
  filter(Movement == 'Sprint') %>%
  group_by(Subject, Config, Order) %>% 
  summarize(HeelContact_sprint = mean(heelArea_Up, na.rm = TRUE), PeakToePress_sprint = mean(maxmaxToes, na.rm = TRUE))
  # mutate(Subject= replace(Subject, Subject == 'TrapperSteinle','TrapperSteinle'))

  
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
  write.table(pwer_ChildDat, file = 'Z:/BigData/DB_V2/CyclingPowerDB_V2.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
} 

######### Static pressure BD ############ 

statParentDat <- read.csv('Z:/BigData/DB_V2/StaticPressureDB.csv',nrows=1)

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
  write.table(statChildDat, file = 'Z:/BigData/DB_V2/StaticPressureDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}





######### Qual BD ############

qual_ParentDat <- read.csv('Z:/BigData/DB_V2/QualitativeBigData_v2.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(qual_ParentDat)

# Read the qualitative data to be added to master data
qual_ChildDat <- read_xlsx('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/Cycling Performance Tests/PP_CyclingUpperStiffnessII_May2024/PP_CyclingUpperStiffnessII_May2024_Qual.xlsx')
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
ChildDat <- read_xlsx('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/Cycling Performance Tests/PP_CyclingUpperStiffnessII_May2024/PP_CyclingUpperStiffnessII_May2024_Qual.xlsx',sheet = 'Sheet3')
ChildDat <- subset(ChildDat,select = -c(FootScan,Compensation))
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

