### Appending new biomech data to big data ###
library(tidyverse) 
library(readxl)

rm(list=ls())




Year <- '2024'
Month <- 'January'
Brand <- 'Solomon'
Model <- 'S/Pro Supra Boa 120'
TestName <- 'EH_Alpine_FullBootvsShell_Mech_Jan2024'
Benefit <- 'P/P'
Type <- 'Performance' 


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


### Qual Dat


qual_ParentDat <- read.csv('Z:\\BigData\\DB_V2\\QualitativeBigData_v2.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(qual_ParentDat)

# Read the qualitative data to be added to master data
ChildDat <- read_xlsx('Z:\\Testing Segments\\Snow Performance\\EH_Alpine_FullBootVsShell_Jan2024\\EH_Alpine_FullBootvsShell_Mech_Jan2024_Qual.xlsx')
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
# ChildDat$Dial1Closure <- rep('Mid', each = noSub) # Dial 1 -  refer to the "READ ME" on how the closures are defined
# ChildDat$Dial2Closure <- rep('NA', each = noSub)# Dial 2
ChildDat$Dial3Closure <- rep('NA', each = noSub)
# Add in the additional dial torque information


ChildDat$L_DialTorque1 <- rep('NA', each = noSub)
ChildDat$L_DialTorque2 <- rep('NA', each = noSub)
ChildDat$L_DialTorque3 <- rep('NA', each = noSub)
ChildDat$R_DialTorque1 <- rep('NA', each = noSub)
ChildDat$R_DialTorque2 <- rep('NA', each = noSub) 
ChildDat$R_DialTorque3 <- rep('NA', each = noSub)



if ('GeneralComments' %in% colnames(ChildDat)) {
  GeneralComments <- ChildDat$GeneralComments
} else {
  ChildDat$GeneralComments <- rep('NA', each = noSub)
}


if ('Cuff' %in% colnames(ChildDat)) {
  Cuff <- ChildDat$Cuff
} else {
  ChildDat$Cuff <- rep('NA', each = noSub)
}

# Sort the qual data into the correct order
# ChildDat <- subset(ChildDat,select = -c(Notes))
ChildDat <- ChildDat[,name_order]

# write output. add a 1 to the end if you are at all unsure of output!!!
a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "Z:\\BigData\\DB_V2\\QualitativeBigData_v2.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}

rm(qual_ParentDat,noSub,name_order,a)


### Updating Config Big Data ###


ParentDat <- read.csv('Z:\\BigData\\DB_V2\\ConfigDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Name.of.Test' = ?..Name.of.Test)
name_order = colnames(ParentDat)

Config <- unique(ChildDat$Config)
noSub <- length(Config)
Config.Long <- c('Full','Shell')
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

######### Sub Visits BD ############

# Read the existing database: Only to get column name order
ParentDat <- read.csv('Z:/BigData/DB_V2/MasterSubjectVisits.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)
# Read in qual sheet to reference names
ChildDat <- read_xlsx('Z:\\Testing Segments\\Snow Performance\\EH_Alpine_FullBootVsShell_Jan2024\\EH_Alpine_FullBootvsShell_Mech_Jan2024_Qual.xlsx',sheet = 'Anthro')
ChildDat <- subset(ChildDat,select = -c(2:3))
ChildDat <- ChildDat %>% rename(Speed.run. = RunSpeed)
noSub <- length(ChildDat$Subject)
ChildDat$Year <- rep(Year, each = noSub)
ChildDat$Month <- rep(Month, each = noSub)
ChildDat$Brand <- rep(Brand, each = noSub)
ChildDat$Model <- rep(Model, each = noSub)
ChildDat$Name.of.Test <- rep(TestName, each = noSub)
ChildDat$Benefit <- rep(Benefit, each = noSub)
ChildDat$Type <- rep(Type, each = noSub)
ChildDat$Resistance <- rep('NA', each = noSub)
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




#####################################################################################################
## Combiming IMU and Pressure Data

# Read the existing database: Only to get column name order
ParentDat <- read.csv('Z:/BigData/DB_V2/AlpineDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read and summarize the overground data:
IMUDat <- read.csv('Z:\\Testing Segments\\Snow Performance\\EH_Alpine_FullBootVsShell_Jan2024\\IMU\\0_IMUOutcomes.csv') 
IMUDat$Subject <- tolower(gsub(" ", "", IMUDat$Subject))
Subject <- unique(IMUDat$Subject)
Config <- unique(IMUDat$Config)

#Check Names and Configs
cmp_strings(Subject,unique(IMUDat$Subject),'subject')
cmp_strings(Config,unique(IMUDat$Config),'config')


IMUDat <- IMUDat %>%
  group_by(Subject, Config) %>%
  summarise(EdgeAng_Dwn_gyr= mean(edgeang_dwn_gyr), EdgeAng_Up_gyr = mean(edgeang_up_gyr), RAD_dwn = mean(RAD_dwn),
            EdgeAngT_Dwn_gyr = mean(edgeangt_dwn_gyr), Freq50fft = mean(freq50fft))


pressDat <- read.csv('Z:\\Testing Segments\\Snow Performance\\EH_Alpine_FullBootVsShell_Jan2024\\XSENSOR\\Cropped\\0_CompiledResults_5.csv') 
pressDat$Subject <- tolower(gsub(" ", "", pressDat$Subject))
Subject <- unique(pressDat$Subject)
Config <- unique(pressDat$Config)


pressDat <- pressDat %>%
  group_by(Subject, Config) %>%
  summarise(MedialProportionMidfoot = mean(medPropMid), UphillMaxForce = mean(uphillMaxF), DownhillMaxForce = mean(downhillMax),
            RFD = mean(RFD))

ChildDat <- list(pressDat,IMUDat) %>%
  reduce(full_join)



ChildDat$Year <- rep(Year, dim(ChildDat)[1])
ChildDat$Month <- rep(Month, dim(ChildDat)[1])
ChildDat$Brand <- rep(Brand, dim(ChildDat)[1])
ChildDat$Model <- rep(Model, dim(ChildDat)[1])

#_______________________________________________________________________________
# Place NaNs for missing data
# ChildDat$COMConWork <- rep('NA', dim(ChildDat)[1])
# ChildDat$PeakKneeAbMoment <- rep('NA', dim(ChildDat)[1])
# ChildDat$PeakAnklePFMoment <- rep('NA', dim(ChildDat)[1])
# ChildDat$PropForce <- rep('NA', dim(ChildDat)[1])
# ChildDat$PeakAnkleInMoment <- rep('NA', dim(ChildDat)[1])
# ChildDat$KneeAbAdROM <- rep('NA', dim(ChildDat)[1])
# ChildDat$COMEccWork <- rep('NA', dim(ChildDat)[1])
#_______________________________________________________________________________

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "Z:/BigData/DB_V2/AlpineDB.csv", sep=',', 
              append = TRUE,col.names = TRUE, row.names = FALSE)
}

rm(ParentDat,ChildDat,name_order,a) 


