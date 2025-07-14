### Appending new biomech data to big data ###
library(tidyverse) 
library(readxl)

rm(list=ls())




Year <- '2023'
Month <- 'August'
Brand <- 'lasportiva'
Model <- 'cyklon'
TestName <- 'Test'
Benefit <- 'A/S'
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


# qual_ParentDat <- read.csv('Z:\\BigData\\DB_V2\\QualitativeBigData_v2.csv',nrows=1)
qual_ParentDat <- read.csv('\\\\boa-srv10\\PFL-DATA\\BigData\\DB_V2\\QualitativeBigData_v2.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(qual_ParentDat)

# Read the qualitative data to be added to master data
ChildDat <- read_xlsx('C:\\Users\\milena.singletary\\OneDrive - BOA Technology Inc\\General - PFL Team\\Testing Segments\\AgilityPerformanceData\\AS_Train_UpperXPanelStiffnessI_Mech_Sept24\\CompiledQualData_XPanelStiffnessI.xlsx')
qualDat <- ChildDat
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
ChildDat$Dial1Closure <- rep('Mid', each = noSub) # Dial 1 -  refer to the "READ ME" on how the closures are defined
ChildDat$Dial2Closure <- rep('Instep', each = noSub)# Dial 2
ChildDat$Dial3Closure <- rep('NA', each = noSub)
# Add in the additional dial torque information
ChildDat$L_DialTorque3 <- rep('NA', each = noSub)
ChildDat$R_DialTorque3 <- rep('NA', each = noSub)
ChildDat$Dial3Closure <- rep('NA', each = noSub)

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
ChildDat <- subset(ChildDat,select = -c(Notes))
ChildDat <- ChildDat[,name_order]

# write output. add a 1 to the end if you are at all unsure of output!!!
a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "\\\\boa-srv10\\PFL-DATA\\BigData\\DB_V2\\QualitativeBigData_v2.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}

rm(ParentDat,noSub,name_order,a)


### Updating Config Big Data ###


ParentDat <- read.csv('\\\\boa-srv10\\PFL-DATA\\BigData\\DB_V2\\ConfigDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Name.of.Test' = ?..Name.of.Test)
name_order = colnames(ParentDat)

Config <- unique(ChildDat$Config)
noSub <- length(Config)
Config.Long <- c('Interal Ankle Straps','External Ankle Straps','Focus Ankle')
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
ParentDat <- read.csv('\\\\boa-srv10\\PFL-DATA\\BigData\\DB_V2\\MasterSubjectVisits.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)
# Read in qual sheet to reference names
#ChildDat <- read_xlsx('C:\\Users\\milena.singletary\\OneDrive - BOA Technology Inc\\General - PFL Team\\Testing Segments\\AgilityPerformanceData\\AS_Train_UpperXPanelStiffnessI_Mech_Sept24\\CompiledQualData_XPanelStiffnessI.xlsx',sheet = 'Anthro')
ChildDat <- read_xlsx('C:\\Users\\milena.singletary\\OneDrive - BOA Technology Inc\\General - PFL Team\\Testing Segments\\AgilityPerformanceData\\AS_Train_UpperXPanelStiffnessI_Mech_Sept24\\CompiledQualData_XPanelStiffnessI.xlsx',sheet = 'Sheet3')
ChildDat <- subset(ChildDat,select = -c(FootScan,Compensation,Height))
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


# Sort the data into the correct order
ChildDat <- ChildDat[,name_order]

# write output. add a 1 to the end if you are at all unsure of output!!!
a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "\\\\boa-srv10\\PFL-DATA\\BigData\\DB_V2\\MasterSubjectVisits.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}
rm(ParentDat,ChildDat,noSub,name_order,a)




#####################################################################################################
## Agility Data

# Read the existing database: Only to get column name order
#ParentDat <- read.csv('Z:/BigData/DB_V2/AgilitySpeedDB.csv',nrows=1)
ParentDat <- read.csv('\\\\boa-srv10\\PFL-DATA\\BigData\\DB_V2\\AgilitySpeedDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read and summarize the overground data:
AgilityDat <- read.csv('\\\\boa-srv10\\PFL-DATA\\Testing Segments\\AgilityPerformanceData\\AS_Train_UpperXPanelStiffnessI_Mech_Sept24\\Overground\\0_CompiledAgilityDataTest.csv') 
AgilityDat$Subject <- tolower(gsub(" ", "", AgilityDat$Subject))
Subject <- unique(AgilityDat$Subject)
Config <- unique(AgilityDat$Config)

#Check Names and Configs
cmp_strings(Subject,unique(AgilityDat$Subject),'subject')
cmp_strings(Config,unique(AgilityDat$Config),'config')



AgilityDat <- AgilityDat %>% 
  group_by(Subject) %>%
  filter(PeakKneeAbMoment < 250) %>%
  mutate(z_score = scale(PeakKneeAbMoment))%>%
  group_by(Config) 

AgilityDat<- subset(AgilityDat, AgilityDat$z_score < 2) #removing outliers  
AgilityDat<- subset(AgilityDat, AgilityDat$z_score > -2)


CMJDat <- AgilityDat %>%
  filter(Movement == 'CMJ') %>%
  group_by(Subject, Config, Order, Movement) %>%
  summarise(ContactTime = mean(CT), PeakAnklePFMoment = mean(peakPFmom), PropForce = mean(peakGRF_Z),
            PeakAnkleInMoment = mean(peakINVmom), KneeAbAdROM = mean(kneeABDrom),PeakKneeAbMoment = mean(PeakKneeAbMoment),
            COMEccWork = mean(eccWork), COMConWork = mean(conWork)) 



SKTDat <- AgilityDat %>%
  filter(Movement == 'Skater') %>%
  group_by(Subject, Config, Order, Movement) %>%
  summarise(ContactTime = mean(CT), PeakAnklePFMoment = mean(peakPFmom), PropForce = mean(peakGRF_X),
            PeakAnkleInMoment = mean(peakINVmom), KneeAbAdROM = mean(kneeABDrom), PeakKneeAbMoment = mean(PeakKneeAbMoment),
            COMEccWork = mean(eccWork), COMConWork = mean(conWork))

ChildDat <- merge(x=CMJDat,y=SKTDat,all=TRUE)
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
  write.table(ChildDat, "Z:/BigData/DB_V2/AgilitySpeedDB.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
}

rm(ParentDat,ChildDat,name_order,a) 

##################################################################################
######### Static pressure BD ############ 

ParentDat <- read.csv('Z:/BigData/DB_V2/StaticPressureDB.csv',nrows=1)
name_order = colnames(ParentDat)

### Import Static Pressure data

staticDat <- read.csv('Z:\\Testing Segments\\AgilityPerformanceData\\AS_Trail_MidfootPanelStiffness_Perf_Oct23\\Xsensor\\Static\\CompiledResults_Static.csv') 
staticDat$Subject <- tolower(gsub(" ", "", staticDat$Subject))

#Check Names and Configs
cmp_strings(Subject,unique(staticDat$Subject),'subject')
cmp_strings(Config,unique(staticDat$Config),'config')


# Adjust names as necessary
# ChildDat <- statChildDat %>%
#   mutate(Subject = replace(Subject, Subject == 'tj','tjditallo'))


staticDat$Year <- rep(Year, dim(staticDat)[1])
staticDat$Month <- rep(Month, dim(staticDat)[1])
staticDat$Brand <- rep(Brand, dim(staticDat)[1])
staticDat$Model <- rep(Model, dim(staticDat)[1]) 


ChildDat <- staticDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'Z:/BigData/DB_V2/StaticPressureDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}



rm(ParentDat,ChildDat,name_order,a)

#####################################################################  

## Agility Pressure 

ParentDat <- read.csv('\\\\boa-srv10\\PFL-DATA\\BigData\\DB_V2\\AgilityPressureDB.csv',nrows=1)
ParentDat <- read.csv('Z:/BigData/DB_V2/AgilityPressureDB.csv',nrows=1)

name_order = colnames(ParentDat)

### Import Dynamic Pressure data

ChildDat <- read.csv('Z:\\Testing Segments\\AgilityPerformanceData\\2025_Mechanistic_WomensPanelStiffnessII_Adidas\\Xsensor\\cropped\\1_CompiledResults.csv')

ChildDat <- subset(ChildDat, ChildDat$Movement == ('cmj')|ChildDat$Movement == ('skater'))

ChildDat <- ChildDat %>%
  group_by(Subject, Config, Order, Movement) %>%
  summarise_all(mean)

#staticDat$Subject[stati
noSub <- length(ChildDat$Subject)
ChildDat$Year <- rep(2025, dim(ChildDat)[1])
ChildDat$Month <- rep('May', dim(ChildDat)[1])
ChildDat$Brand <- rep('Adidas', dim(ChildDat)[1])
ChildDat$Model <- rep('Barricade', dim(ChildDat)[1]) 
ChildDat$toeArea_mid <- rep('NA', each = noSub)
ChildDat$ffP_late <- rep('NA', each = noSub)
ChildDat$ffArea_late <- rep('NA', each = noSub)
ChildDat$ffP_Mid <- rep('NA', each = noSub)
ChildDat$ffArea_Mid <- rep('NA', each = noSub)
ChildDat$ffPMax_late <- rep('NA', each = noSub)

ChildDat$mfP_late <- rep('NA', each = noSub)
ChildDat$mfArea_late <- rep('NA', each = noSub)
ChildDat$mfP_Mid <- rep('NA', each = noSub)
ChildDat$mfArea_Mid <- rep('NA', each = noSub)
ChildDat$latP_late <- rep('NA', each = noSub)
ChildDat$latArea_late <- rep('NA', each = noSub)


ChildDat$medP_late <- rep('NA', each = noSub)
ChildDat$medArea_late <- rep('NA', each = noSub)

ChildDat$ffDorsalEarlyP <- rep('NA', each = noSub)
ChildDat$ffDorsalMidP <- rep('NA', each = noSub)
ChildDat$ffDorsalLateP <- rep('NA', each = noSub)
ChildDat$mfDorsalEarlyP <- rep('NA', each = noSub)
ChildDat$mfDorsalMidP <- rep('NA', each = noSub)
ChildDat$mfDorsalLateP <- rep('NA', each = noSub)
ChildDat$instepEarlyP <- rep('NA', each = noSub)
ChildDat$instepMidP <- rep('NA', each = noSub)
ChildDat$instepLateP <- rep('NA', each = noSub)
ChildDat$ffDorsalMax <- rep('NA', each = noSub)
ChildDat$mfDorsalMax <- rep('NA', each = noSub)
ChildDat$instepMax <- rep('NA', each = noSub)

ChildDat <- ChildDat[,name_order]


a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'Z:/BigData/DB_V2/AgilityPressureDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}

rm(ParentDat,ChildDat,name_order,a)

##################################################################### 

## Walk / Run BD 

# Read the existing database: Only to get column name order
ParentDat <- read.csv('\\\\boa-srv10\\PFL-DATA\\BigData\\DB_V2\\WalkRunDB.csv',nrows=1)
#ParentDat <- read.csv('Z:\\BigData\\DB_V2\\WalkRunDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read and summarize the Pressure Data:
PressDat <- read.csv('\\\\boa-srv10\\PFL-DATA\\Testing Segments\\AgilityPerformanceData\\AS_Train_UpperXPanelStiffnessI_Mech_Sept24\\XSENSOR\\cropped\\0_CompiledResults.csv')
PressDat$Subject <- gsub(" ", "", PressDat$Subject) # remove spaces in names
PressDat <- subset(PressDat, PressDat$Movement == 'run')


PressDat <- PressDat %>%
  # filter(Side == 'Right') %>%
  group_by(Subject, Config, Order) %>%
  summarize(PeakToePress = mean(maxmaxToes, na.rm = TRUE), HeelContact = mean(heelAreaP, na.rm = TRUE))
# mutate(Subject= replace(Subject, Subject == 'Olivia','OliviaBojan'))

# Read and summarize other treadmill data:
TreadDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/AgilityPerformanceData/AS_Trail_DorsalPressureVariationIII_PFLMech_July2023/Treadmill/TreadmillOutcomes.csv')
TreadDat[TreadDat == Inf] <- NA
TreadDat$Slope <- as.character(TreadDat$Slope)

TreadDat <- TreadDat %>%
  group_by(Subject, Config, Order, Speed, Slope) %>%
  summarize(LoadingRate = mean(VALR, na.rm = TRUE), PosCOMWork = mean(COMWork_pos), NegCOMWork = mean(COMWork_neg)) 
# mutate(Subject= replace(Subject, Subject == 'Amanda','AmandaBuchholtz'))

ChildDat <- list(PressDat,TreadDat) %>%
  reduce(full_join)

# Append other necessary information to the DataFrame
ChildDat$Speed <- rep(3.0, dim(ChildDat)[1])
ChildDat$Slope <- rep(0, dim(ChildDat)[1])
ChildDat$Year <- rep(Year, dim(ChildDat)[1])
ChildDat$NegFootWork <- rep(NA, dim(ChildDat)[1])
ChildDat$PosAnkleWork <- rep(NA, dim(ChildDat)[1])
ChildDat$PeakAnkleEvVel <- rep(NA, dim(ChildDat)[1])
ChildDat$Month <- rep(Month, dim(ChildDat)[1])
ChildDat$Brand <- rep(Brand, dim(ChildDat)[1])
ChildDat$Model <- rep(Model, dim(ChildDat)[1])

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'Z:\\BigData\\DB_V2\\WalkRunDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
} 



