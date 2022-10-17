### Updating Quantitative Big Data ###
library(tidyverse)
library(readxl)
rm(list=ls())

# Set path for the master big data folder
masterQual <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/QualitativeBigData_v2.csv')
masterQual <- masterQual %>%
  rename('Subject' = ï..Subject)


# Read the qualitative data to be added to master data
qualDat <- read_xlsx('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/ZonalFit_Midcut_Aug2022/CompiledQualData.xlsx')
noSub <- length(qualDat$Subject)

### Set up data to append ###
# Data to be set for each study
Year <- rep('2022', each = noSub)
Month <- rep('August', each = noSub)
Brand <- rep('Tecnica', each = noSub)
Model <- rep('Forge GTX', each = noSub)
# Data to be pulled from qual sheet from each study
Subject <- qualDat$Subject
Config <- qualDat$Config
Overall <- qualDat$OverallFit
Forefoot <- qualDat$Forefoot
Midfoot <- qualDat$Midfoot
Heel <- qualDat$Heel
GoodComments <- qualDat$GoodComments
BadComments <- qualDat$BadComments

if ('Cuff' %in% colnames(qualDat)) {
  Cuff <- qualDat$Cuff
} else {
  Cuff <- rep('NA', each = noSub)
}

if ('GeneralComments' %in% colnames(qualDat)) {
  GeneralComments <- qualDat$GeneralComments
} else {
  GeneralComments <- rep('NA', each = noSub)
}


# Conglomerate all metrics together
dat_to_append <- data.frame(Subject, Config, Year, Month, Brand, Model,
                            Overall, Forefoot, Midfoot, Heel, Cuff,
                            GoodComments, BadComments, GeneralComments)

masterQual <- rbind(masterQual, dat_to_append)

# write output. add a 1 to the end if you are at all unsure of output!!!
a <- winDialog(type = 'yesno', message = 'about to overwrite DB')
if (a == 'YES'){
  write.table(masterQual, "C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/QualitativeBigData_v2.csv", sep=',', row.names = FALSE)
  
}