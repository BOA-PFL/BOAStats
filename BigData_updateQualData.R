### Updating Quantitative Big Data ###
library(tidyverse)
library(readxl)
rm(list=ls())

# Read the existing database: Only to get column name order
ParentDat <- read.csv('Z:\\BigData\\DB_V2\\QualitativeBigData_v2.csv',nrows=1)
ParentDat <- ParentDat %>%
  rename('Subject' = ï..Subject)
name_order = colnames(ParentDat)

# Read the qualitative data to be added to master data
ChildDat <- read_xlsx('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Hike/ZonalFit_Midcut_Aug2022/CompiledQualData.xlsx')
ChildDat <- ChildDat %>%
  rename('Overall' = OverallFit)
noSub <- length(ChildDat$Subject)

### Set up data to append ###
# Data to be set for each study
ChildDat$Year <- rep('2022', each = noSub)
ChildDat$Month <- rep('August', each = noSub)
ChildDat$Brand <- rep('Tecnica', each = noSub)
ChildDat$Model <- rep('Forge GTX', each = noSub)

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
  write.table(ChildDat, "C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/QualitativeBigData_v2.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}
