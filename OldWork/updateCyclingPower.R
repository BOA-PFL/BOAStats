### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())


# helper functions --------------------------------------------------------

replaceName <- function(DF, toReplace, newName){ 
  
  # replace incorrect subject names with new name
  DF <- DF %>% 
    mutate(Subject = replace(Subject, Subject == toReplace, newName))
  return(DF)
}

# read in the existing cycling power DB
dat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/DB_V2/CyclingPowerDB_V2.csv')
dat <- dat %>%
  rename('Subject' = ï..Subject)

## Load in new cycling data
newDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/Testing Segments/Cycling Performance Tests/CyclingHL_May2022/WattBike/CompiledPowerData.csv')
# the below line helps to join the sprint pressure data below
newDat$Trial <- rep(c(1,2),22)
# add in pressure data ----------------------------------------------------

sprintDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/Testing Segments/Cycling Performance Tests/CyclingHL_May2022/Xsensor/SprintPressureData.csv')

summarizedSprint <- sprintDat %>%
  group_by(Subject, Config, Trial) %>%
  summarize(
    avgHeelVar = mean(overallHeelVar),
    avgPeakP = mean(overallPeakP),
    avgHeelCA = mean(Sprint_HeelContactArea)
  ) %>%
  separate(Trial, c("Trial",NA))

summarizedSprint$Trial <- as.numeric(summarizedSprint$Trial)

summarizedSprint <- replaceName(summarizedSprint, 'Brendan', 'BrendanLee')
summarizedSprint <- replaceName(summarizedSprint, 'Conrad', 'ConradMcCarthy')



newDat <- left_join(newDat, summarizedSprint)


#TODO - Jan and Feb cycling pressure data does not have heel contact
#       Trial format needs similar outputs
  
# combine with master -----------------------------------------------------


## these should be manually updated based onthe test
# Add Year Month Brand and Model
Model <- rep('testModel', dim(newDat)[1])
Brand <- rep('TestBrand', dim(newDat)[1])
Month <- rep('November', dim(newDat)[1])
Year <- rep(2022, dim(newDat)[1])

newDat$Model <- Model
newDat$Brand <- Brand
newDat$Month <- Month
newDat$Year <- Year

dat <- rbind(dat, newDat)



# write output - this alter the original file!add a 1 to the end if you are at all unsure of ourput!!!
a <- winDialog(type = 'yesno', message = 'about to overwrite DB')

if (a == 'YES'){
  write.table(dat, "C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/DB_V2/CyclingPowerDB_V2.csv", sep=',')
  
}
