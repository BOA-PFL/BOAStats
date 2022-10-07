### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())

# read in the existing cycling power DB
dat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/DB_V2/CyclingPowerDB_V2.csv')
dat <- dat %>%
  rename('Subject' = ï..Subject)

## Load in new cycling data
newDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/Testing Segments/Cycling Performance Tests/CyclingHL_May2022/WattBike/CompiledPowerData.csv')


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
write.table(dat, "C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/DB_V2/CyclingPowerDB_V2.csv", sep=',')
