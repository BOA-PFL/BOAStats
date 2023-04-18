### Load libraries

library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)

rm(list=ls())


### Import Dynamic Pressure data

ChildDat <- read.csv(file.choose())

ChildDat <- subset(ChildDat, ChildDat$Movement == ('cmj')|ChildDat$Movement == ('skater'))

#staticDat$Subject[stati
ChildDat$Year <- rep(2023, dim(ChildDat)[1])
ChildDat$Month <- rep('January', dim(ChildDat)[1])
ChildDat$Brand <- rep('NoBull', dim(ChildDat)[1])
ChildDat$Model <- rep('Trainer', dim(ChildDat)[1]) 
#childDat$Subject == 'WesWebber'] <- 'WesWeber'



a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'C:/Users/kate.harrison/Boa Technology Inc/PFL Team - General/BigData/DB_V2/AgilityPressureDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}




