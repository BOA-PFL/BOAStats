### Load libraries

library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)

rm(list=ls())

### Import parent database

ParentDat <- read.csv('C:/Users/kate.harrison/Boa Technology Inc/PFL Team - General/BigData/DB_V2/StaticPressureDB.csv')

name_order = colnames(ParentDat)

### Import Static Pressure data

ChildDat <- read.csv(file.choose())

#staticDat$Subject[staticDat$Subject == 'WesWebber'] <- 'WesWeber'

ChildDat$Year <- rep(2023, dim(ChildDat)[1])
ChildDat$Month <- rep('January', dim(ChildDat)[1])
ChildDat$Brand <- rep('NOBULL', dim(ChildDat)[1])
ChildDat$Model <- rep('Trainer', dim(ChildDat)[1]) 


ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  # Check the Child Data before!!
  write.table(ChildDat, file = 'C:/Users/kate.harrison/Boa Technology Inc/PFL Team - General/BigData/DB_V2/StaticPressureDB.csv', sep = ',',
              append = TRUE,col.names = FALSE, row.names = FALSE)
}




