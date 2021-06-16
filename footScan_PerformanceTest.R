library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(lme4)
library(readxl)

#testing 

rm(list=ls())

#load compiled outcomes

 testDat <- read.csv('C:/Users/kate.harrison/Dropbox (Boa)/EndurancePerformance/NewBalanceRC_May2021/Forces/CompiledRunData.csv')

# load footscan database

 footDat <- read_excel('C:/Users/kate.harrison/Boa Technology Inc/PFL - Documents/General/BigData2021/MasterSubjectSizes1.xlsx')
 
 
 # split into left and right 
 
 RfootDat <- subset(footDat, footDat$Side == 'R')
 LfootDat <- subset(footDat, footDat$Side == 'L')

   
   
# join dataframes

 testFootDat <- inner_join (testDat, RfootDat)
 
# Find unique subject entries

testFootDat <- testFootDat[!duplicated(testFootDat$SubjectName), ]