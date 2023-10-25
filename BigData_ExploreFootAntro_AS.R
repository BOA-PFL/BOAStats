rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(patchwork)
library(effsize)
library(ggstatsplot)

#-------------------------------------------------------------------------------
# The purpose of this code is to examine A&S databases and compare metrics
# against one another
#-------------------------------------------------------------------------------
ASDB <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/AgilitySpeedDB.csv')
ASDB <- ASDB %>%
  rename('Subject' = ï..Subject)
ASDB$Subject <- tolower(gsub(" ", "", ASDB$Subject)) # Remove spaces in config names

# Open the Aetrex Master Scan
aetrexdat <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/FootScan Data/MasterSubjectSizes_March2023.csv')
aetrexdat$Subject <- tolower(gsub(" ", "", aetrexdat$Subject)) # Remove spaces in config names


aetrexdat <- aetrexdat %>%
  filter(Side == 'R') %>%
  select(-c(Email,CadFileMade,Side))

combDat <- left_join(ASDB, aetrexdat, by = 'Subject')

combDat <- combDat %>%
  filter(Sex == 'M', Movement == 'Skater')

ggscatterstats(data = combDat, x=Instep, y=ContactTime, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Instep', ylab = 'Contact Time (sec)')

ggscatterstats(data = combDat, x=Width, y=ContactTime, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Width', ylab = 'Contact Time (sec)')

ggscatterstats(data = combDat, x=HeelWidth, y=ContactTime, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Heel Width', ylab = 'Contact Time (sec)')

ggscatterstats(data = combDat, x=Girth, y=ContactTime, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Girth', ylab = 'Contact Time (sec)')

ggscatterstats(data = combDat, x=HeelWidth, y=PropForce, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Heel Width', ylab = 'Propulsive Force (N)')

