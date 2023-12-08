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
ASPDB <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/AgilityPressureDB.csv')
RunDB <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/WalkRunDB.csv')
subDB <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/MasterSubjectVisits.csv')

subDB$Subject <- gsub(" ", "", subDB$Subject) # Remove spaces in config names
subDB <- subDB %>%
  select(-c(Benefit,Name.of.Test,Type,Speed.run.,Resistance))

RunDB <- RunDB %>%
  rename('Subject' = ï..Subject)
RunDB$Config <- gsub(" ", "", RunDB$Config) # Remove spaces in config names



# Only examine tests from 2022-23
ASDB <- ASDB %>%
  rename('Subject' = ï..Subject) %>%
  filter(Year > 2021)
ASDB$Config <- gsub(" ", "", ASDB$Config) # Remove spaces in config names

ASPDB <- ASPDB %>%
  filter(Year > 2021)
ASPDB$Config <- gsub(" ", "", ASPDB$Config) # Remove spaces in config names

CMJdat <- ASDB %>%
  filter(Movement == 'CMJ') %>%
  filter(PropForce > 500)

ggplot(CMJdat, aes(x=PropForce, y=ContactTime, label = Config)) + geom_point(size=2) + geom_text(hjust=0, vjust=0) +
  xlab('Propulsive Force (N)') + ylab('Contact Time (sec)')

ggscatterstats(data = CMJdat, x=PropForce, y=ContactTime, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Propulsive Force (N)', ylab = 'Contact Time (sec)')

Skaterdat <- ASDB %>%
  filter(Movement == 'Skater')

# Join the databases
combdat <- left_join(CMJdat, RunDB,
                     by = c("Subject",'Config' , "Brand", "Year","Month", "Model"))

combdat <- left_join(combdat,subDB, 
                      by = c("Subject","Brand", "Year","Month", "Model"))

combdat <- combdat %>%
  filter(PropForce != 'NA', HeelContact != 'NA', Mass != 'NA')

ggplot(combdat, aes(x=HeelContact, y=PropForce)) + geom_point(size=2)

combdat$PropForce <- combdat$PropForce/(combdat$Mass*9.81) 

ggscatterstats(data = combdat, x=HeelContact, y=PropForce, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Running: Heel Contact (%)', ylab = 'CMJ: Propulsive Force (BW)')

ggscatterstats(data = combdat, x=HeelContact, y=ContactTime, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Running: Heel Contact (%)', ylab = 'CMJ: ContactTime (sec)')

ggscatterstats(data = combdat, x=PropForce, y=ContactTime, marginal = FALSE, bf.message = FALSE, 
               xlab = 'CMJ: Propulsive Force (BW)', ylab = 'CMJ: Contact Time (sec)')

combdat <- combdat %>%
  filter(PeakAnklePFMoment < 1000)

combdat$PeakAnklePFMoment <- combdat$PeakAnklePFMoment/combdat$Mass

ggscatterstats(data = combdat, x=HeelContact, y=PeakAnklePFMoment, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Running: Heel Contact (%)', ylab = 'CMJ: Peak Plantarflexion Moment (Nm/kg)')

# Join the databases
combdat <- left_join(Skaterdat, RunDB,
                     by = c("Subject",'Config' , "Brand", "Year","Month", "Model"))

combdat <- left_join(combdat,subDB, 
                     by = c("Subject","Brand", "Year","Month", "Model"))

combdat <- combdat %>%
  filter(PropForce != 'NA', HeelContact != 'NA', Mass != 'NA')

combdat$PropForce <- combdat$PropForce/(combdat$Mass*9.81)
combdat$PeakAnklePFMoment <- combdat$PeakAnklePFMoment/combdat$Mass

ggplot(combdat, aes(x=HeelContact, y=PropForce)) + geom_point(size=2)

ggscatterstats(data = combdat, x=HeelContact, y=PropForce, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Running: Heel Contact (%)', ylab = 'Skater: Propulsive Force (BW)')

ggscatterstats(data = combdat, x=HeelContact, y=ContactTime, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Running: Heel Contact (%)', ylab = 'Skater: Contact Time (sec)')

ggscatterstats(data = combdat, x=HeelContact, y=PeakAnklePFMoment, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Running: Heel Contact (%)', ylab = 'Skater: Peak Plantarflexion Moment (Nm/kg)')

