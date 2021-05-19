rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)


# SL Landings -------------------------------------------------------------

LandingDat <- read_xlsx('C:/Users/Daniel.Feeney/Dropbox (Boa)/EnduranceProtocolWork/EnduranceProtocolHike/SLLandings.xlsx')
LandingDat <- subset(LandingDat, LandingDat$Movement == 'SLlanding' & LandingDat$StabTime < 100)

ggplot(data = LandingDat, mapping = aes(x = Sub, y = StabTime, fill = Config)) + geom_boxplot() 
ggplot(data = LandingDat, mapping = aes(x = Sub, y = pkForce, fill = Config)) + geom_boxplot() 



# Pressures ---------------------------------------------------------------


walkPressure <- read_xlsx('C:/Users/Daniel.Feeney/Dropbox (Boa)/EnduranceProtocolWork/EnduranceProtocolHike/PressureWalkingResultsHike.xlsx')
walkPressure <- subset(walkPressure, walkPressure$Condition != 'log')

walkPressure['cvMets'] = walkPressure['sdMets'] / walkPressure['meanMets']
walkPressure['cvFF'] = walkPressure['sdFF'] / walkPressure['meanFF']
walkPressure['cvMF'] = walkPressure['sdMF'] / walkPressure['meanMF']

ggplot(data = walkPressure, mapping = aes(x = Subject, y = sdMF, fill = Config)) + geom_boxplot() + facet_grid(~Condition)
ggplot(data = walkPressure, mapping = aes(x = Subject, y = sdMets, fill = Config)) + geom_boxplot() + facet_grid(~Condition)
ggplot(data = walkPressure, mapping = aes(x = Subject, y = sdFF, fill = Config)) + geom_boxplot() + facet_grid(~Condition)

ggplot(data = walkPressure, mapping = aes(x = Subject, y = MidStanceHeelP, fill = Config)) + geom_boxplot() + facet_grid(~Condition)
ggplot(data = walkPressure, mapping = aes(x = Subject, y = HeelDecay, fill = Config)) + geom_boxplot() + facet_grid(~Condition)

ggplot(data = walkPressure, mapping = aes(x = Subject, y = maxPlantMetP, fill = Config)) + geom_boxplot() + facet_grid(~Condition)
ggplot(data = walkPressure, mapping = aes(x = Subject, y = maxToeP, fill = Config)) + geom_boxplot() + facet_grid(~Condition)


# Walking Forces ----------------------------------------------------------

walkForce <- read_xlsx('C:/Users/Daniel.Feeney/Dropbox (Boa)/EnduranceProtocolWork/EnduranceProtocolHike/WalkForcesResultsHike.xlsx')
#WalkForce$PkLat <- -1 * WalkForce$PkLat

# remove VLR outliers
walkForce <- subset(walkForce, as.numeric(walkForce$VLR) < 10000)
walkForce <- subset(walkForce, abs(walkForce$brakeImpulse) < 19000)

ggplot(data = walkForce, mapping = aes(x = Config, y = abs(peakBrake), fill = Config)) + geom_boxplot() + facet_wrap(~ Level + Subject)

ggplot(data = walkForce, mapping = aes(x = Config, y = abs(brakeImpulse), fill = Config)) + geom_boxplot()+ facet_wrap(~ Level + Subject)

ggplot(data = walkForce, mapping = aes(x = Config, y = as.numeric(VLR), fill = Config)) + geom_boxplot()+ facet_wrap(~ Level + Subject)

ggplot(data = walkForce, mapping = aes(x = Config, y = PkMed, fill = Config)) + geom_boxplot()+ facet_wrap(~ Level + Subject)

ggplot(data = walkForce, mapping = aes(x = Config, y = PkLat, fill = Config)) + geom_boxplot()+facet_wrap(~ Level + Subject)

ggplot(data = walkForce, mapping = aes(x = Config, y = NL, fill = Config)) + geom_boxplot()+facet_wrap(~ Level + Subject)
