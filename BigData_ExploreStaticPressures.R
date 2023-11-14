rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(patchwork)
library(effsize)
library(effects)o
library(ggstatsplot)
library(performance)

#-------------------------------------------------------------------------------
# The purpose of this code is to examine A&S databases and compare metrics
# against one another
#-------------------------------------------------------------------------------
ASPressDB <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/AgilityPressureDB.csv')
ASPressDB$Subject <- tolower(gsub(" ", "", ASPressDB$Subject)) # Remove spaces in config names
staticDB <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/StaticPressureDB.csv')
staticDB$Subject <- tolower(gsub(" ", "", staticDB$Subject)) # Remove spaces in config names
subDB <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/MasterSubjectVisits.csv')
subDB$Subject <- tolower(gsub(" ", "", subDB$Subject)) # Remove spaces in config names
perDB <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/QualitativeBigData_v2.csv')
perDB$Subject <- tolower(gsub(" ", "", perDB$Subject)) # Remove spaces in config names

staticDB <- staticDB %>%
  filter(Movement == "Standing")

combStaticDB <- left_join(staticDB,perDB, by = c("Subject","Config","Brand", "Year","Month", "Model"))
combStaticDB <- left_join(combStaticDB, subDB, by = c("Subject","Brand", "Year","Month", "Model"))


# Eval different pressures
ggscatterstats(data = combStaticDB, x=maxDorsalPressure, y=Overall, marginal = FALSE, bf.message = FALSE)
ggscatterstats(data = combStaticDB, x=meanDorsalPressure, y=Overall, marginal = FALSE, bf.message = FALSE)
ggscatterstats(data = combStaticDB, x=sdDorsalPressure, y=Overall, marginal = FALSE, bf.message = FALSE)
ggscatterstats(data = combStaticDB, x=totalDorsalPressure, y=Overall, marginal = FALSE, bf.message = FALSE)
ggscatterstats(data = combStaticDB, x=ffDorsalMaxPressure, y=Overall, marginal = FALSE, bf.message = FALSE)
ggscatterstats(data = combStaticDB, x=mfDorsalMaxPressure, y=Overall, marginal = FALSE, bf.message = FALSE)



my_mod = lmer('Overall ~ toePressure + (1|Model)', data = combStaticDB, REML = TRUE, na.action = "na.omit")
summary(my_mod)
coef(my_mod)
r2(my_mod)



