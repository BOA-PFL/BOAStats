library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

###############

testAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (1|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "lace"))
  return(newList)
}

# Call the .csv file with the IMU speed data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/CombinedGPS.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs'))

# Filter subjects that did not have repeatable laps
dat <- dat %>% filter(Subject != "S19")

# Examine Historgrams
ggplot(data = dat, aes(x = AvgHRS1, color = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = dat, aes(x = AvgHRS2, color = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = dat, aes(x = AvgHRS3, color = Config)) + geom_histogram() + facet_wrap(~Subject)

testAnova('AvgHRS1',dat)
testAnova('AvgHRS2',dat)
testAnova('AvgHRS3',dat)
