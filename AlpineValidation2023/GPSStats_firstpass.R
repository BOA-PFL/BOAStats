library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

# Functions:
testAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (1|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  newList <- list("randEffectMod" = summary(full.mod),"Coefficients" = coef(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "Buckle"))
  return(newList)
}

# Call the .csv file with the kinetics data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Snow Performance/SkiValidation_Dec2022/GPS/GPSOutcomes.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('BOA', 'Buckle'))

# Histogram
ggplot(data = dat, aes(x = TopSpeed, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = dat, aes(x = AvgSpeed, fill = Config)) + geom_histogram() + facet_wrap(~Subject)

testAnova('TopSpeed',dat)

testAnova('AvgSpeed',dat)
