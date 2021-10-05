rm(list=ls())
############# Cycling pressure analysis 9/2021 ##############
library(tidyverse)
library(lme4)
library(emmeans)


dat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL - General/Cycling2021/EH_CyclingPilot_2021/mvaResults.csv')


testRandSlopes <- function(metric, df) {
  
  myformula <- as.formula(paste0(metric," ~ config", " + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (config|Subject)"))
  
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit" )
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  
  conditions.emm <- emmeans(full.mod, "config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "Velcro") 
  
  
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "Velcro"))
  return(newList)
  
}

testRandSlopesDH <- function(metric, df) {
  
  myformula <- as.formula(paste0(metric," ~ config", " + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (config|Subject)"))
  
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit" )
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  
  conditions.emm <- emmeans(full.mod, "config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "Lace") 
  
  
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "Lace"))
  return(newList)
  
}


## Tidying of names ##
dat <- dat %>% 
  mutate(condition = replace(condition, condition == 'moderate', 'Moderate'))
dat <- dat %>% 
  mutate(condition = replace(condition, condition == 'Modederate', 'Moderate'))
dat <- dat %>% 
  mutate(condition = replace(condition, condition == 'sprint1', 'Sprint1'))
dat <- dat %>% 
  mutate(condition = replace(condition, condition == 'sprint2', 'Sprint2'))
dat <- dat %>% 
  mutate(condition = replace(condition, condition == 'Sprint1', 'Sprint2'))
dat <- dat %>%
  mutate(config = replace(config, config == 'dd4guide', 'DD4Guide'))
dat <- dat %>%
  mutate(config = replace(config, config == 'DD4guide', 'DD4Guide'))
dat <- dat %>%
  mutate(config = replace(config, config == 'sdvelcro', 'SDVelcro'))
dat <- dat %>%
  mutate(config = replace(config, config == 'Velco', 'Velcro'))
dat <- dat %>%
  mutate(config = replace(config, config == 'velcro', 'Velcro'))
dat <- dat %>%
  mutate(Subject = replace(Subject, Subject == 'cristophbessler', 'christophbessler'))


### plotting ###
dat <- subset(dat, dat$endPct < 900 & dat$endPct > 0)
dat <- subset(dat, dat$initialPct < 900)
dat <- subset(dat, dat$peakPct < 900)

ggplot(data = dat, mapping = aes(x = Subject, y = initialPct, fill = config)) + geom_boxplot() +
  facet_wrap(~condition) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggplot(data = dat, mapping = aes(x = Subject, y = peakPct, fill = config)) + geom_boxplot() +
  facet_wrap(~condition) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dat, mapping = aes(x = Subject, y = endPct, fill = config)) + geom_boxplot() +
  facet_wrap(~condition) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sprintDat <- subset(dat, dat$condition == 'Sprint2')
testRandSlopes('peakPct', sprintDat)
testRandSlopes('initialPct', sprintDat) #both boa models much better
testRandSlopes('endPct', sprintDat) #both boa models better

moderateDat <- subset(dat, dat$condition == 'Moderate')
testRandSlopes('peakPct', moderateDat)
testRandSlopes('initialPct', moderateDat) #both boa models much better
testRandSlopes('endPct', moderateDat) #both boa models better


# Downhill pressure mapping -----------------------------------------------

dhDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL - General/Cycling2021/DH_PressureTest_Sept2021/mvaResults.csv')

replaceConfig <- function(DF, toReplace, newName){
  
  #replace the config with newName above
  DF <- DF %>% 
    mutate(config = replace(config, config == toReplace, newName))
  return(DF)
}
## names should be 
# 1) Lace 2) SD LR 3) DD LR
# 4) SD Quad 5) SD OP 6) DD OP

dhDat<- replaceConfig(dhDat, 'DD4guide', 'DDOP')
dhDat<- replaceConfig(dhDat, 'SL', 'Lace')
dhDat<- replaceConfig(dhDat, 'SDOv4guide', 'SDOP4guide')
dhDat<- replaceConfig(dhDat, 'SDOvguide', 'SDOP4guide')
dhDat<- replaceConfig(dhDat, 'SDOP4guide', 'SDOP')


### plotting ###
ggplot(data = dhDat, mapping = aes(x = Subject, y = initialPct, fill = config)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggplot(data = dhDat, mapping = aes(x = Subject, y = peakPct, fill = config)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dhDat, mapping = aes(x = Subject, y = endPct, fill = config)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

testRes <- testRandSlopesDH('peakPct', dhDat)
plot(testRes$contrasts)

testRes2 <- testRandSlopesDH('initialPct', dhDat)
plot(testRes2$contrasts)

testRes3 <- testRandSlopesDH('endPct', dhDat)
plot(testRes3$contrasts)
