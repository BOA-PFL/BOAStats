library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)
library(lmerTest)

#Clearing the environment
rm(list=ls())

# Functions
testAnovaInt <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Setting*Config + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ Setting + Config + (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "Coefficients" = coef(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = 1))
  return(newList)
}

testAnovaLoc <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Setting + Config + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ Config + (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "Coefficients" = coef(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = 1))
  return(newList)
}

testAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "Coefficients" = coef(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "lace"))
  return(newList)
}

#_______________________________________________________________________________
# Call the .csv file with the in-lab data
datI <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/InLabData/InLabIMUmetrics.csv')

datI <- as_tibble(datI) # creating the data frame
datI$Config <- factor(datI$Config, c('lace', 'pfs','sock'))

datI$Setting <- as.factor(rep(1, dim(datI)[1]))

# Convert from the indoor subject #s to the outdoor subject #s
datI <- datI %>%
  mutate(Subject = replace(Subject, Subject == 'S01','S20')) %>%
  mutate(Subject = replace(Subject, Subject == 'S02','S25')) %>%
  mutate(Subject = replace(Subject, Subject == 'S03','S22')) %>%
  mutate(Subject = replace(Subject, Subject == 'S04','S13')) %>%
  mutate(Subject = replace(Subject, Subject == 'S05','S31')) %>%
  mutate(Subject = replace(Subject, Subject == 'S06','S05')) %>%
  mutate(Subject = replace(Subject, Subject == 'S07','S04')) %>%
  mutate(Subject = replace(Subject, Subject == 'S08','S27')) %>%
  mutate(Subject = replace(Subject, Subject == 'S09','S02')) %>%
  mutate(Subject = replace(Subject, Subject == 'S10','S24'))

test <- datI %>%
  select(1:4)

#_______________________________________________________________________________
# Call the .csv file with the outdoor
datO <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/InLabData/IMUmetrics_trail.csv')

datO <- as_tibble(datO) # creating the data frame
datO$Config <- factor(datO$Config, c('lace', 'pfs'))
datO$Setting <- as.factor(rep(0, dim(datO)[1]))
datO$Speed <- rep('ps', dim(datO)[1])

#_______________________________________________________________________________
# Combine the two dataframes
dat <- rbind(datI,datO)
dat$Setting <- as.factor(dat$Setting)
dat$imuSpeed <- as.numeric(dat$imuSpeed)

# Just look at the person-specific speeds
dat <- dat %>% filter(Speed == 'ps')

dat1 <- dat %>% filter(Label == 1)
dat2 <- dat %>% filter(Label == 2)
dat3 <- dat %>% filter(Label == 3)

datTR1 <- dat %>% filter(Label == 1, Setting == 0)
datTR2 <- dat %>% filter(Label == 2, Setting == 0)
datTR3 <- dat %>% filter(Label == 3, Setting == 0)

datIL1 <- dat %>% filter(Label == 1, Setting == 1)
datIL2 <- dat %>% filter(Label == 2, Setting == 1)
datIL3 <- dat %>% filter(Label == 3, Setting == 1)
#_______________________________________________________________________________
# Peak Jerk statistics
testAnova('pJerk',datIL1)
testAnova('pJerk',datTR1)
testAnovaLoc('pJerk',dat1)
testAnovaInt('pJerk',dat1)

testAnova('pJerk',datIL2)
testAnova('pJerk',datTR2)
testAnovaLoc('pJerk',dat2)
testAnovaInt('pJerk',dat2)

testAnova('pJerk',datIL3)
testAnova('pJerk',datTR3)
testAnovaLoc('pJerk',dat3)
testAnovaInt('pJerk',dat3)

#_______________________________________________________________________________
# Peak Acceleration statistics
testAnova('pAcc',datIL1)
testAnova('pAcc',datTR1)
testAnovaLoc('pAcc',dat1)
testAnovaInt('pAcc',dat1)

testAnova('pAcc',datIL2)
testAnova('pAcc',datTR2)
testAnovaLoc('pAcc',dat2)
testAnovaInt('pAcc',dat2)

testAnova('pAcc',datIL3)
testAnova('pAcc',datTR3)
testAnovaLoc('pAcc',dat3)
testAnovaInt('pAcc',dat3)

#_______________________________________________________________________________
# Peak IE Gyro statistics
testAnova('pIEgyro',datIL1)
testAnova('pIEgyro',datTR1)
testAnovaLoc('pIEgyro',dat1)
testAnovaInt('pIEgyro',dat1)

testAnova('pIEgyro',datIL2)
testAnova('pIEgyro',datTR2)
testAnovaLoc('pIEgyro',dat2)
testAnovaInt('pIEgyro',dat2)

testAnova('pIEgyro',datIL3)
testAnova('pIEgyro',datTR3)
testAnovaLoc('pIEgyro',dat3)
testAnovaInt('pIEgyro',dat3)


#_______________________________________________________________________________
# IMU Speed statistics
testAnova('imuSpeed',datTR1)
testAnova('imuSpeed',datTR2)
testAnova('imuSpeed',datTR3)

testAnova('imuSpeed',datIL1)
testAnova('imuSpeed',datIL2)
testAnova('imuSpeed',datIL3)

testAnovaInt('imuSpeed',dat1)
testAnovaInt('imuSpeed',dat2)
testAnovaInt('imuSpeed',dat3)

testAnovaLoc('imuSpeed',dat1)
testAnovaLoc('imuSpeed',dat2)
testAnovaLoc('imuSpeed',dat3)

#_______________________________________________________________________________
# M/L Acceleration statistics
testAnova('rMLacc',datIL1)
testAnova('rMLacc',datTR1)
testAnovaLoc('rMLacc',dat1)
testAnovaInt('rMLacc',dat1)

testAnova('rMLacc',datIL2)
testAnova('rMLacc',datTR2)
testAnovaLoc('rMLacc',dat2)
testAnovaInt('rMLacc',dat2)

testAnova('rMLacc',datIL3)
testAnova('rMLacc',datTR3)
testAnovaLoc('rMLacc',dat3)
testAnovaInt('rMLacc',dat3)


#_______________________________________________________________________________
summarydat1 <- dat %>%
  filter(Label == 1) %>%
  group_by(Subject, Config, Setting) %>%
  summarize(mpJerk = mean(pJerk), mpAcc = mean(pAcc), mpGyro = mean(pIEgyro), prMLacc = mean(rMLacc))

summarydat2 <- dat %>%
  filter(Label == 2) %>%
  group_by(Subject, Config, Setting) %>%
  summarize(mpJerk = mean(pJerk), mpAcc = mean(pAcc), mpGyro = mean(pIEgyro), prMLacc = mean(rMLacc))

summarydat3 <- dat %>%
  filter(Label == 3) %>%
  group_by(Subject, Config, Setting) %>%
  summarize(mpJerk = mean(pJerk), mpAcc = mean(pAcc), mpGyro = mean(pIEgyro), prMLacc = mean(rMLacc))

labelnames <- c(
  '1' = "Indoor",
  '0' = "Outdoor")

# point and error bar plot
# Peak Jerk
ggplot(data = summarydat1, mapping = aes(x = Config, y = mpJerk, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Peak Jerk [m/s3]') + xlab('Configuration') + ylim(0,8e4) + theme(axis.text.y=element_blank()) 

ggplot(data = summarydat2, mapping = aes(x = Config, y = mpJerk, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Peak Jerk [m/s3]') + xlab('Configuration') + ylim(0,8e4) + theme(axis.text.y=element_blank())

ggplot(data = summarydat3, mapping = aes(x = Config, y = mpJerk, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Peak Jerk [m/s3]') + xlab('Configuration') + ylim(0,8e4) + theme(axis.text.y=element_blank())

# Peak Acceleration
ggplot(data = summarydat1, mapping = aes(x = Config, y = mpAcc, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Peak Acceleration [m/s2]') + xlab('Configuration') + ylim(0,400) + theme(axis.text.y=element_blank()) 

ggplot(data = summarydat2, mapping = aes(x = Config, y = mpAcc, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Peak Acceleration [m/s2]') + xlab('Configuration') + ylim(0,400) + theme(axis.text.y=element_blank())

ggplot(data = summarydat3, mapping = aes(x = Config, y = mpAcc, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Peak Acceleration [m/s2]') + xlab('Configuration') + ylim(0,400) + theme(axis.text.y=element_blank())

# Peak Gyro
ggplot(data = summarydat1, mapping = aes(x = Config, y = mpGyro, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Peak Ev. Vel. [deg/s]') + xlab('Configuration') + ylim(0,900) + theme(axis.text.y=element_blank()) 

ggplot(data = summarydat2, mapping = aes(x = Config, y = mpGyro, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Peak Ev. Vel. [deg/s]') + xlab('Configuration') + ylim(0,900) + theme(axis.text.y=element_blank())

ggplot(data = summarydat3, mapping = aes(x = Config, y = mpGyro, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Peak Ev. Vel. [deg/s]') + xlab('Configuration') + ylim(0,900) + theme(axis.text.y=element_blank())

# Medial/Lateral Range
ggplot(data = summarydat1, mapping = aes(x = Config, y = prMLacc, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('M/L Acc. Range [m/s2]') + xlab('Configuration') + ylim(0,250) + theme(axis.text.y=element_blank()) 

ggplot(data = summarydat2, mapping = aes(x = Config, y = prMLacc, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('M/L Acc. Range [m/s2]') + xlab('Configuration') + ylim(0,250) + theme(axis.text.y=element_blank())

ggplot(data = summarydat3, mapping = aes(x = Config, y = prMLacc, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('M/L Acc. Range [m/s2]') + xlab('Configuration') + ylim(0,250) + theme(axis.text.y=element_blank())

