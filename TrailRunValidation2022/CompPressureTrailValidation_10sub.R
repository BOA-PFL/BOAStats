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
  #conditions.emm
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
  #conditions.emm
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
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "Coefficients" = coef(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "lace"))
  return(newList)
}

#_______________________________________________________________________________
# Call the .csv file with the in-lab pressure data
datI <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/InLabData/InLabPressureOutcomes.csv')

datI <- as_tibble(datI) # creating the data frame
datI$Config <- factor(datI$Config, c('lace', 'pfs','sock'))

datI$Setting <- as.factor(rep(1, dim(datI)[1]))

datI <- datI %>%
  select(-c(SetSpeed))

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
#_______________________________________________________________________________
# Call the .csv file with the outdoor pressure data
datO <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/InLabData/OutPressureOutcomes.csv')

datO <- as_tibble(datO) # creating the data frame
datO$Config <- factor(datO$Config, c('lace', 'pfs'))
datO$Setting <- as.factor(rep(0, dim(datO)[1]))
datO$Speed <- rep('ps', dim(datO)[1])
#_______________________________________________________________________________
# Combine the two dataframes
dat <- rbind(datI,datO)
dat$Setting <- as.factor(dat$Setting)

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
# Heel contact statistics
testAnova('HeelCon',datIL1)
testAnova('HeelCon',datTR1)
testAnovaLoc('HeelCon',dat1)
testAnovaInt('HeelCon',dat1)

testAnova('HeelCon',datIL2)
testAnova('HeelCon',datTR2)
testAnovaLoc('HeelCon',dat2)
testAnovaInt('HeelCon',dat2)

testAnova('HeelCon',datIL3)
testAnova('HeelCon',datTR3)
testAnovaLoc('HeelCon',dat3)
testAnovaInt('HeelCon',dat3)

#_______________________________________________________________________________
# Heel peak pressure statistics
testAnova('m_heelPP',datIL1)
testAnova('m_heelPP',datTR1)
testAnovaLoc('m_heelPP',dat1)
testAnovaInt('m_heelPP',dat1)

testAnova('m_heelPP',datIL2)
testAnova('m_heelPP',datTR2)
testAnovaLoc('m_heelPP',dat2)
testAnovaInt('m_heelPP',dat2)

testAnova('m_heelPP',datIL3)
testAnova('m_heelPP',datTR3)
testAnovaLoc('m_heelPP',dat3)
testAnovaInt('m_heelPP',dat3)

#_______________________________________________________________________________
# Toe contact statistics
testAnova('ToeCon',datIL1)
testAnova('ToeCon',datTR1)
testAnovaLoc('ToeCon',dat1)
testAnovaInt('ToeCon',dat1)

testAnova('ToeCon',datIL2)
testAnova('ToeCon',datTR2)
testAnovaLoc('ToeCon',dat2)
testAnovaInt('ToeCon',dat2)

testAnova('ToeCon',datIL3)
testAnova('ToeCon',datTR3)
testAnovaLoc('ToeCon',dat3)
testAnovaInt('ToeCon',dat3)

#_______________________________________________________________________________
# Toe peak pressure statistics
testAnova('m_toePP',datIL1)
testAnova('m_toePP',datTR1)
testAnovaLoc('m_toePP',dat1)
testAnovaInt('m_toePP',dat1)

testAnova('m_toePP',datIL2)
testAnova('m_toePP',datTR2)
testAnovaLoc('m_toePP',dat2)
testAnovaInt('m_toePP',dat2)

testAnova('m_toePP',datIL3)
testAnova('m_toePP',datTR3)
testAnovaLoc('m_toePP',dat3)
testAnovaInt('m_toePP',dat3)

#_______________________________________________________________________________
# Histograms

ggplot(data = datIL3, aes(x = HeelCon)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat1, aes(x = Setting, y = HeelCon, color = Config)) + geom_boxplot() + facet_wrap(~Subject)

#_______________________________________________________________________________
summarydat1 <- dat %>%
  filter(Label == 1) %>%
  group_by(Subject, Config, Setting) %>%
  summarize(mHeelCon = mean(HeelCon), mHeelPP = mean(m_heelPP), mToeCon = mean(ToeCon), mToePP = mean(m_toePP))

summarydat2 <- dat %>%
  filter(Label == 2) %>%
  group_by(Subject, Config, Setting) %>%
  summarize(mHeelCon = mean(HeelCon), mHeelPP = mean(m_heelPP), mToeCon = mean(ToeCon), mToePP = mean(m_toePP))

summarydat3 <- dat %>%
  filter(Label == 3) %>%
  group_by(Subject, Config, Setting) %>%
  summarize(mHeelCon = mean(HeelCon), mHeelPP = mean(m_heelPP), mToeCon = mean(ToeCon), mToePP = mean(m_toePP))


labelnames <- c(
  '1' = "Indoor",
  '0' = "Outdoor")

# point and error bar plot
# Heel Contact Area
ggplot(data = summarydat1, mapping = aes(x = Config, y = mHeelCon, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Heel Contact [%]') + xlab('Configuration') + ylim(0,100) + theme(axis.text.y=element_blank()) 

ggplot(data = summarydat2, mapping = aes(x = Config, y = mHeelCon, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Heel Contact [%]') + xlab('Configuration') + ylim(0,100) + theme(axis.text.y=element_blank())

ggplot(data = summarydat3, mapping = aes(x = Config, y = mHeelCon, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Heel Contact [%]') + xlab('Configuration') + ylim(0,100) + theme(axis.text.y=element_blank())

# Heel Peak Pressure

ggplot(data = summarydat1, mapping = aes(x = Config, y = mHeelPP, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Heel Peak Pressure [kPa]') + xlab('Configuration') + ylim(0,600) + theme(axis.text.y=element_blank())

ggplot(data = summarydat2, mapping = aes(x = Config, y = mHeelPP, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Heel Peak Pressure [kPa]') + xlab('Configuration') + ylim(0,600) + theme(axis.text.y=element_blank())

ggplot(data = summarydat3, mapping = aes(x = Config, y = mHeelPP, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Heel Peak Pressure [kPa]') + xlab('Configuration') + ylim(0,600) + theme(axis.text.y=element_blank())


# Toe Contact Area
ggplot(data = summarydat1, mapping = aes(x = Config, y = mToeCon, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Toe Contact [%]') + xlab('Configuration') + ylim(0,100) + theme(axis.text.y=element_blank()) 

ggplot(data = summarydat2, mapping = aes(x = Config, y = mToeCon, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Toe Contact [%]') + xlab('Configuration') + ylim(0,100) + theme(axis.text.y=element_blank())

ggplot(data = summarydat3, mapping = aes(x = Config, y = mToeCon, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Toe Contact [%]') + xlab('Configuration') + ylim(0,100) + theme(axis.text.y=element_blank())

# Heel Peak Pressure

ggplot(data = summarydat1, mapping = aes(x = Config, y = mToePP, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Toe Peak Pressure [kPa]') + xlab('Configuration') + ylim(0,800) + theme(axis.text.y=element_blank())

ggplot(data = summarydat2, mapping = aes(x = Config, y = mToePP, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Toe Peak Pressure [kPa]') + xlab('Configuration') + ylim(0,800) + theme(axis.text.y=element_blank())

ggplot(data = summarydat3, mapping = aes(x = Config, y = mToePP, fill = Config)) + facet_grid(~Setting,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Toe Peak Pressure [kPa]') + xlab('Configuration') + ylim(0,800) + theme(axis.text.y=element_blank())



# +
#   theme(axis.text.y=element_blank())





