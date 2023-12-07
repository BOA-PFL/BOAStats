library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

# Functions
testAnovaC <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Setting*Config + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ Setting+Config + (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = 1) 
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
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
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "lace"))
  return(newList)
}

# Call the .csv file with the IMU speed data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/CompPressureOutcomes.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs'))
dat$Setting <- as.factor(dat$Setting)
dat <- dat %>%
  mutate(Subject = replace(Subject, Subject == 'ISO1','S20')) %>%
  mutate(Subject = replace(Subject, Subject == 'ISO2','S22')) %>%
  mutate(Subject = replace(Subject, Subject == 'ISO3','S06')) %>%
  mutate(Subject = replace(Subject, Subject == 'ISO4','S27')) %>%
  mutate(Subject = replace(Subject, Subject == 'ISO5','S04'))


dat1 <- dat %>% filter(Label == 1)
dat2 <- dat %>% filter(Label == 2)
dat3 <- dat %>% filter(Label == 3)

datTR1 <- dat %>% filter(Label == 1, Setting == 1)
datTR2 <- dat %>% filter(Label == 2, Setting == 1)
datTR3 <- dat %>% filter(Label == 3, Setting == 1)

datIL1 <- dat %>% filter(Label == 1, Setting == 0)
datIL2 <- dat %>% filter(Label == 2, Setting == 0)
datIL3 <- dat %>% filter(Label == 3, Setting == 0)


testAnova('HeelCon',datTR1)
testAnova('HeelCon',datTR2)
testAnova('HeelCon',datTR3)

testAnova('HeelCon',datIL1)
testAnova('HeelCon',datIL2)
testAnova('HeelCon',datIL3)

testAnovaC('HeelCon',dat1)
testAnovaC('HeelCon',dat2)
testAnovaC('HeelCon',dat3)

ggplot(data = datIL3, aes(x = HeelCon)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat1, aes(x = Setting, y = HeelCon, color = Config)) + geom_boxplot() + facet_wrap(~Subject)

avgdat <- dat %>%
  filter(Setting == 1, Label > 0) %>%
  group_by(Subject,Config,Label) %>% 
  summarize(avgIE = mean(HeelCon)) %>% 
  group_by(Config, Label) %>%
  summarize(stdvar = sd(avgIE),avgvar = mean(avgIE))

ggplot(data = avgdat, aes(x = Label, y = avgvar, fill = Config)) + geom_bar(stat = 'identity', position = position_dodge()) + theme_minimal() +
  geom_errorbar(aes(ymin = avgvar-stdvar, ymax = avgvar+stdvar), width = 0.2, position=position_dodge(0.9)) + theme(text = element_text(size = 24)) + 
  ylab('% Heel Contact') + scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none") + ylim(0,100)


avgdat <- dat %>%
  filter(Setting == 0, Label > 0, SetSpeed == 'ps') %>%
  group_by(Subject,Config,Label) %>% 
  summarize(avgIE = mean(HeelCon)) %>% 
  group_by(Config, Label) %>%
  summarize(stdvar = sd(avgIE),avgvar = mean(avgIE))

ggplot(data = avgdat, aes(x = Label, y = avgvar, fill = Config)) + geom_bar(stat = 'identity', position = position_dodge()) + theme_minimal() +
  geom_errorbar(aes(ymin = avgvar-stdvar, ymax = avgvar+stdvar), width = 0.2, position=position_dodge(0.9)) + theme(text = element_text(size = 24)) + 
  ylab('% Heel Contact') + scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none") + ylim(0,100)
