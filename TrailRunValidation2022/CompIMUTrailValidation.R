library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

# Functions
testAnovaC <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Setting*Config + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ Setting+Config + (1|Subject)"))
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
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/CompIMUmetrics.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs'))

# Replace
dat$imuSpeed <- as.numeric(dat$imuSpeed)
dat$Setting <- as.factor(dat$Setting)
dat <- dat %>%
  # filter(Setting == 0) %>%
  mutate(Subject = replace(Subject, Subject == 'IS01','S20')) %>%
  mutate(Subject = replace(Subject, Subject == 'IS02','S22')) %>%
  mutate(Subject = replace(Subject, Subject == 'IS03','S06')) %>%
  mutate(Subject = replace(Subject, Subject == 'IS04','S27')) %>%
  mutate(Subject = replace(Subject, Subject == 'IS05','S04'))

dat1 <- dat %>% filter(Label == 1)
dat2 <- dat %>% filter(Label == 2)
dat3 <- dat %>% filter(Label == 3)

ggplot(data = dat1, aes(x = Setting, y = rIEgyro, color = Config)) + geom_boxplot() + facet_wrap(~Subject)

testAnova('rIEgyro',dat1)
testAnova('rIEgyro',dat2)
testAnova('rIEgyro',dat3)


testAnovaC('rIEgyro',dat1)
testAnovaC('rIEgyro',dat2)
testAnovaC('rIEgyro',dat3)

labelnames <- c(
  '1' = "Uphill",
  '2' = "Top",
  '3' = "Downhill"
)

exdat <- dat2 %>%
  filter(Subject == 'S04', Config == 'pfs', SetSpeed == 'tr')

ggplot(exdat, aes(x = Config, y=imuSpeed)) + geom_dotplot(binaxis='y',stackdir='center', dotsize=.5) + theme_minimal() +
  scale_fill_manual(values='blue') + theme(text = element_text(size = 24), axis.text.x=element_blank()) + ylab('Speed (m/s)') + xlab('') + ylim(0,4)


exdat <- dat2 %>%
  filter(Subject == 'S04', Config == 'pfs', SetSpeed == 'ps')

ggplot(exdat, aes(x = Config, y=imuSpeed)) + geom_dotplot(binaxis='y',stackdir='center', dotsize=.5) + theme_minimal() +
  scale_fill_manual(values=c("#00966C")) + theme(text = element_text(size = 24), axis.text.x=element_blank()) + ylab('Speed (m/s)') + xlab('') + ylim(0,4)

exdat <- dat %>%
  filter(SetSpeed != 'ss', Label == 2) %>%
  group_by(Subject, SetSpeed) %>%
  summarize(varSpeed = sd(imuSpeed))

ggplot(dat = exdat, aes(x = Subject, y = varSpeed, fill = SetSpeed)) + geom_bar(stat = 'identity', position = position_dodge()) + 
  theme_minimal() + theme(text = element_text(size = 24)) + ylab('STD Speed (m/s)') + scale_fill_manual(values=c('red','blue'))

summarydat <- dat %>%
  filter(Setting == 1, Label > 0) %>%
  group_by(Subject,Config,Label) %>% 
  summarize(avgIE = mean(rIEgyro))

ggplot(data = summarydat, mapping = aes(x = Config, y = avgIE, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Inv/Ever Gyro Range [rad/sec]') + xlab('Configuration')

avgdat <- dat %>%
  filter(Setting == 1, Label > 0) %>%
  group_by(Subject,Config,Label) %>% 
  summarize(avgIE = mean(rIEgyro)) %>% 
  group_by(Config, Label) %>%
  summarize(stdvar = sd(avgIE),avgvar = mean(avgIE))

ggplot(data = avgdat, aes(x = Label, y = avgvar, fill = Config)) + geom_bar(stat = 'identity', position = position_dodge()) + theme_minimal() +
  geom_errorbar(aes(ymin = avgvar-stdvar, ymax = avgvar+stdvar), width = 0.2, position=position_dodge(0.9)) + theme(text = element_text(size = 24)) + 
  ylab('In./Ev.Range (rad/sec)') + scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none") + ylim(0,1000)


summarydat <- dat %>%
  filter(Setting == 0, Label > 0, SetSpeed == 'ps') %>%
  group_by(Subject,Config,Label) %>% 
  summarize(avgIE = mean(rIEgyro))

ggplot(data = summarydat, mapping = aes(x = Config, y = avgIE, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Inv/Ever Gyro Range [rad/sec]') + xlab('Configuration')

avgdat <- dat %>%
  filter(Setting == 0, Label > 0, SetSpeed == 'ps') %>%
  group_by(Subject,Config,Label) %>% 
  summarize(avgIE = mean(rIEgyro)) %>% 
  group_by(Config, Label) %>%
  summarize(stdvar = sd(avgIE),avgvar = mean(avgIE))

ggplot(data = avgdat, aes(x = Label, y = avgvar, fill = Config)) + geom_bar(stat = 'identity', position = position_dodge()) + theme_minimal() +
  geom_errorbar(aes(ymin = avgvar-stdvar, ymax = avgvar+stdvar), width = 0.2, position=position_dodge(0.9)) + theme(text = element_text(size = 24)) + 
  ylab('In./Ev.Range (rad/sec)') + scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none") + ylim(0,1000)

avgdat <- dat %>%
  filter(Setting == 0, Label > 0, SetSpeed == 'ps') %>%
  group_by(Subject,Config,Label) %>% 
  summarize(avgIE = mean(pJerk)) %>% 
  group_by(Config, Label) %>%
  summarize(stdvar = sd(avgIE),avgvar = mean(avgIE))

ggplot(data = avgdat, aes(x = Label, y = avgvar, fill = Config)) + geom_bar(stat = 'identity', position = position_dodge()) + theme_minimal() +
  geom_errorbar(aes(ymin = avgvar-stdvar, ymax = avgvar+stdvar), width = 0.2, position=position_dodge(0.9)) + theme(text = element_text(size = 24)) + 
  ylab('Jerk') + scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none") + ylim(0,65000)

avgdat <- dat %>%
  filter(Setting == 1, Label > 0, SetSpeed == 'tr') %>%
  group_by(Subject,Config,Label) %>% 
  summarize(avgIE = mean(pJerk)) %>% 
  group_by(Config, Label) %>%
  summarize(stdvar = sd(avgIE),avgvar = mean(avgIE))

ggplot(data = avgdat, aes(x = Label, y = avgvar, fill = Config)) + geom_bar(stat = 'identity', position = position_dodge()) + theme_minimal() +
  geom_errorbar(aes(ymin = avgvar-stdvar, ymax = avgvar+stdvar), width = 0.2, position=position_dodge(0.9)) + theme(text = element_text(size = 24)) + 
  ylab('Jerk') + scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none") + ylim(0,65000)

