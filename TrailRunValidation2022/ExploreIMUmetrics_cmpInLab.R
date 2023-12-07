library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

# Functions
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

testAnovaC <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + imuSpeed + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ imuSpeed + (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "lace"))
  return(newList)
}

testAnovaS <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + imuSpeed + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ Config + (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "lace"))
  return(newList)
}

testAnova_small <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config", " + (1|Subject)"))
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

withinSubPlot <- function(inputDF, colName, dir,ylabel) {
  # Specify ylabel in function or default to the original name
  if(missing(ylabel)){
    ylabel = paste0({{colName}})
  }
  
  # direction can be 'lower' or higher'. It is the direction of change that is better.
  # For example, for contact time lower is better. so we put 'lower'. for jump height, higher is better, so we put higher.
  meanDat <- inputDF %>%
    group_by(Subject, Config) %>%
    summarize(mean = mean(!! sym(colName)))
  
  if (dir == 'lower'){
    whichConfig <- meanDat %>%
      group_by(Subject) %>%
      summarize(
        BestConfig = Config[which.min(mean)]
      )
    
  } else if (dir == 'higher') {
    whichConfig <- meanDat %>%
      group_by(Subject) %>%
      summarize(
        BestConfig = Config[which.max(mean)]
      )
    
  }
  
  # "Best of" line plot code
  whichConfig <- merge(meanDat, whichConfig)
  
  ggplot(data = whichConfig, mapping = aes(x = as.factor(Config), y = mean, col = BestConfig, group = Subject)) + geom_point(size = 4) +
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 26)) + ylab(ylabel)
  
}


# Call the .csv file with the IMU speed data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/IMUmetrics.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs'))

# Filter subjects 
dat <- dat %>% filter(Subject == "S20" | Subject == "S25" |
                        Subject == "S22" | Subject == "S13" |
                        Subject == "S31" | Subject == "S05" | 
                        Subject == "S02" | Subject == "S04" |
                        Subject == "S27" | Subject == "S24",
                      Label > 0)
dat$imuSpeed <- as.numeric(dat$imuSpeed)

dat1 <- dat %>% filter(Label == 1)
dat2 <- dat %>% filter(Label == 2)
dat3 <- dat %>% filter(Label == 3)

withinSubPlot(dat1, colName = 'pIEgyro', dir = 'lower','Max Ev. Vel. (deg/s)')
withinSubPlot(dat2, colName = 'pIEgyro', dir = 'lower','Max Ev. Vel. (deg/s)')
withinSubPlot(dat3, colName = 'pIEgyro', dir = 'lower','Max Ev. Vel. (deg/s)')

testAnovaS('pJerk',dat1)
testAnovaC('pJerk',dat1)
testAnovaS('pJerk',dat2)
testAnovaC('pJerk',dat2)
testAnovaS('pJerk',dat3)
testAnovaC('pJerk',dat3)

testAnova('pJerk',dat1)
testAnova('pJerk',dat2)
testAnova('pJerk',dat3)

testAnovaS('rMLacc',dat1)
testAnovaC('rMLacc',dat1)
testAnovaS('rMLacc',dat2)
testAnovaC('rMLacc',dat2)
testAnovaS('rMLacc',dat3)
testAnovaC('rMLacc',dat3)

testAnova('rMLacc',dat1)
testAnova('rMLacc',dat2)
testAnova('rMLacc',dat3)

testAnovaS('pAcc',dat1)
testAnovaC('pAcc',dat1)
testAnovaS('pAcc',dat2)
testAnovaC('pAcc',dat2)
testAnovaS('pAcc',dat3)
testAnovaC('pAcc',dat3)

testAnova('pAcc',dat1)
testAnova('pAcc',dat2)
testAnova('pAcc',dat3)

testAnovaS('pIEgyro',dat1)
testAnovaC('pIEgyro',dat1)
testAnovaS('pIEgyro',dat2)
testAnovaC('pIEgyro',dat2)
testAnovaS('pIEgyro',dat3)
testAnovaC('pIEgyro',dat3)

testAnova('pIEgyro',dat1)
testAnova('pIEgyro',dat2)
testAnova('pIEgyro',dat3)


# Examine the different variables
ggplot(data = dat, aes(x = pJerk, color = Config)) + geom_histogram() + facet_wrap(~Subject) + xlim(0,100000)

ggplot(data = dat, aes(x = pAcc, color = Config)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = rMLacc, color = Config)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = pIEgyro, color = Config)) + geom_histogram() + facet_wrap(~Subject)+ xlim(0,2000)


ggplot(data = dfvar, aes(x = rMLacc, color = Config)) + geom_histogram() + facet_wrap(~Subject)

dfvar1 <- dfvar %>% filter(Label == 1)
dfvar2 <- dfvar %>% filter(Label == 2)
dfvar3 <- dfvar %>% filter(Label == 3)

testAnova('rMLacc',dfvar1)
testAnova('rMLacc',dfvar2)
testAnova('rMLacc',dfvar3)

summarydat <- dat %>%
  group_by(Subject, Config, Label) %>%
  filter(Label > 0) %>%
  summarize(pJerk = mean(pJerk), avgML = mean(rMLacc), varML = var(rMLacc), pIEgyro = mean(pIEgyro))

bardat <- summarydat %>%
  group_by(Config,Label) %>%
  summarize(sdJerk = sd(pJerk), avgJerk = mean(pJerk), sdIEgyro = sd(rIEgyro),avgIEgyro = mean(rIEgyro), sdML = sd(avgML), avgML = mean(avgML))

summarydat1 <-summarydat %>% filter(Label == 1)
summarydat2 <-summarydat %>% filter(Label == 2)
summarydat3 <-summarydat %>% filter(Label == 3)

testAnova_small('sdML',summarydat1)
testAnova_small('sdML',summarydat2)
testAnova_small('sdML',summarydat3)

labelnames <- c(
  '1' = "Uphill",
  '2' = "Top",
  '3' = "Downhill"
)

# Bar Plots
avgdat <- dat %>%
  filter(Label > 0) %>%
  group_by(Subject,Config,Label) %>% 
  summarize(avgIE = mean(rIEgyro)) %>% 
  group_by(Config, Label) %>%
  summarize(stdvar = sd(avgIE),avgvar = mean(avgIE))

ggplot(data = avgdat, aes(x = Label, y = avgvar, fill = Config)) + geom_bar(stat = 'identity', position = position_dodge()) + theme_minimal() +
  geom_errorbar(aes(ymin = avgvar-stdvar, ymax = avgvar+stdvar), width = 0.2, position=position_dodge(0.9)) + theme(text = element_text(size = 24)) + 
  ylab('In./Ev.Range (rad/sec)') + scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none") + ylim(0,1000)


ggplot(data = bardat, mapping = aes(x = Config, y = avgJerk, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_bar(stat='identity') + theme_minimal() + 
  geom_errorbar(aes(ymin = avgJerk-sdJerk, ymax = avgJerk+sdJerk)) + theme(text = element_text(size = 24)) + ylab(bquote('Peak Jerk '(m/s^3))) + xlab('Configuration') + 
  scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none")

ggplot(data = bardat, mapping = aes(x = Config, y = avgIEgyro, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_bar(stat='identity') + theme_minimal() + 
  geom_errorbar(aes(ymin = avgIEgyro-sdIEgyro, ymax = avgIEgyro+sdIEgyro)) + theme(text = element_text(size = 24)) + ylab('In./Ev.Range (rad/sec)') + xlab('Configuration') + 
  scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none")

ggplot(data = bardat, mapping = aes(x = Config, y = avgML, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_bar(stat='identity') + theme_minimal() + 
  geom_errorbar(aes(ymin = avgML-sdML, ymax = avgML+sdML)) + theme(text = element_text(size = 24)) + ylab(bquote('M/L Acc. Range '(m/s^2))) + xlab('Configuration') + 
  scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none")

# point and error bar plot
ggplot(data = summarydat, mapping = aes(x = Config, y = pJerk, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab(bquote('Peak Jerk '(m/s^3))) + xlab('Configuration') + 
  theme(axis.text.y=element_blank())

ggplot(data = summarydat, mapping = aes(x = Config, y = pIEgyro, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('In./Ev.Range (rad/sec)') + xlab('Configuration') # + 
  # theme(axis.text.y=element_blank())

ggplot(data = summarydat, mapping = aes(x = Config, y = avgML, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab(bquote('M/L Acc. Range '(m/s^2))) + xlab('Configuration') + 
  theme(axis.text.y=element_blank())

# Overall plots
ggplot(data = summarydat, mapping = aes(x = Config, y = pJerk, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Peak Jerk [m/s^3]') + xlab('Configuration')

ggplot(data = summarydat, mapping = aes(x = Config, y = avgML, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('M/L Acc. Range [m/s^2]') + xlab('Configuration')

ggplot(data = summarydat, mapping = aes(x = Config, y = sdML, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('SD M/L Acc. Range [m/s^2]') + xlab('Configuration')

ggplot(data = summarydat, mapping = aes(x = Config, y = rIEgyro, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Inv/Ever Gyro Range [rad/sec]') + xlab('Configuration')

