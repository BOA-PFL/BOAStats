library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)
 
#Clearing the environment
rm(list=ls())

# Functions:
testAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + SetSpeed + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ SetSpeed + (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
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
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 40)) + ylab(ylabel)
  
}

# Call the .csv file with the kinetics data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/InLabData/AnkleFootWork.csv')

subanth <- read_excel('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/MasterListIndoor.xlsx')
subanth <- subanth %>%
  select(c('Subject','Mass [kg]'))
subanth <- subanth %>%
  rename('Mass' = 'Mass [kg]')

dat <- left_join(dat, subanth,by = c("Subject"))

# Normalize Kinetic Outcomes by Mass
dat$NegFootWork <- scale(dat$NegFootWork/dat$Mass*mean(dat$Mass))
dat$PosFootWork <- scale(dat$PosFootWork/dat$Mass*mean(dat$Mass))

dat$NegAnkWork <- scale(dat$NegAnkWork/dat$Mass*mean(dat$Mass))
dat$PosAnkWork <- scale(dat$PosAnkWork/dat$Mass*mean(dat$Mass))

dat$NegCOMWork <- scale(dat$NegCOMWork/dat$Mass*mean(dat$Mass))
dat$PosCOMWork <- scale(dat$PosCOMWork/dat$Mass*mean(dat$Mass))

dat$SetSpeed <- scale(dat$SetSpeed)


dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs'))
dat$perc_posAnkle <- dat$PosAnkWork/dat$PosCOMWork
dat$perc_negAnkle <- dat$NegAnkWork/dat$NegCOMWork

dat <- dat %>%
  filter(Config != 'NA')

# Examine Histograms of Variables of Interest
ggplot(data = dat, aes(x = NegFootWork, fill = SetSlope)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = dat, aes(x = PosFootWork, fill = SetSlope)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = NegAnkWork, fill = SetSlope)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = dat, aes(x = PosAnkWork, fill = SetSlope)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = NegCOMWork, fill = SetSlope)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = dat, aes(x = PosCOMWork, fill = SetSlope)) + geom_histogram() + facet_wrap(~Subject)


# Best of plots:
dat1 <- dat %>% filter(SetSlope == 'p4')
dat2 <- dat %>% filter(SetSlope == 'p2')
dat3 <- dat %>% filter(SetSlope == 'n6')
withinSubPlot(dat1, colName = 'NegFootWork', dir = 'lower','Foot Absorption [J]')
withinSubPlot(dat2, colName = 'NegFootWork', dir = 'lower','Foot Absorption [J]')
withinSubPlot(dat3, colName = 'NegFootWork', dir = 'higher','Foot Absorption [J]')

# Stats:
testAnova('NegFootWork',dat1)
testAnova('NegFootWork',dat2)
testAnova('NegFootWork',dat3)

testAnova('PosFootWork',dat1)
testAnova('PosFootWork',dat2)
testAnova('PosFootWork',dat3)

testAnova('NegAnkWork',dat1)
testAnova('NegAnkWork',dat2)
testAnova('NegAnkWork',dat3)

testAnova('PosAnkWork',dat1)
testAnova('PosAnkWork',dat2)
testAnova('PosAnkWork',dat3)

testAnova('NegCOMWork',dat1)
testAnova('NegCOMWork',dat2)
testAnova('NegCOMWork',dat3)

testAnova('PosCOMWork',dat1)
testAnova('PosCOMWork',dat2)
testAnova('PosCOMWork',dat3)

testAnova('perc_posAnkle',dat1)
testAnova('perc_posAnkle',dat2)
testAnova('perc_posAnkle',dat3)

testAnova('perc_negAnkle',dat1)
testAnova('perc_negAnkle',dat2)
testAnova('perc_negAnkle',dat3)

# Plotting 
summarydat <- dat %>%
  filter(SetSpeed == 3) %>%
  group_by(Subject, Config,SetSlope) %>%
  summarize(NegFootWork = mean(NegFootWork), PosFootWork = mean(PosFootWork), 
            NegAnkWork = mean(NegAnkWork), PosAnkWork = mean(PosAnkWork))

ggplot(data = summarydat, mapping = aes(x = Config, y = NegFootWork, fill = Config)) + facet_wrap(~SetSlope) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Negative Foot Work [J]') + xlab('Configuration') + 
  ylim(-70,0) + theme(axis.text.y=element_blank()) 

ggplot(data = summarydat, mapping = aes(x = Config, y = PosFootWork, fill = Config)) + facet_wrap(~SetSlope) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Positive Foot Work [J]') + xlab('Configuration') + 
  ylim(0,70) + theme(axis.text.y=element_blank()) 

ggplot(data = summarydat, mapping = aes(x = Config, y = NegAnkWork, fill = Config)) + facet_wrap(~SetSlope) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Negative Ankle Work [J]') + xlab('Configuration') + 
  ylim(-70,0) + theme(axis.text.y=element_blank()) 

ggplot(data = summarydat, mapping = aes(x = Config, y = PosAnkWork, fill = Config)) + facet_wrap(~SetSlope) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') +
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Positive Ankle Work [J]') + xlab('Configuration') + 
  ylim(0,70) + theme(axis.text.y=element_blank())


