rm(list=ls())

library(tidyverse)
library(ggplot2)

hikeDat <- read.csv(file.choose())
names(hikeDat)[1] <- 'Subject'

sub1 <- subset(sub1, sub1$HipWork < 10000) #removing unrealistically high hip work values



#### Differences in peak ankle power (lower in DD), similar to ankle work trend
ggplot(data = sub1, aes(x=as.factor(Config), y = MaxAnklePow)) + geom_boxplot() + 
  geom_point() + theme_bw(base_size = 16) + xlab('Config') +
  ylab('Peak Ankle Power') + labs(fill = 'Configuration')

ggplot(data = sub1, aes(x=as.factor(Config), y = MaxHipP)) + geom_boxplot() + 
  geom_point() + theme_bw(base_size = 16) + xlab('Config') +
  ylab('Peak Hip Power') + labs(fill = 'Configuration')

#### Force data alone ###
fDat <- read.csv(file.choose())
fDat <- subset(fDat, fDat$LoadRate > 40) #removing outlier trials

fDat %>%
  group_by(SubName, TrialNo, Config) %>%
  summarize(
    avgLR = mean(LoadRate),
    avgStepL = mean(StepLen),
    avgVImpulse = mean(vertImpulse),
    avgBImpulse = mean(brakeImpulse),
    avgPImpulse = mean(propImpulse)
  ) 

ggplot(data = fDat, mapping = aes( x = SubName, y = LoadRate, color = Config)) + 
  geom_boxplot() + geom_point() + geom_jitter() +facet_wrap(~TrialNo) + theme_bw()

ggplot(data = fDat, mapping = aes( x = SubName, y = brakeImpulse, color = Config)) + 
  geom_violin() + facet_wrap(~TrialNo) + theme_classic()
  
ggplot(data = fDat, mapping = aes( x = SubName, y = StepLen, color = Config)) + 
  geom_violin() + facet_wrap(~TrialNo) + theme_classic()










ggplot(data = dh, aes(x=as.factor(Config), y = StepLen)) + geom_boxplot() + 
  geom_point() + theme_bw(base_size = 16) + xlab('Config') +
  ylab('Stance Time (ms) DH') + labs(fill = 'Configuration')

ggplot(data = uphill, aes(x=as.factor(Config), y = StepLen)) + geom_boxplot() + 
  geom_point() + theme_bw(base_size = 16) + xlab('Config') +
  ylab('Stance Time (ms) UH') + labs(fill = 'Configuration')

ggplot(data = dh, aes(x=as.factor(Config), y = vertImpulse)) + geom_boxplot() + 
  geom_point() + theme_bw(base_size = 16) + xlab('Config') +
  ylab('Vertical Impulse') + labs(fill = 'Configuration')

#Some differences in braking impulse
ggplot(data = dh, aes(x=as.factor(Config), y = brakeImpulse)) + geom_boxplot() + 
  geom_point() + theme_bw(base_size = 16) + xlab('Config') +
  ylab('Braking Impulse DH') + labs(fill = 'Configuration')

ggplot(data = uphill, aes(x=as.factor(Config), y = brakeImpulse)) + geom_boxplot() + 
  geom_point() + theme_bw(base_size = 16) + xlab('Config') +
  ylab('Braking Impulse UH') + labs(fill = 'Configuration')

ggplot(data = dh, aes(x=as.factor(Config), y = LoadRate)) + geom_boxplot() + 
  geom_point() + theme_bw(base_size = 16) + xlab('Config') +
  ylab('Loading Rate DH') + labs(fill = 'Configuration')

ggplot(data = uphill, aes(x=as.factor(Config), y = LoadRate)) + geom_boxplot() + 
  geom_point() + theme_bw(base_size = 16) + xlab('Config') +
  ylab('Loading Rate UH') + labs(fill = 'Configuration')
