rm(list=ls())
library(tidyverse)
library(patchwork)

dat <- read.csv('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/BigData2021/BigDataAgility_newMetrics.csv')

dat <- subset(dat, dat$CT > 20)
dat <- subset(dat, dat$impulse > 100)
dat$CTNorm <- ( dat$CT / dat$impulse ) * 100
dat <- subset(dat, dat$CTNorm < 1)
dat <- subset(dat, dat$copExc > 0.01)
dat$RFD <- abs(dat$RFD)
dat <- dat %>% 
  mutate(Subject = replace(Subject, Subject == 'Ted', 'Ted Barnett'))

skate <- subset(dat, dat$Movement == 'Skater' | dat$Movement == 'skater')
cmj <- subset(dat, dat$Movement == 'CMJ' | dat$Movement == 'cmj')

ggplot(data = skate, mapping = aes(x = Config, y = copExc)) + geom_boxplot() + facet_wrap(~Subject)

ggplot(data = skate, mapping = aes(x = Config, y = timingDiff)) + geom_boxplot() + facet_wrap(~Subject)

ggplot(data = skate, mapping = aes(x = Config, y = COPtraj)) + geom_boxplot() + facet_wrap(~Subject)


ggplot(data = skate, mapping = aes(x = copExc, y = CTNorm, color = Config)) + geom_point() + facet_wrap(~Subject) +
  ggtitle('Skater Jump Contact Time vs. COP excursion') + theme_bw()

ggplot(data = cmj, mapping = aes(x = copExc, y = CTNorm, color = Config)) + geom_point() + facet_wrap(~Subject) +
  ggtitle('CMJ Contact Time vs. COP excursion') + theme_bw()

ggplot(data = skate, mapping = aes(x = RFD, y = CTNorm, color = Config)) + geom_point() + facet_wrap(~Subject) +
  ggtitle('Skater Jump RFD concentric vs. COP excursion') + theme_bw()

ggplot(data = skate, mapping = aes(x = timingDiff, y = RFD, color = Config)) + geom_point() + facet_wrap(~Subject) +
  ggtitle('Skater Jump Time Diff to RFD concentric') + theme_bw()

ggplot(data = skate, mapping = aes(x = COPtraj, y = CTNorm, color = Config)) + geom_point() + facet_wrap(~Subject) +
  ggtitle('Skater Jump Time CT Norm vs. COPTraj') + theme_bw()
