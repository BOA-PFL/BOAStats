rm(list=ls())
library(tidyverse)
library(patchwork)

dat <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/AgilityPerformance/COPanalysis.csv')
dat <- subset(dat, dat$CT > 20)
dat <- subset(dat, dat$impulse > 100)
dat$CTNorm <- ( dat$CT / dat$impulse ) * 100
dat <- subset(dat, dat$CTNorm < 1)
dat$RFD <- abs(dat$RFD)

skate <- subset(dat, dat$Movement == 'Skater')
cmj <- subset(dat, dat$Movement == 'CMJ')

p1 <- ggplot(data = skate, mapping = aes(x = Config, y = copExc)) + geom_boxplot() + facet_wrap(~Sub)

p2 <- ggplot(data = skate, mapping = aes(x = Config, y = timingDiff)) + geom_boxplot() + facet_wrap(~Sub)

p3 <- ggplot(data = skate, mapping = aes(x = Config, y = COPtraj)) + geom_boxplot() + facet_wrap(~Sub)

p1 / p2 / p3

r1 <- ggplot(data = skate, mapping = aes(x = copExc, y = CTNorm, color = Config)) + geom_point() + facet_wrap(~Sub) +
  ggtitle('Skater Jump Contact Time vs. COP excursion') + theme_bw()

r2 <- ggplot(data = cmj, mapping = aes(x = copExc, y = CTNorm, color = Config)) + geom_point() + facet_wrap(~Sub) +
  ggtitle('CMJ Contact Time vs. COP excursion') + theme_bw()
r1/r2

ggplot(data = skate, mapping = aes(x = RFD, y = CTNorm, color = Config)) + geom_point() + facet_wrap(~Sub) +
  ggtitle('Skater Jump RFD concentric vs. COP excursion') + theme_bw()

ggplot(data = skate, mapping = aes(x = timingDiff, y = RFD, color = Config)) + geom_point() + facet_wrap(~Sub) +
  ggtitle('Skater Jump Time Diff to RFD concentric') + theme_bw()

ggplot(data = skate, mapping = aes(x = COPtraj, y = CTNorm, color = Config)) + geom_point() + facet_wrap(~Sub) +
  ggtitle('Skater Jump Time CT Norm vs. COPTraj') + theme_bw()
