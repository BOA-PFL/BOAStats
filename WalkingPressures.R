rm(list=ls())
library(tidyverse)

dat <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/Hike Work Research/Work Pilot 2021/pressuresComb.csv')
dat$Config <- as.factor(dat$Config)
dat$Subject <- as.factor(dat$Subject)

ggplot(data = dat, mapping = aes(x = Config, y = sdRHeel)) + geom_boxplot() + facet_wrap(~Subject)
ggplot(data = dat, mapping = aes(x = Config, y = meanRToes)) + geom_boxplot() + facet_wrap(~Subject)

ggplot(data = dat, mapping = aes(x = Config, y = meanRHeel)) + geom_boxplot() + facet_wrap(~Subject)

ggplot(data = dat, mapping = aes(x = Config, y = sdRLatFF)) + geom_boxplot()  + facet_wrap(~Subject)

ggplot(data = dat, mapping = aes(x = Config, y = meanRLatFF)) + geom_boxplot()  + facet_wrap(~Subject)

ggplot(data = dat, mapping = aes(x = Config, y = sdDorLMF)) + geom_boxplot()  + facet_wrap(~Subject)

ggplot(data = dat, mapping = aes(x = Config, y = sdDorMMF)) + geom_boxplot() + facet_wrap(~Subject)

ggplot(data = dat, mapping = aes(x = Config, y = CT)) + geom_boxplot()  + facet_wrap(~Subject)
