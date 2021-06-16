
rm(list=ls())
library(tidyverse)
library(lme4)


# ROC data ----------------------------------------------------------------


dat <- read.csv(file.choose())

ggplot(data = dat, mapping = aes(x = Comparison, y = ROC)) + geom_point(aes(color = Config)) + facet_wrap( ~ Subject)

fullmod <- lmer(ROC ~ Config * Comparison + (1|Subject), data = dat)
summary(fullmod)


# Accuracy alone ----------------------------------------------------------


accdat <- read.csv(file.choose())

ggplot(data = accdat, mapping = aes(x = Config, y = Accuracy)) + geom_boxplot()

accmod <- lmer(Accuracy ~ Config  * (1| Subject), data = accdat)
summary(accmod)

