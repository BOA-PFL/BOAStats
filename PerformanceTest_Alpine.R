######## Alpine Ski Analysis #############

library(tidyverse)

dat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL - General/Snow Performance/Alpine_V1vsV2_Internal_Feb2022/testresults.csv')
dat$Subject <- as.factor(dat$Subject)
dat$Configuration <- as.factor(dat$Configuration)

ggplot(data = dat, mapping = aes(x = Configuration, y = PeakForce, color = Configuration)) +
  geom_boxplot() + facet_wrap(~ Subject + TurnType)
