library(tidyverse)
library(lme4)

rm(list=ls())

dat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/Testing Segments/Snow Performance/2022/AlpinePressureMapping_Dec2022/Pressure/CompiledResults.csv')

# EDA ---------------------------------------------------------------------
# note: dorsal

dat$calfCVPressure <- dat$calfSDPressure / dat$calfAvgPressure
dat$shinCVPressure <- dat$sdShinPressure / dat$meanShinPressure

ggplot(data = dat, aes(x = Subject, y = calfContact, color = Config)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dat, aes(x = Subject, y = meanShinPressure, color = Config)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dat, aes(x = Subject, y = maxShinPressure, color = Config)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dat, aes(x = Subject, y = totalShinPressure, color = Config)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dat, aes(x = Subject, y = calfPeakPressure, color = Config)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dat, aes(x = Subject, y = calfAvgPressure, color = Config)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dat, aes(x = Subject, y = calfSDPressure, color = Config)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dat, aes(x = Subject, y = calfTotalPressure, color = Config)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dat, aes(x = Subject, y = calfCVPressure, color = Config)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = dat, aes(x = Subject, y = shinCVPressure, color = Config)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Summary: 4/5 testers had substantially greater contact on their calf &
# improved closure. 

contactMod <- lmer(calfContact ~ Config + (1|Subject), data = dat)
summary(contactMod)

# positive means greater contact area (good)
dat %>%
  group_by(Subject)%>%
  mutate(pctChange = ((calfContact - calfContact[Config=='Buckle']) / calfContact) * 100 )%>%
  select('pctChange')%>%
  filter(pctChange != 0)

# negative means lower CV (good thing)
dat %>%
  group_by(Subject)%>%
  mutate(pctChange = ((calfCVPressure - calfCVPressure[Config=='Buckle']) / calfCVPressure) * 100 )%>%
  select('pctChange')%>%
  filter(pctChange != 0)

# negative means lower CV (good thing)
dat %>%
  group_by(Subject)%>%
  mutate(pctChange = ((shinCVPressure - shinCVPressure[Config=='Buckle']) / shinCVPressure) * 100 )%>%
  select('pctChange')%>%
  filter(pctChange != 0)

