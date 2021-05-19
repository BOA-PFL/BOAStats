rm(list=ls())

library(tidyverse)
library(brms)
library(lme4)
library(emmeans)

dat <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/FBS Abstract/MetabolicTrialResults/MetResults4Points.csv')
weightNew <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/FBS Abstract/MetabolicTrialResults/weights2.csv') #correcting for different mass each day
weightNew <- rename(weightNew, Subject = ï..Subject)
weightNew <- rename(weightNew, Config = Condition)

dat <- merge(dat, weightNew)


ggplot(data = dat, mapping = aes(x = as.factor(Subject), y = EEm, fill = Config)) + geom_boxplot() 

#dat <- subset(dat, dat$EEm >8) #removing non-physiological values for energetic expenditure

### Outlier removal based on 25 quantile - 1.5* IQR or 75 + 1.5*IQR ###
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Level
dat <- dat %>%
  group_by(Subject, Config) %>%
  mutate(
    EEm2 = remove_outliers(EEm)
  )
dat <- subset(dat, dat$EEm2 > 5) # removing one non physiological rates not detected by the outlier
dat['EEmFixed'] <- dat$EEm2 * dat$Ratio

ggplot(data = dat, mapping = aes(x = as.factor(Subject), y = EEmFixed, fill = Config)) + geom_boxplot() 
#Subjects 28 and 30 have very odd SL values
# 

ggplot(data = dat, mapping = aes(x = as.factor(TimePoints), y = EEmFixed, fill = Config)) + geom_point(aes(color = Config)) +
  facet_wrap( ~ Subject )
# Simplest models ---------------------------------------------------------

# Simple model collapsing across time points
mod1 <- lmer(EEmFixed ~ Config + (1|Subject), data = dat)
summary(mod1)
modnull <- lmer(EEmFixed ~ (1|Subject), data = dat)
anova(mod1,modnull) #not significant


conditions.emm <- emmeans(mod1, "Config")
#conditions.emm
contrast(conditions.emm, "trt.vs.ctrl", ref = "SL") #p-value of 0.22 for single dial vs lace, estimated 1.9% improvement


0.3/12.1979 * 100

# Adding in layer of time -------------------------------------------------

timeMod1 <- lmer(EEmFixed ~ Config + as.factor(TimePoints) + (1|Subject), data = dat)
summary(timeMod1)

conditions.emm <- emmeans(timeMod1, "Config")
#conditions.emm
contrast(conditions.emm, "trt.vs.ctrl", ref = "SL") #p-value of 0.22 for single dial vs lace, estimated 1.9% improvement

ggplot(data = dat, mapping = aes(x = as.factor(TimePoints), y = EEmFixed, fill = Config)) + geom_point(aes(color = Config)) +
  facet_wrap( ~ Subject )

nullMod <- lmer(EEm ~  as.factor(TimePoints) + (1|Subject), data = dat)
anova(timeMod1, nullMod)

timeMod2 <- lmer(EEm ~ Config * as.factor(TimePoints) + (1|Subject), data = dat)
summary(timeMod1)

#HR mod
HRdat <- subset(dat, dat$HR != 0) #when HR monitor malfunctioned
ggplot(data = HRdat, mapping = aes(x = as.factor(Subject), y = HR, fill = Config)) + geom_boxplot() 

ggplot(data = HRdat, mapping = aes(x = as.factor(TimePoints), y = HR, fill = Config)) + geom_point(aes(color = Config)) + 
  facet_wrap( ~ Subject) #Some arbitrarily low HR values for some subjects (e.g. 19 and 21 where HRM must have malfunctioned)
HRdat <- subset(HRdat, HRdat$HR > 100)

HRmod <- lmer(HR ~ Config + (1|Subject), data = HRdat)
summary(HRmod)

null2mod <- lmer(HR ~ (1|Subject), data = HRdat)
anova(HRmod, null2mod) #signficaitn impact of configuration

conditions.emm <- emmeans(HRmod, "Config")
#conditions.emm
contrast(conditions.emm, "trt.vs.ctrl", ref = "SL") #p-value of 0.22 for single dial vs lace, estimated 1.9% improvement


# Checking temperatures
tempMod <- lmer(Temp ~ Config + (1|Subject), data = dat)
summary(tempMod)
ggplot(data = dat, mapping = aes(x = as.factor(Subject), y = Temp, fill = Config)) + geom_boxplot() 

ggplot(data = dat, mapping = aes(x = as.factor(TimePoints), y = Temp, fill = Config)) + geom_point(aes(color = Config)) + 
  facet_wrap( ~ Subject)


