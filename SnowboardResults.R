
rm(list=ls())

library(tidyverse)
library(lme4)
library(patchwork)

dat <- read.csv(file.choose())
dat <- read.csv(file.choose())


# exploration of data -----------------------------------------------------

dat %>%
  group_by(Subject, Config, Side, TurnType) %>%
  summarize(
    avgPkForce = mean(MaxForceToes),
    avgMaxRFDUp = mean(MaxRFDUp),
    avgMaxRFDdn = mean(MaxRFDdn),
    avgTimeToPeak = mean(timeToPeak),
    avgSDPeak = mean(stdPeak)
  )

#dat <- subset(dat, dat$MaxRFDUp < 300) #based on initial look, removing unrealistic RFD values
dat <- subset(dat, dat$Side == 'R')

p1 <- ggplot(data = dat, mapping = aes(x = Subject, y = MaxForceToes, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)


p2 <- ggplot(data = dat, mapping = aes(x = Subject, y = MaxRFDUp, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

p3 <- ggplot(data = dat, mapping = aes(x = Subject, y = abs(MaxRFDdn), fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

(p1 | p2 ) / p3

ggplot(data = dat, mapping = aes(x = Subject, y = timeToPeak, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

ggplot(data = dat, mapping = aes(x = Subject, y = stdPeak / MaxForceToes, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

toeDat <- subset(dat, dat$TurnType == 'Toes')
pkForce.mod <- lmer(MaxForceToes ~ Config + (1 | Subject), data = toeDat)
summary(pkForce.mod)

RFDdn.mod <- lmer(abs(MaxRFDdn) ~ Config + (1 | Subject), data = toeDat)
summary(RFDdn.mod)

timeToPeak.mod <- lmer(timeToPeak ~ Config + (1 | Subject), data = toeDat)
summary(timeToPeak.mod)


# Probabilistic Models ----------------------------------------------------------
library(brms)

pkForceModel <- brm(data = toeDat,
                  family = gaussian,
                  MaxForceToes ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
                  prior = c(prior(normal(50, 15), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                            prior(normal(0, 20), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                            prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                            prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
                  iter = 2000, warmup = 1000, chains = 4, cores = 4,
                  control = list(adapt_delta = .975, max_treedepth = 20),
                  seed = 190831)

print(pkForceModel)
plot(pkForceModel)

posterior <- posterior_samples(pkForceModel) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigDD < 0) / length(posterior$b_ConfigDD) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigDD) #The maximal a posteriori estimate 

# peak force estimate to be 6 N higher in BOA witha  confidence of 64%


RFDdnModel <- brm(data = dat,
              family = gaussian,
              abs(MaxRFDdn) ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
              prior = c(prior(normal(50, 20), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 10), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                        prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

print(RFDdnModel)
plot(RFDdnModel)

posterior <- posterior_samples(RFDdnModel) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigLace < 0) / length(posterior$b_ConfigLace) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigLace) #The maximal a posteriori estimate 

# less of a difference in Bayesian model than Prob model

timeToPeakMod <- brm(data = dat,
                  family = gaussian,
                  timeToPeak ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
                  prior = c(prior(normal(50, 20), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                            prior(normal(0, 10), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                            prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                            prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
                  iter = 2000, warmup = 1000, chains = 4, cores = 4,
                  control = list(adapt_delta = .975, max_treedepth = 20),
                  seed = 190831)

print(timeToPeakMod)
plot(timeToPeakMod)

posterior <- posterior_samples(timeToPeakMod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigLace > 0) / length(posterior$b_ConfigLace) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigLace) #The maximal a posteriori estimate 

# estimated 0.5 seconds longer to get to peak to peak force (9%) with 90% confidence


### L/R differences
boaDat <- subset(dat, dat$Config == 'BOA')
ggplot(data = boaDat, mapping = aes(x = Subject, y = MaxForceToes, fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

ggplot(data = boaDat, mapping = aes(x = Subject, y = timeToPeak, fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

ggplot(data = boaDat, mapping = aes(x = Subject, y = MaxRFDUp, fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

ggplot(data = boaDat, mapping = aes(x = Subject, y = abs(MaxRFDdn), fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

rm(list=ls())

library(tidyverse)
library(lme4)

dat <- read.csv(file.choose())


# exploration of data -----------------------------------------------------

dat %>%
  group_by(Subject, Config, Side, TurnType) %>%
  summarize(
    avgPkForce = mean(MaxForceToes),
    avgMaxRFDUp = mean(MaxRFDUp),
    avgMaxRFDdn = mean(MaxRFDdn),
    avgTimeToPeak = mean(timeToPeak),
    avgSDPeak = mean(stdPeak)
  )

dat <- subset(dat, dat$MaxRFDUp < 300) #based on initial look, removing unrealistic RFD values

ggplot(data = dat, mapping = aes(x = Subject, y = MaxForceToes, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)


ggplot(data = dat, mapping = aes(x = Subject, y = MaxRFDUp, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

ggplot(data = dat, mapping = aes(x = Subject, y = abs(MaxRFDdn), fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

ggplot(data = dat, mapping = aes(x = Subject, y = timeToPeak, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

ggplot(data = dat, mapping = aes(x = Subject, y = stdPeak / MaxForceToes, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

toeDat <- subset(dat, dat$TurnType == 'Toes')
pkForce.mod <- lmer(MaxForceToes ~ Config + (1 | Subject), data = toeDat)
summary(pkForce.mod)

RFDdn.mod <- lmer(abs(MaxRFDdn) ~ Config + (1 | Subject), data = toeDat)
summary(RFDdn.mod)

timeToPeak.mod <- lmer(timeToPeak ~ Config + (1 | Subject), data = toeDat)
summary(timeToPeak.mod)


# Probabilistic Models ----------------------------------------------------------
library(brms)

pkForceModel <- brm(data = toeDat,
                  family = gaussian,
                  MaxForceToes ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
                  prior = c(prior(normal(50, 15), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                            prior(normal(0, 20), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                            prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                            prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
                  iter = 2000, warmup = 1000, chains = 4, cores = 4,
                  control = list(adapt_delta = .975, max_treedepth = 20),
                  seed = 190831)

print(pkForceModel)
plot(pkForceModel)

posterior <- posterior_samples(pkForceModel) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigLace < 0) / length(posterior$b_ConfigLace) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigLace) #The maximal a posteriori estimate 

# peak force estimate to be 6 N higher in BOA witha  confidence of 64%


RFDdnModel <- brm(data = dat,
              family = gaussian,
              abs(MaxRFDdn) ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
              prior = c(prior(normal(50, 20), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 10), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                        prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

print(RFDdnModel)
plot(RFDdnModel)

posterior <- posterior_samples(RFDdnModel) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigLace < 0) / length(posterior$b_ConfigLace) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigLace) #The maximal a posteriori estimate 

# less of a difference in Bayesian model than Prob model

timeToPeakMod <- brm(data = dat,
                  family = gaussian,
                  timeToPeak ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
                  prior = c(prior(normal(50, 20), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                            prior(normal(0, 10), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                            prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                            prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
                  iter = 2000, warmup = 1000, chains = 4, cores = 4,
                  control = list(adapt_delta = .975, max_treedepth = 20),
                  seed = 190831)

print(timeToPeakMod)
plot(timeToPeakMod)

posterior <- posterior_samples(timeToPeakMod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigLace > 0) / length(posterior$b_ConfigLace) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigLace) #The maximal a posteriori estimate 

# estimated 0.5 seconds longer to get to peak to peak force (9%) with 90% confidence


### L/R differences
boaDat <- subset(dat, dat$Config == 'BOA')
ggplot(data = boaDat, mapping = aes(x = Subject, y = MaxForceToes, fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

ggplot(data = boaDat, mapping = aes(x = Subject, y = timeToPeak, fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

ggplot(data = boaDat, mapping = aes(x = Subject, y = MaxRFDUp, fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

ggplot(data = boaDat, mapping = aes(x = Subject, y = abs(MaxRFDdn), fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

rm(list=ls())

library(tidyverse)
library(lme4)

dat <- read.csv(file.choose())


# exploration of data -----------------------------------------------------

dat %>%
  group_by(Subject, Config, Side, TurnType) %>%
  summarize(
    avgPkForce = mean(MaxForceToes),
    avgMaxRFDUp = mean(MaxRFDUp),
    avgMaxRFDdn = mean(MaxRFDdn),
    avgTimeToPeak = mean(timeToPeak),
    avgSDPeak = mean(stdPeak)
  )

dat <- subset(dat, dat$MaxRFDUp < 300) #based on initial look, removing unrealistic RFD values

ggplot(data = dat, mapping = aes(x = Subject, y = MaxForceToes, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)


ggplot(data = dat, mapping = aes(x = Subject, y = MaxRFDUp, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

ggplot(data = dat, mapping = aes(x = Subject, y = abs(MaxRFDdn), fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

ggplot(data = dat, mapping = aes(x = Subject, y = timeToPeak, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

ggplot(data = dat, mapping = aes(x = Subject, y = stdPeak / MaxForceToes, fill = Config)) + geom_boxplot() +
  facet_wrap(~Side + TurnType)

toeDat <- subset(dat, dat$TurnType == 'Toes')
pkForce.mod <- lmer(MaxForceToes ~ Config + (1 | Subject), data = toeDat)
summary(pkForce.mod)

RFDdn.mod <- lmer(abs(MaxRFDdn) ~ Config + (1 | Subject), data = toeDat)
summary(RFDdn.mod)

timeToPeak.mod <- lmer(timeToPeak ~ Config + (1 | Subject), data = toeDat)
summary(timeToPeak.mod)


# Probabilistic Models ----------------------------------------------------------
library(brms)

pkForceModel <- brm(data = toeDat,
                  family = gaussian,
                  MaxForceToes ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
                  prior = c(prior(normal(50, 15), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                            prior(normal(0, 20), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                            prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                            prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
                  iter = 2000, warmup = 1000, chains = 4, cores = 4,
                  control = list(adapt_delta = .975, max_treedepth = 20),
                  seed = 190831)

print(pkForceModel)
plot(pkForceModel)

posterior <- posterior_samples(pkForceModel) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigLace < 0) / length(posterior$b_ConfigLace) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigLace) #The maximal a posteriori estimate 

# peak force estimate to be 6 N higher in BOA witha  confidence of 64%


RFDdnModel <- brm(data = dat,
              family = gaussian,
              abs(MaxRFDdn) ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
              prior = c(prior(normal(50, 20), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 10), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                        prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

print(RFDdnModel)
plot(RFDdnModel)

posterior <- posterior_samples(RFDdnModel) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigLace < 0) / length(posterior$b_ConfigLace) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigLace) #The maximal a posteriori estimate 

# less of a difference in Bayesian model than Prob model

timeToPeakMod <- brm(data = dat,
                  family = gaussian,
                  timeToPeak ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
                  prior = c(prior(normal(50, 20), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                            prior(normal(0, 10), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                            prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                            prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
                  iter = 2000, warmup = 1000, chains = 4, cores = 4,
                  control = list(adapt_delta = .975, max_treedepth = 20),
                  seed = 190831)

print(timeToPeakMod)
plot(timeToPeakMod)

posterior <- posterior_samples(timeToPeakMod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigLace > 0) / length(posterior$b_ConfigLace) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigLace) #The maximal a posteriori estimate 

# estimated 0.5 seconds longer to get to peak to peak force (9%) with 90% confidence


### L/R differences
boaDat <- subset(dat, dat$Config == 'BOA')
ggplot(data = boaDat, mapping = aes(x = Subject, y = MaxForceToes, fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

ggplot(data = boaDat, mapping = aes(x = Subject, y = timeToPeak, fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

ggplot(data = boaDat, mapping = aes(x = Subject, y = MaxRFDUp, fill = Side)) + geom_boxplot() +
  facet_wrap(~as.factor(TurnType))

ggplot(data = boaDat, mapping = aes(x = Subject, y = abs(MaxRFDdn), fill = Side)) + geom_boxplot() +

