rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(brms)
dat <- read_xlsx(file.choose())

## Use this script by importing resuls from the Carv system. Both prob
## and frequentist models are written in here

ggplot(data = dat, mapping = aes(x = Subject, y = SkiIQ, fill = Config)) + geom_boxplot()


#dat <- subset(dat, dat$Subject != 'AA')
# Variables of interest ---------------------------------------------------

ggplot(data = dat, mapping = aes(x = Subject, y = ForwardStance, fill = Config)) + geom_boxplot()

fwdStanceMod <- lmer(ForwardStance ~ Config + (1 | Subject), data = dat)
summary(fwdStanceMod)

## Foot roll
ggplot(data = dat, mapping = aes(x = Subject, y = FootRoll, fill = Config)) + geom_boxplot()

footRolleMod <- lmer(FootRoll ~ Config + (1 | Subject), data = dat)
summary(footRolleMod)

## EdgeSymmetry
ggplot(data = dat, mapping = aes(x = Subject, y = EdgeSymmetry, fill = Config)) + geom_boxplot()

EdgeSymmetryMod <- lmer(EdgeSymmetry ~ Config + (1 | Subject), data = dat)
summary(EdgeSymmetryMod)

## PressureSymmetry
ggplot(data = dat, mapping = aes(x = Subject, y = PressureSymmetry, fill = Config)) + geom_boxplot()

PressureSymmetryMod <- lmer(PressureSymmetry ~ Config + (1 | Subject), data = dat)
summary(PressureSymmetryMod)

## Rotary
ggplot(data = dat, mapping = aes(x = Subject, y = Rotary, fill = Config)) + geom_boxplot()

RotaryMod <- lmer(Rotary ~ Config + (1 | Subject), data = dat)
summary(RotaryMod)



# Prob models -------------------------------------------------------------

ForwardStanceMod <- brm(data = dat,
                    family = gaussian,
                    ForwardStance ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
                    prior = c(prior(normal(50, 15), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                              prior(normal(0, 20), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                              prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                              prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
                    iter = 2000, warmup = 1000, chains = 4, cores = 4,
                    control = list(adapt_delta = .975, max_treedepth = 20),
                    seed = 190831)

print(ForwardStanceMod)
plot(ForwardStanceMod)

posterior <- posterior_samples(ForwardStanceMod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigBuckles < 0) / length(posterior$b_ConfigBuckles) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigBuckles) #The maximal a posteriori estimate 

# Foot roll
FootRolleMod <- brm(data = dat,
                        family = gaussian,
                        FootRoll ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
                        prior = c(prior(normal(50, 15), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                                  prior(normal(0, 20), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                                  prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                                  prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
                        iter = 2000, warmup = 1000, chains = 4, cores = 4,
                        control = list(adapt_delta = .975, max_treedepth = 20),
                        seed = 190831)

print(FootRolleMod)
plot(FootRolleMod)

posterior <- posterior_samples(FootRolleMod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigBuckles < 0) / length(posterior$b_ConfigBuckles) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigBuckles) #The maximal a posteriori estimate 



# Edge Symmetry
RotaryMod <- brm(data = dat,
                    family = gaussian,
                    EdgeSimilarity ~ Config + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
                    prior = c(prior(normal(50, 15), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                              prior(normal(0, 20), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                              prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                              prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
                    iter = 2000, warmup = 1000, chains = 4, cores = 4,
                    control = list(adapt_delta = .975, max_treedepth = 20),
                    seed = 190831)

print(EdgeSymMod)
plot(EdgeSymMod)

posterior <- posterior_samples(EdgeSymMod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ConfigBuckles < 0) / length(posterior$b_ConfigBuckles) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ConfigBuckles) #The maximal a posteriori estimate 

