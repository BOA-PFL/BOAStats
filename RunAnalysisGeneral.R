## Bayesian analysis of run data ##
#loading required packages, etc. If you are less familiar with R, you may need to install these packages first
rm(list=ls())
library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)

dat <- read.csv(file.choose()


# Summary features --------------------------------------------------------
## Clean VLR up to BW/s ##
dat['VLR2'] <- dat$VLR / 9.81 / 75

dat %>%
  group_by(Subject, Shoe) %>%
  summarize(
    AvgRate = mean(VLR2),
    SDRate = sd(VLR2),
    pBF = mean(pBF),
    sdBF = sd(pBF),
    bImp = mean(Bimp)
    
  ) %>%
  ggplot(mapping = aes(x = Subject, y = AvgRate, fill = Shoe)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=AvgRate-SDRate, ymax=AvgRate+SDRate), width=.2,
                position=position_dodge(.9))
# brake impulse
dat['Bimp2'] <- -1 * dat['Bimp']
dat %>%
  group_by(Subject, Shoe) %>%
  summarize(
    AvgRate = mean(VLR2),
    SDRate = sd(VLR2),
    pBF = mean(pBF),
    sdBF = sd(pBF),
    bImp = mean(Bimp2),
    sdBimp = sd(Bimp2)
    
  ) %>%
  ggplot(mapping = aes(x = Subject, y = bImp, fill = Shoe)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=bImp-sdBimp, ymax=bImp+sdBimp), width=.2,
                position=position_dodge(.9))

# peak brake force
dat %>%
  group_by(Subject, Shoe) %>%
  summarize(
    AvgRate = mean(VLR2),
    SDRate = sd(VLR2),
    pBF = mean(pBF),
    sdBF = sd(pBF),
    bImp = mean(Bimp2),
    sdBimp = sd(Bimp2)
    
  ) %>%
  ggplot(mapping = aes(x = Subject, y = pBF, fill = Shoe)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=pBF-sdBF, ymax=pBF+sdBF), width=.2,
                position=position_dodge(.9))

# Scaling VLR -------------------------------------------------------------

test <- dat %>% 
  group_by(Subject) %>% 
  mutate(z_score = scale(VLR2))

ggplot(data = test) + geom_boxplot(mapping = aes(x = Subject, y = z_score, fill = Shoe))

# Probabilistic Models ----------------------------------------------------------


runmod <- brm(data = dat,
              family = gaussian,
              VLR2 ~ Shoe + (1 + Shoe |Subject), #fixed efect of configuration with a different intercept and slope for each subject
              prior = c(prior(normal(50, 75), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 10), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                        prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

print(runmod)
plot(runmod)

## predictive interval
pp <- predict(runmod)
head(pp)
modCoef <- coef(runmod)
modCoef


# The output provides estimates for the parameters of interest: in the simplified case: what is the average change in loading rate for switching from DD to lace? In this case it is about 1.1. Below, we manipualte the posterior to get some more information. 

posterior <- posterior_samples(runmod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_ShoeQuicklace > 0) / length(posterior$b_ShoeQuicklace) #There is a 81.4% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior$b_ShoeQuicklace) #The maximal a posteriori estimate 

#  Prob model with Z-score data --------------------------------------------------------------------


runmod2 <- brm(data = test,
              family = gaussian,
              z_score ~ Shoe + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
              prior = c(prior(normal(0, 10), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpceted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 10), class = b), #beta for the intercept for the change in loadinr rate for each configuration
                        #prior(cauchy(0, 5), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variabiltiy that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

print(runmod2)
plot(runmod2)

posterior2 <- posterior_samples(runmod2) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior2$b_ShoeQuicklace > 0) / length(posterior2$b_ShoeQuicklace) #There is a 98% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior2$b_ShoeQuicklace)


# no prior model ----------------------------------------------------------


runmod3 <- brm(data = test,
               family = gaussian,
               z_score ~ Shoe + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
               iter = 2000, warmup = 1000, chains = 4, cores = 4,
               control = list(adapt_delta = .975, max_treedepth = 20),
               seed = 190831)

print(runmod3)
plot(runmod3)

posterior3 <- posterior_samples(runmod3) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior3$b_ShoeQuicklace > 0) / length(posterior2$b_ShoeQuicklace) #There is a 98% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior3$b_ShoeQuicklace)

posterior3 
ggplot(data = posterior3) + geom_density(mapping = aes(x = b_ShoeQuicklace))

# peak BF model ----------------------------------------------------------


brakeMod <- brm(data = dat,
               family = gaussian,
               pBF ~ Shoe + (1 | Subject), #fixed efect of configuration with a different intercept and slope for each subject
               iter = 2000, warmup = 1000, chains = 4, cores = 4,
               control = list(adapt_delta = .975, max_treedepth = 20),
               seed = 190831)

print(brakeMod)
plot(brakeMod)

posterior3 <- posterior_samples(brakeMod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior3$b_ShoeQuicklace > 0) / length(posterior3$b_ShoeQuicklace) #There is a 98% chance lace results in a greater VLR (count the number of samples where the lace configuration intercept is greater than 0 (e.g. it is higher than DD))
mean(posterior3$b_ShoeQuicklace)

ggplot(data = posterior3) + geom_density(mapping = aes(x = b_ShoeQuicklace))

# LMER model --------------------------------------------------------------

newMod <- lmer(VLR2 ~ Shoe + (1 | Subject), data = dat)
summary(newMod)

newMod2 <- lmer(z_score ~ Shoe + (1 | Subject), data = test)
summary(newMod2)

