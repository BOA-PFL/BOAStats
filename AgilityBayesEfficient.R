library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(lme4)

rm(list=ls())

# Change to appropriate filepath
dat <- read.csv(file.choose())
#datagil <- read.csv('C:/Users/Adam.Luftglass/OneDrive - Boa Technology Inc/Documents/R/CompiledAgilityData.csv')

# Change to the movement you want to look at (we analyze CMJ and Skater separately for most agility tests)
dat <- subset(dat, dat$Movement == 'CMJ')
dat <- as_tibble(dat)


#Change to Config names used in your data, with the baseline model listed first.
dat$Config <- factor(dat$Config, c('Lace', 'LR', 'Lateralhigh', 'Lateralmid'))
dat$CTnorm <- dat$CT / dat$impulse


dat <- dat %>% 
  group_by(Sub) %>%
  mutate(z_scoreIV = scale(RAnkPeakIV),
         z_scoreEV = scale(RAnkPeakEV),
         z_scoreCT = scale(CTnorm),
         z_scoreDF = scale(RAnkDorsiflexion)) %>% # Change to the variable you want to test.Variable will be column name in dat
  group_by(Config)


dat <- subset(dat, dat$z_scoreIV < 2 & dat$z_scoreIV > -2)
dat <- subset(dat, dat$z_scoreEV < 2 &  dat$z_scoreEV > -2)
dat <-subset(dat, dat$z_scoreCT < 2 & dat$z_scoreCT > -2)
dat <- subset(dat, dat$z_scoreDF < 2 & dat$z_scoreDF > -2)


makePlot <- function(inputDF, colName) {
  # custom function pass it a variable name in quotes and get a plot
  ggplot(data = inputDF) + geom_boxplot(mapping = aes(x = Sub, y = .data[[colName]], fill = Config)) + 
    scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))
  
}


makePlot(dat, 'RAnkPeakEV')
makePlot(dat, 'RAnkPeakIV')
makePlot(dat, 'CTnorm') 

extractVals <- function(originalDat, posteriorName, colNo, colName, configName) {
  # This function takes the original dataframe, the name of the psoterior and the 
  # name of the configuration and outputs the pctChange in units of the original metric
  # this defaults the baseline configuration to lace. 
  # first output is pct posteiror below 0
  # 2nd output is pct change
  
  pctPosterior <- sum(posteriorName[,colNo] < 0) / length(posteriorName[,colNo]) 
  estimatedChange <- mean(posteriorName[,colNo]) 
  
  Ref <- subset(originalDat, originalDat$Config == 'Lace') # Change to baseline shoe name
  RefMean <- mean(Ref[[colName]]) 
  
  NewConfig <- subset(originalDat, originalDat$Config == configName) # Change to shoe being tested against baseline
  NewConfigMean <- mean(NewConfig[[colName]])   
  
  actualChange <- NewConfigMean - RefMean # outputs change from baseline to comparison shoe in units of measurement
  
  
  pctChange <- actualChange/RefMean * 100 # outputs change from baseline to comparison shoe in percentage
  
  return(list(pctPosterior, pctChange))
}

runmod <- brm(data = dat,
              family = gaussian,
              z_scoreIV ~ Config + (1 | Sub), #fixed effect of configuration with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #Since we use z-scores, the intercept prior is set as a mean of 0 with SD of 1
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in outcome for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

posteriorIV <- posterior_samples(runmod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
plot(runmod)

# pct change inversion
makePlot(dat, 'RAnkPeakIV')
extractVals(dat, posteriorIV, 2, 'RAnkPeakIV','LR')
extractVals(dat, posteriorIV, 3, 'RAnkPeakIV','Lateralhigh')
extractVals(dat, posteriorIV, 4, 'RAnkPeakIV','Lateralmid')


# eversion model
evMod <- brm(data = dat,
              family = gaussian,
              z_scoreEV ~ Config + (1 | Sub), #fixed effect of configuration with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #Since we use z-scores, the intercept prior is set as a mean of 0 with SD of 1
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in outcome for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)
plot(evMod)
posteriorEV <- posterior_samples(evMod)
# finding pct changes eversion
extractVals(dat, posteriorEV, 2, 'RAnkPeakEV','LR')
extractVals(dat, posteriorEV, 3, 'RAnkPeakEV','Lateralhigh')
extractVals(dat, posteriorEV, 4, 'RAnkPeakEV','Lateralmid')
makePlot(dat, 'RAnkPeakEV')

## contact time normalized to impulse 
ctMod <- brm(data = dat,
             family = gaussian,
             z_scoreCT ~ Config + (1 | Sub), #fixed effect of configuration with a different intercept and slope for each subject
             prior = c(prior(normal(0, 1), class = Intercept), #Since we use z-scores, the intercept prior is set as a mean of 0 with SD of 1
                       prior(normal(0, 1), class = b), #beta for the intercept for the change in outcome for each configuration
                       prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                       prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)
plot(ctMod)
posteriorCT <- posterior_samples(ctMod)
# finding pct changes CT
extractVals(dat, posteriorCT, 2,'CTnorm', 'LR')
extractVals(dat, posteriorCT, 3, 'CTnorm','Lateralhigh')
extractVals(dat, posteriorCT, 4, 'CTnorm','Lateralmid')
makePlot(dat, 'CTnorm')
