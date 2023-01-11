

library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)

rm(list=ls())

####### Functions

withinSubPlot <- function(inputDF, colName, dir) {
  
  # direction can be 'lower' or higher'. It is the direction of change that is better. 
  # For example, for contact time lower is better. so we put 'lower'. for jump height, higher is better, so we put higher. 
  meanDat <- inputDF %>%
    group_by(Subject, Config) %>%
    summarize(mean = mean(!! sym(colName)))
  
  if (dir == 'lower'){
    whichConfig <- meanDat %>%
      group_by(Subject) %>%
      summarize(
        BestConfig = Config[which.min(mean)]
      )
    
  } else if (dir == 'higher') {
    whichConfig <- meanDat %>%
      group_by(Subject) %>%
      summarize(
        BestConfig = Config[which.max(mean)]
      )
    
  }
  
  whichConfig <- merge(meanDat, whichConfig)
  
  ggplot(data = whichConfig, mapping = aes(x = as.factor(Config), y = mean, col = BestConfig, group = Subject)) + geom_point(size = 4) + 
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 16)) + ylab(paste0({{colName}})) 
  
}

#
extractVals <- function(dat, mod, configNames, baseConfig, var, dir) {
  
  #dat <- skaterDat
  #mod <- runmod
  #configNames <- otherConfigs
  #baseConfig <- baseline
  #var <- 'peakPFmom'
  #dir <- 'higher'
  
  
  Config = rep(NA, length(configNames))
  ProbImp = matrix(0, length(configNames))
  lowCI = matrix(0, length(configNames))
  highCI = matrix(0, length(configNames))
  
  for (i in 1:length(configNames)) {
    # This function takes the original dataframe (dat, same one entered into runmod), the Bayesian model from brms (runmod), 
    # the configuration Name, and the variable you are testing. It returns:
    # [1] the probabality the variable was better in the test config vs. the baseline config
    # [3] the lower bound of the bayesian 95% posterior interval (as percent change from baseline) 
    # [4] the upper bound of the bayesian 95% posterior interval (as percent change from baseline)
    #i = 1
    
    configName = configNames[i]
    configColName <- paste('b_Config', configName, sep = "")
    posterior <- as_draws_matrix(mod)
    
    if (dir == 'lower'){
      prob <- sum(posterior[,configColName] < 0) / length(posterior[,configColName])
      
    } else if (dir == 'higher') {
      
      prob <- sum(posterior[,configColName] > 0) / length(posterior[,configColName])
    }
    
    ci <- posterior_interval(mod, prob = 0.80)
    ciLow <- ci[configColName,1] 
    ciHigh <- ci[configColName,2]
    
    SDdat <- dat %>%
      group_by(Subject) %>%
      summarize(sd = sd(!! sym(var), na.rm = TRUE), mean = mean(!! sym(var), na.rm = TRUE))
    
    meanSD = mean(SDdat$sd)
    mean = mean(SDdat$mean)
    ci_LowPct <- meanSD*ciLow/mean*100
    ci_HighPct <- meanSD*ciHigh/mean*100
    
    output = list('Config:', configName, 'Probability of Improvement:', prob, 'Worse end of CI:', ci_LowPct, 'Best end of CI:', ci_HighPct)
    Config[i] = configName
    ProbImp[i] = prob
    lowCI[i] = ci_LowPct
    highCI[i] = ci_HighPct
  }
  ProbImp = round(ProbImp*100)
  lowCI = round(lowCI, 1)
  highCI = round(highCI,1)
  output = cbind(Config, ProbImp, lowCI, highCI)
  
  colnames(output) = c('Config', 'Probability of Improvement', 'Low end of CI', 'High end of CI')
  
  sentences = rep(NA, nrow(output))
  
  for (i in 1:nrow(output)){
    if (as.numeric(output[i,2]) >= 90){
      sentences[i] <- paste0('We have meaningful confidence that ',output[i,1], ' outperformed ', baseConfig, ' (',output[i,2], '%)', '\n', '\t', '- Estimated difference: ',output[i,3],' to ',output[i,4],'%' )
    } else if (as.numeric(output[i,2]) >= 80) {      
      sentences[i] <- paste('We have moderate confidence that',output[i,1], 'outperformed', baseConfig, '(',output[i,2], '%)','\n', '\t', '- Estimated difference:',output[i,3],'to',output[i,4],'%')
    } else if (as.numeric(output[i,2]) >= 70){
      sentences[i] <- paste('We have minimal confidence that',output[i,1], 'outperformed', baseConfig, '(',output[i,2], '%)','\n', '\t', 'Estimated difference:',output[i,3],'to',output[i,4],'%')
    } else if (as.numeric(output[i,2]) >= 30){
      sentences[i] <- paste('There were inconsistent differences between',output[i,1],'and',baseConfig,'(',output[i,2],'%)','\n', '\t', 'Estimated difference:',output[i,3],'to',output[i,4],'%')
    } else if (as.numeric(output[i,2]) >= 20){
      sentences[i] <- paste('We have minimal confidence that',output[i,1],'performed worse than',baseConfig,'(',(100 - as.numeric(output[i,2])),'%)','\n', '\t', 'Estimated difference:',output[i,3],'to',output[i,4],'%')
    } else if (as.numeric(output[i,2]) >= 10){
      sentences[i] <- paste('We have moderate confidence that',output[i,1],'performed worse than',baseConfig,'(',(100 - as.numeric(output[i,2])),'%)','\n', '\t', 'Estimated difference:',output[i,3],'to',output[i,4],'%')
    } else {
      sentences[i] <- paste('We have meaningful confidence that',output[i,1],'performed worse than',baseConfig,'(',(100 - as.numeric(output[i,2])),'%)','\n', '\t', 'Estimated difference:',output[i,3],'to',output[i,4],'%')
    }
  }
  
  writeLines(sentences)
  return()
}

###############################

dat <- read.csv(file.choose())

dat <- as_tibble(dat)

#dat <- subset(dat, dat$Movement == 'Trail')

baseConfig <- 'Lace' # baseline config

otherConfigs <- c('WM', 'WL') # list configs being tested against baseline

allConfigs <- c(baseConfig, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs)


############ Checking data 

dat <- dat %>% 
  filter(ContactTime < 35) %>% ### Filter out impossible contact times. 35 for running. 100? for walking?
  group_by(Subject) %>%
  mutate(z_score = scale(ContactTime)) %>% 
  group_by(Config)

dat<- subset(dat, dat$z_score < 2) #removing outliers  
dat<- subset(dat, dat$z_score > -2)

ggplot(data = dat, aes(x = ContactTime)) + geom_histogram() + facet_wrap(~Subject) 



dat <- dat %>% 
  #filter(meanTotalP < 35) %>% ### Filter out impossible contact times. 35 for running. 100? for walking?
  group_by(Subject) %>%
  mutate(z_score = scale(meanTotalP)) %>% 
  group_by(Config)

dat<- subset(dat, dat$z_score < 2) #removing outliers  
dat<- subset(dat, dat$z_score > -2)

ggplot(data = dat, aes(x = meanTotalP)) + geom_histogram() + facet_wrap(~Subject) 

###### Heel Pressure Variability

dat <- dat %>% 
  filter(cvHeel > 0.1) %>%
  group_by(Subject) %>%
  mutate(z_score = scale(cvHeel)) %>% 
  group_by(Config)

dat<- subset(dat, dat$z_score < 2) #removing outliers  
dat<- subset(dat, dat$z_score > -2)

ggplot(data = dat, aes(x = cvHeel)) + geom_histogram() + facet_wrap(~Subject) 



p <- withinSubPlot(dat, colName = 'cvHeel', dir = 'lower')
p+ ylab('Variation in Heel pressure')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# Change configName to the config you want to compare to baseline (must match config name in data sheet)

extractVals(dat, runmod, otherConfigs, baseConfig, 'cvHeel', 'lower') 



###### Heel Contact Area Late Stance

dat <- dat %>% 
  #filter(cvHeel > 0.1) %>%
  group_by(Subject) %>%
  mutate(z_score = scale(heelArea)) %>% 
  group_by(Config)

dat<- subset(dat, dat$z_score < 2) #removing outliers  
dat<- subset(dat, dat$z_score > -2)

ggplot(data = dat, aes(x = heelArea)) + geom_histogram() + facet_wrap(~Subject) 



p <- withinSubPlot(dat, colName = 'heelArea', dir = 'higher')
p+ ylab('Heel Contact Area Late Stance (%)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

extractVals(dat, runmod, otherConfigs, baseConfig, 'heelArea', 'higher') 


###### Mean Forefoot pressure

dat <- dat %>% 
 # filter(meanFf > 25) %>%
  #filter(meanFf < 100) %>%
  group_by(Subject) %>%
  mutate(z_score = scale(meanFf)) %>% 
  group_by(Config)

#dat<- subset(dat, dat$z_score < 2) #removing outliers  
#dat<- subset(dat, dat$z_score > -2)

ggplot(data = dat, aes(x = meanFf, color = Config)) + geom_histogram() + facet_wrap(~Subject) 



p <- withinSubPlot(dat, colName = 'meanFf', dir = 'higher')
p+ ylab('Mean Forefoot Pressure (kPa)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

extractVals(dat, runmod, otherConfigs, baseConfig, 'meanFf', 'higher') 


###### Forefoot contact area Early stance

dat <- dat %>% 
  #filter(ffAreaEarly > 30) %>%
  #filter(meanFf < 100) %>%
  group_by(Subject) %>%
  mutate(z_score = scale(ffAreaEarly)) %>% 
  group_by(Config)

#dat<- subset(dat, dat$z_score < 2) #removing outliers  
#dat<- subset(dat, dat$z_score > -2)

ggplot(data = dat, aes(x = ffAreaEarly, color = Config)) + geom_histogram() + facet_wrap(~Subject) 



p <- withinSubPlot(dat, colName = 'ffAreaEarly', dir = 'higher')
p+ ylab('Forefoot Contact Area (cm^2)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

extractVals(dat, runmod, otherConfigs, baseConfig, 'ffAreaEarly', 'higher') 


###### Max of the mean Toe Pressure

dat <- dat %>% 
  #filter(ffAreaEarly > 30) %>%
  #filter(meanFf < 100) %>%
  group_by(Subject) %>%
  mutate(z_score = scale(maxmeanToes)) %>% 
  group_by(Config)

#dat<- subset(dat, dat$z_score < 2) #removing outliers  
#dat<- subset(dat, dat$z_score > -2)

ggplot(data = dat, aes(x = maxmeanToes, color = Config)) + geom_histogram() + facet_wrap(~Subject) 



p <- withinSubPlot(dat, colName = 'maxmeanToes', dir = 'lower')
p+ ylab('Maximum Toe Pressure (kPa)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

extractVals(dat, runmod, otherConfigs, baseConfig, 'maxmeanToes', 'lower') 


###### Max of the max Toe Pressure

dat <- dat %>% 
  #filter(ffAreaEarly > 30) %>%
  #filter(meanFf < 100) %>%
  group_by(Subject) %>%
  mutate(z_score = scale(maxmaxToes)) %>% 
  group_by(Config)

#dat<- subset(dat, dat$z_score < 2) #removing outliers  
#dat<- subset(dat, dat$z_score > -2)

ggplot(data = dat, aes(x = maxmaxToes, color = Config)) + geom_histogram() + facet_wrap(~Subject) 



p <- withinSubPlot(dat, colName = 'maxmaxToes', dir = 'lower')
p+ ylab('Maximum Toe Pressure (kPa)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


extractVals(dat, runmod, otherConfigs, baseConfig, 'maxmeanToes', 'lower') 



###### Max Toe Pressure Normalized to Mean Pressure Across the entire foot

dat <- dat %>% 
  #filter(ffAreaEarly > 30) %>%
  #filter(meanFf < 100) %>%
  group_by(Subject) %>%
  mutate(maxToePnorm = maxmeanToes/meanTotalP) %>%
  mutate(z_score = scale(maxToePnorm)) %>% 
  group_by(Config)

#dat<- subset(dat, dat$z_score < 2) #removing outliers  
#dat<- subset(dat, dat$z_score > -2)

ggplot(data = dat, aes(x = maxToePnorm, color = Config)) + geom_histogram() + facet_wrap(~Subject) 


p <- withinSubPlot(dat, colName = 'maxToePnorm', dir = 'lower')
p+ ylab('Toe pressure/Mean pressure')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

extractVals(dat, runmod, otherConfigs, baseConfig, 'maxToePnorm', 'lower') 


###### Max of the Max Toe Pressure Normalized to Mean Pressure Across the entire foot

dat <- dat %>% 
  #filter(ffAreaEarly > 30) %>%
  #filter(meanFf < 100) %>%
  group_by(Subject) %>%
  mutate(maxMaxToePnorm = maxmaxToes/meanTotalP) %>%
  mutate(z_score = scale(maxToePnorm)) %>% 
  group_by(Config)

#dat<- subset(dat, dat$z_score < 2) #removing outliers  
#dat<- subset(dat, dat$z_score > -2)

ggplot(data = dat, aes(x = maxMaxToePnorm, color = Config)) + geom_histogram() + facet_wrap(~Subject) 



p <- withinSubPlot(dat, colName = 'maxMaxToePnorm', dir = 'lower')
p+ ylab('Toe pressure/Mean pressure')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

extractVals(dat, runmod, otherConfigs, baseConfig, 'maxToePnorm', 'lower') 


