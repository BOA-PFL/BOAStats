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
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000","#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 26)) + ylab(paste0({{colName}})) 
  
}


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

dat <- read_csv(file.choose()) # Reading in the CSV

dat <- as_tibble(dat) # creating the data frame


# Defining the baseline and other configs
baseline <- 'PFS-FFW'

otherConfigs <- c('PFS')

allConfigs <- c(baseline, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs)


################################

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(ToePressure)) %>% 
  group_by(Config)

#cmjDat<- subset(cmjDat, cmjDat$z_score < 2) #removing outliers  
#cmjDat<- subset(cmjDat, cmjDat$z_score > -2)

ggplot(data = dat, aes(x = ToePressure, color = Config)) + geom_histogram() + facet_wrap(~Subject) 


p <- withinSubPlot(dat, colName = 'ToePressure', dir = 'higher')
p+ ylab('Pressure (kPa)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1+Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


extractVals(dat, runmod, otherConfigs, baseline, 'ToePressure', 'higher') 


################################

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(ToeContactArea)) %>% 
  group_by(Config)

#cmjDat<- subset(cmjDat, cmjDat$z_score < 2) #removing outliers  
#cmjDat<- subset(cmjDat, cmjDat$z_score > -2)

ggplot(data = dat, aes(x = ToeContactArea, color = Config)) + geom_histogram() + facet_wrap(~Subject) 


p <- withinSubPlot(dat, colName = 'ToeContactArea', dir = 'higher')
p+ ylab('Area (%)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1+Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


extractVals(dat, runmod, otherConfigs, baseline, 'ToeContactArea', 'higher') 



################################

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(MetatarsalPressure)) %>% 
  group_by(Config)

#cmjDat<- subset(cmjDat, cmjDat$z_score < 2) #removing outliers  
#cmjDat<- subset(cmjDat, cmjDat$z_score > -2)

ggplot(data = dat, aes(x = MetatarsalPressure, color = Config)) + geom_histogram() + facet_wrap(~Subject) 


p <- withinSubPlot(dat, colName = 'MetatarsalPressure', dir = 'higher')
p+ ylab('Pressure (kPa)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1+Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)



extractVals(dat, runmod, otherConfigs, baseline, 'MetatarsalPressure', 'higher') 


################################

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(MetatarsalContactArea)) %>% 
  group_by(Config)

#cmjDat<- subset(cmjDat, cmjDat$z_score < 2) #removing outliers  
#cmjDat<- subset(cmjDat, cmjDat$z_score > -2)

ggplot(data = dat, aes(x = MetatarsalContactArea, color = Config)) + geom_histogram() + facet_wrap(~Subject) 


p <- withinSubPlot(dat, colName = 'MetatarsalContactArea', dir = 'higher')
p+ ylab('Area (%)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1+Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


extractVals(dat, runmod, otherConfigs, baseline, 'MetatarsalContactArea', 'higher') 


################################

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(MidfootPressure)) %>% 
  group_by(Config)

#cmjDat<- subset(cmjDat, cmjDat$z_score < 2) #removing outliers  
#cmjDat<- subset(cmjDat, cmjDat$z_score > -2)

ggplot(data = dat, aes(x = MidfootPressure, color = Config)) + geom_histogram() + facet_wrap(~Subject) 


p <- withinSubPlot(dat, colName = 'MidfootPressure', dir = 'higher')
p+ ylab('Pressure (kPa)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1+Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


extractVals(dat, runmod, otherConfigs, baseline, 'MidfootPressure', 'higher') 


################################

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(MidfootContactArea)) %>% 
  group_by(Config)

#cmjDat<- subset(cmjDat, cmjDat$z_score < 2) #removing outliers  
#cmjDat<- subset(cmjDat, cmjDat$z_score > -2)

ggplot(data = dat, aes(x = MidfootContactArea, color = Config)) + geom_histogram() + facet_wrap(~Subject) 


p <- withinSubPlot(dat, colName = 'MidfootContactArea', dir = 'higher')
p+ ylab('Area (%)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1+Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


extractVals(dat, runmod, otherConfigs, baseline, 'MidfootContactArea', 'higher') 


################################

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(HeelPressure)) %>% 
  group_by(Config)

#cmjDat<- subset(cmjDat, cmjDat$z_score < 2) #removing outliers  
#cmjDat<- subset(cmjDat, cmjDat$z_score > -2)

ggplot(data = dat, aes(x = HeelPressure, color = Config)) + geom_histogram() + facet_wrap(~Subject) 


p <- withinSubPlot(dat, colName = 'HeelPressure', dir = 'higher')
p+ ylab('Pressure (kPa)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1+Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


extractVals(dat, runmod, otherConfigs, baseline, 'HeelPressure', 'higher') 


################################

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(HeelContactArea)) %>% 
  group_by(Config)

#cmjDat<- subset(cmjDat, cmjDat$z_score < 2) #removing outliers  
#cmjDat<- subset(cmjDat, cmjDat$z_score > -2)

ggplot(data = dat, aes(x = HeelContactArea, color = Config)) + geom_histogram() + facet_wrap(~Subject) 


p <- withinSubPlot(dat, colName = 'HeelContactArea', dir = 'higher')
p+ ylab('Area (%)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1+Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


extractVals(dat, runmod, otherConfigs, baseline, 'HeelContactArea', 'higher') 


################################

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(StdDevP)) %>% 
  group_by(Config)

#cmjDat<- subset(cmjDat, cmjDat$z_score < 2) #removing outliers  
#cmjDat<- subset(cmjDat, cmjDat$z_score > -2)

ggplot(data = dat, aes(x = StdDevP, color = Config)) + geom_histogram() + facet_wrap(~Subject) 


p <- withinSubPlot(dat, colName = 'StdDevP', dir = 'lower')
p+ ylab('SD pressure (kPa)')

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_score ~ Config + (1+ Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


extractVals(dat, runmod, otherConfigs, baseline, 'StdDevP', 'lower') 

