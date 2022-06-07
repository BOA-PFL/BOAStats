library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)

rm(list=ls()) # Clears the environment


# This code uses Bayesian analysis specifically for variables we look for in alpine segments




####### Functions 
# Making the "Best of" line plots and defining direction
# direction can be 'lower' or higher'. It is the direction of change that is better. 
# For example, for contact time lower is better. so we put 'lower'. for jump height, higher is better, so we put higher. 


withinSubPlot <- function(inputDF, colName, dir) {
  
  
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
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 26)) + ylab(paste0({{colName}})) 
  
}


extractVals <- function(dat, mod, configNames, var, dir) {
  
  
  
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
    posterior <- posterior_samples(mod)
    
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
  ProbImp = round(ProbImp, 2)
  lowCI = round(lowCI, 1)
  highCI = round(highCI,1)
  output = cbind(Config, ProbImp, lowCI, highCI)
  
  colnames(output) = c('Config', 'Probability of Improvement', 'Low end of CI', 'High end of CI')
  return(output)
}

###############################

dat <- read.csv(file.choose()) # Reading in the CSV

dat <- as_tibble(dat) # creating the data frame

 
# Defining the baseline and other configs
baseline <- 'V1'

otherConfigs <- c('V2')

allConfigs <- c(baseline, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs) 


###### Weight transfer to outside foot

#This section of code is organizing data - grouping by subject and config by the variable being observed
#Filtering out values by removing outliers by filtering any scores that are 2sds above or below the mean
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(OutsideFootForce)) %>% 
  group_by(Config)


dat<- subset(dat, dat$z_score < 2) 
dat<- subset(dat, dat$z_score > -2)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = OutsideFootForce)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
p <- withinSubPlot(dat, colName = 'OutsideFootForce', dir = 'higher')
yTitle <- expression(Better~weight~transfer%->%"")
p + ylab(yTitle)

## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = dat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# # Output of the Confidence Interval
extractVals(dat, runmod, otherConfigs, 'OutsideFootForce', 'higher') 


###### Foot roll (Pressure targeted on medial ball of foot) 

#This section of code is organizing data - grouping by subject and config by the variable being observed
#Filtering out values by removing outliers by filtering any scores that are 2sds above or below the mean
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(OutsideFootMedialForce)) %>% 
  group_by(Config)

dat<- subset(dat, dat$z_score < 2) #removing outliers  
dat<- subset(dat, dat$z_score > -2)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = OutsideFootMedialForce)) + geom_histogram() + facet_wrap(~Subject) 


# "best of" Line plot 
p <- withinSubPlot(dat, colName = 'OutsideFootMedialForce', dir = 'higher')
yTitle <- expression(Better~foot~roll%->%"")
p + ylab(yTitle)


## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = dat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)



# # Output of the Confidence Interval
extractVals(dat, runmod, otherConfigs, 'OutsideFootMedialForce', 'higher') 


###### Forward stance (Pressure on toes during early stance)

#This section of code is organizing data - grouping by subject and config by the variable being observed
#Filtering out values by removing outliers by filtering any scores that are 2sds above or below the mean

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(avgOutsideHeelStart)) %>% 
  group_by(Config)


dat<- subset(dat, dat$z_score < 2) #removing outliers  
dat<- subset(dat, dat$z_score > -2)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = avgOutsideHeelStart)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line plot 
p<-withinSubPlot(dat, colName = 'avgOutsideHeelStart', dir = 'lower')
yTitle <- expression(""%<-%Better~forward~stance)
p + ylab(yTitle)


## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = dat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# # Output of the Confidence Interval
extractVals(dat, runmod, otherConfigs, 'avgOutsideHeelStart', 'lower') 


###### Balance (Even heel/toe pressure late in turn)


#This section of code is organizing data - grouping by subject and config by the variable being observed
#Filtering out values by removing outliers by filtering any scores that are 2sds above or below the mean
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(pkOutsideHeelLate)) %>% 
  group_by(Config)


dat<- subset(dat, dat$z_score < 2) #removing outliers  
dat<- subset(dat, dat$z_score > -2)


#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = pkOutsideHeelLate)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line plot 
p<-withinSubPlot(dat, colName = 'pkOutsideHeelLate', dir = 'higher')
yTitle <- expression(Better~balance%->%"")
p + ylab(yTitle)

## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = dat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


# # Output of the Confidence Interval
extractVals(dat, runmod, otherConfigs, 'pkOutsideHeelLate', 'higher') 


###### cv Forc

#This section of code is organizing data - grouping by subject and config by the variable being observed
#Filtering out values by removing outliers by filtering any scores that are 2sds above or below the mean
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(CVForce)) %>% 
  group_by(Config)


dat<- subset(dat, dat$z_score < 2) #removing outliers  
dat<- subset(dat, dat$z_score > -2)


#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = CVForce)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line plot 
withinSubPlot(dat, colName = 'CVForce', dir = 'lower')

## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = dat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)


# # Output of the Confidence Interval
extractVals(dat, runmod, otherConfigs, 'CVForce', 'lower') 

