
library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)

rm(list=ls()) # Clears the environment

# This code uses Bayesian analysis specifically for variables we look for with the Pressure insoles



####### Functions 


# Creating plots
withinSubPlot <- function(inputDF, colName, dir,ylabel) {
  # Specify ylabel in function or default to the original name
  if(missing(ylabel)){
    ylabel = paste0({{colName}})
  }
  
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
  
  # "Best of" line plot code
  whichConfig <- merge(meanDat, whichConfig)
  
  ggplot(data = whichConfig, mapping = aes(x = as.factor(Config), y = mean, col = BestConfig, group = Subject)) + geom_point(size = 4) +
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 26)) + ylab(ylabel)
  
}


extractVals <- function(dat, mod, configNames, var, dir) { # This sets us up for extracting our values in relation to the posterior
  
  #configNames = otherConfigs
  #mod = runmod
  #dir = 'higher'
  #var = 'CarryFlatLength'
  
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
    
    ci <- posterior_interval(mod, prob = 0.95)
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

dat <- read.csv('C:/Users/milena.singletary/Boa Technology Inc/PFL Team - Documents/General/Testing Segments/WorkWear_Performance/Jalas_July2022/PressureCompiledPressureData.csv')# Reading in the CompiledAgilityData.csv

dat <- as_tibble(dat) # creating the data frame


#Change to Config names used in your data, with the baseline model listed first.
baseline <- 'TP'

otherConfigs <- c('OP')

allConfigs <- c(baseline, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs)

######## Pressure Data #########################################
LevelDat <- dat %>%
  filter(Condition == 'Walking') %>% 
  #filter(Side == 'Right') %>%
  group_by(Subject, Config)

DownDat <- dat %>%
  filter(Condition == 'Walking6deg') %>% 
  #filter(Side == 'Right') %>%
  group_by(Subject, Config)

### plotting unfiltered data

ggplot(data = LevelDat, aes(x = meanHeelContactArea, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 
ggplot(data = DownDat, aes(x = meanHeelContactArea, fill = Config)) + geom_histogram() + facet_wrap(~Subject)

# Another ggplot left v right:
ggplot(data = LevelDat, aes(x = meanHeelContactArea, fill = Side)) + geom_histogram() + facet_wrap(~Subject) 
ggplot(data = DownDat, aes(x = meanHeelContactArea, fill = Side)) + geom_histogram() + facet_wrap(~Subject) 

withinSubPlot(LevelDat, colName = 'meanHeelContactArea', dir = 'higher', 'Heel Contact Area %')                                                                                                     
withinSubPlot(DownDat, colName = 'meanHeelContactArea', dir = 'higher', 'Heel Contact Area %')



######### Heel Contact Area #######
# Level Walking ###

#organizing data - grouping by subject and config by the variable being observed
subdat <- dat %>% 
  group_by(Subject) %>%
  filter(Condition == 'Walking') %>%
  # filtered to right side only after initial histograms 
  filter(Side == 'Right') %>%
  mutate(z_score = scale(meanHeelContactArea)) %>% 
  group_by(Config)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = subdat, aes(x = meanHeelContactArea, fill = Config)) + geom_histogram() + facet_wrap(~Subject)



# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(subdat, colName = 'meanHeelContactArea', dir = 'higher', ylabel = 'Heel Contact Area %') 

## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = subdat, 
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
extractVals(subdat, runmod, otherConfigs, 'meanHeelContactArea', 'higher')  



# Downhill ### 
#organizing data - grouping by subject and config by the variable being observed
subdat <- dat %>% 
  group_by(Subject) %>%
  filter(Condition == 'Walking6deg') %>%
  filter(Side == 'Right') %>%
  mutate(z_score = scale(meanHeelContactArea)) %>% 
  group_by(Config)

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(subdat, colName = 'meanHeelContactArea', dir = 'higher', ylabel = 'Heel Contact Area %') 

## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = subdat, 
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
extractVals(subdat, runmod, otherConfigs, 'meanHeelContactArea', 'higher') 



##### PEAK TOE PRESSURE ####

# Level
#organizing data - grouping by subject and config by the variable being observed
subdat <- dat %>% 
  group_by(Subject) %>%
  filter(Condition == 'Walking') %>%
  filter(Side == 'Right') %>%
  mutate(z_score = scale(maxmaxToes)) %>% 
  group_by(Config)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = subdat, aes(x = maxmaxToes, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 

# Another ggplot left v right:
ggplot(data = dat, aes(x = maxmaxToes, fill = Side)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(subdat, colName = 'maxmaxToes', dir = 'lower', ylabel = 'Peak Toe Pressure [psi]') 

## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = subdat, 
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
extractVals(subdat, runmod, otherConfigs, 'maxmaxToes', 'lower')  


# Downhill
#organizing data - grouping by subject and config by the variable being observed
subdat <- dat %>% 
  group_by(Subject) %>%
  filter(Condition == 'Walking6deg') %>%
  #filter(Side == 'Right') %>%
  mutate(z_score = scale(maxmaxToes)) %>% 
  group_by(Config)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = subdat, aes(x = maxmaxToes, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(subdat, colName = 'maxmaxToes', dir = 'lower', ylabel = 'Peak Toe Pressure [psi]') 

## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = subdat, 
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
extractVals(subdat, runmod, otherConfigs, 'maxmaxToes', 'lower') 





######## TIME to STABILIZE#######
######

dat <- read.csv('C:/Users/milena.singletary/Boa Technology Inc/PFL Team - Documents/General/Testing Segments/WorkWear_Performance/Jalas_July2022/Overground/CompiledBalanceData.csv')# Reading in the CompiledData.csv

dat <- as_tibble(dat) # creating the data frame


#Change to Config names used in your data, with the baseline model listed first.
baseline <- 'TP'

otherConfigs <- c('OP')

allConfigs <- c(baseline, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs)

dat <- dat %>% 
  group_by(Subject) %>%
  group_by(Config)


### filtered data
SubDat <- dat %>%
  filter(TimetoStabilize < 1) %>%
  filter(TimetoStabilize > 0) %>%
  mutate(z_score = scale(TimetoStabilize)) %>%
  filter(z_score < 1)
  group_by(Subject, Config)

HopDat <- dat %>%
  filter(Task == 'SLH') %>% 
  filter(TimetoStabilize < 1) %>%
  filter(TimetoStabilize > 0) %>%
  mutate(z_score = scale(TimetoStabilize)) %>% 
  group_by(Subject, Config)

LandDat <- dat %>%
  filter(Task == 'SLL') %>% 
  filter(TimetoStabilize < 1) %>%
  mutate(z_score = scale(TimetoStabilize)) %>% 
  group_by(Subject, Config)



### plotting data

ggplot(data = SubDat, aes(x = TimetoStabilize, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 
ggplot(data = HopDat, aes(x = TimetoStabilize, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = LandDat, aes(x = TimetoStabilize, fill = Config)) + geom_histogram() + facet_wrap(~Subject)


# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(SubDat, colName = 'TimetoStabilize', dir = 'lower', 'Time to Stabilize [s]')      
withinSubPlot(HopDat, colName = 'TimetoStabilize', dir = 'lower', 'Time to Stabilize [s]')  
withinSubPlot(LandDat, colName = 'TimetoStabilize', dir = 'lower', 'Time to Stabilize [s]')


## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = SubDat, 
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
extractVals(SubDat, runmod, otherConfigs, 'TimetoStabilize', 'lower') 



