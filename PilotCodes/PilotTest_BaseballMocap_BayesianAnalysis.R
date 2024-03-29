library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)
library(readxl)

 

####### Functions
# Making the "Best of" line plots and defining direction
# direction can be 'lower' or higher'. It is the direction of change that is better. 
# For example, for contact time lower is better. so we put 'lower'. for jump height, higher is better, so we put higher. 

withinSubPlot <- function(inputDF, colName, dir,ylabel) {
  
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
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 26)) + ylab(ylabel)
  
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
    ci_LowPct <- meanSD*ciLow/(mean)*100
    ci_HighPct <- meanSD*ciHigh/(mean)*100
    
    
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


################## Throwing Mocap Data
### Set up data

dat <- read_csv(file.choose()) # Reading in the CSV

dat <- as_tibble(dat) # creating the data frame


#dat <- allDat
# Defining the baseline and other configs
baseline <- 'Lace'

otherConfigs <- c('BOA')

allConfigs <- c(baseline, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs) 



### Lead Distal Rearfoot Negative Work


#organizing data - grouping by subject and config by the variable being observed
footNegWorkDat <- dat %>% 
  group_by(Subject) %>%
 
  mutate(z_score = scale(leadDistalRFnegWork)) %>%
  group_by(Config)

#dat <- subset(dat, dat$z_score<3)
#dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers

ggplot(data = footNegWorkDat, aes(x = leadDistalRFnegWork, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(footNegWorkDat, colName = 'leadDistalRFnegWork', dir = 'lower', ylabel = 'Lead Distal Foot Negative Work') 



# Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = footNegWorkDat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# Output of the Confidence Interval
extractVals(footNegWorkDat, runmod, otherConfigs, baseline, 'leadDistalRFnegWork', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)




### Lead Peak Distal Rearfoot Negative Power


#organizing data - grouping by subject and config by the variable being observed
footNegWorkDat <- dat %>% 
  group_by(Subject) %>%
  
  mutate(z_score = scale(peakNegLeadDistalRFpower)) %>%
  group_by(Config)

#dat <- subset(dat, dat$z_score<3)
#dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers

ggplot(data = footNegWorkDat, aes(x = peakNegLeadDistalRFpower, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(footNegWorkDat, colName = 'peakNegLeadDistalRFpower', dir = 'lower', ylabel = 'Lead Distal Foot Power') 



# Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = footNegWorkDat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# Output of the Confidence Interval
extractVals(footNegWorkDat, runmod, otherConfigs, baseline, 'peakNegLeadDistalRFpower', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)





### Rear Peak Distal Rearfoot Positive Power


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  
  mutate(z_score = scale(peakPosRearDistalRFpower)) %>%
  group_by(Config)

#dat <- subset(dat, dat$z_score<3)
#dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers

ggplot(data = dat, aes(x = peakPosRearDistalRFpower, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'peakPosRearDistalRFpower', dir = 'higher', ylabel = 'Rear Peak Distal Foot Power') 



# Bayes model 
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

# Output of the Confidence Interval
extractVals(dat, runmod, otherConfigs, baseline, 'peakPosRearDistalRFpower', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


### Rear Foot Energy


#organizing data - grouping by subject and config by the variable being observed
rearFootEnergyDat <- dat %>% 
  group_by(Subject) %>%
  #filter(rearShankEnergy < 100)%>%
  mutate(z_score = scale(rearFootEnergy)) %>%
  group_by(Config)

#dat <- subset(dat, dat$z_score<3)
#dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = rearFootEnergyDat, aes(x = rearFootEnergy, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(rearFootEnergyDat, colName = 'rearFootEnergy', dir = 'higher', ylabel = "Rear Foot Energy (J)") 



# Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = rearFootEnergyDat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# Output of the Confidence Interval
extractVals(rearFootEnergyDat, runmod, otherConfigs, 'rearFootEnergy', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


posterior <- posterior_samples(runmod)

plot_title <- ggtitle("Posterior Distribution")

mcmc_areas(posterior, 
           pars = c("b_ConfigBOA"), prob = 0.8
) + plot_title



### Rear Shank Energy


#organizing data - grouping by subject and config by the variable being observed
rearShankEnergyDat <- dat %>% 
  group_by(Subject) %>%
  filter(rearShankEnergy < 100)%>%
  mutate(z_score = scale(rearShankEnergy)) %>%
  group_by(Config)

#dat <- subset(dat, dat$z_score<3)
#dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = rearShankEnergyDat, aes(x = rearShankEnergy, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(rearShankEnergyDat, colName = 'rearShankEnergy', dir = 'higher', ylabel = 'Rear Shank Energy') 



# Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = rearShankEnergyDat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# Output of the Confidence Interval
extractVals(rearShankEnergyDat, runmod, otherConfigs, 'rearShankEnergy', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)
posterior <- posterior_samples(runmod)

plot_title <- ggtitle("Posterior Distribution")

mcmc_areas(posterior, 
           pars = c("b_ConfigBOA"), prob = 0.7
) + plot_title


### Rear Thigh Energy


#organizing data - grouping by subject and config by the variable being observed
rearThighEnergyDat <- dat %>% 
  group_by(Subject) %>%
  filter(rearThighEnergy < 150)%>%
  mutate(z_score = scale(rearThighEnergy)) %>%
  group_by(Config)

#at <- subset(dat, dat$z_score<3)
#dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = rearThighEnergyDat, aes(x = rearThighEnergy, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(rearThighEnergyDat, colName = 'rearThighEnergy', dir = 'higher', ylabel = 'Rear Thigh Energy') 



# Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = rearThighEnergyDat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# Output of the Confidence Interval
extractVals(rearThighEnergyDat, runmod, otherConfigs, 'rearThighEnergy', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)

posterior <- posterior_samples(runmod)

plot_title <- ggtitle("Posterior Distribution")

mcmc_areas(posterior, 
           pars = c("b_ConfigBOA"), prob = 0.7
) + plot_title




### Peak Lead Knee Extension Velocity


#organizing data - grouping by subject and config by the variable being observed
leadKneeExtVeldat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(pkKneeExtVel)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = leadKneeExtVeldat, aes(x = pkKneeExtVel, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(leadKneeExtVeldat, colName = 'pkKneeExtVel', dir = 'higher', ylabel = 'Peak Knee Extension Velocity (deg/s)') 



# Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = leadKneeExtVeldat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# Output of the Confidence Interval
extractVals(leadKneeExtVeldat, runmod, otherConfigs, 'pkKneeExtVel', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)



################### Knee Extension ROM


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(kneeExtROM)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = kneeExtROM, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'kneeExtROM', dir = 'higher', ylabel = 'Knee Ext ROM (deg)') 



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
extractVals(dat, runmod, otherConfigs, 'kneeExtROM', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)





### Pelvis Velocity


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(pelVel = abs(pelVel)) %>%
  mutate(z_score = scale(pelVel)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = pelVel, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'pelVel', dir = 'higher', ylabel = 'Pelvis Velocity') 



# Bayes model 
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

# Output of the Confidence Interval
extractVals(dat, runmod, otherConfigs, 'pelVel', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)

posterior <- posterior_samples(runmod)

plot_title <- ggtitle("Posterior Distribution")

mcmc_areas(posterior, 
           pars = c("b_ConfigBOA"), prob = 0.7
) + plot_title

### Ball Velocity


#organizing data - grouping by subject and config by the variable being observed
dat$Velocity <- as.numeric(dat$Velocity)

dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(Velocity)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = Velocity, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'Velocity', dir = 'higher', ylabel = 'Ball Velocity') 



# Bayes model 
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

# Output of the Confidence Interval
extractVals(dat, runmod, otherConfigs, 'Velocity', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


##################### Batting Mocap data
### Set up data

dat <- read_csv(file.choose()) # Reading in the CSV

dat <- as_tibble(dat) # creating the data frame

dat <- allDat

# Defining the baseline and other configs
baseline <- 'Lace'

otherConfigs <- c('BOA')

allConfigs <- c(baseline, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs) 



### Peak Knee Extension Velocity


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(pkKneeExtVel)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = kneeExtROM, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'pkKneeExtVel', dir = 'higher', ylabel = 'Peak Knee Extension Velocity (deg/s)') 



# Bayes model 
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

posterior <- posterior_samples(runmod)

plot_title <- ggtitle("Posterior Distribution")

mcmc_areas(posterior, 
           pars = c("b_ConfigBOA"), prob = 0.7
) + plot_title
# Output of the Confidence Interval
extractVals(dat, runmod, otherConfigs, 'pkKneeExtVel', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


### Knee Extension ROM


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(kneeExtROM)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = kneeExtROM, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'kneeExtROM', dir = 'higher', ylabel = 'Knee Ext ROM (deg)') 



# Bayes model 
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

# Output of the Confidence Interval
extractVals(dat, runmod, otherConfigs, baseline, 'kneeExtROM', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)







### Lead Distal Foot Peak Neg Power


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(peakNegLeadDistalRFpower)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat , aes(x = peakNegLeadDistalRFpower, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat , colName = 'peakNegLeadDistalRFpower', dir = 'lower', ylabel = 'Peak Lead Distal RF Neg Power (W)') 



# Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = dat , 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# Output of the Confidence Interval
extractVals(dat , runmod, otherConfigs, baseline, 'peakNegLeadDistalRFpower', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


### Lead Distal Foot Negative Work


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(leadDistalRFnegWork)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat , aes(x = leadDistalRFnegWork, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat , colName = 'leadDistalRFnegWork', dir = 'lower', ylabel = 'Lead Distal RF Neg Work (J)') 



# Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = dat , 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# Output of the Confidence Interval
extractVals(dat , runmod, otherConfigs, baseline, 'leadDistalRFnegWork', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


### Pelvis Velocity


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(pelVel)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat , aes(x = pelVel, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat , colName = 'pelVel', dir = 'higher', ylabel = 'Peak Pelvis Rotation Velocity (deg/s)') 



# Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = dat , 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# Output of the Confidence Interval
extractVals(dat , runmod, otherConfigs, baseline, 'pelVel', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


