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

extractVals <- function(dat, mod, configNames, var, dir) {
  
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
    
    ci <- posterior_interval(mod, prob = 0.8)
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


#################### Set up data

dat <- read_csv(file.choose()) # Reading in the CSV

dat <- as_tibble(dat) # creating the data frame


#dat <- allDat
# Defining the baseline and other configs
baseline <- 'Lace'

otherConfigs <- c('BOA')

allConfigs <- c(baseline, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs) 

dat <- dat[complete.cases(dat),]




################### Peak Braking Force

#organizing data - grouping by subject and config by the variable being observed
pBFdat <- dat %>%
  filter(pBF <0)%>%
  group_by(Subject) %>%
  mutate(z_score = scale(pBF)) %>% 
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = pBFdat, aes(x = pBF, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(pBFdat, colName = 'pBF', dir = 'lower', ylabel = 'Peak Braking Force (N)') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = pBFdat, 
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
extractVals(pBFdat, runmod, otherConfigs, 'pBF', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)




################### Peak Lead Ankle Neg Power


#organizing data - grouping by subject and config by the variable being observed
leadAnkNegPowdat <- dat %>% 
  group_by(Subject) %>%
  filter(leadAnklePeakNegPower > -400) %>%
  mutate(z_score = scale(leadAnklePeakNegPower)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = leadAnkNegPowdat, aes(x = leadAnklePeakNegPower, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(leadAnkNegPowdat, colName = 'leadAnklePeakNegPower', dir = 'higher', ylabel = 'Peak Lead Ankle Neg Power (W)') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = leadAnkNegPowdat, 
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
extractVals(leadAnkNegPowdat, runmod, otherConfigs, 'leadAnklePeakNegPower', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


################### Lead Ditsal Rearfoot Negative Work


#organizing data - grouping by subject and config by the variable being observed
footNegWorkDat <- dat %>% 
  group_by(Subject) %>%
  filter(footNegWork > -20000)%>%
  mutate(z_score = scale(footNegWork)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = footNegWorkDat, aes(x = footNegWork, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(footNegWorkDat, colName = 'footNegWork', dir = 'lower', ylabel = 'Lead Distal Foot Negative Work') 



## Bayes model 
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

# # Output of the Confidence Interval
extractVals(footNegWorkDat, runmod, otherConfigs, 'footNegWork', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


################### Lead Foot Energy


#organizing data - grouping by subject and config by the variable being observed
leadFootEnergyDat <- dat %>% 
  group_by(Subject) %>%
  filter(leadFootEnergy < 50)%>%
  mutate(z_score = scale(leadFootEnergy)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = leadFootEnergyDat, aes(x = leadFootEnergy, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(leadFootEnergyDat, colName = 'leadFootEnergy', dir = 'lower', ylabel = 'Lead Foot Energy') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = leadFootEnergyDat, 
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
extractVals(leadFootEnergyDat, runmod, otherConfigs, 'leadFootEnergy', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


################### Lead Shank Energy


#organizing data - grouping by subject and config by the variable being observed
leadShankEnergyDat <- dat %>% 
  group_by(Subject) %>%
  filter(leadShankEnergy < 100)%>%
  mutate(z_score = scale(leadShankEnergy)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = leadShankEnergyDat, aes(x = leadShankEnergy, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(leadShankEnergyDat, colName = 'leadShankEnergy', dir = 'higher', ylabel = 'Lead Shank Energy') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = leadShankEnergyDat, 
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
extractVals(leadShankEnergyDat, runmod, otherConfigs, 'leadShankEnergy', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


################### Lead Thigh Energy


#organizing data - grouping by subject and config by the variable being observed
leadThighEnergyDat <- dat %>% 
  group_by(Subject) %>%
  #filter(leadThighEnergy < 100)%>%
  mutate(z_score = scale(leadThighEnergy)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = leadThighEnergyDat, aes(x = leadThighEnergy, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(leadThighEnergyDat, colName = 'leadThighEnergy', dir = 'higher', ylabel = 'Lead Thigh Energy') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = leadThighEnergyDat, 
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
extractVals(leadThighEnergyDat, runmod, otherConfigs, 'leadThighEnergy', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)




################### Rear Foot Energy


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



## Bayes model 
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

# # Output of the Confidence Interval
extractVals(rearFootEnergyDat, runmod, otherConfigs, 'rearFootEnergy', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


posterior <- posterior_samples(runmod)

plot_title <- ggtitle("Posterior Distribution")

mcmc_areas(posterior, 
           pars = c("b_ConfigBOA"), prob = 0.8
) + plot_title



################### Rear Shank Energy


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



## Bayes model 
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

# # Output of the Confidence Interval
extractVals(rearShankEnergyDat, runmod, otherConfigs, 'rearShankEnergy', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)
posterior <- posterior_samples(runmod)

plot_title <- ggtitle("Posterior Distribution")

mcmc_areas(posterior, 
           pars = c("b_ConfigBOA"), prob = 0.7
) + plot_title


################### Rear Thigh Energy


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



## Bayes model 
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

# # Output of the Confidence Interval
extractVals(rearThighEnergyDat, runmod, otherConfigs, 'rearThighEnergy', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)

posterior <- posterior_samples(runmod)

plot_title <- ggtitle("Posterior Distribution")

mcmc_areas(posterior, 
           pars = c("b_ConfigBOA"), prob = 0.7
) + plot_title





################### Rear Ankle Extension Velocity

#organizing data - grouping by subject and config by the variable being observed
rearAnkleExtVelDat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(rearAnkleExtVel)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = rearAnkleExtVelDat, aes(x = rearAnkleExtVel, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(rearAnkleExtVelDat, colName = 'rearAnkleExtVel', dir = 'lower', ylabel = 'Rear Ankle Extension Velocity') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = rearAnkleExtVelDat, 
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
extractVals(rearAnkleExtVelDat, runmod, otherConfigs, 'rearAnkleExtVel', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)









################### Peak Lead Hip Extension Velocity


#organizing data - grouping by subject and config by the variable being observed
leadHipExtVeldat <- dat %>% 
  group_by(Subject) %>%
  #filter(rearAnklePeakNegPower > -1000) %>%
  mutate(z_score = scale(leadHipExtVel))

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = leadHipExtVeldat, aes(x = leadHipExtVel, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(leadHipExtVeldat, colName = 'leadHipExtVel', dir = 'lower', ylabel = 'Lead Hip Extension Velocity') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = leadHipExtVeldat, 
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
extractVals(leadHipExtVeldat, runmod, otherConfigs, 'leadHipExtVel', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)



################### Peak Rear Hip Extension Velocity


#organizing data - grouping by subject and config by the variable being observed
rearHipExtVeldat <- dat %>% 
  group_by(Subject) %>%
  #filter(rearAnklePeakNegPower > -1000) %>%
  mutate(z_score = scale(rearHipExtVel))

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = rearHipExtVeldat, aes(x = rearHipExtVel, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(rearHipExtVeldat, colName = 'rearHipExtVel', dir = 'lower', ylabel = 'Lead Hip Extension Velocity') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = rearHipExtVeldat, 
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
extractVals(rearHipExtVeldat, runmod, otherConfigs, 'rearHipExtVel', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)





################### Peak Lead Knee Extension Velocity


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



## Bayes model 
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

# # Output of the Confidence Interval
extractVals(leadKneeExtVeldat, runmod, otherConfigs, 'pkKneeExtVel', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


################### Knee Flexion Ball Release


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(kneeFlexBR)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = kneeFlexBR, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'kneeFlexBR', dir = 'higher', ylabel = 'Knee Flexion at Ball Release (deg)') 



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
extractVals(dat, runmod, otherConfigs, 'kneeFlexBR', 'higher')  

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



################### Minimum Free Moment


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(minFreeMoment)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = minFreeMoment, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'minFreeMoment', dir = 'lower', ylabel = 'Minimum Free Moment') 



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
extractVals(dat, runmod, otherConfigs, 'minFreeMoment', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)



################### Maximum Free Moment


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(maxFreeMoment)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = maxFreeMoment, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'maxFreeMoment', dir = 'higher', ylabel = 'Maximum Free Moment') 



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
extractVals(dat, runmod, otherConfigs, 'maxFreeMoment', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


################### Lead Foot Distal Foot Neg Work


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(footNegWork)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = footNegWork, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'footNegWork', dir = 'lower', ylabel = 'Lead Distal Foot Negative Work') 



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
extractVals(dat, runmod, otherConfigs, 'footNegWork', 'lower')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)







################### Pelvis Velocity


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
extractVals(dat, runmod, otherConfigs, 'pelVel', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)

posterior <- posterior_samples(runmod)

plot_title <- ggtitle("Posterior Distribution")

mcmc_areas(posterior, 
           pars = c("b_ConfigBOA"), prob = 0.7
) + plot_title

###################Ball Velocity


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
extractVals(dat, runmod, otherConfigs, 'Velocity', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)




################### Shoulder ER


#organizing data - grouping by subject and config by the variable being observed
shoulderERdat <- dat %>% 
  group_by(Subject) %>%
  filter(!is.na(shoulderER)) %>%
  mutate(z_score = scale(shoulderER)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = shoulderERdat, aes(x = shoulderER, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(shoulderERdat, colName = 'shoulderER', dir = 'higher', ylabel = 'Shoulder Rotation Velocity') 



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
extractVals(dat, runmod, otherConfigs, 'shoulderER', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


################### ElbowValgus


#organizing data - grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  filter(elbowValugs < 0)
  mutate(z_score = scale(elbowValugs)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = elbowValugs, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'elbowValugs', dir = 'higher', ylabel = 'Shoulder External Rotation') 



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
extractVals(dat, runmod, otherConfigs, 'elbowValugs', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


################### Lead Leg Neg Work


#organizing data - grouping by subject and config by the variable being observed
leadLegNegWorkDat <- dat %>% 
  group_by(Subject) %>%
  filter(leadAnkleNegWork > -5000)%>%
  filter(leadKneeNegWork > -5000)%>%
  mutate(z_score = scale(leadLegNegWork)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = leadLegNegWorkDat, aes(x = leadLegNegWork, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(leadLegNegWorkDat, colName = 'leadLegNegWork', dir = 'higher', ylabel = 'Lead Leg Negative Work') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = leadLegNegWorkDat, 
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
extractVals(leadLegNegWorkDat, runmod, otherConfigs, 'leadLegNegWork', 'higher')  

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


###################Rear Leg Pos Work


#organizing data - grouping by subject and config by the variable being observed
rearLegPosWorkDat <- dat %>% 
  group_by(Subject) %>%
  filter(rearAnklePosWork < 10000)%>%
  filter(rearKneePosWork < 10000)%>%
  filter(rearHipPosWork < 22000)%>%
  filter(rearHipPosWork > 500)%>%
  mutate(z_score = scale(rearLegPosWork)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = rearLegPosWorkDat, aes(x = rearLegPosWork, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(rearLegPosWorkDat, colName = 'rearLegPosWork', dir = 'lower', ylabel = 'Rear Leg Pos Work') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = rearLegPosWorkDat, 
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
extractVals(rearLegPosWorkDat, runmod, otherConfigs, 'rearLegPosWork', 'lower') 




####### Total Lower Extremity Joint Work

totalWorkDat <- dat %>% 
  group_by(Subject) %>%
  filter(rearAnklePosWork < 10000)%>%
  filter(rearKneePosWork < 10000)%>%
  filter(rearHipPosWork < 22000)%>%
  filter(rearHipPosWork > 500)%>%
  filter(leadAnkleNegWork > -5000)%>%
  filter(leadKneeNegWork > -5000)%>%
  mutate(z_score = scale(totalWork)) %>%
  group_by(Config)

dat <- subset(dat, dat$z_score<3)
dat <- subset(dat, dat$z_score>-3)

#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = totalWorkDat, aes(x = totalWork, color = Config)) + geom_histogram() + facet_wrap(~Subject) 

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(totalWorkDat, colName = 'totalWork', dir = 'lower', ylabel = 'Total Lower Extremity Joint Work') 



## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = totalWorkDat, 
              family = gaussian,
              z_score ~ Config + (1 + Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# model1 <-lmer(steadyPower ~ Config + (1 + Config| SubjectNo),data = dat) 
# summary(model1)


# # Output of the Confidence Interval
extractVals(totalWorkDat, runmod, otherConfigs, 'totalWork', 'lower') 
