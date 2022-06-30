
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
  
  # direction can be 'lower' or 'higher'. It is the direction of change that is better. 
  # For example, for contact time lower is better. for jump height, higher is better. 
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

#############

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


########

sdData <- function(dat, var) {
  
  subjects <- unique(dat$Subject)
  shoes <- unique(dat$Config)
  sdVar <- matrix(0, length(subjects)*length(shoes))
  cvVar <- matrix(0, length(subjects)*length(shoes))
  Sub <- rep(NA, length(subjects)*length(shoes))
  Config <- rep(NA, length(subjects)*length(shoes))
  r = 1

  for (sub in subjects) {
  
    tmp_sub <- subset(dat, dat$Subject == sub)
  
    for (s in shoes) {
      tmp_shoe <- subset(tmp_sub, tmp_sub$Config == s)
      Sub[r] <- sub
      Config[r] <- s
      sdVar[r] <- sd(as.numeric(unlist(tmp_shoe[, var]))) 
      cvVar[r] <- sd(as.numeric(unlist(tmp_shoe[, var])))/mean(as.numeric(unlist(tmp_shoe[, var])))
      r = r+1
    
    }
  }

  SDdat <- cbind(Sub, Config, sdVar, cvVar)
  sdcolName <- paste('SD_', var,sep = '')
  cvcolName <- paste('CV_', var,sep = '')
  colnames(SDdat) <- c('Subject', 'Config', sdcolName, cvcolName)
  SDdat <- SDdat[complete.cases(SDdat),]
  SDdat <- as_tibble(SDdat)
  SDdat$Config <- factor(SDdat$Config, shoes) 

  return(SDdat)

}


############################### Load data

dat <- read.csv(file.choose())

dat <- as_tibble(dat)


#Change to Config names used in your data, with the baseline model listed first.
dat$Config <- factor(dat$Config, c('Loose', 'Tight'))

########################################## Analyze Data 

moveDat <- subset(dat, dat$Movement == 'CMJ') # Choose movement to analyze


###### Ankle ROM X

moveDat <- moveDat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(ankleROM_X)) %>% 
  group_by(Config)

#moveDat<- subset(moveDat, dat$z_score < 2) #removing outliers  
#moveDat<- subset(moveDat, dat$z_score > -2)

ggplot(data = moveDat, aes(x = ankleROM_X)) + geom_histogram() + facet_wrap(~Subject) 

withinSubPlot(moveDat, colName = 'ankleROM_X', dir = 'higher')


runmod <- brm(data = moveDat, # Bayes model
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

extractVals(moveDat, runmod, configName = 'Tight', 'ankleROM_X', 'higher') 



###### Ankle ROM X Variability

sdDat <- sdData(moveDat, 'ankleROM_X')

sdDat <- sdDat %>%
  mutate_at(vars(matches('CV_ankleROM_X')), list(as.numeric)) %>%
  mutate(z_score = scale(CV_ankleROM_X)) %>%
  group_by(Config)

withinSubPlot(sdDat, colName = 'CV_ankleROM_X', dir = 'lower')


runmod <- brm(data = sdDat, # Bayes model
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

extractVals(sdDat, runmod, configName = 'Tight', 'CV_ankleROM_X', 'lower') 




###### Ankle ROM Y

moveDat <- moveDat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(ankleROM_Y)) %>% 
  group_by(Config)

#moveDat<- subset(moveDat, dat$z_score < 2) #removing outliers  
#moveDat<- subset(moveDat, dat$z_score > -2)

ggplot(data = moveDat, aes(x = ankleROM_Y)) + geom_histogram() + facet_wrap(~Subject) 

withinSubPlot(moveDat, colName = 'ankleROM_Y', dir = 'lower')


runmod <- brm(data = moveDat, # Bayes model
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

extractVals(moveDat, runmod, configName = 'Tight', 'ankleROM_Y', 'higher') 



###### Ankle ROM Y Variability

sdDat <- sdData(moveDat, 'ankleROM_Y')

sdDat <- sdDat %>%
  mutate_at(vars(matches('SD_ankleROM_Y')), list(as.numeric)) %>%
  mutate(z_score = scale(SD_ankleROM_Y)) %>%
  group_by(Config)

withinSubPlot(sdDat, colName = 'SD_ankleROM_Y', dir = 'lower')


runmod <- brm(data = sdDat, # Bayes model
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

extractVals(sdDat, runmod, configName = 'Tight', 'SD_ankleROM_Y', 'lower') 



##### Midfoot ROM sagittal

moveDat <- moveDat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(mfROM_X)) %>% 
  group_by(Config)

#moveDat<- subset(moveDat, moveDat$z_score < 2) #removing outliers  
#moveDat<- subset(moveDat, moveDat$z_score > -2)

ggplot(data = moveDat, aes(x = mfROM_X)) + geom_histogram() + facet_wrap(~Subject) 

withinSubPlot(moveDat, colName = 'mfROM_X', dir = 'lower')


runmod <- brm(data = moveDat, # Bayes model
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

extractVals(moveDat, runmod, configName = 'Tight', 'mfROM_X', 'lower') 



###### Midfoot ROM X Variability

sdDat <- sdData(moveDat, 'mfROM_X')

sdDat <- sdDat %>%
  mutate_at(vars(matches('CV_mfROM_X')), list(as.numeric)) %>%
  mutate(z_score = scale(CV_mfROM_X)) %>%
  group_by(Config)

withinSubPlot(sdDat, colName = 'CV_mfROM_X', dir = 'lower')


runmod <- brm(data = sdDat, # Bayes model
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

extractVals(sdDat, runmod, configName = 'Tight', 'SD_mfROM_X', 'lower') 


##### Toe ROM sagittal

moveDat <- moveDat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(toeROM_X)) %>% 
  group_by(Config)

#moveDat<- subset(moveDat, moveDat$z_score < 2) #removing outliers  
#moveDat<- subset(moveDat, moveDat$z_score > -2)

ggplot(data = moveDat, aes(x = toeROM_X)) + geom_histogram() + facet_wrap(~Subject) 

withinSubPlot(moveDat, colName = 'toeROM_X', dir = 'lower')


runmod <- brm(data = moveDat, # Bayes model
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

extractVals(moveDat, runmod, configName = 'Tight', 'toeROM_X', 'lower')


###### Midfoot ROM X Variability

sdDat <- sdData(moveDat, 'toeROM_X')

sdDat <- sdDat %>%
  mutate_at(vars(matches('CV_toeROM_X')), list(as.numeric)) %>%
  mutate(z_score = scale(CV_toeROM_X)) %>%
  group_by(Config)

withinSubPlot(sdDat, colName = 'CV_toeROM_X', dir = 'lower')


runmod <- brm(data = sdDat, # Bayes model
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

extractVals(sdDat, runmod, configName = 'Tight', 'SD_toeROM_X', 'lower') 


##### Foot sliding M/L

moveDat <- moveDat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(footROM_X)) %>% 
  group_by(Config)

#moveDat<- subset(moveDat, moveDat$z_score < 2) #removing outliers  
#moveDat<- subset(moveDat, moveDat$z_score > -2)

ggplot(data = moveDat, aes(x = footROM_X)) + geom_histogram() + facet_wrap(~Subject) 

withinSubPlot(moveDat, colName = 'footROM_X', dir = 'lower')


runmod <- brm(data = moveDat, # Bayes model
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

extractVals(moveDat, runmod, configName = 'Tight', 'footROM_X', 'lower')


###### Foot Sliding M/L variability

sdDat <- sdData(moveDat, 'footROM_X')

sdDat <- sdDat %>%
  mutate_at(vars(matches('CV_footROM_X')), list(as.numeric)) %>%
  mutate(z_score = scale(CV_footROM_X)) %>%
  group_by(Config)

withinSubPlot(sdDat, colName = 'CV_footROM_X', dir = 'lower')


runmod <- brm(data = sdDat, # Bayes model
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

extractVals(sdDat, runmod, configName = 'Tight', 'SD_footROM_X', 'lower') 



##### Foot sliding A/P

moveDat <- moveDat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(footROM_Y)) %>% 
  group_by(Config)

#moveDat<- subset(moveDat, moveDat$z_score < 2) #removing outliers  
#moveDat<- subset(moveDat, moveDat$z_score > -2)

ggplot(data = moveDat, aes(x = footROM_Y)) + geom_histogram() + facet_wrap(~Subject) 

withinSubPlot(moveDat, colName = 'footROM_Y', dir = 'lower')


runmod <- brm(data = moveDat, # Bayes model
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

extractVals(moveDat, runmod, configName = 'Tight', 'footROM_Y', 'lower')

###### Foot Sliding M/L variability

sdDat <- sdData(moveDat, 'footROM_Y')

sdDat <- sdDat %>%
  mutate_at(vars(matches('SD_footROM_Y')), list(as.numeric)) %>%
  mutate(z_score = scale(SD_footROM_Y)) %>%
  group_by(Config)

withinSubPlot(sdDat, colName = 'SD_footROM_Y', dir = 'lower')


runmod <- brm(data = sdDat, # Bayes model
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

extractVals(sdDat, runmod, configName = 'Tight', 'SD_footROM_Y', 'lower') 


