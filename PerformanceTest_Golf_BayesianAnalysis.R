
library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)
library(readxl)

rm(list=ls()) # Clears the environment

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
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 16)) + ylab(paste0({{colName}})) 
  
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
#################### Set up data

dat <- read_xlsx(file.choose()) # Select csv with compiled TrackMan data

dat <- as_tibble(dat) # creating the data frame


# Defining the baseline and other configs
baseline <- 'LR'
  
otherConfigs <- c('PFW')

allConfigs <- c(baseline, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs) 


################### Drive Distance

#grouping by subject and config by the variable being observed
dat <- dat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(CarryFlatLength)) %>% 
  group_by(Config)


#Normalization histograms, Check for normalish distribution/outliers
ggplot(data = dat, aes(x = CarryFlatLength)) + geom_histogram() + facet_wrap(~Subject) #Change x axis variable to your variable of interest. Check for normal-ish distribution.

# "best of" Line graph 
# This graph shoes a "Snap shot" of subject's best trial in each shoe. This is for demonstration purposes only, try to not take this graph too literally 
withinSubPlot(dat, colName = 'CarryFlatLength', dir = 'higher') 

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
extractVals(dat, runmod, otherConfigs, 'CarryFlatLength', 'higher') 

#################### Drive accuracy
#grouping by subject and config by the variable being observed
dat <- dat %>% 
  mutate(AbsAccuracy = abs(LaunchDirection)) %>%
  group_by(Subject) %>%
  mutate(z_score = scale(AbsAccuracy))

# "best of" Line graph 
withinSubPlot(dat, colName = 'AbsAccuracy', 'lower')


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
extractVals(dat, runmod, otherConfigs, 'AbsAccuracy', 'lower') 


######################## Dirve Distance Consistency


#This section of code is setting up variables to be going through a For Loop to be looking at the variation of drive distances per person, per shoe
subjects <- unique(dat$Subject)
shoes <- unique(dat$Config)
sdDistance <- matrix(0, length(subjects)*length(shoes))
Sub <- rep(NA, length(subjects)*length(shoes))
Config <- rep(NA, length(subjects)*length(shoes))
r = 1



for (sub in subjects) {
  
  tmp_sub <- subset(dat, dat$Subject == sub)
  
  for (s in shoes) {
    tmp_shoe <- subset(tmp_sub, tmp_sub$Config == s)
    Sub[r] <- sub
    Config[r] <- s
    sdDistance[r] <- sd(tmp_shoe$CarryFlatLength) 
    r = r+1
    
  }
}
# Looking into the variation of drive distances via standard diviations
SDdat <- cbind(Sub, Config, sdDistance)
colnames(SDdat) <- c('Subject', 'Config', 'SD_Distance')
SDdat <- SDdat[complete.cases(SDdat),]
SDdat <- as_tibble(SDdat)
SDdat$Config <- factor(SDdat$Config, allConfigs) 

SDdat <- SDdat %>%
  mutate_at(vars(matches('SD_Distance')), list(as.numeric)) %>%
  mutate(z_score = scale(SD_Distance)) %>%
  group_by(Config)


# "best of" Line graph 
withinSubPlot(SDdat, 'SD_Distance', 'lower')


## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = SDdat,
              family = gaussian,
              z_score ~ Config, #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 0.5), class = b), #beta for the intercept for the change in loading rate for each configuration
                        #prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# # Output of the Confidence Interval
extractVals(SDdat, runmod, configNames = , 'SD_Distance', 'lower') 


################################# Peak Force Magnitude (target foot)
 


forceDat <- read.csv(file.choose()) # Upload Overground force data csv

forceDat <- as_tibble(forceDat) # creating the data frame

forceDat$Config <- factor(forceDat$Config, allConfigs) 


# Looking at peak force data over time of the drives
forceDat <- forceDat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(targetPeakFz))


# "best of" Line graph 
withinSubPlot(forceDat, colName = 'targetPeakFz', 'higher')


## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = forceDat,
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
extractVals(forceDat, runmod, otherConfigs, 'targetPeakFz', 'higher') 


################################## Peak force consistency (target foot)


#This section of code is setting up variables to be going through a For Loop to be looking at the variation of peak force consistency per person, per shoe
subjects <- unique(forceDat$Subject)
shoes <- unique(forceDat$Config)
sdForce <- matrix(0, length(subjects)*length(shoes))
Sub <- rep(NA, length(subjects)*length(shoes))
Config <- rep(NA, length(subjects)*length(shoes))
r = 1

for (sub in subjects) {
  
  tmp_sub <- subset(forceDat, forceDat$Subject == sub)
  
  for (s in shoes) {
    tmp_shoe <- subset(tmp_sub, tmp_sub$Config == s)
    Sub[r] <- sub
    Config[r] <- s
    sdForce[r] <- sd(tmp_shoe$targetPeakFz) 
    r = r+1
    
  }
}
# Looking into the variation of peak force consistency via standard deviations
SDforceDat <- cbind(Sub, Config, sdForce)
colnames(SDforceDat) <- c('Subject', 'Config', 'SD_Force')
SDforceDat <- SDforceDat[complete.cases(SDforceDat),]
SDforceDat <- as_tibble(SDforceDat)
SDforceDat$Config <- factor(SDforceDat$Config, allConfigs) 

SDforceDat <- SDforceDat %>%
  mutate_at(vars(matches('SD_Force')), list(as.numeric)) %>%
  mutate(z_score = scale(SD_Force)) %>%
  group_by(Config)

# "best of" Line graph 
withinSubPlot(SDforceDat, 'SD_Force', 'lower')

## Bayes model 
# This model takes a while to run and may  crash your session 
#Wait until you receive a warning about rtools to run anything else
runmod <- brm(data = SDforceDat,
              family = gaussian,
              z_score ~ Config, #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 0.5), class = b), #beta for the intercept for the change in loading rate for each configuration
                        #prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# # Output of the Confidence Interval
extractVals(SDforceDat, runmod, otherConfigs, 'SD_Force', 'lower') 





################################################  EXTRA STUFF NOT IN PFL DATA REPORTS ###########################################################################
###############################################################################################################################
############################################################################################################################
### Plot prior, liklihood, posterior
posterior <- posterior_samples(runmod)
prior = as.data.frame(rnorm(4000, 0, 1))
likelihood <- as.data.frame(sample(dat$z_score, 4000, replace = TRUE, prob = NULL))
posterior_b <- posterior$b_ConfigSingle

plotTitles <- c('Prior', 'likelihood', 'posterior')

plotDat <- cbind(prior, likelihood, posterior_b)

colnames(plotDat) <- plotTitles

plotDatLong <- melt(plotDat)

ggplot(plotDatLong, aes(x = value, fill = variable)) + geom_density(alpha = 0.5) + xlab('Change between configurations, in SD' )


# Correlations between outcomes

dat <- dat %>% 
  filter(ContactTime > 10) %>% #remove values with impossible contact time
  filter(ContactTime < 100) %>%
  filter(pRanklePower <1500) %>% 
  
  group_by(SubjectName) %>%
  mutate(z_score = scale(pRanklePower)) %>% # Change to the variable you want to test
  group_by(Shoe)

outliers <- boxplot(dat$peakRankleINV, plot=FALSE)$out
dat<- dat[-which(dat$peakRankleINV %in% outliers),]

outliers <- boxplot(dat$CT_HorzNorm, plot=FALSE)$out
dat<- dat[-which(dat$CT_HorzNorm %in% outliers),]

plot(dat$FyPeak, dat$ContactTime)

cor.test(dat$FzPeak, dat$ContactTime, method = 'pearson')


for (config in otherConfigs) {
  
  print(config)
  
}
