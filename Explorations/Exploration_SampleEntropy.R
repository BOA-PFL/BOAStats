### This script examines the effects of footwear, slope, sex, running speed, and trial order on sample entropy, using lmer models.


library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)
library(readxl)
library(lmerTest)

rm(list=ls())

####### Functions

withinSubPlot <- function(inputDF, colName, dir) {
  
  # direction can be 'lower' or higher'. It is the direction of change that is better. 
  # For example, for contact time lower is better. so we put 'lower'. for jump height, higher is better, so we put higher. 
  meanDat <- inputDF %>%
    group_by(Sex, Subject, Config) %>%
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
  
  ggplot(data = whichConfig, mapping = aes(x = as.factor(Config), y = mean, col = BestConfig, group = Subject)) + geom_point(aes( shape = Sex), size = 4) + 
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000","#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 26)) + ylab(paste0({{colName}})) 
  
}


extractVals <- function(dat, mod, configNames, baseConfig, var, dir) {
  
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
      sentences[i] <- paste('We have minimal confidence that',output[i,1], 'outperformed', baseConfig, '(',output[i,2], '%)','\n', '\t', '- Estimated difference:',output[i,3],'to',output[i,4],'%')
    } else if (as.numeric(output[i,2]) >= 30){
      sentences[i] <- paste('There were inconsistent differences between',output[i,1],'and',baseConfig,'(',output[i,2],'%)','\n', '\t', '- Estimated difference:',output[i,3],'to',output[i,4],'%')
    } else if (as.numeric(output[i,2]) >= 20){
      sentences[i] <- paste('We have minimal confidence that',output[i,1],'performed worse than',baseConfig,'(',(100 - as.numeric(output[i,2])),'%)','\n', '\t', '- Estimated difference:',output[i,3],'to',output[i,4],'%')
    } else if (as.numeric(output[i,2]) >= 10){
      sentences[i] <- paste('We have moderate confidence that',output[i,1],'performed worse than',baseConfig,'(',(100 - as.numeric(output[i,2])),'%)','\n', '\t', '- Estimated difference:',output[i,3],'to',output[i,4],'%')
    } else {
      sentences[i] <- paste('We have meaningful confidence that',output[i,1],'performed worse than',baseConfig,'(',(100 - as.numeric(output[i,2])),'%)','\n', '\t', '- Estimated difference:',output[i,3],'to',output[i,4],'%')
    }
  }
  
  writeLines(sentences)
  return()
}

############################### sample entropy
## Load sample entropy data
sedat <- read_csv('C:\\Users\\Kate.Harrison\\Boa Technology Inc\\PFL Team - General\\Testing Segments\\EndurancePerformance\\TrailRun_2022\\OutdoorData\\IMUData\\CompiledSampleEntropy_Rotated.csv') # Reading in the CSV
subDat <- read_xlsx('C:\\Users\\Kate.Harrison\\Boa Technology Inc\\PFL Team - General\\Testing Segments\\EndurancePerformance\\TrailRun_2022\\MasterListOutdoor.xlsx')
colnames(subDat)[1] = 'Subject'

## Load trial order data
trialDat <- read_xlsx('C:\\Users\\Kate.Harrison\\Boa Technology Inc\\PFL Team - General\\Testing Segments\\EndurancePerformance\\TrailRun_2022\\OutdoorTrialOrder.xlsx')
trialDat$Config <- tolower(trialDat$Config)
colnames(trialDat)[2] = 'Order'


## Load running speed data
speedDat <- read_csv('C:\\Users\\Kate.Harrison\\Boa Technology Inc\\PFL Team - General\\Testing Segments\\EndurancePerformance\\TrailRun_2022\\OutdoorData\\IMUspeed.csv')
speedDat <- subset(speedDat, select = -c(1))
colnames(speedDat)[3:4] <- c('Trial', 'Slope')

speedSummary <- speedDat %>% ## Average speed data across sections within each trial
  group_by(Subject, Config, Slope, Trial) %>%
  subset(Slope != 0) %>%
  mutate(Slope = replace(Slope, Slope == 1, 'up'))%>%
  mutate(Slope = replace(Slope, Slope == 2, 'flat'))%>%
  mutate(Slope = replace(Slope, Slope == 3, 'down'))%>%
  summarise(Speed = mean(Speed, na.rm = TRUE))

speedDat <- speedDat %>%
  subset(Slope != 0) %>%
  mutate(Slope = replace(Slope, Slope == 1, 'up'))%>%
  mutate(Slope = replace(Slope, Slope == 2, 'flat'))%>%
  mutate(Slope = replace(Slope, Slope == 3, 'down'))
speed <- merge(speedDat, trialDat, by = c('Trial', 'Config', 'Subject'))
speed <- merge(speed, subDat, by = c('Subject'))


## join data 

dat1 <- merge(sedat, subDat, by = 'Subject' )
dat2 <- merge(dat1, speedSummary, by = c('Subject', 'Config', 'Trial', 'Slope'))
dat <- merge(dat2, trialDat, by = c('Subject', 'Trial', 'Config'))
dat <- as_tibble(dat) # creating the data frame
# Defining the baseline and other configs
baseline <- 'lace'

otherConfigs <- c('pfs')

allConfigs <- c(baseline, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs)

#### Sample Entropy X - Vertical

# create histogram to check data quality
ggplot(data = dat, aes(x = sampEntX_foot, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 

# plot subject average data to see which config had lowest entropy
p <- withinSubPlot(dat, colName = 'sampEntX_foot', dir = 'lower')
p + ylab('Sample Entropy (X)')

xmod1 <- lmer(sampEntX_foot ~ Config*Sex + Order + Slope + Speed + (1|Subject), data = dat)
summary(xmod1)


#### Sample Entropy Y - mediolateral

ggplot(data = dat, aes(x = sampEntY_foot, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 

p <- withinSubPlot(dat, colName = 'sampEntY_foot', dir = 'lower')
p + ylab('Sample Entropy (Y)')

ymod1 <- lmer(sampEntY_foot ~ Config*Sex + Order + Slope + Speed + (1|Subject), data = dat) # all variables
summary(ymod1)

ymod2 <- lmer(sampEntY_foot ~ Order + Slope + Speed + (1|Subject), data = dat) # removing Config
summary(ymod2) 

smod <- lmer(Speed ~ Config*Sex + Order*Sex + Slope*Sex +(1|Subject), data = speed)
summary(smod)


#### Sample Entropy Z - anteroposterior

ggplot(data = dat, aes(x = sampEntZ_foot, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 

p <- withinSubPlot(dat, colName = 'sampEntZ_foot', dir = 'lower')
p + ylab('Sample Entropy (Z)')

zmod1 <- lmer(sampEntZ_foot ~ Config*Sex + Order + Slope + Speed + (1|Subject), data = dat) # all variables
summary(zmod1)

zmod2 <- lmer(sampEntZ_foot ~ Order + Slope + Speed + Sex + (1|Subject), data = dat) # removing Config
summary(zmod2) # p = 0.06, trend towards men have less entropy

zmod3 <- lmer(sampEntZ_foot ~ Order + Speed + Sex + (1|Subject), data = dat) # remove Slope
summary(zmod3) # p=0.07, trend towards pfs increases entropy


############################### lyapunov exponent

imudat <- read_csv('C:\\Users\\Kate.Harrison\\Boa Technology Inc\\PFL Team - General\\Testing Segments\\EndurancePerformance\\TrailRun_2022\\OutdoorData\\IMUData\\CompiledStabilityData.csv') # Reading in the CSV
subDat <- read_xlsx('C:\\Users\\Kate.Harrison\\Boa Technology Inc\\PFL Team - General\\Testing Segments\\EndurancePerformance\\TrailRun_2022\\MasterListOutdoor.xlsx')
colnames(subDat)[1] = 'Subject'

trialDat <- read_xlsx('C:\\Users\\Kate.Harrison\\Boa Technology Inc\\PFL Team - General\\Testing Segments\\EndurancePerformance\\TrailRun_2022\\OutdoorTrialOrder.xlsx')
trialDat$Config <- tolower(trialDat$Config)
colnames(trialDat)[2] = 'Order'

dat1 <- merge(imudat, subDat, by = 'Subject' )
dat2 <- merge(dat1, speedSummary, by = c('Subject', 'Trial', 'Config', 'Slope'))
dat <- merge(dat2, trialDat, by = c('Subject', 'Trial', 'Config'))
dat <- as_tibble(dat) # creating the data frame
# Defining the baseline and other configs
baseline <- 'lace'

otherConfigs <- c('pfs')

allConfigs <- c(baseline, otherConfigs)

dat$Config <- factor(dat$Config, allConfigs)

#### lyapunov exponent

ggplot(data = dat, aes(x = lyapE, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 

p <- withinSubPlot(dat, colName = 'lyapE', dir = 'lower')
p + ylab('Lyapunov Exponent')

mod1 <- lmer(lyapE ~ Config + Order + Slope + Sex + Side + Speed + (1|Subject), data = dat)
summary(mod1)

mod2 <- lmer(lyapE ~ Config + Order + Slope + Side + Speed + (1|Subject), data = dat) # remove sex
anova(mod1, mod2) # p = 0.27, remove Sex

mod3 <- lmer(lyapE ~ Slope + Order + Side + Speed + (1|Subject), data = dat) # remove Config
anova(mod2, mod3) # p = 0.53, remove config

mod4 <- lmer(lyapE ~ Order + Side + Speed + (1|Subject), data = dat) # remove slope
anova(mod3, mod4) # p = 0.000000002, keep slope

mod5 <- lmer(lyapE ~ Slope + Side + (1|Subject), data = dat) # remove speed
anova(mod3, mod5) # p = 0.00000000009, keep speed

nullmod <- lmer(lyapE ~ (1|Subject), data = dat)
anova(mod3, nullmod)




