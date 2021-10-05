
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
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 16)) + ylab(paste0({{colName}})) 
  
}


extractVals <- function(dat, mod, configName, var, dir) {
  
  # This function takes the original dataframe (dat, same one entered into runmod), the Bayesian model from brms (runmod), 
  # the configuration Name, and the variable you are testing. It returns:
  # [1] the probabality the variable was lower in the teste Configuration compared to baseline
  # [2] tje probability the variable was higher in the tested configuration
  # [3] the lower bound of the bayesian 95% posterior interval (as percent change from baseline) 
  # [4] the upper bound of the bayesian 95% posterior interval (as percent change from baseline)
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
  
  return(list(prob, ci_LowPct, ci_HighPct))
  
}


###############################



# Change to appropriate filepath
dat <- read.csv(file.choose())

# Change to the movement you want to look at (we analyze CMJ and Skater separately for most agility tests)
dat <- subset(dat, dat$Movement == 'Skater')
dat <- as_tibble(dat)

dat <- subset(dat, dat$Config != 'Barefoot')
#Change to Config names used in your data, with the baseline model listed first.
dat$Config <- factor(dat$Config, c('Nothing', 'Single', '4guide'))


dat <- dat %>% 
  filter(ContactTime > 10) %>% #remove values with impossible contact time
  filter(ContactTime < 100) %>%
  group_by(Subject) %>%
  mutate(z_score = scale(ContactTime)) %>% # Change to the variable you want to test
  group_by(Config)


#removing outliers of the variable of interest. 

dat<- subset(dat, dat$z_score < 2)
dat<- subset(dat, dat$z_score > -2)

#Change x axis variable to your variable of interest. Check for normal-ish distribution.

ggplot(data = dat, aes(x = ContactTime)) + geom_histogram() + facet_wrap(~Subject) 

#Change mean variable to your variable of interest

withinSubPlot(dat, 'ContactTime', 'lower')

##### Bayes model

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





  
extractVals(dat, runmod, '4guide', 'ContactTime', 'lower')






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
