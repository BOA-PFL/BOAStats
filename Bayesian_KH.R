library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(lme4)

rm(list=ls())

# Change to appropriate filepath
dat <- read.csv('C:/Users/kate.harrison/Dropbox (Boa)/EndurancePerformance/TNF_Scrambler_Apr_21/Novel_Data/Running/CompiledPressureData.csv')

# Change to the movement you want to look at (we analyze CMJ and Skater separately for most agility tests)
dat <- subset(dat, dat$Movement == 'Skater')
dat <- as_tibble(dat)

#Change to Config names used in your data, with the baseline model listed first.
dat$Shoe <- factor(dat$Shoe, c('Lace', 'BOA'))


dat <- dat %>% 
  filter(contactTime > 10) %>% #remove values with impossible contact time
  group_by(SubjectName) %>%
  mutate(z_score = scale(CT_HorzNorm)) %>% # Change to the variable you want to test
  group_by(Shoe)

#removing outliers of the variable of interest. 

outliers <- boxplot(dat$z_score, plot=FALSE)$out
dat<- dat[-which(dat$z_score %in% outliers),]

#Change x axis variable to your variable of interest. Check for normal-ish distribution.
ggplot(data = dat, aes(x = CT_HorzNorm)) + geom_histogram() + facet_wrap(~Subject) 

#Change y axis variable to your variable of interest
ggplot(data = dat) + geom_boxplot(mapping = aes(x = SubjectName, y = toMedFF, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))

runmod <- brm(data = dat,
              family = gaussian,
              z_score ~ Shoe + (1 | SubjectName), #fixed effect of configuration with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #Since we use z-scores, the intercept prior is set as a mean of 0 with SD of 1
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in outcome for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

print(runmod)



posterior <- posterior_samples(runmod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)

# Change column number to the column of the shoe you are analyzing. This outputs the probability that the variable in the comparison shoe is higher 
# or lower than the baseline shoe (i.e if probability > 0.5, value of comparison shoe is greater than baseline, if probability <0.5, value of comparison 
# shoe is lower than baseline). Be careful when interpreting to consider if a higher or lower value is better. 

sum(posterior[,2] < 0) / length(posterior[,2]) 

estimatedChange <- mean(posterior[,2]) #The maximum a posteriori estimate 

Ref <- subset(dat, dat$Shoe == 'Lace') # Change to baseline shoe name
RefMean <- mean(Ref$EE)        # Change to variable of interest

NewConfig <- subset(dat, dat$Shoe == 'TC_BOA') # Change to shoe being tested against baseline
NewConfigMean <- mean(NewConfig$EE)        # Change to variable of interest

actualChange <- NewConfigMean - RefMean # outputs change from baseline to comparison shoe in units of measurement

pctChange <- actualChange/RefMean * 100 # outputs change from baseline to comparison shoe in percentage

