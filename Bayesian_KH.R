
library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(lme4)



rm(list=ls())

# Change to appropriate filepath
dat <- read.csv(file.choose())


# Change to the movement you want to look at (we analyze CMJ and Skater separately for most agility tests)
dat <- subset(dat, dat$Movement == 'Skater')
dat <- as_tibble(dat)

#Change to Config names used in your data, with the baseline model listed first.

dat$Shoe <- factor(dat$Shoe, c('Tri', 'Asym'))


dat <- dat %>% 
  filter(ContactTime > 10) %>% #remove values with impossible contact time
  filter(ContactTime < 100) %>%
  group_by(Subject) %>%
  mutate(z_score = scale(ContactTime)) %>% # Change to the variable you want to test
  group_by(Shoe)


#removing outliers of the variable of interest. 

dat<- subset(dat, dat$z_score > 2)
dat<- subset(dat, dat$z_score < -2)

#Change x axis variable to your variable of interest. Check for normal-ish distribution.
ggplot(data = dat, aes(x = ContactTime)) + geom_histogram() + facet_wrap(~Subject) 

#Change y axis variable to your variable of interest
ggplot(data = dat) + geom_boxplot(mapping = aes(x = Subject, y = RankleNegWork, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

runmod <- brm(data = dat,
              family = gaussian,
              z_score ~ Shoe + (1 + Shoe | Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
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


prior = as.data.frame(rnorm(1000, 0, 1))

bayes_plot = ggplot(data = prior) + geom_density(mapping = aes(x = prior[,1]), color = "#ECE81A", fill = "#ECE81A", alpha = 0.5 )

bayes_plot
bayes_plot = bayes_plot + geom_density(data = dat, mapping = aes( x = dat$z_score), color = "#003D4C", fill = "#003D4C", alpha = 0.5)
bayes_plot

bayes_plot + geom_density(data = posterior, mapping = aes(x = b_ShoeAsym), color = "#00966C", fill = "#00966C", alpha = 0.5) + ggtitle("Prior = yellow (0,1), data = blue, posterior = green ") + xlab('SD')

#find peak of posterior

posteriorDens = density(posterior[,2])
posteriorDens_x = posteriorDens$x
posteriorDens_y = posteriorDens$y
MaxPosterior_y = max(posteriorDens_y)
posteriorPeak = posteriorDens_x[posteriorDens_y == MaxPosterior_y]


# Find mean change from the data
Ref <- subset(dat, dat$Shoe == 'Tri') # Change to baseline shoe name
RefMean <- mean(Ref$ContactTime)        # Change to variable of interest

NewConfig <- subset(dat, dat$Shoe == 'Asym') # Change to shoe being tested against baseline
NewConfigMean <- mean(NewConfig$ContactTime)        # Change to variable of interest

estimatedChange <- mean(posterior[,2]) #The maximum a posteriori estimate 

Ref <- subset(dat, dat$Shoe == 'Lace') # Change to baseline shoe name
RefMean <- mean(Ref$EE)        # Change to variable of interest

NewConfig <- subset(dat, dat$Shoe == 'TC_BOA') # Change to shoe being tested against baseline
NewConfigMean <- mean(NewConfig$EE)        # Change to variable of interest


actualChange <- NewConfigMean - RefMean # outputs change from baseline to comparison shoe in units of measurement

pctChange <- actualChange/RefMean * 100 # outputs change from baseline to comparison shoe in percentage


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

