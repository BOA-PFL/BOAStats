library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(lme4)

rm(list=ls())

# Change to appropriate filepath
dat <- read.csv('C:/Users/Adam.Luftglass/OneDrive - Boa Technology Inc/Documents/R/CompiledKinematicData.csv')
#datagil <- read.csv('C:/Users/Adam.Luftglass/OneDrive - Boa Technology Inc/Documents/R/CompiledAgilityData.csv')

# Change to the movement you want to look at (we analyze CMJ and Skater separately for most agility tests)
dat <- subset(dat, dat$Movement == 'Skater')
dat <- as_tibble(dat)
#datagil <- subset(datagil, datagil$Movement == 'CMJ')
#datagil <- as_tibble(datagil)

#Change to Config names used in your data, with the baseline model listed first.
dat$Shoe <- factor(dat$Shoe, c('Lace', 'LR', 'Lateralhigh', 'Lateralhmid'))
#datagil$Shoe <- factor(datagil$Shoe, c('Lace', 'LR', 'Lateralhigh', 'Lateralhmid'))

dat <- dat %>% 
  group_by(SubjectName) %>%
  mutate(z_score = scale(pkINV)) %>% # Change to the variable you want to test
  group_by(Shoe)
#datagil <- datagil %>% 
 # filter(ContactTime > 10) %>% #remove values with impossible contact time
  #group_by(SubjectName) %>%
  #mutate(z_score = scale(CT_VertNorm)) %>% # Change to the variable you want to test
  #group_by(Shoe)
#removing outliers of the variable of interest. 

outliers <- boxplot(dat$z_score, plot=FALSE)$out
#outliersagil <- boxplot(datagil$z_score, plot=FALSE)$out
#dat<- dat[-which(dat$z_score %in% outliers),]
#datagil<- datagil[-which(datagil$z_score %in% outliersagil),]

#Change x axis variable to your variable of interest. Check for normal-ish distribution.
ggplot(data = dat, aes(x = pkINV)) + geom_histogram()  
#ggplot(data = datagil, aes(x = ContactTime)) + geom_histogram() 

#Change y axis variable to your variable of interest
ggplot(data = dat) + geom_boxplot(mapping = aes(x = SubjectName, y = pkINV, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))
#ggplot(data = datagil) + geom_boxplot(mapping = aes(x = SubjectName, y = ContactTime, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))


makePlot <- function(inputDF, colName) {
  ggplot(data = inputDF) + geom_boxplot(mapping = aes(x = SubjectName, y = .data[[colName]], fill = Shoe)) + 
    scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))
  
}

makePlot(dat, 'pkINV')
makePlot(dat, 'pkEV')


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

#print(runmod)
#runmodagil <- brm(data = datagil,
 #             family = gaussian,
  #            z_score ~ Shoe + (1 | SubjectName), #fixed effect of configuration with a different intercept and slope for each subject
   #           prior = c(prior(normal(0, 1), class = Intercept), #Since we use z-scores, the intercept prior is set as a mean of 0 with SD of 1
    #                    prior(normal(0, 1), class = b), #beta for the intercept for the change in outcome for each configuration
     #                   prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
      #                  prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
       #       iter = 2000, warmup = 1000, chains = 4, cores = 4,
        #      control = list(adapt_delta = .975, max_treedepth = 20),
         #     seed = 190831)

#print(runmodagil)



posterior <- posterior_samples(runmod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
#posterioragil <- posterior_samples(runmodagil)

# Change column number to the column of the shoe you are analyzing. This outputs the probability that the variable in the comparison shoe is higher 
# or lower than the baseline shoe (i.e if probability > 0.5, value of comparison shoe is greater than baseline, if probability <0.5, value of comparison 
# shoe is lower than baseline). Be careful when interpreting to consider if a higher or lower value is better. 

sum(posterior[,3] < 0) / length(posterior[,3]) 
#sum(posterioragil[,3] < 0) / length(posterioragil[,3]) 

estimatedChange <- mean(posterior[,3]) #The maximum a posteriori estimate 
#estimatedChangeagil <- mean(posterioragil[,3])

Ref <- subset(dat, dat$Shoe == 'Lace') # Change to baseline shoe name
RefMean <- mean(Ref$pkINV)    # Change to variable of interest
#Refagil <- subset(datagil, datagil$Shoe == 'Lace') # Change to baseline shoe name
#RefMeanagil <- mean(Refagil$CT_VertNorm)    

NewConfig <- subset(dat, dat$Shoe == 'Lateralhigh') # Change to shoe being tested against baseline
NewConfigMean <- mean(NewConfig$pkINV)        # Change to variable of interest
#NewConfigagil <- subset(datagil, datagil$Shoe == 'Lateralhigh') # Change to shoe being tested against baseline
#NewConfigMeanagil <- mean(NewConfigagil$CT_VertNorm)        # Change to variable of interest

actualChange <- NewConfigMean - RefMean # outputs change from baseline to comparison shoe in units of measurement
#actualChangeagil <- NewConfigMeanagil - RefMeanagil # outputs change from baseline to comparison shoe in units of measurement


pctChange <- actualChange/RefMean * 100 # outputs change from baseline to comparison shoe in percentage
#pctChangeagil <- actualChangeagil/RefMeanagil * 100 # outputs change from baseline to comparison shoe in percentage

