
library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(readxl)

rm(list=ls())

dat <- read_excel('C:/Users/kate.harrison/Dropbox (Boa)/GolfPerformance/BOA_Heel_July2021/TrackManData/CompiledGolfData.xlsx')
dat <- as_tibble(dat)
dat$Shoe <- factor(dat$Shoe, c('Lace', 'A', 'B', 'C'))
### Drive Distance

datDist <- dat %>% 
  group_by(Subject) %>% 
  mutate(z_score = scale(Distance)) %>%
  group_by(Shoe)

datDist <- subset(datDist, datDist$z_score < 2)
datDist <- subset(datDist, datDist$z_score > -2)
  
ggplot(data = datDist) + geom_boxplot(mapping = aes(x = Subject, y = Distance, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

runmod <- brm(data = datDist,
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
sum(posterior[,4] < 0) / length(posterior[,4]) #The probability that this configuration results in a better outcome than the baseline (NOTE: Change >/< sign according to whether an increase or decrease in the outcome is desirable)

estimatedChange <- mean(posterior[,4]) #The maximum a posteriori estimate 

Ref <- subset(dat, dat$Shoe == 'Lace') # Change to baseline shoe name
RefMean <- mean(Ref$Distance)        # Change to variable of interest

NewConfig <- subset(dat, dat$Shoe == 'C') # Change to shoe being tested against baseline
NewConfigMean <- mean(NewConfig$Distance)        # Change to variable of interest

actualChange <- NewConfigMean - RefMean

pctChange <- actualChange/RefMean * 100



### Drive distance consistency

subjects <- unique(dat$Subject)
shoes <- unique(dat$Shoe)
sdDistance <- matrix(0, length(subjects)*length(shoes))
Sub <- rep(NA, length(subjects)*length(shoes))
Config <- rep(NA, length(subjects)*length(shoes))
r = 1

for (sub in subjects) {
  
  tmp_sub <- subset(dat, dat$Subject == sub)
  
  for (s in shoes) {
    tmp_shoe <- subset(tmp_sub, tmp_sub$Shoe == s)
    Sub[r] <- sub
    Config[r] <- s
    sdDistance[r] <- sd(tmp_shoe$Distance) 
    r = r+1
    
  }
}

SDdat <- cbind(Sub, Config, sdDistance)
colnames(SDdat) <- c('Subject', 'Shoe', 'SD_Distance')
SDdat <- SDdat[complete.cases(SDdat),]
SDdat <- as_tibble(SDdat)
SDdat$Shoe <- factor(SDdat$Shoe, c('Lace', 'A', 'B', 'C'))

SDdat <- SDdat %>%
  mutate_at(vars(matches('SD_Distance')), list(as.numeric)) %>%
  mutate(z_score = scale(SD_Distance)) %>%
  group_by(Shoe)

ggplot(data = SDdat) + geom_boxplot(mapping = aes(x = Shoe, y = SD_Distance, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))


runmod <- brm(data = SDdat,
              family = gaussian,
              z_score ~ Shoe, #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 0.5), class = b), #beta for the intercept for the change in loading rate for each configuration
                        #prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

print(runmod)

posterior <- posterior_samples(runmod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior[,4] < 0) / length(posterior[,4]) #The probability that this configuration results in a better outcome than the baseline (NOTE: Change >/< sign according to whether an increase or decrease in the outcome is desirable)

estimatedChange <- mean(posterior[,4]) #The maximum a posteriori estimate 

Ref <- subset(SDdat, SDdat$Shoe == 'Lace') # Change to baseline shoe name
RefMean <- mean(Ref$SD_Distance)        

NewConfig <- subset(SDdat, SDdat$Shoe == 'C') # Change to shoe being tested against baseline
NewConfigMean <- mean(NewConfig$SD_Distance)        

actualChange <- NewConfigMean - RefMean

pctChange <- actualChange/RefMean * 100


### Drive Direction


dat <- dat %>% 
  group_by(Subject) %>% 
  mutate(z_score = scale(AbsDirection))

ggplot(data = dat) + geom_boxplot(mapping = aes(x = Subject, y = AbsDirection, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

runmod <- brm(data = dat,
               family = gaussian,
               z_score ~ Shoe + (1 + Shoe | Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
               prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                         prior(normal(0, 0.5), class = b), #beta for the intercept for the change in loading rate for each configuration
                         prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                         prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
               iter = 2000, warmup = 1000, chains = 4, cores = 4,
               control = list(adapt_delta = .975, max_treedepth = 20),
               seed = 190831)

print(runmod)

posterior <- posterior_samples(runmod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)

sum(posterior[,4] < 0) / length(posterior[,4]) #The probability that this configuration results in a better outcome than the baseline (NOTE: Change >/< sign according to whether an increase or decrease in the outcome is desirable)

estimatedChange <- mean(posterior[,4]) #The maximum a posteriori estimate 

Ref <- subset(dat, dat$Shoe == 'Lace') # Change to baseline shoe name
RefMean <- mean(Ref$Direction)        # Change to variable of interest

NewConfig <- subset(dat, dat$Shoe == 'C') # Change to shoe being tested against baseline
NewConfigMean <- mean(NewConfig$Direction)        # Change to variable of interest

actualChange <- NewConfigMean - RefMean

pctChange <- actualChange/RefMean * 100



#### COP analysis #########################

rm(list=ls())

dat <- read.csv(file.choose())

subjects <- unique(dat$Subject)
shoes <- unique(dat$Shoe)
sd_MLrange <- matrix(0, length(subjects)*length(shoes))
Sub <- rep(NA, length(subjects)*length(shoes))
Config <- rep(NA, length(subjects)*length(shoes))
r = 1

for (sub in subjects) {
  
  tmp_sub <- subset(dat, dat$Subject == sub)
  
  for (s in shoes) {
    tmp_shoe <- subset(tmp_sub, tmp_sub$Shoe == s)
    Sub[r] <- sub
    Config[r] <- s
    sd_MLrange[r] <- sd(tmp_shoe$targetBack_range) 
    r = r+1
    
  }
}

SDdat <- cbind(Sub, Config, sd_MLrange)
colnames(SDdat) <- c('Subject', 'Shoe', 'SD_MLrange')
SDdat <- SDdat[complete.cases(SDdat),]
SDdat <- as_tibble(SDdat)
SDdat$Shoe <- factor(SDdat$Shoe, c('Lace', 'A', 'B', 'C'))

SDdat <- SDdat %>%
  mutate_at(vars(matches('SD_MLrange')), list(as.numeric)) %>%
  mutate(z_score = scale(SD_MLrange)) %>%
  group_by(Shoe)

ggplot(data = SDdat) + geom_boxplot(mapping = aes(x = Shoe, y = SD_MLrange, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))


runmod <- brm(data = SDdat,
              family = gaussian,
              z_score ~ Shoe, #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 0.5), class = b), #beta for the intercept for the change in loading rate for each configuration
                        #prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

print(runmod)

posterior <- posterior_samples(runmod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior[,4] < 0) / length(posterior[,4]) #The probability that this configuration results in a better outcome than the baseline (NOTE: Change >/< sign according to whether an increase or decrease in the outcome is desirable)

estimatedChange <- mean(posterior[,4]) #The maximum a posteriori estimate 

Ref <- subset(SDdat, SDdat$Shoe == 'Lace') # Change to baseline shoe name
RefMean <- mean(Ref$SD_MLrange)        

NewConfig <- subset(SDdat, SDdat$Shoe == 'C') # Change to shoe being tested against baseline
NewConfigMean <- mean(NewConfig$SD_MLrange)        

actualChange <- NewConfigMean - RefMean

pctChange <- actualChange/RefMean * 100