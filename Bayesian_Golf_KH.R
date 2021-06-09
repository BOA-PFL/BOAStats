
library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)

rm(list=ls())

dat <- read.csv('C:/Users/kate.harrison/Dropbox (Boa)/EndurancePerformance/NewBalanceRC_May2021/Forces/CompiledRunData.csv')
dat <- as_tibble(dat)
dat$Shoe <- factor(dat$Shoe, c('1400_Lace', 'RC_BOA'))
### Drive Distance

dat <- subset(dat, dat$contactTime >0.1)

dat <- dat %>% 
  group_by(SubjectName) %>% 
  mutate(z_score = scale(VALR)) %>%
  group_by(Shoe)

outliers <- boxplot(dat$z_score, plot=FALSE)$out

dat<- dat[-which(dat$z_score %in% outliers),]
  
ggplot(data = dat) + geom_boxplot(mapping = aes(x = SubjectName, y = VALR, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))

runmod <- brm(data = dat,
              family = gaussian,
              z_score ~ Shoe + (1 + Shoe | SubjectName), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

print(runmod)

posterior <- posterior_samples(runmod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior[,2] < 0) / length(posterior[,2]) #The probability that this configuration results in a better outcome than the baseline (NOTE: Change >/< sign according to whether an increase or decrease in the outcome is desirable)

estimatedChange <- mean(posterior[,2]) #The maximum a posteriori estimate 

Ref <- subset(dat, dat$Shoe == '1400_Lace') # Change to baseline shoe name
RefMean <- mean(Ref$VALR)        # Change to variable of interest

NewConfig <- subset(dat, dat$Shoe == 'RC_BOA') # Change to shoe being tested against baseline
NewConfigMean <- mean(NewConfig$VALR)        # Change to variable of interest

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

SDdat <- SDdat %>%
  group_by(Subject) %>%
  mutate_at(vars(matches('SD_Distance')), list(as.numeric)) %>%
  mutate(z_score = scale(SD_Distance)) %>%
  group_by(Shoe)

ggplot(data = SDdat) + geom_boxplot(mapping = aes(x = Subject, y = SD_Distance, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))


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
sum(posterior[,2] < 0) / length(posterior[,2]) #The probability that this configuration results in a better outcome than the baseline (NOTE: Change >/< sign according to whether an increase or decrease in the outcome is desirable)

estimatedChange <- mean(posterior[,2]) #The maximum a posteriori estimate 

Ref <- subset(SDdat, SDdat$Shoe == 'Lace') # Change to baseline shoe name
RefMean <- mean(Ref$SD_Distance)        

NewConfig <- subset(SDdat, SDdat$Shoe == 'Medial') # Change to shoe being tested against baseline
NewConfigMean <- mean(NewConfig$SD_Distance)        

actualChange <- NewConfigMean - RefMean

pctChange <- actualChange/RefMean * 100


### Drive Direction

dat %>%
  group_by(SUBJECT, SHOE) %>%
  summarize(
    Avg = mean(ABS_DIRECTION),
    SD = sd(ABS_DIRECTION)
  ) %>%
  
  ggplot(mapping = aes(x = SHOE, y = Avg, fill = SHOE)) +
  geom_bar(position="dodge", stat="identity") + facet_wrap(~SUBJECT) +
  geom_errorbar(aes(ymin=Avg-SD, ymax=Avg+SD), width=.2,
                position=position_dodge(.9))

dat <- dat %>% 
  group_by(SUBJECT) %>% 
  mutate(z_score = scale(ABS_DIRECTION))

outliers <- boxplot(dat$z_score, plot=FALSE)$out

dat<- dat[-which(dat$z_score %in% outliers),]

runmod <- brm(data = dat,
               family = gaussian,
               z_score ~ SHOE + (1 + SHOE | SUBJECT), #fixed effect of configuration and time period with a different intercept and slope for each subject
               prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                         prior(normal(0, 0.5), class = b), #beta for the intercept for the change in loading rate for each configuration
                         prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                         prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
               iter = 2000, warmup = 1000, chains = 4, cores = 4,
               control = list(adapt_delta = .975, max_treedepth = 20),
               seed = 190831)

print(runmod)

posterior <- posterior_samples(runmod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
sum(posterior$b_SHOE2MLR < 0) / length(posterior$b_SHOE2MLR)
sum(posterior$b_SHOE3MAP < 0) / length(posterior$b_SHOE3MAP)
sum(posterior$b_SHOE4MMono < 0) / length(posterior$b_SHOE4MMono)

accuracy_mean <- mean(dat$ABS_DIRECTION)
accuracy_sd <- sd(dat$ABS_DIRECTION)