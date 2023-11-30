### This code joins data from the baseball radar system and motion capture systems. It then uses lmer to understand which biomechanical variables 
### impact pitching/throwing performance


library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)
library(readxl)
library(lubridate)
library(chron)
library(stringr)
library(ggpubr)

rm(list=ls())# Clears the environment


# Get times for radar data
radarDat <- read.csv(file.choose())


radarTime <- times(c())
### convert time to appropriate format to be able to match and join with other data frames

for (i in 1:nrow(radarDat)){

  tmpDateTime <- radarDat$timeStamp[i]  
  hourMin <- str_sub(tmpDateTime, -11, -4)
  radarTime[i] <- times(hourMin)
  #time <- as.POSIXct(hourMin,format="%H:%M:%S",tz=Sys.timezone())
  #radarMin[i] <- format(as.POSIXct(time), format = "%M")
  #radarSec[i] <- format(as.POSIXct(time), format = "%S")
  
}
radarDat <- cbind(radarTime, radarDat)
  
#Get times for mocap data

mocapDat<- read.csv(file.choose())
mocapDat$t0 <- times(mocapDat$t0)
mocapDat$t0 <- mocapDat$t0 - times('06:00:00') # correcting six hour offset between mocap and radar time stamps
throwEndDiff <- seconds_to_period(mocapDat$throwEnd)
sprintf('%02d %02d:%02d:%02d', day(td), td@hour, minute(td), second(td))
radarStart <- times(c())
radarEnd <- times(c())

for (i in 1:nrow(mocapDat)){
  
  #i = 1
  
  tmpStart = seconds_to_period(mocapDat$throwEnd[i])
  tmpStart = sprintf("%02d:%02d", minute(tmpStart), second(tmpStart))
  tmpStart = paste('00:', sep = "", tmpStart)
  radarStart[i] = times(tmpStart)
  
  #tmpEnd = seconds_to_period(mocapDat$radarWindowEnd[i])
  #tmpEnd = sprintf("%02d:%02d", minute(tmpEnd), second(tmpEnd))
  #tmpEnd = paste('00:', sep = "", tmpEnd)
  #radarEnd[i] = times(tmpEnd)
  
  
}

mocapDat['radarSearchStart'] <- mocapDat$t0 + radarStart

mocapDat['radarSearchEnd'] <- mocapDat$t0 + radarStart + times('00:00:07')


# Join mocap and radar data by subject, Config, and radarDat$radarTime between mocapDat$radarSearchStart and mocapDat$radarSearchEnd


allDat <-  full_join(radarDat, mocapDat, by=c("Subject", 'Config'))

allDat <- filter(allDat, radarTime >= radarSearchStart, radarTime <= radarSearchEnd )


# Look at correlations between velo and biomech
allDat$Velocity <- as.numeric(allDat$Velocity)

allDat$Config <- factor(allDat$Config, c('Lace', 'BOA'))

allDat %>%
  ggplot(aes(x= pkKneeExtVel ,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Knee Extension Velocity (deg/s)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


mod1 <- lmer(Velocity ~ pkKneeExtVel + (pkKneeExtVel|Subject), data = allDat)
summary(mod1)

allDat <- allDat %>% 
  group_by(Subject) %>%
  mutate(z = scale(pkKneeExtVel)) %>%
  mutate(z_velo = scale(Velocity)) 

bmod1 <- brm(data = allDat, 
              family = gaussian,
              z_velo ~ z + (z| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

allDat %>%
  filter(pBF > 200) %>%
  ggplot(aes(x= pBF ,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Braking Force", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

mod1 <- lmer(Velocity ~ pkKneeExtVel + (pkKneeExtVel|Subject), data = allDat)
summary(mod1)

allDat <- allDat %>% 
  filter(pBF > 200) %>%
  group_by(Subject) %>%
  mutate(z = scale(pBF)) %>%
  mutate(z_velo = scale(Velocity)) 

bmod2 <- brm(data = allDat, 
             family = gaussian,
             z_velo ~ z + (z| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
             prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                       prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                       prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                       prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)

summary(bmod2)
post <- as_draws_matrix(bmod2)


allDat %>%
  filter(pVGRF > 500) %>%
  ggplot(aes(x= pVGRF ,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Vertical Force", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat <- allDat %>% 
  filter(pVGRF > 500) %>%
  group_by(Subject) %>%
  mutate(z = scale(pVGRF)) %>%
  mutate(z_velo = scale(Velocity)) 

bmod3 <- brm(data = allDat, 
             family = gaussian,
             z_velo ~ z + (z| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
             prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                       prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                       prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                       prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)

summary(bmod3)
post <- as_draws_matrix(bmod3)
sum(post[,2] > 0)/nrow(post)
ranef(bmod3)



allDat %>%
  #filter(pAF > 500) %>%
  ggplot(aes(x= pAF ,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Anterior Force", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")





allDat %>%
  ggplot(aes(x= pelVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Pelvis Velocity (deg/s)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat <- allDat %>% 
  group_by(Subject) %>%
  mutate(z = scale(pelVel)) %>%
  mutate(z_velo = scale(Velocity)) 

bmod5 <- brm(data = allDat, 
             family = gaussian,
             z_velo ~ z + (z| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
             prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                       prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                       prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                       prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)

summary(bmod5)
post <- as_draws_matrix(bmod5)
sum(post[,2]>0)/nrow(post)
ranef(bmod5)


bmod6 <- brm(data = allDat, 
             family = gaussian,
             z ~ Config + (Config| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
             prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                       prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                       prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                       prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)

summary(bmod6)
post <- as_draws_matrix(bmod6)
sum(post[,2]>0)/nrow(post)
ranef(bmod5)


allDat %>%
  filter(kneeExtROM>0)%>%
  ggplot(aes(x= kneeExtROM,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "KneeExtROM (deg)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  filter(rearFootEnergy < 20)%>%
  ggplot(aes(x= rearFootEnergy,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Foot Energy", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(rearShankEnergy < 50)%>%
  ggplot(aes(x= rearShankEnergy,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Shank Energy", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(rearThighEnergy < 150)%>%
  ggplot(aes(x= rearThighEnergy,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Thigh Energy", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= peakNegLeadDistalRFpower,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Distal RF Peak Neg Power", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat <- allDat %>% 
  group_by(Subject) %>%
  mutate(z = scale(peakNegLeadDistalRFpower)) %>%
  mutate(z_velo = scale(Velocity)) 

bmod4 <- brm(data = allDat, 
             family = gaussian,
             z_velo ~ z + (z| Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
             prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                       prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                       prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                       prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
             iter = 2000, warmup = 1000, chains = 4, cores = 4,
             control = list(adapt_delta = .975, max_treedepth = 20),
             seed = 190831)

summary(bmod4)
post <- as_draws_matrix(bmod4)
sum(post[,2] > 0)/nrow(post)
ranef(bmod4)




allDat %>%
  ggplot(aes(x= leadDistalRFnegWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Distal RF Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")




