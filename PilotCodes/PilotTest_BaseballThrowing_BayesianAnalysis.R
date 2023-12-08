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
library(cAIC4)
library(lmerTest)

rm(list=ls())

# Get times for radar data
radarDat <- read.csv('C:\\Users\\Kate.Harrison\\Boa Technology Inc\\PFL Team - General\\Testing Segments\\Baseball\\Baseball_Pilot\\Radar\\Throw\\CompiledRadarThrowingData.csv')


radarTime <- times(c())


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

mocapDat<- read.csv('C:\\Users\\Kate.Harrison\\Boa Technology Inc\\PFL Team - General\\Testing Segments\\Baseball\\Baseball_Pilot\\Mocap\\Throw\\CompiledResults7.csv')
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


dat <-  full_join(radarDat, mocapDat, by=c("Subject", 'Config'))

dat <- filter(dat, radarTime >= radarSearchStart, radarTime <= radarSearchEnd )


# Look at correlations between velo and biomech
dat$Velocity <- as.numeric(dat$Velocity)

dat$Config <- factor(dat$Config, c('Lace', 'BOA'))
dat$peakNegLeadDistalRFpower <- abs(dat$peakNegLeadDistalRFpower)




###### Velocity ~ Biomech


dat <- dat %>% 
  
  group_by(SubjectID) %>%
  mutate(z_Velocity = scale(Velocity)) %>% 
  mutate(z = scale(pkKneeExtVel)) 


### lmer

mod2 <- lmer(z_Velocity ~ z + (z|SubjectID), data = dat)
summary(mod2)


### brms

runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z_Velocity ~ z + (z|SubjectID), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

posterior <- as_draws_matrix(runmod)

prob <- sum(posterior[,'b_z'] > 0) / nrow(posterior)
prob


params <- ranef(runmod)

fixParams <- fixed.effects(runmod)
fixParams
params$SubjectID

fixInt = fixParams[1,1]
fixB = fixParams[2,1]

df_int <- as.data.frame(params$SubjectID[,,1]) %>% 
  rownames_to_column("SubjectID") %>% 
  as_tibble() %>% 
  mutate(EstimateAbs = Estimate + fixInt) %>%
  rename(Int_Est = EstimateAbs, IntEstErr = Est.Error, IntQ2_5 = Q2.5, IntQ97_5 = Q97.5) 

df_b <- as.data.frame(params$SubjectID[,,2]) %>%
  rownames_to_column('SubjectID') %>%
  mutate(EstimateAbs = Estimate + fixB) %>%
  as_tibble()%>%
  rename(b_Est = EstimateAbs, bEstErr = Est.Error, bQ2_5 = Q2.5, bQ97_5 = Q97.5)


df_pooled <- left_join(df_int, df_b, by = "SubjectID")

df_pooled <- left_join(df_pooled, dat, by = 'SubjectID')



ggplot(df_pooled) + 
  aes(x = z, y = z_Velocity, color = SubjectID) + ylab('Normalized Velocity') + xlab('Normalized Peak Knee Extension Velocity') +
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(
    aes(intercept = Int_Est, slope = b_Est, color = SubjectID, size = 1.5),
    size = 0.75
  ) + scale_colour_grey() +
  geom_point() +
  geom_jitter(width = .2)+
  #facet_wrap("SubjectNumber") +
  #facet_wrap(~ SubjectNumber + Sex)+
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
  theme(legend.position="bottom")





##### Velocity/Biomech ~ Config

dat <- dat %>% 
  
  group_by(Subject) %>%
  mutate(z = scale(Velocity)) 


### lmer

mod6 <- lmer(z ~ Config + (Config|Subject), data = dat)
summary(mod6)

### brms


runmod <- brm(data = dat, # Bayes model
              family = gaussian,
              z ~ Config + (Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

posterior <- as_draws_matrix(runmod)

prob <- sum(posterior[,'b_ConfigBOA'] > 0) / nrow(posterior)
prob


params <- ranef(runmod)

fixParams <- fixed.effects(runmod)
fixParams

params$Subject

fixInt = fixParams[1,1]
fixB = fixParams[2,1]

df_int <- as.data.frame(params$Subject[,,1]) %>% 
  rownames_to_column("Subject") %>% 
  as_tibble() %>% 
  mutate(EstimateAbs = Estimate + fixInt) %>%
  rename(Int_Est = EstimateAbs, IntEstErr = Est.Error, IntQ2_5 = Q2.5, IntQ97_5 = Q97.5) 

df_b <- as.data.frame(params$Subject[,,2]) %>%
  rownames_to_column('Subject') %>%
  mutate(EstimateAbs = Estimate + fixB) %>%
  as_tibble()%>%
  rename(b_Est = EstimateAbs, bEstErr = Est.Error, bQ2_5 = Q2.5, bQ97_5 = Q97.5)


df_pooled <- left_join(df_int, df_b, by = "Subject")

df_pooled <- left_join(df_pooled, dat, by = 'Subject')



ggplot(df_pooled) + 
  aes(x = Config, y = z, color = Subject) + ylab('Normalized Peak Negative Foot work') + xlab('Shoe') +
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(
    aes(intercept = Int_Est, slope = b_Est, color = Subject, size = 1.5),
    size = 0.75
  ) + scale_color_grey() +
  geom_point() +
  geom_jitter(width = .2)+
  #facet_wrap("SubjectNumber") +
  #facet_wrap(~ SubjectNumber + Sex)+
  #scale_colour_manual(values=c("azure4","gold")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position="bottom")



