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
radarStart <- times(c())
radarEnd <- times(c())

for (i in 1:nrow(mocapDat)){
  
  #i = 1
  
  tmpStart = seconds_to_period(mocapDat$batEndTime[i])
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
allDat$Velocity <- as.numeric(allDat$exitSpeed)

allDat$distance <- as.numeric(allDat$distance)


allDat %>%
  ggplot(aes(x= pkKneeExtVel ,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Knee Extension Velocity (deg/s)", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= pkKneeExtVel ,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Knee Extension Velocity (deg/s)", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= pelVel,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Pelvis Velocity (deg/s)", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= pelVel,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Pelvis Velocity (deg/s)", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")




allDat %>%
  filter(kneeExtROM>0)%>%
  ggplot(aes(x= kneeExtROM,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "KneeExtROM (deg)", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  filter(kneeExtROM>0)%>%
  ggplot(aes(x= kneeExtROM,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "KneeExtROM (deg)", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= leadDistalRFnegWork ,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Distal RF Neg Work (J)", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= leadDistalRFnegWork ,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Distal RF Neg Work (J)", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= peakNegLeadDistalRFpower ,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Distal RF Peak Neg Power (W)", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= peakNegLeadDistalRFpower ,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Distal RF Peak Neg Power (W)", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")





