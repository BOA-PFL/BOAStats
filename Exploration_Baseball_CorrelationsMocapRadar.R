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

allDat %>%
  ggplot(aes(x= pkKneeExtVel ,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Knee Extension Velocity (deg/s)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= pelVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Pelvis Velocity (deg/s)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= rearKneeExtVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Knee Ext Vel", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

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

allDat %>%
  ggplot(aes(x= leadDistalRFnegWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Distal RF Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")




