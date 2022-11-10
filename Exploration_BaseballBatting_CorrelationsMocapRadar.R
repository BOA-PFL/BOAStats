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
  
  
  
  tmpStart = seconds_to_period(mocapDat$radarWindowStart[i])
  tmpStart = sprintf("%02d:%02d", minute(tmpStart), second(tmpStart))
  tmpStart = paste('00:', sep = "", tmpStart)
  radarStart[i] = times(tmpStart)
  
  tmpEnd = seconds_to_period(mocapDat$radarWindowEnd[i])
  tmpEnd = sprintf("%02d:%02d", minute(tmpEnd), second(tmpEnd))
  tmpEnd = paste('00:', sep = "", tmpEnd)
  radarEnd[i] = times(tmpEnd)
  
  
}

mocapDat['radarSearchStart'] = mocapDat$t0 + radarStart

mocapDat['radarSearchEnd'] = mocapDat$t0 + radarEnd


# Join mocap and radar data by subject, Config, and radarDat$radarTime between mocapDat$radarSearchStart and mocapDat$radarSearchEnd


allDat <-  full_join(radarDat, mocapDat, by=c("Subject", 'Config'))

allDat <- filter(allDat, radarTime >= radarSearchStart, radarTime <= radarSearchEnd )


# Look at correlations between velo and biomech
allDat$Velocity <- as.numeric(allDat$exitSpeed)


allDat %>%
  ggplot(aes(x= pkKneeExtVel ,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Knee Extension Velocity (deg/s)", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadAnklePeakNegPower>-150) %>%
  ggplot(aes(x= leadAnklePeakNegPower,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Ankle Peak Negative Power", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadAnkleExtVel>-500)%>%
  ggplot(aes(x= leadAnkleExtVel,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Ankle Extension Velocity", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= pBF,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Braking Force (N)", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= pPF,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Propulsive Force (N)", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= pVGRF,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Vertical Force (N)", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadKneePeakNegPower >-450) %>%
  ggplot(aes(x= leadKneePeakNegPower,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Knee Peak Negative Power", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadHipPeakNegPower>-2000)%>%
  ggplot(aes(x= leadHipPeakNegPower,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Hip Peak Negative Power", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= leadHipExtVel,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Hip Extension Velocity", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= pelVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Pelvis Velocity (deg/s)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearAnklePeakPosPower,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Ankle Peak Power", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearAnkleExtVel,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Ankle Extension Velocity", y="ExitSpeed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearKneePeakPosPower,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Knee Peak Power", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearKneeExtVel,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Knee Ext Vel", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearHipPeakPosPower,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Hip Peak Power", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearHipExtVel,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Hip Extension Velocity", y="Exit Speed (mph)")+
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
  ggplot(aes(x= pkKneeExtVel ,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Knee Extension Velocity (deg/s)", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadAnklePeakNegPower>-150) %>%
  ggplot(aes(x= leadAnklePeakNegPower,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Ankle Peak Negative Power", y="distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadAnkleExtVel>-500)%>%
  ggplot(aes(x= leadAnkleExtVel,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Ankle Extension Velocity", y="Distance (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= pBF,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Braking Force (N)", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= pPF,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Propulsive Force (N)", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= pVGRF,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Vertical Force (N)", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadKneePeakNegPower >-450) %>%
  ggplot(aes(x= leadKneePeakNegPower,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Knee Peak Negative Power", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadHipPeakNegPower>-2000)%>%
  ggplot(aes(x= leadHipPeakNegPower,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Hip Peak Negative Power", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= leadHipExtVel,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Hip Extension Velocity", y="Distance (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= pelVel,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Pelvis Velocity (deg/s)", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearAnklePeakPosPower,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Ankle Peak Power", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearAnkleExtVel,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Ankle Extension Velocity", y="Distance")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearKneePeakPosPower,y=distance)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Knee Peak Power", y="Distance)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearKneeExtVel,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Knee Ext Vel", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearHipPeakPosPower,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Hip Peak Power", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearHipExtVel,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Hip Extension Velocity", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(kneeExtROM>0)%>%
  ggplot(aes(x= kneeExtROM,y=exitSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "KneeExtROM (deg)", y="Exit Speed (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

