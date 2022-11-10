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
allDat$Velocity <- as.numeric(allDat$Velocity)

allDat %>% 
  filter(kneeFlexBR <0) %>%
  ggplot(aes(x=kneeFlexBR,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Knee Flexion at Ball Release (deg)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= pkKneeExtVel ,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Knee Extension Velocity (deg/s)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadAnklePeakNegPower>-400) %>%
  ggplot(aes(x= leadAnklePeakNegPower,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Ankle Peak Negative Power", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadAnkleExtVel>-500)%>%
  ggplot(aes(x= leadAnkleExtVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Ankle Extension Velocity", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= pBF,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Braking Force (N)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= pPF,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Propulsive Force (N)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= pVGRF,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Vertical Force (N)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadKneePeakNegPower >-1000) %>%
  ggplot(aes(x= leadKneePeakNegPower,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Knee Peak Negative Power", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadHipPeakNegPower>-2000)%>%
  ggplot(aes(x= leadHipPeakNegPower,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Hip Peak Negative Power", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= leadHipExtVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Hip Extension Velocity", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= pelVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Pelvis Velocity (deg/s)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= elbowValugs,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Elbow Valgus (deg)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= elbowVarusVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Elbow Velocity", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


allDat %>%
  ggplot(aes(x= shoulderER,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Shoulder External Rotation (deg)", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= shoulderRotationVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Shoulder Rotation Velocity", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearAnklePeakPosPower,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Ankle Peak Power", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearAnkleExtVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Ankle Extension Velocity", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearKneePeakPosPower,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Knee Peak Power", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearKneeExtVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Knee Ext Vel", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearHipPeakPosPower,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Hip Peak Power", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  ggplot(aes(x= rearHipExtVel,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Hip Extension Velocity", y="Throw Velocity (mph)")+
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
  filter(footNegWork > -20000)%>%
  ggplot(aes(x= footNegWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Distal Foot Neg Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadFootEnergy < 50)%>%
  ggplot(aes(x= leadFootEnergy,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Foot Energy", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadShankEnergy < 100)%>%
  ggplot(aes(x= leadShankEnergy,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Shank Energy", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  #filter(leadThighEnergy < 50)%>%
  ggplot(aes(x= leadThighEnergy,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Thigh Energy", y="Throw Velocity (mph)")+
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
 # filter(rearLegNegWork < 150)%>%
  ggplot(aes(x= rearLegPosWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Leg Neg Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(rearAnklePosWork < 10000)%>%
  ggplot(aes(x= rearAnklePosWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Ankle Pos Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(rearKneePosWork < 10000)%>%
  ggplot(aes(x= rearKneePosWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Knee Pos Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(rearHipPosWork < 22000)%>%
  filter(rearHipPosWork > 500)%>%
  ggplot(aes(x= rearHipPosWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Hip Pos Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  # filter(rearLegNegWork < 150)%>%
  ggplot(aes(x= leadLegNegWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Rear Leg Neg Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadAnkleNegWork > -5000)%>%
  ggplot(aes(x= leadAnkleNegWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Ankle Neg Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  filter(leadKneeNegWork > -5000)%>%
  ggplot(aes(x= leadKneeNegWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Knee Neg Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

allDat %>%
  #filter(leadHipNegWork > -7500)%>%
  ggplot(aes(x= leadHipNegWork,y=Velocity)) +
  geom_point(alpha=0.5) +
  labs(x= "Lead Hip Neg Work", y="Throw Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")



