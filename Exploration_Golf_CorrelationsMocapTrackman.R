# -*- coding: utf-8 -*-


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

library(hms)
library(chron)
library(lubridate)
# library needed for model 
library(performance)

rm(list=ls())# Clears the environment


DRF <- read.csv('C:/Users/milena.singletary/Boa Technology Inc/PFL Team - Documents/General/Testing Segments/PowerPerformance/2023/PP_Golf_NewVariables_PFLMech_ June23/Overground/DFW/0_CompiledDrive_DFW_final.csv')
# compiled OG file must display t0 as character of "h:mm:ss.0" ex. "10:10:54.9" formatting can be changed in the CSV by going to number -> category:custom  -> type: mm:ss.0
# if improper formatting error will be thrown with as.times()
CompiledOG <- read.csv('C:/Users/milena.singletary/Boa Technology Inc/PFL Team - Documents/General/Testing Segments/PowerPerformance/2023/PP_Golf_NewVariables_PFLMech_ June23/Overground/Drive/0_CompiledOG_Drive_latest.csv')
CompiledTrk <- read.csv('C:/Users/milena.singletary/Boa Technology Inc/PFL Team - Documents/General/Testing Segments/PowerPerformance/2023/PP_Golf_NewVariables_PFLMech_ June23/0_CompiledTrackmanData.csv')


MoCapEntries <- list.files('C:/Users/milena.singletary/Boa Technology Inc/PFL Team - Documents/General/Testing Segments/PowerPerformance/2023/PP_Golf_NewVariables_PFLMech_ June23/Overground/Drive/Drive_old', pattern = '\\.txt$')
TManEntries <- list.files('C:/Users/milena.singletary/Boa Technology Inc/PFL Team - Documents/General/Testing Segments/PowerPerformance/2023/PP_Golf_NewVariables_PFLMech_ June23/Trackman/', '.csv')

DRF <- DRF%>%
  filter(Subject !=  'TaraMoen')

CompiledTrk <- CompiledTrk %>%
  filter(Club == 'Driver')%>%
  mutate(Subject = gsub(" ", "", Subject))%>%
  filter(Subject != "TaraMoen")
  
CompiledOG <- CompiledOG %>%
  mutate(Subject = if_else( Subject == "JoshFaulkenberry", "JoshuaFaulkenberry", Subject))%>%
  filter(Subject != "TaraMoen")


# create new columns
CompiledOG$SubjectT <- NA
CompiledOG$SubjectT <-  NA
CompiledOG$ConfigT <-  NA
CompiledOG$ShotTime<-  NA
CompiledOG$BallSpeed <-   NA
CompiledOG$ClubSpeed <-  NA
CompiledOG$CarryFlatLength <-  NA
CompiledOG$Curve <-  NA
CompiledOG$EstTotalFlat <-  NA
CompiledOG$LaunchDirection <-  NA
CompiledOG$Side <-  NA


# loop to match shots
for (ii in 1:nrow(CompiledTrk)){
  subT <- CompiledTrk$Subject[ii]
  configT <- CompiledTrk$Config[ii]
  matchT <- CompiledTrk$ShotTime[ii]
  
  for (jj in 1:nrow(CompiledOG)){
    subOG <- CompiledOG$Subject[jj]
    configOG <- CompiledOG$Config[jj]
    TimeT <- CompiledOG$t3[jj]
    # TimeT1 <- as.times(TimeT) + 3.472222e-05     # int as 3 seconds represented as day: 3/(60*60*24)
    # TimeT2 <- as.times(TimeT) - 3.472222e-05
    # TimeT1 <- as.times(TimeT) + 5.787037e-05     # int as 5 seconds represented as day: 5/(60*60*24)
    # TimeT2 <- as.times(TimeT) - 5.787037e-05
    TimeT1 <- as.times(TimeT) + 8.101852e-05     # int as 7 seconds represented as day: 7/(60*60*24)
    TimeT2 <- as.times(TimeT) - 8.101852e-05
    
    
    if (subOG == subT & configOG == configT & matchT <= TimeT1 & matchT >= TimeT2){
      CompiledOG$SubjectT[jj] <- subT
      CompiledOG$ConfigT[jj] <- configT
      CompiledOG$ShotTime[jj] <- TimeT
      CompiledOG$BallSpeed[jj] <- CompiledTrk$BallSpeed[ii]
      CompiledOG$ClubSpeed[jj] <- CompiledTrk$ClubSpeed[ii]
      CompiledOG$CarryFlatLength[jj] <- CompiledTrk$CarryFlatLength[ii]
      CompiledOG$Curve[jj] <- CompiledTrk$Curve[ii]
      CompiledOG$EstTotalFlat[jj] <- CompiledTrk$`Est..Total.Flat...Length`[ii]
      CompiledOG$LaunchDirection[jj] <- CompiledTrk$LaunchDirection[ii]
      CompiledOG$Side[jj] <- CompiledTrk$Side[ii]
      #print(paste(ii, jj, 'match'))
    } else { rm(subOG,configOG,TimeT, TimeT1, TimeT2)}
    
  } 
}




check1 <- CompiledOG %>%
  select(Subject, SubjectT, Config, ConfigT, t0, t3, ShotTime )
check1 <- check1 %>%
  filter(ShotTime != 'NA')


# create new columns for DRF df
DRF$SubjectT <- NA
DRF$SubjectT <-  NA
DRF$ConfigT <-  NA
DRF$ShotTime<-  NA
DRF$BallSpeed <-   NA
DRF$ClubSpeed <-  NA
DRF$CarryFlatLength <-  NA
DRF$Curve <-  NA
DRF$EstTotalFlat <-  NA
DRF$LaunchDirection <-  NA
DRF$Side <-  NA



# loop to match shots w DRF df
for (ii in 1:nrow(CompiledTrk)){
  subT <- CompiledTrk$Subject[ii]
  configT <- CompiledTrk$Config[ii]
  matchT <- CompiledTrk$ShotTime[ii]
  
  for (jj in 1:nrow(DRF)){
    subDRF <- DRF$Subject[jj]
    configDRF <- DRF$Config[jj]
    TimeDRF <- DRF$t3[jj]
    # TimeT1 <- as.times(TimeDRF) + 3.472222e-05     # int as 3 seconds represented as day: 3/(60*60*24)
    # TimeT2 <- as.times(TimeDRF) - 3.472222e-05
    # TimeT1 <- as.times(TimeDRF) + 5.787037e-05     # int as 5 seconds represented as day: 5/(60*60*24)
    # TimeT2 <- as.times(TimeDRF) - 5.787037e-05
    TimeT1 <- as.times(TimeDRF) + 8.101852e-05     # int as 7 seconds represented as day: 7/(60*60*24)
    TimeT2 <- as.times(TimeDRF) - 8.101852e-05
    
    
    if (subDRF == subT & configDRF == configT & matchT <= TimeT1 & matchT >= TimeT2){
      DRF$SubjectT[jj] <- subT
      DRF$ConfigT[jj] <- configT
      DRF$ShotTime[jj] <- TimeDRF
      DRF$BallSpeed[jj] <- CompiledTrk$BallSpeed[ii]
      DRF$ClubSpeed[jj] <- CompiledTrk$ClubSpeed[ii]
      DRF$CarryFlatLength[jj] <- CompiledTrk$CarryFlatLength[ii]
      DRF$Curve[jj] <- CompiledTrk$Curve[ii]
      DRF$EstTotalFlat[jj] <- CompiledTrk$`Est..Total.Flat...Length`[ii]
      DRF$LaunchDirection[jj] <- CompiledTrk$LaunchDirection[ii]
      DRF$Side[jj] <- CompiledTrk$Side[ii]
      
    } 
    
  } 
}




check2 <- DRF %>%
  select(Subject, SubjectT, Config, ConfigT, t0, t3, ShotTime )

check2 <- check2 %>%
  filter(ShotTime != 'NA')








# Look at correlations between velo and biomech
# VGRF DS
ggplot(data = CompiledOG, aes(x = peakGRFZ_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 
CompiledOG %>%
  filter(ShotTime != 'NA')%>%
  ggplot(aes(x= peakGRFZ_lead_Downswing ,y=ClubSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Vertical GRF: Downswing (N)", y="ClubSpeed Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

ggplot(CompiledOG, aes(peakGRFZ_lead_Downswing, ClubSpeed, colour = Subject)) + geom_point()+geom_smooth(se = FALSE, method = lm)


#GRF_mod = lmer('ClubSpeed ~ peakGRFZ_lead_Downswing + (1|Subject)', data = CompiledOG, REML = TRUE, na.action = "na.omit") #with random intercept per sub
GRF_mod = lmer('ClubSpeed ~ peakGRFZ_lead_Downswing + (peakGRFZ_lead_Downswing|Subject)', data = CompiledOG, REML = TRUE, na.action = "na.omit") #with ran slope& intercept per sub
summary(GRF_mod)
coef(GRF_mod)
r2(GRF_mod)



# pelvis 

ggplot(data = DRF, aes(x = pkPelvisVel_DS, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 
pelv <- DRF %>%
  group_by(Subject) %>%
  mutate(z_score = scale(pkPelvisVel_DS)) %>% 
  group_by(Config)

pelv<- subset(pelv, pelv$z_score < 2) #removing outliers
pelv<- subset(pelv, pelv$z_score > -2)
  
pelv %>%
  filter(ShotTime != 'NA')%>%
  ggplot(aes(x= pkPelvisVel_DS ,y=ClubSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Pelvis Velcity: Downswing (N)", y="ClubSpeed Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

ggplot(pelv, aes(pkPelvisVel_DS, ClubSpeed, colour = Subject)) + geom_point()+geom_smooth(se = FALSE, method = lm)

pelv_mod = lmer('ClubSpeed ~ pkPelvisVel_DS + (1|Subject)', data = pelv, REML = TRUE, na.action = "na.omit")
summary(pelv_mod)
coef(pelv_mod)
r2(pelv_mod)


# Hip Power DS
ggplot(data = CompiledOG, aes(x = peakHipPower_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 
hip <- CompiledOG %>%
  group_by(Subject) %>%
  mutate(z_score = scale(peakHipPower_lead_Downswing)) %>% 
  group_by(Config)

hip<- subset(hip, hip$z_score < 2) #removing outliers
hip<- subset(hip, hip$z_score > -2)

ggplot(data = hip, aes(x = peakHipPower_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 


hip %>%
  filter(ShotTime != 'NA' & Subject != 'SteveBerzon')%>%
  ggplot(aes(x= peakHipPower_lead_Downswing ,y=ClubSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Hip Power: Downswing (W)", y="ClubSpeed Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")

ggplot(hip, aes(peakHipPower_lead_Downswing, ClubSpeed, colour = Subject)) + geom_point()+geom_smooth(se = FALSE, method = lm)

hip_mod = lmer('ClubSpeed ~ peakHipPower_lead_Downswing + (1|Subject)', data = hip, REML = TRUE, na.action = "na.omit")
summary(hip_mod)
coef(hip_mod)
r2(hip_mod)







# Knee Ext V DS

ggplot(data = CompiledOG, aes(x = KneeExtensionVelo_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 
kneeExt<-CompiledOG %>%
  group_by(Subject) %>%
  mutate(z_score = scale(KneeExtensionVelo_lead_Downswing)) %>% 
  group_by(Config)%>%
  filter(KneeExtensionVelo_lead_Downswing > 5)

kneeExt<- subset(kneeExt, kneeExt$z_score < 2) #removing outliers
kneeExt<- subset(kneeExt, kneeExt$z_score > -2)
ggplot(data = kneeExt, aes(x = KneeExtensionVelo_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(kneeExt, aes(KneeExtensionVelo_lead_Downswing, ClubSpeed, colour = Subject)) + geom_point()+geom_smooth(se = FALSE, method = lm)

kneeExt %>%
  filter(ShotTime != 'NA')%>%
  filter(KneeExtensionVelo_lead_Downswing < 1000)%>%
  ggplot(aes(x= KneeExtensionVelo_lead_Downswing ,y=ClubSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Knee Extenstion Velocity: Downswing (deg/sec)", y="ClubSpeed Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


kneeX_mod = lmer('ClubSpeed ~ KneeExtensionVelo_lead_Downswing + (1|Subject)', data = kneeExt, REML = TRUE, na.action = "na.omit")
summary(kneeX_mod)
coef(kneeX_mod)
r2(kneeX_mod)



# Knee ROM DS
ggplot(data = CompiledOG, aes(x = KneeROMSag_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 
kneeROM<-CompiledOG %>%
  group_by(Subject) %>%
  mutate(z_score = scale(KneeROMSag_lead_Downswing)) %>% 
  group_by(Config)%>%
  filter(KneeROMSag_lead_Downswing < 150)

kneeROM<- subset(kneeROM, kneeROM$z_score < 2) #removing outliers
kneeROM<- subset(kneeROM, kneeROM$z_score > -2)
ggplot(data = kneeROM, aes(x = KneeROMSag_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(kneeROM, aes(KneeROMSag_lead_Downswing, ClubSpeed, colour = Subject)) + geom_point()+geom_smooth(se = FALSE, method = lm)

kneeROM %>%
  filter(ShotTime != 'NA' & Subject != 'KadeBracken')%>%
  #filter(KneeROMSag_lead_Downswing < 1000)%>%
  ggplot(aes(x= KneeROMSag_lead_Downswing ,y=ClubSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Knee ROM: Downswing (deg)", y="ClubSpeed Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


kneeROM_mod = lmer('ClubSpeed ~ KneeROMSag_lead_Downswing + (1|Subject)', data = kneeROM, REML = TRUE, na.action = "na.omit")
summary(kneeROM_mod)
coef(kneeROM_mod)
r2(kneeROM_mod)



# Ank Ev V DS
ggplot(data = CompiledOG, aes(x = peakAnkEvVelo_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 

AnkEv <- CompiledOG%>%
    group_by(Subject) %>%
    mutate(z_score = scale(peakAnkEvVelo_lead_Downswing)) %>% 
    group_by(Config)%>%
    filter(peakAnkEvVelo_lead_Downswing < 450 &  peakAnkEvVelo_lead_Downswing > 0)

AnkEv<- subset(AnkEv, AnkEv$z_score < 2) #removing outliers
AnkEv<- subset(AnkEv, AnkEv$z_score > -2)
ggplot(data = AnkEv, aes(x = peakAnkEvVelo_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(AnkEv, aes(peakAnkEvVelo_lead_Downswing, ClubSpeed, colour = Subject)) + geom_point(aes(shape = Config))+geom_smooth(se = FALSE, method = lm)

AnkEv %>%
  filter(ShotTime != 'NA')%>%
  filter(peakAnkEvVelo_lead_Downswing < 500)%>%
  ggplot(aes(x= peakAnkEvVelo_lead_Downswing ,y=ClubSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Ankle Ev.Velocity: Downswing (deg/sec)", y="ClubSpeed Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


AnkEv_mod = lmer('ClubSpeed ~ peakAnkEvVelo_lead_Downswing + (1|Subject)', data = AnkEv, REML = TRUE, na.action = "na.omit")
summary(AnkEv_mod)
coef(AnkEv_mod)
r2(AnkEv_mod)


# Ank ROM DS
ggplot(data = CompiledOG, aes(x = AnkROMFrontal_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject) 
ankROM<-CompiledOG %>%
  group_by(Subject) %>%
  mutate(z_score = scale(AnkROMFrontal_lead_Downswing)) %>% 
  group_by(Config)%>%
  filter(AnkROMFrontal_lead_Downswing < 30)

ankROM<- subset(ankROM, ankROM$z_score < 2) #removing outliers
ankROM<- subset(ankROM, ankROM$z_score > -2)
ggplot(data = ankROM, aes(x = AnkROMFrontal_lead_Downswing, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(ankROM, aes(AnkROMFrontal_lead_Downswing, ClubSpeed, colour = Subject)) + geom_point()+geom_smooth(se = FALSE, method = lm)

ankROM %>%
  filter(ShotTime != 'NA')%>%
  ggplot(aes(x= AnkROMFrontal_lead_Downswing ,y=ClubSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Ankle Frontal ROM: Downswing (deg)", y="ClubSpeed Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


ankROM_mod = lmer('ClubSpeed ~ AnkROMFrontal_lead_Downswing + (1|Subject)', data = ankROM, REML = TRUE, na.action = "na.omit")
summary(ankROM_mod)
coef(ankROM_mod)
r2(ankROM_mod)





# Neg DRF Pwr DS

ggplot(data = DRFp, aes(x = peakNegLeadDistalRFpower_DS, fill = Config)) + geom_histogram() + facet_wrap(~Subject)

DRFp <- DRF%>%
  group_by(Subject) %>%
  mutate(z_score = scale(peakNegLeadDistalRFpower_DS)) %>% 
  group_by(Config)%>%
  filter(peakNegLeadDistalRFpower_DS > -400)

DRFp<- subset(DRFp, DRFp$z_score < 2) #removing outliers
DRFp<- subset(DRFp, DRFp$z_score > -2)
ggplot(data = DRFp, aes(x = peakNegLeadDistalRFpower_DS, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(DRFp, aes(peakNegLeadDistalRFpower_DS, ClubSpeed, colour = Subject)) + geom_point(aes(shape = Config))+geom_smooth(se = FALSE, method = lm)


DRFp %>%
  ggplot(aes(x= peakNegLeadDistalRFpower_DS ,y=ClubSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Neg DRF Power: Downswing (W)", y="ClubSpeed Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")


DRFds_mod = lmer('ClubSpeed ~ peakNegLeadDistalRFpower_DS + (1|Subject)', data = DRFp, REML = TRUE, na.action = "na.omit")
summary(DRFds_mod)
coef(DRFds_mod)
r2(DRFds_mod)


# Neg DRF Pwr Fullswing
DRFf <- DRF%>%
  group_by(Subject) %>%
  mutate(z_score = scale(peakNegLeadDistalRFpower_full)) %>% 
  group_by(Config)%>%
  filter(peakNegLeadDistalRFpower_full > -300)

DRFf<- subset(DRFf, DRFf$z_score < 2) #removing outliers
DRFf<- subset(DRFf, DRFf$z_score > -2)

ggplot(data = DRFf, aes(x = peakNegLeadDistalRFpower_full, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(DRFf, aes(peakNegLeadDistalRFpower_full, ClubSpeed, colour = Subject)) + geom_point(aes(shape = Config))+geom_smooth(se = FALSE, method = lm)

  
DRFf %>%
  ggplot(aes(x= peakNegLeadDistalRFpower_full ,y=ClubSpeed)) +
  geom_point(alpha=0.5) +
  labs(x= "Peak Neg DRF Power: Full swing (W)", y="ClubSpeed Velocity (mph)")+
  geom_smooth(method=lm) + 
  stat_cor(method = "pearson")



DRFfull = lmer('ClubSpeed ~ peakNegLeadDistalRFpower_full + (1|Subject)', data = DRFf, REML = TRUE, na.action = "na.omit")
summary(DRFfull)
coef(DRFfull)
r2(DRFfull)




