
library(tidyverse)
library(readxl)
library(gt)

#Clearing the environment
rm(list=ls())

################

#Load in Compiled Qualitative Sheet -> needs to be csv for the torque to be recognized as numeric
qualDat <- read_xlsx(file.choose())
qualDat %>%
  group_by(Config)%>%
  summarize(
    R_Torque_Prox = mean(R_DialTorque2, na.rm = TRUE), # Change to L_DialTorque1 if Single Dial
    R_Torque_Proxsd = sd(R_DialTorque2, na.rm = TRUE),# Change to L_DialTorque1 if Single Dial
    R_Torque_Dist = mean(R_DialTorque1, na.rm = TRUE), 
    R_Torque_Distsd = sd(R_DialTorque1, na.rm = TRUE),
    L_Torque_Prox = mean(L_DialTorque2, na.rm = TRUE), # Change to R_DialTorque1 if Single Dial
    L_Torque_Proxsd = sd(L_DialTorque2, na.rm = TRUE),# Change to R_DialTorque1 if Single Dial
    L_Torque_Dist = mean(L_DialTorque1, na.rm = TRUE), 
    L_Torque_Distsd = sd(L_DialTorque1, na.rm = TRUE)
  )%>%
  gt()

ggplot(qualDat, aes(x=Config, y = L_DialTorque1, color = Config, group= Subject)) + 
  geom_point(size = 4)+ 
  geom_line(aes(color=Config))+
  # facet_wrap(~Subject)+
  scale_color_manual(values=c("#000000","#00966C", "#ECE81A","#DC582A","#CAF0E4"))+ 
  theme(text = element_text(size = 26))+ 
  ylab('L Distal Dial Torque [Ncm]')+ 
  xlab('Config')+
  ggtitle('Left Foot')

ggplot(qualDat, aes(x=Config, y = L_DialTorque2, color = Config, group= Subject)) + # Change to L_DialTorque1 if Single Dial
  geom_point(size = 4)+ 
  geom_line(aes(color=Config))+
  # facet_wrap(~Subject)+
  scale_color_manual(values=c("#000000","#00966C", "#ECE81A","#DC582A","#CAF0E4"))+ 
  theme(text = element_text(size = 26))+ 
  ylab('L Proximal Dial Torque [Ncm]')+ 
  xlab('Config')+
  ggtitle('Left Foot')



ggplot(qualDat, aes(x=Config, y = R_DialTorque1, color = Config, group= Subject)) + 
  geom_point(size = 4)+ 
  geom_line(aes(color=Config))+
  # facet_wrap(~Subject)+
  scale_color_manual(values=c("#000000","#00966C", "#ECE81A","#DC582A","#CAF0E4"))+ 
  theme(text = element_text(size = 26))+ 
  ylab('R Distal Dial Torque [Ncm]')+ 
  xlab('Config')+
  ggtitle('Right Foot')


ggplot(qualDat, aes(x=Config, y = R_DialTorque2, color = Config, group= Subject)) + # Change to R_DialTorque1 if Single Dial
  geom_point(size = 4)+ 
  geom_line(aes(color=Config))+
  facet_wrap(~Subject)+
  scale_color_manual(values=c("#000000","#00966C", "#ECE81A","#DC582A","#CAF0E4"))+ 
  theme(text = element_text(size = 26))+ 
  ylab('R Proximal Dial Torque [Ncm]')+ 
  xlab('Config')+
  ggtitle('Right Foot')
  


