
library(tidyverse)
library(readxl)
library(gt)

#Clearing the environment
rm(list=ls())

################

#Load in Compiled Qualitative Sheet -> needs to be csv for the torque to be recognized as numeric
qualDat <- read_xlsx('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/Testing Segments/AgilityPerformanceData/AS_Trail_HeelLockAgility_Perf_Apr23/Qual_AS_Trail_HeelLockAgility_Perf_Apr23.xlsx')
qualDat %>%
  group_by(Config)%>%
  summarize(
    R_Torque_Prox = mean(R_DialTorque1, na.rm = TRUE),
    R_Torque_Dist = mean(R_DialTorque2, na.rm = TRUE),
    L_Torque_Prox = mean(L_DialTorque1, na.rm = TRUE),
    L_Torque_Dist = mean(L_DialTorque2, na.rm = TRUE)
  )%>%
  gt()

ggplot(qualDat, aes(x=Config, y = L_DialTorque1, color = Config, group= Subject)) + 
  geom_point(size = 4)+ 
  geom_line(aes(color=Config))+
  # facet_wrap(~Subject)+
  scale_color_manual(values=c("#000000","#00966C", "#ECE81A","#DC582A","#CAF0E4"))+ 
  theme(text = element_text(size = 16))+ 
  ylab('Proximal (instep) Dial - Torque [N-cm]')+ 
  xlab('Config')+
  ggtitle('Left Foot')



  


