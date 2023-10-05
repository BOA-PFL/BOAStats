rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(effsize)
library(ggstatsplot)
library(emmeans)
library(lmerTest)

#-------------------------------------------------------------------------------
# The purpose of this code is to examine dial torque for different segments
#-------------------------------------------------------------------------------
DBT <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/QualitativeBigData_v2.csv')

DBT <- DBT %>%
  filter(Year == 2023)

DBT$Brand <- tolower(gsub(" ", "", DBT$Brand)) # Remove spaces/caps in subject name
  

Brand <- unique(DBT$Brand)
Segment <- c('trail','train','workwear','trail','cycle')

DBSeg <- data.frame(Brand,Segment)

DB <- left_join(DBT,DBSeg, by = 'Brand')
DB <- DB %>%
  select(c(Brand,R_DialTorque1,R_DialTorque2,L_DialTorque1,L_DialTorque2,Dial1Closure,Dial2Closure,Segment))

# Assign Number of Dials
tmpDialNo = rep('0',length(DB$Brand))
for (ii in 1:length(DB$Brand)) {
  if ((is.na(DB$R_DialTorque1[ii]) == FALSE) & (is.na(DB$R_DialTorque2[ii]) == FALSE)) {
    tmpDialNo[ii] = '2'
  } else if ((is.na(DB$R_DialTorque1[ii]) == FALSE) | (is.na(DB$R_DialTorque2[ii]) == FALSE)) {
    tmpDialNo[ii] = '1'
  }
}

DialTorque <- c(DB$R_DialTorque1,DB$R_DialTorque2,DB$L_DialTorque1,DB$L_DialTorque2)
Location <- c(DB$Dial1Closure,DB$Dial2Closure,DB$Dial1Closure,DB$Dial2Closure)
Segment <- c(DB$Segment,DB$Segment,DB$Segment,DB$Segment)
DialNo <- c(tmpDialNo,tmpDialNo,tmpDialNo,tmpDialNo)

DB2 <- data.frame(Segment,DialTorque,Location,DialNo)

DBCuff <- DB2 %>%
  filter(Location == 'Cuff')

DB2 <- DB2 %>%
  filter(DialTorque != 'NA')%>%
  filter(Location != 'Cuff')


ggplot(data = DB2, aes(x = DialTorque, color = DialNo)) + geom_histogram() + facet_wrap(~Segment) + xlab("Dial Torque (N-cm)")
