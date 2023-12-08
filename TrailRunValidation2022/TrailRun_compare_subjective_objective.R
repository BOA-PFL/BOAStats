# Compare qualitative analysis to heel platar pressure
library(tidyverse)
library(readxl)
library(tidybayes)
library(lme4)
library(dplyr)
library(rlang)
library(reshape2)


qualDat <- read_xlsx('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/TrailRunQual.xlsx')
qualDat$Config <- factor(qualDat$Config, c('Lace', 'PFS'))

qualDat <- qualDat %>% 
  filter(Subject != "S19")

dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/PressureOutcomes.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs'))
dat <- dat %>% filter(Subject != "S19", Label > 0)

pressdat <- dat %>%
  # filter(Label == 3) %>%
  group_by(Subject, Config) %>%
  summarize(avgHeel = mean(HeelCon)) 

pressdat$SubHeel <- qualDat$Heel
pressdat$SubOv <- qualDat$OverallFit

ggplot(pressdat, aes(x=avgHeel, y=SubHeel, col = Config, group = Subject)) + geom_point() + 
  geom_line() + ylab('Heel Subjective Score') + xlab('Average % Heel Contact')

ggplot(pressdat, aes(x=avgHeel, y=SubOv, col = Config, group = Subject)) + geom_point() + 
  geom_line() + ylab('Overall Subjective Score') + xlab('Average % Heel Contact')
  

