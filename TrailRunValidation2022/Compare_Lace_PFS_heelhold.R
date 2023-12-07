library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

################################################################################
# Functions
replaceName <- function(DF, toReplace, newName){ 
  
  # replace incorrect subject names with new name
  DF <- DF %>% 
    mutate(Subject = replace(Subject, Subject == toReplace, newName))
  return(DF)
}
################################################################################

dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/OutdoorData/PressureOutcomes.csv')

subnames <- read_excel('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/MasterListOutdoor.xlsx')

# Subsitute the subject numbers with subject names
subnames <- subnames %>%
  select(c('Subject Number','Name','Size','Sex')) %>%
  rename('Subject' = 'Subject Number') %>%
  filter(Subject != 'NA')

dat <- left_join(dat,subnames, by = 'Subject')

dat <- dat %>%
  select(-c('Subject')) %>%
  rename('Subject' = 'Name')

dat$Subject <- gsub(" ", "", dat$Subject) # Remove spaces

dat$Config <- factor(dat$Config, c('lace', 'pfs'))
dat <- dat %>% 
  filter(Subject != "S19", Label > 0)


lacedat <- dat %>%
  filter(Config == 'lace') %>%
  group_by(Subject,Label) %>%
  summarize(lace_avgHeelCon = mean(HeelCon))

pfsdat <- dat %>%
  filter(Config == 'pfs') %>%
  group_by(Subject,Label) %>%
  summarize(pfs_HeelCon = mean(HeelCon))

avgdat <- left_join(lacedat,pfsdat,by = c('Subject','Label'))
avgdat$diffHeelCon <- avgdat$pfs_HeelCon-avgdat$lace_avgHeelCon

# Read in the foot scan data
subSizes <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/FootScan Data/MasterSubjectSizes_March2023.csv')
subSizes$Subject <- gsub(" ", "", subSizes$Subject) # Remove spaces
# Only examine right foot data
subSizes <- subSizes%>%
  filter(Side == 'R') %>%
  select(c('Subject','Sex','Length','Width','Instep','Girth','HeelWidth'))

subSizes <- replaceName(subSizes, 'DanFeeney', 'DanielFeeney')
subSizes <- replaceName(subSizes, 'AlexBrown', 'AlexBrowne')

testdat <- subnames %>%
  select(-c('Subject')) %>%
  rename('Subject' = 'Name')
testdat$Subject <- gsub(" ", "", testdat$Subject) # Remove spaces

testdat <- left_join(testdat,subSizes, by = 'Subject')

testdat <- left_join(testdat,avgdat, by = 'Subject')

testdat <- testdat %>%
  filter(Length != 'NA', Size == 11)

################################################################################
# Plotting
ggplot(data = testdat, mapping = aes(x = Instep, y = diffHeelCon, color = Label)) + geom_point() +
  ylab('PFS Heel contact - Lace Heel Contact (%)') + xlab('Instep Height (cm)')

ggplot(data = testdat, mapping = aes(x = HeelWidth, y = diffHeelCon, color = Label)) + geom_point() +
  ylab('PFS Heel contact - Lace Heel Contact (%)') + xlab('Heel Width (cm)')

ggplot(data = testdat, mapping = aes(x = Girth, y = diffHeelCon, color = Label)) + geom_point() +
  ylab('PFS Heel contact - Lace Heel Contact (%)') + xlab('Foot Girth (cm)')

ggplot(data = testdat, mapping = aes(x = Width, y = diffHeelCon, color = Label)) + geom_point() +
  ylab('PFS Heel contact - Lace Heel Contact (%)') + xlab('Forefoot Width (cm)')
