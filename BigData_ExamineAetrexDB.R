rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(patchwork)
library(effsize)

#-------------------------------------------------------------------------------
# The purpose of this code is to understand the differences between male and 
# female foot anthroprometrics
#-------------------------------------------------------------------------------
# Loading in the data frame and organizing left and right sides into date frames
footAnth <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/FootScan Data/March23_export.csv')
colnames(footAnth)[which(names(footAnth) == 'Gender')] <- 'Sex'

footAnth <- footAnth %>% rename('Location' = 'ï..Location')

NAdat <- footAnth %>%
  filter(Location == 'Canada' | Location == 'Mexico' | Location == 'Puerto Rico' | Location == 'United States')

Eurodat <- footAnth %>%
  filter(Location == 'Denmark' | Location == 'Germany' | Location == 'Netherlands' | Location == 'Russia' | Location == 'Spain' | Location == 'United Kingdom')

Asiadat <- footAnth %>%
  filter(Location == 'China' | Location == 'Indonesia' | Location == 'Japan' | Location == 'United Arab Emirates')


ggplot(footAnth, aes(x=Avg..Length*10,y=Avg..Width,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Forefoot Width (mm)')

ggplot(NAdat, aes(x=Avg..Length*10,y=Avg..Width*10,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Forefoot Width (mm)')

ggplot(Eurodat, aes(x=Avg..Length*10,y=Avg..Width*10,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Forefoot Width (mm)')

ggplot(Asiadat, aes(x=Avg..Length*10,y=Avg..Width*10,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Forefoot Width (mm)')


ggplot(footAnth, aes(x=Avg..Length*10,y=Avg..DorsalHeight*10,color=Sex)) + geom_point(size=2) + 
  xlab('Foot Length (mm)') + ylab('Instep Height (mm)') + theme(text = element_text(size = 30))

ggplot(NAdat, aes(x=Avg..Length*10,y=Avg..DorsalHeight*10,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Dorsal Height (mm)')

ggplot(Eurodat, aes(x=Avg..Length*10,y=Avg..DorsalHeight*10,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Dorsal Height (mm)')

ggplot(Asiadat, aes(x=Avg..Length*10,y=Avg..DorsalHeight*10,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Dorsal Height (mm)')






