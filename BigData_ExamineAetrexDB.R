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

footAnth$ï..Loc

NAdat <- footAnth %>%
  filter(ï..Location == 'Canada' | ï..Location == 'Mexico' | ï..Location == 'Puerto Rico' | ï..Location == 'United States')

Eurodat <- footAnth %>%
  filter(ï..Location == 'Denmark' | ï..Location == 'Germany' | ï..Location == 'Netherlands' | ï..Location == 'Russia' | ï..Location == 'Spain' | ï..Location == 'United Kingdom')

Asiadat <- footAnth %>%
  filter(ï..Location == 'China' | ï..Location == 'Indonesia' | ï..Location == 'Japan' | ï..Location == 'United Arab Emirates')


ggplot(footAnth, aes(x=Avg..Length*10,y=Avg..Width,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Forefoot Width (mm)')

ggplot(NAdat, aes(x=Avg..Length*10,y=Avg..Width*10,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Forefoot Width (mm)')

ggplot(Eurodat, aes(x=Avg..Length*10,y=Avg..Width*10,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Forefoot Width (mm)')

ggplot(Asiadat, aes(x=Avg..Length*10,y=Avg..Width*10,color=Sex)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Forefoot Width (mm)')


ggplot(footAnth, aes(x=Avg..Length*10,y=Avg..DorsalHeight*10,color=Sex)) + geom_point(size=2) + 
  xlab('Foot Length (mm)') + ylab('Instep Height (mm)') + theme(text = element_text(size = 30))

ggplot(NAdat, aes(x=Avg..Length*10,y=Avg..DorsalHeight*10,color=Gender)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Dorsal Height (mm)')

ggplot(Eurodat, aes(x=Avg..Length*10,y=Avg..DorsalHeight*10,color=Gender)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Dorsal Height (mm)')

ggplot(Asiadat, aes(x=Avg..Length*10,y=Avg..DorsalHeight*10,color=Gender)) + geom_point(size=2) + xlab('Foot Length (mm)') + ylab('Dorsal Height (mm)')






