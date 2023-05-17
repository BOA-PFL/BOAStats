rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(patchwork)
library(effsize)

# Examine foot metrics computed from the foot scans
footmetrics <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Data/Aetrex Object Files/python_files/SummaryMetrics.csv')

# Reorganize the data:
footmetrics$Side <- 'NA'
for (ii in 1:nrow(footmetrics)){
  if (grepl('left_',footmetrics$Name_Side[ii],ignore.case=TRUE)){
    footmetrics$Side[ii] <- 'L'
    footmetrics$Subject[ii] <- substr(footmetrics$Name_Side[ii],1,str_length(footmetrics$Name_Side[ii])-6)
  }
  if (grepl('right_',footmetrics$Name_Side[ii],ignore.case=TRUE)) {
    footmetrics$Side[ii] = 'R'
    footmetrics$Subject[ii] <- substr(footmetrics$Name_Side[ii],1,str_length(footmetrics$Name_Side[ii])-7)
  }
}

footmetrics <- footmetrics %>% select(-c(Name_Side))

# Open the Aetrex Master Scan
aetrexdat <- read_xlsx('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/FootScan Data/MasterSubjectSizes.xlsx')

# Just grab the columns of interest
aetrexdat <- aetrexdat %>%
  select(c(Subject,Side,Sex))

# Merge the databases
test <- left_join(footmetrics, aetrexdat, by = c('Subject'))

test <- test %>%
  filter(Sex != 'NA')

ggplot(test, aes(x=FootLength,y=MTP1,color=Sex)) + geom_point(size=2) + 
  xlab('Foot Length (m)') + ylab('Location of MTP1 (m)') + theme(text = element_text(size = 30))

ggplot(test, aes(x=FootLength*1000,y=MTP1/FootLength*100,color=Sex)) + geom_point(size=2) + 
  xlab('Foot Length (mm)') + ylab('Location of MTP1 (%)') + theme(text = element_text(size = 30))

ggplot(test, aes(x=MTP1/FootLength*100,color=Sex)) + geom_histogram() + xlab('Location of MTP1 (% Foot Length)')                  
