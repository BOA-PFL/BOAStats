rm(list=ls())
library(tidyverse)
library(readxl)

# Look at data ------------------------------------------------------------


dd <- read.csv("C:/Users/Daniel.Feeney/Dropbox (Boa)/Boa Team Folder/BigData2021/BigDataRun2.csv")

unique(dd$Subject)
summary(dd$VALR)
unique(dd$Brand)

dd%>%
  group_by(Configuration) %>%
  summarize(avgVALR = mean(VALR))

# Agility -----------------------------------------------------------------
library(readxl)
agilityDat <- read_xlsx('C:/Users/Daniel.Feeney/Dropbox (Boa)/Boa Team Folder/BigData2021/BigDataAgility.xlsx')
unique(agilityDat$Subject)

max(agilityDat$ContactTime)
min(agilityDat$ContactTime)
hist(agilityDat$jumpHeight)
length( subset( agilityDat, agilityDat$jumpHeight > 1) )
agilityDat %>%
  group_by(Movement, Configuration) %>%
  summarize(ContactTime = mean(ContactTime))
# this section can be modified to change subject names but should  --------


# update each time!
bigData <- read_xlsx('C:/Users/Daniel.Feeney/Dropbox (Boa)/Boa Team Folder/BigData2021/BigDataRun2.csv')

# load in new file to append
dat_to_append <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/EndurancePerformance/TNF_Scrambler_Apr_21/TM_data/CompiledRunData.csv')

# Replace names to full names. Manual!
dat_to_append <- dat_to_append %>% 
  mutate(Subject = replace(Subject, Subject == 'Ando', 'Matt Anderson'))

dat_to_append <- dat_to_append %>% 
  mutate(Subject = replace(Subject, Subject == 'Braden', 'Braden Forsyth'))

dat_to_append <- dat_to_append %>% 
  mutate(Subject = replace(Subject, Subject == 'Ian', 'Ian Anderson'))

dat_to_append <- dat_to_append %>% 
  mutate(Subject = replace(Subject, Subject == 'Matt', 'Matt Dietrich'))

dat_to_append <- dat_to_append %>% 
  mutate(Subject = replace(Subject, Subject == 'Sidney', 'Sidney Foster'))

dat_to_append <- dat_to_append %>% 
  mutate(Subject = replace(Subject, Subject == 'Greg', 'Greg Orticelle'))

# Add brand, year, month, shoe name
shoeName <- rep('Scrambler', dim(dat_to_append)[1])
Brand <- rep('TNF', dim(dat_to_append)[1])
config <- rep('Overlapping Panel', dim(dat_to_append)[1])
yr <- rep('2021', dim(dat_to_append)[1])
month <- rep('April', dim(dat_to_append)[1])
Benefit <- rep('Endurance & Health', dim(dat_to_append)[1])
Segment <- rep('Trail', dim(dat_to_append)[1])

dat_to_append$Configuration <- config
dat_to_append$Benefit <- Benefit
dat_to_append$Segment <- Segment
dat_to_append$Shoe <- shoeName
dat_to_append$Brand <- Brand
dat_to_append$Month <- month
dat_to_append$Year <- yr

newDat <- rbind(bigData, dat_to_append)
# write output
write.table(newDat, "C:/Users/Daniel.Feeney/Dropbox (Boa)/Boa Team Folder/BigData2021/BigDataRun2.csv", sep=',')


