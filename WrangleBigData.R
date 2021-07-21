rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(patchwork)


# -------------------------------------------------------------------------

subSizes <- read_xlsx('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/BigData2021/MasterSubjectSizes.xlsx')
rightDat <- subset(subSizes, subSizes$Side == 'R')
leftDat <- subset(subSizes, subSizes$Side == 'L')

a <- hist(rightDat$`Length (cm)`)
a <- ggplot(data = rightDat, mapping = aes(x = `Length (cm)`)) + geom_histogram() 
b <- ggplot(data = rightDat, mapping = aes(x = as.numeric(`Shoe Sizes (Reported)`))) + geom_histogram() +
  xlab('Reported Shoe Size (US)')
c <- ggplot(data = rightDat, mapping = aes(x = `Width (cm)`)) + geom_histogram()
d <- ggplot(data = rightDat, mapping = aes(x = `Girth(cm)`)) + geom_histogram()
(a | b)/(c|d)

hist(rightDat$`Width/length`)


# Agility -----------------------------------------------------------------
agilityDat <- read_csv('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/BigData2021/BigDataAgility_newMetrics.csv')
#agilityDat <- read.csv(file.choose())

agilityDat <- merge(subSizes, agilityDat, by = "Subject" )
# Replace names to full names. Manual!
agilityDat <- agilityDat %>% 
  mutate(Movement = replace(Movement, Movement == 'skater', 'Skater'))
agilityDat <- agilityDat %>% 
  mutate(Config = replace(Config, Config == 'Tri', 'Tri Panel'))



agilityDat['CTNorm'] <- (agilityDat$CT / agilityDat$impulse) * 100
agilityDat <- subset(agilityDat, agilityDat$CTNorm > 0)
agilityDat <- subset(agilityDat, agilityDat$CTNorm < 0.5)

ggplot(data = agilityDat, mapping = aes(x = CTNorm, y = COPtraj, col = Config, group = Config)) + geom_point() +
  facet_wrap(~Movement + Subject)

ggplot(data = agilityDat, mapping = aes(x = CTNorm, y = copExc, col = Config, group = Config)) + geom_point() +
  facet_wrap(~Movement + Subject)

full.mod = lmer(CTNorm ~ Config + (1|Model) + (Config|Subject), data = agilityDat, REML = TRUE, na.action = "na.omit" )
summary(full.mod)

##
ggplot(data = agilityDat, mapping = aes(x = CTNorm, fill = Config)) + geom_density(alpha=.5) +theme_classic() +
  facet_wrap(~Movement + Subject)





# this section can be modified to change subject names but should  --------
hist(agilityDat$`Width/length`)

# update each time!
bigData <- read_xlsx(file.choose())

# load in new file to append
dat_to_append <- read.csv(file.choose())

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


