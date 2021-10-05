rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(patchwork)

replaceName <- function(DF, toReplace, newName){
  
  # replace incorrect subject names with newname
  DF <- DF %>% 
    mutate(Subject = replace(Subject, Subject == toReplace, newName))
  return(DF)
}
replaceMove <- function(DF, toReplace, newName){
  
 #replace the movement with newName above
    DF <- DF %>% 
    mutate(Movement = replace(Movement, Movement == toReplace, newName))
  return(DF)
}

replaceConfig <- function(DF, toReplace, newName){
  
  #replace the config with newName above
  DF <- DF %>% 
    mutate(Config = replace(Config, Config == toReplace, newName))
  return(DF)
}

replaceConfiguration <- function(DF, toReplace, newName){
  
  #replace the config with newName above. Use if configuration is col name
  DF <- DF %>% 
    mutate(Configuration = replace(Configuration, Configuration == toReplace, newName))
  return(DF)
}
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
agilityDat <- read_csv('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/BigData2021/BigDataAgilityNew.csv')
#agilityDat <- read.csv(file.choose())

agilityDat <- merge(subSizes, agilityDat, by = "Subject" )
# Replace names to full names. Manual!


agilityDat <- replaceConfig(agilityDat, 'BOA', 'Tri Panel')
agilityDat <- replaceMove(agilityDat, 'skater', 'Skater')


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





# this section can be modified to change subject names but should 
### only use this for scratch work --------
hist(agilityDat$`Width/length`)

# update each time!
bigData <- read.csv('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/BigData2021/BigDataAgility_newMetrics.csv')

# load in new file to append
dat_to_append <- read.csv(file.choose())

# Replace names to full names. using functio nabove

dat_to_append <- replaceName(dat_to_append, 'Adam', 'Adam Luftglass')

dat_to_append <- dat_to_append %>% 
  mutate(Movement = replace(Movement, Movement == 'skater', 'Skater'))
dat_to_append <- dat_to_append %>% 
  mutate(Config = replace(Config, Config == 'Tri', 'Tri Panel'))

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
write.table(dat_to_append, "C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/BigData2021/BigDataAgilityNew.csv", sep=',')



# Run data ----------------------------------------------------------------

runData <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL - General/BigData2021/BigDataRun.csv')

runData <- replaceName(runData, 'Dan', 'Dan Feeney')
runData <- replaceName(runData, 'Sean', 'Sean Hopkins')
runData <- replaceName(runData, 'Ryan', 'Ryan Krol')
runData <- replaceName(runData, 'AmandaB', 'Amanda Basham')
runData <- replaceName(runData, 'Jennifer', 'Jennifer Dormann')
runData <- replaceName(runData, 'Katie', 'Katie Carbiener')
runData <- replaceName(runData, 'Jeff', 'Jeff Gay')
runData <- replaceName(runData, 'Tucker', 'Tucker Grouse')
runData <- replaceName(runData, 'AmandaK', 'Amanda Kirkby')
unique(runData$Subject)

runData <- replaceConfiguration(runData, 'BOA', 'Dual Dial Panel')
runData$VALR <- as.numeric(runData$VALR)

sumDat <- runData %>%
  group_by(Subject, ShoeCondition, Shoe) %>%
  summarize(
    avg = mean(VALR, na.rm = TRUE),
    median = median(VALR, na.rm = TRUE)
  )

runData <- merge(subSizes, runData, by = "Subject" )
runData <- subset(runData, (runData$VALR < 200) & (runData$VALR > 0) )

ggplot(runData, aes(x = VALR, color = Configuration)) + geom_density() + facet_wrap(~Subject)

### metabolic running data ###
metData <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL - General/BigData2021/MetabolicBigData.csv')


ggplot(metData, aes(x = as.factor(Configuration), y = EE, color = Configuration, fill = Configuration)) + geom_boxplot() + facet_wrap(~Subject + Shoe)


