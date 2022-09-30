rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(patchwork)
library(effsize)

## This code is meant to organize multiple CSVs into their respective BigData segment sections 
# cycling test ------------------------------------------------------------
# 9/29/22 update to show architecture of V1 database 

rm(list=ls())
biomDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/CyclingPowerDB_V2.csv')
biomDat <- biomDat %>%
  rename('Subject' = ï..Subject)

visits <- read_xlsx('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/MasterSubjectVisits.xlsx')
shoes <- read_xlsx('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/ShoesTestedDB_V2.xlsx')

# combine shoe data and athlete visit data
combDat <- left_join(biomDat, visits,
          by = c("Subject", "Brand", "Year","Month", "Model"))

combDat <- left_join(combDat, shoes, 
                     by = c("Brand","Model", "Year", "Month", "Config"))

# combine with athlete foot size
# Loading in the data frame and organizing left and right sides into date frames
subSizes <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/MasterSubjectSizes.csv')
subSizes$Sex <- as.factor(subSizes$Sex)
subSizes$Subject <- gsub(" ", "", subSizes$Subject) # remove spaces in names

rightDat <- subset(subSizes, subSizes$Side == 'R')
leftDat <- subset(subSizes, subSizes$Side == 'L')

totalDat <- left_join(combDat, rightDat,
          by = "Subject")






# old work ----------------------------------------------------------------


# This section is replacing incorrect or inconsistent names, to keep things consistent in the respective data sets

replaceName <- function(DF, toReplace, newName){ 
  
  # replace incorrect subject names with new name
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
# Subject foot sizes & shapes

# Loading in the data frame and organizing left and right sides into date frames
subSizes <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/MasterSubjectSizes.csv')
subSizes$Sex <- as.factor(subSizes$Sex)
rightDat <- subset(subSizes, subSizes$Side == 'R')
leftDat <- subset(subSizes, subSizes$Side == 'L')

# Density Plotting metrics: Length, Instep, Width, Girth by Sex 
a <- ggplot(data = rightDat, mapping = aes(x = `Length (cm)`, fill = as.factor(Sex), color = as.factor(Sex) )) + 
  geom_histogram(alpha=0.5, position="identity") + scale_fill_manual(values=c("#DC582A", "#00966C")) + 
  theme(legend.position = "none")+ scale_color_manual(values=c("#DC582A", "#00966C"))
  
b <- ggplot(data = rightDat, mapping = aes(x = `Instep (cm)`, color = Sex, fill = Sex)) + 
  geom_histogram(alpha=0.5, position="identity")  + scale_color_manual(values=c("#DC582A", "#00966C"))  +
  theme(legend.position = "none")+ scale_fill_manual(values=c("#DC582A", "#00966C"))

c <- ggplot(data = rightDat, mapping = aes(x = `Width (cm)`, color = Sex, fill = Sex)) + 
  geom_histogram(alpha=0.5, position="identity") + scale_color_manual(values=c("#DC582A", "#00966C")) +
  scale_fill_manual(values=c("#DC582A", "#00966C"))

d <- ggplot(data = rightDat, mapping = aes(x = `Girth(cm)`, color = Sex, fill = Sex)) + 
  geom_histogram(alpha=0.5, position="identity")  + scale_color_manual(values=c("#DC582A", "#00966C")) +
  theme(legend.position = "none")+ scale_fill_manual(values=c("#DC582A", "#00966C"))
(a | b)/(c|d)
 

# Summarizing R side data averages by sex within the data frame
rightDat %>%
  group_by(Sex)%>%
  summarize(
    avgLen = mean(`Length (cm)`),
    sdLen = sd(`Length (cm)`),
    avgInstep = mean(`Instep (cm)`),
    sdInstep = sd(`Instep (cm)`),
    avgWidth = mean(`Width (cm)`),
    sdWidth = sd(`Width (cm)`),
    avgGirth = mean(`Girth(cm)`),
    sdGirth = sd(`Girth(cm)`)
  )

# Calculating standardized mean difference - measuring effect size 
cohen.d(`Length (cm)` ~ Sex, data = rightDat, paired = FALSE)
cohen.d(`Instep (cm)` ~ Sex, data = rightDat, paired = FALSE)
cohen.d(`Width (cm)` ~ Sex, data = rightDat, paired = FALSE)
cohen.d(`Girth(cm)` ~ Sex, data = rightDat, paired = FALSE)

#  Plotting pressures and calculating the effect size 

f <- ggplot(data = rightDat, mapping = aes(x = `TotalArea (cm^2)`, fill = as.factor(Sex), color = as.factor(Sex) )) + 
  geom_histogram(alpha=0.5, position="identity") + scale_fill_manual(values=c("#DC582A", "#00966C")) + 
  theme(legend.position = "none")+ scale_color_manual(values=c("#DC582A", "#00966C"))
f
cohen.d(`TotalArea (cm^2)` ~ Sex, data = rightDat, paired = FALSE)


# Agility -----------------------------------------------------------------
#agilityDat <- read_csv('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/BigData2021/BigDataAgilityNew.csv')

agilityDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/BigDataAgilityNew.csv')


#need to make sure brand, month, year, config in agility dat matches master shoes tested!!

agilityDat <- merge(subSizes, agilityDat, by = "Subject" )


#Update the config names that were tested
# Replace names to full names. Manual!
agilityDat <- replaceConfig(agilityDat, 'BOA', 'Tri Panel')
agilityDat <- replaceMove(agilityDat, 'skater', 'Skater')
agilityDat <- replaceConfig(agilityDat, 'OPtri', 'OPTri')


# Normalizing contact time
agilityDat['CTNorm'] <- (agilityDat$CT / agilityDat$impulse) * 100
agilityDat <- subset(agilityDat, agilityDat$CTNorm > 0)
agilityDat <- subset(agilityDat, agilityDat$CTNorm < 0.5)
agilityDat <- subset(agilityDat, agilityDat$COPtraj > 0.05) 
agilityDat <- subset(agilityDat, (agilityDat$Movement == 'CMJ' | agilityDat$Movement == 'Skater') )


# cop excursion and CTNorm plotting
ggplot(data = agilityDat, mapping = aes(x = COPtraj, y = CTNorm, col = Subject, fill = Subject)) + geom_point(size = 2) +
  facet_wrap(~Movement) + geom_smooth(method=lm, aes(color = Subject))
ggplot(data = agilityDat, mapping = aes(x = COPtraj, y = CT, col = Subject, fill = Subject)) + geom_point(size = 2) +
  facet_wrap(~Movement) + geom_smooth(method=lm, aes(color = Subject))

ggplot(data = agilityDat, mapping = aes(x = CT, y = copExc, col = Config, group = Config)) + geom_point() +
  facet_wrap(~Movement + Subject)

#Subsetting Skater jump data
#Contact time during skater jump 
#Omitting NANs 
#Correlation between CT and COP Trajectory
skate <- subset(agilityDat, agilityDat$Movement == 'Skater')
skate.mod = lmer(CTNorm ~ Config + (1|Model) + (Config|Subject), data = skate, REML = TRUE, na.action = "na.omit" )
summary(skate.mod)
cor.test(skate$CT, skate$COPtraj)

#Subsetting CMJ data
#Contact time during CMJ 
#Omitting NANs 
#Correlation between CT and COP Trajectory
cmj <- subset(agilityDat, agilityDat$Movement == 'CMJ')
cmj.mod = lmer(CTNorm ~ Config + (1|Model) + (Config|Subject), data = cmj, REML = TRUE, na.action = "na.omit" )
summary(cmj.mod)
cor.test(cmj$CT, cmj$COPtraj)

## Plotting CT during the movements
ggplot(data = agilityDat, mapping = aes(x = CTNorm, fill = Config)) + geom_density(alpha=.5) +theme_classic() +
  facet_wrap(~Movement + Subject)


# add in shoe data --------------------------------------------------------

shoes <- read_xlsx('C:/Users/daniel.feeney/Boa Technology Inc/PFL - General/BigData/ShoeTested.xlsx')
agilityDat$Configuration <- agilityDat$Config
agilityShoes <- merge(agilityDat, shoes)
skaterShoes <- subset(agilityShoes, agilityShoes$Movement == 'Skater')

skateMod <- lmer(CTNorm ~  Config  + HeelCounter +(Config|Subject), data = skaterShoes, REML = TRUE, na.action = "na.omit")
summary(skateMod)
levels(as.factor(skaterShoes$Model))

g <- ggplot(data = skaterShoes, aes(x = CT, y = impulse, color = Configuration)) + 
  geom_point()

h <- ggplot(data = skaterShoes, aes(x = CT, y = abs(RFD), color = Configuration)) + 
  geom_point()
g/h



# Endurance & Health ------------------------------------------------------

endDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL - General/BigData/BigDataRun.csv')
endDat$VALR <- as.numeric(endDat$VALR)
endDat <- subset(endDat, endDat$VALR > 10)

ggplot(data = endDat, aes(x = VALR, y = VILR, color = Configuration)) + geom_point() +
  facet_wrap(~Subject)

ggplot(data = endDat, aes(x = abs(pBF), y = VILR, color = Shoe)) + geom_point() +
  facet_wrap(~Subject)

endShoes <- merge(endDat, shoes)

### TODO: Add qualitative and speeds for subjects



# Power & Precisoin -------------------------------------------------------

pwrDat <- read_xlsx('C:/Users/daniel.feeney/BOA Technology Inc/PFL - General/BigData2021/BigData_TrackMan.xlsx')

ggplot(data = pwrDat, aes(x = `Launch Direction`, y = Distance, color = Config)) +
  geom_point() + facet_wrap(~Subject)

ggplot(data = pwrDat, aes(x = `Launch Direction`, y = Distance, color = Config)) +
  geom_point() + facet_wrap(~Subject)
# archived section for yak shaving ----------------------------------------




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



