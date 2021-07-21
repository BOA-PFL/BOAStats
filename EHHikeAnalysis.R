# EH HIke analusis 
rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(emmeans)
library(patchwork)


# generic function --------------------------------------------------------

testAnova <- function(metric, df) {
  
  myformula <- as.formula(paste0(metric," ~ Config", " + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (1|Subject)"))
  
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit" )
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "Lace") 
  
  
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "Lace"))
  return(newList)
  
}


plotAndStoreFacet <- function(col, dfName){
  
  genPlot <- ggplot(data = dfName, mapping = aes(x = Subject, y = abs(.data[[col]]), fill = Config)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4")) + facet_wrap(~Level)
  
  return(genPlot)
}
# walk forces -------------------------------------------------------------

WalkKinematics <- read_csv('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/HikePilot_2021/Hike Pilot 2021/Data/WalkForceComb.csv')
WalkKinematics$Subject <- as.factor(WalkKinematics$Subject)
WalkKinematics$Config <- as.factor(WalkKinematics$Config)
WalkKinematics <- subset(WalkKinematics, WalkKinematics$VLR < 11000)

plotAndStoreFacet('VLR',WalkKinematics)

d <- plotAndStoreFacet('peakBrake',WalkKinematics)

f <- plotAndStoreFacet('brakeImpulse', WalkKinematics)
d/f

upHill <- subset(WalkKinematics, WalkKinematics$Level == 'UH')
downHill <- subset(WalkKinematics, WalkKinematics$Level == 'DH')


testAnova('peakBrake', downHill)
testAnova('brakeImpulse', downHill)
testAnova('VLR', downHill)

testAnova('peakBrake', upHill)
testAnova('brakeImpulse', upHill)
testAnova('VLR', upHill)


# Kinetics --------------------------------------------------------------

dat <- read.csv('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/HikePilot_2021/Hike Pilot 2021/Data/KinematicsKinetics.csv')
plotAndStore <- function(col, dfName){
  
  genPlot <- ggplot(data = dfName, mapping = aes(x = Subject, y = .data[[col]], fill = Config)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))
  
  return(genPlot)
}
dat <- subset(dat, dat$AnkleInvROM < 20)
plotAndStore('AnkleNegWork',dat)
plotAndStore('AnklePosWork',dat)
plotAndStore('AnkleInvROM',dat)
plotAndStore('AnkleAbdROM',dat)

testAnova('AnkleAbdROM', dat)
testAnova('AnkleInvROM',dat)

plotAndStore('pkKneeRot',dat)
testAnova('pkKneeRot',dat)


# Single leg landings -----------------------------------------------------


landingDat <- read.csv('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/HikePilot_2021/Hike Pilot 2021/SLLForces.csv')
landingDat$Subject <- as.factor(landingDat$Sub)
landingDat$Config <- as.factor(landingDat$Config)
landingDat$Movement <- as.factor(landingDat$Movement)

#plotting
ggplot(data = landingDat, mapping = aes(x = Subject, y = StabTime, fill = Config)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~Movement) + ylim(0,100) + ylab('Time To Stabilize (cs)') + scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))

ggplot(data = landingDat, mapping = aes(x = Subject, y = pkForce, fill = Config)) + geom_boxplot() + 
  facet_wrap(~Movement) + ylab('Peak Force (N)') + scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))

#moeling. Stabilizatino time. Shorter is better
sll <- subset(landingDat, landingDat$Movement == 'SLL')
testAnova('StabTime', sll)
testAnova('pkForce', sll)

sllDrop <- subset(landingDat, landingDat$Movement == 'SLLDrop')
testAnova('StabTime', sllDrop)
testAnova('pkForce', sllDrop)


# making word clouds ------------------------------------------------------

library(readxl)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

qualDat <- read_xlsx('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/HikePilot_2021/Hike Pilot 2021/Data/Qual.xlsx')

qualDat %>%
  pivot_longer(cols = Performance:Heel,
               names_to = "Location", values_to = "Rating") %>%
  group_by(Location, Config) %>%
  summarize(
    avg = mean(Rating),
    medAvg = median(Rating)
  )

qualDat %>%
  group_by(Config) %>%
  summarize(
    avgPerf = mean(Performance),
    medPerf = median(Performance)
  )
  
  
qualDat %>%
  pivot_longer(cols = Performance:Heel,
               names_to = "Location", values_to = "Rating") %>%
  filter(Location == 'Performance') %>%
  ggplot(mapping = aes(x = Config, y = Rating, fill = Config)) + geom_boxplot() +
  theme_classic() + 
  scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))


qualDat %>%
  pivot_longer(cols = Performance:Ankle,
               names_to = "Location", values_to = "Rating") %>%
  filter(Location != 'Performance') %>%
  ggplot(mapping = aes(x = Rating, fill = Config)) + geom_density() + 
  facet_wrap(~Location) +scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))

ggplot(data = qualDat, mapping = aes(x = as.factor(Config), y = Performance, col = Subject, group = Subject)) + geom_point(size = 4) + 
  geom_line() + xlab('Configuration') + theme(text = element_text(size = 20)) + ylab('Overall Rating of Shoe') +
  scale_y_continuous(limits=c(1,10))



DD <- subset(qualDat, qualDat$Config == 'DD')
SD <- subset(qualDat, qualDat$Config == 'SD')
lace <- subset(qualDat, qualDat$Config == 'Lace')
DDNotes <- DD$Comments
SDnotes <- SD$Comments
laceNotes <- lace$Comments


makeWordCloud <- function(inputText) {
  
  docs <- Corpus(VectorSource(inputText))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation)
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("like", "feel","feels","lace","bottom","steel","replacement","toe.","toe",
                                      "felt","tri","blah")) 
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=25, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}

makeWordCloud(DDNotes)
makeWordCloud(SDnotes)
makeWordCloud(laceNotes)


# ratings related to foot size? -------------------------------------------


subSizes <- read_xlsx('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/BigData2021/MasterSubjectSizes.xlsx')
combDat <- merge(subSizes, qualDat, by = "Subject" )
combDat <- subset(combDat, combDat$Side == 'R')

ggplot(data = combDat, mapping = aes(x = `TotalArea (cm^2)`, y = Performance , col = Config, group = Config)) + geom_point()
ggplot(data = combDat, mapping = aes(x = `Length (cm)`, y = Performance , col = Config, group = Config)) + geom_point(size = 3)

ggplot(data = combDat, mapping = aes(x = `Width (cm)`, y = Performance , col = Config, group = Config)) + geom_point(size = 3)
ggplot(data = combDat, mapping = aes(x = `Instep (cm)`, y = Performance , col = Config, group = Config)) + geom_point(size = 3)
ggplot(data = combDat, mapping = aes(x = `ArchHt (cm)`, y = Performance , col = Config, group = Config)) + geom_point(size = 3)

ggplot(data = combDat, mapping = aes(y = `Length (cm)`, x = `Width (cm)`, color = Subject)) + geom_point(size = 3)


# perf and sizes ----------------------------------------------------------

# Replace names to full names. Manual!
WalkKinematics <- WalkKinematics %>% 
  mutate(Subject = replace(Subject, Subject == 'Alex', 'Alex Browne'))

WalkKinematics <- WalkKinematics %>% 
  mutate(Subject = replace(Subject, Subject == 'Bryan', 'Bryan Banning'))

WalkKinematics <- WalkKinematics %>% 
  mutate(Subject = replace(Subject, Subject == 'Damiel', 'Damien Halverson'))

WalkKinematics <- WalkKinematics %>% 
  mutate(Subject = replace(Subject, Subject == 'Josh', 'Josh Simpson'))

WalkKinematics <- WalkKinematics %>% 
  mutate(Subject = replace(Subject, Subject == 'Kevin', 'Kevin Love'))

WalkKinematics <- WalkKinematics %>% 
  mutate(Subject = replace(Subject, Subject == 'Ryan', 'Ryan Truxal'))

WalkKinematics <- WalkKinematics %>% 
  mutate(Subject = replace(Subject, Subject == 'Ted', 'Ted Bloom'))

fullDat <- merge(combDat, WalkKinematics)

ggplot(data = fullDat, mapping = aes(x =  `Length (cm)`, y = abs(peakBrake), col = Config, group = Config)) + geom_point(position = 'jitter') +
  facet_wrap(~ Level) + 
  scale_colour_manual(values=c("#000000", "#ECE81A", "#CAF0E4")) + theme_classic() + ylab('Peak Braking Force (N)') +
  xlab('Length (cm)')

fullDat <- subset(fullDat, fullDat$VLR < 30000)
ggplot(data = fullDat, mapping = aes(x =  `Length (cm)`, y = VLR, col = Config, fill = Config)) + geom_point(position = 'jitter') +
  facet_wrap(~ Level) + 
  scale_colour_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))+ theme_classic() + ylab('Vertical loading rate (N/s)') +
  xlab('Length (cm)')
