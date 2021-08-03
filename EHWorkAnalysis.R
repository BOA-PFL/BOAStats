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

# Single leg landings -----------------------------------------------------


landingDat <- read.csv(file.choose())
landingDat$Subject <- as.factor(landingDat$Sub)
landingDat$Config <- as.factor(landingDat$Config)
landingDat$Movement <- as.factor(landingDat$Movement)

#plotting
ggplot(data = landingDat, mapping = aes(x = Subject, y = StabTime, fill = Config)) + geom_boxplot() + 
  facet_wrap(~Movement) + ylim(0,100) + ylab('Time To Stabilize (cs)') + scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))

ggplot(data = landingDat, mapping = aes(x = Subject, y = pkForce, fill = Config)) + geom_boxplot() + 
  facet_wrap(~Movement) + ylab('Peak Force (N)') 

#moeling. Stabilizatino time. Shorter is better
sll <- subset(landingDat, landingDat$Movement == 'SLL')
testAnova('StabTime', sll)
testAnova('pkForce', sll)

sllDrop <- subset(landingDat, landingDat$Movement == 'SLLDrop')
testAnova('StabTime', sllDrop)
testAnova('pkForce', sllDrop)


# Walk forces ---------------------------------------------------------

WalkKinematics <- read_csv(file.choose())
WalkKinematics$Subject <- as.factor(WalkKinematics$Subject)
WalkKinematics$Config <- as.factor(WalkKinematics$Config)
WalkKinematics <- subset(WalkKinematics, WalkKinematics$NL > 100)
WalkKinematics <- subset(WalkKinematics, WalkKinematics$VLR < 11000)

ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = NL, fill = Config)) + geom_boxplot()
ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = VLR, fill = Config)) + geom_boxplot() +
    ylab('Vertical loading rate (N/s)') + scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))

b <- ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = PkMed, fill = Config)) + geom_boxplot() +
  ylab('Peak Medial Force (N)')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))

c <- ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = abs(PkLat), fill = Config)) + geom_boxplot() +
  ylab('Peak Lateral Force (N)')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))


d <- ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = abs(peakBrake), fill = Config)) + geom_boxplot() +
  ylab('Braking Force (N)')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))

f <- ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = abs(brakeImpulse), fill = Config)) + geom_boxplot() +
  ylab('Braking Impulse (N)') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))

(b + c) / (d + f)

testAnova('PkMed', WalkKinematics)
testAnova('PkLat', WalkKinematics)
testAnova('peakBrake', WalkKinematics)
testAnova('brakeImpulse', WalkKinematics)
testAnova('VLR', WalkKinematics)

# pressures ---------------------------------------------------------------
WalkPressure <- read.csv(file.choose())
WalkPressure$Config <- as.factor(WalkPressure$Config)
WalkPressure$Subject <- as.factor(WalkPressure$Subject)
WalkPressure <- subset(WalkPressure, WalkPressure$sdRHeel > 10) #removing false steps

p1 <- ggplot(data = WalkPressure, mapping = aes(x = Subject, y = sdRHeel, fill = Config)) + geom_boxplot() +
  ylab('Heel Pressure Variation') + scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))
testAnova('sdRHeel', WalkPressure)

p2 <- ggplot(data = WalkPressure, mapping = aes(x = Subject, y = meanRToes, fill = Config)) + geom_boxplot() +
  ylab('Mean Toe Pressure (PSI)') + scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))
testAnova('meanRToes', WalkPressure)

p1 / p2

ggplot(data = WalkPressure, mapping = aes(x = Subject, y = meanRHeel, fill = Config)) + geom_boxplot() +
  ylab('Mean Heel Pressure')
testAnova('meanRHeel', WalkPressure)

ggplot(data = WalkPressure, mapping = aes(x = Subject, y = sdRLatFF, fill = Config)) + geom_boxplot()

ggplot(data = WalkPressure, mapping = aes(x = Subject, y = sdRMedFF, fill = Config)) + geom_boxplot()

ggplot(data = WalkPressure, mapping = aes(x = Subject, y = meanRLatFF, fill = Config)) + geom_boxplot()



# qual --------------------------------------------------------------------

library(readxl)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

qualDat <- read_xlsx('C:/Users/daniel.feeney/Dropbox (Boa)/Hike Work Research/Work Pilot 2021/Qual.xlsx')

qualDat %>%
  pivot_longer(cols = Performance:Heel,
               names_to = "Location", values_to = "Rating") %>%
  group_by(Location, Config) %>%
  summarize(
    avg = mean(Rating),
    medAvg = median(Rating)
  )
  
qualDat %>%
  pivot_longer(cols = Performance:Heel,
               names_to = "Location", values_to = "Rating") %>%
  filter(Location == 'Performance') %>%
  ggplot(mapping = aes(x = Config, y = Rating, fill = Config)) + geom_boxplot() +
    theme_classic() + 
  scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))


qualDat %>%
  pivot_longer(cols = Performance:Heel,
               names_to = "Location", values_to = "Rating") %>%
  filter(Location != 'Performance') %>%
  ggplot(mapping = aes(x = Rating, fill = Config)) + geom_density() + 
  facet_wrap(~Location) +scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))


# making word clouds ------------------------------------------------------

tri <- subset(qualDat, qualDat$Config == 'Tri')
LR <- subset(qualDat, qualDat$Config == 'LR')
lace <- subset(qualDat, qualDat$Config == 'Lace')
triNotes <- tri$Comments
LRnotes <- LR$Comments
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

makeWordCloud(triNotes)
makeWordCloud(LRnotes)
makeWordCloud(laceNotes)

