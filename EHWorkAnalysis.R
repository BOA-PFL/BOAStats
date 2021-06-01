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


landingDat <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/Hike Work Research/Work Pilot 2021/SLLForces.csv')
landingDat$Subject <- as.factor(landingDat$Sub)
landingDat$Config <- as.factor(landingDat$Config)
landingDat$Movement <- as.factor(landingDat$Movement)

#plotting
ggplot(data = landingDat, mapping = aes(x = Subject, y = StabTime, fill = Config)) + geom_boxplot() + 
  facet_wrap(~Movement) + ylim(0,100) + ylab('Time To Stabilize (cs)')

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

WalkKinematics <- read_csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/Hike Work Research/Work Pilot 2021/WalkForceComb.csv')
WalkKinematics$Subject <- as.factor(WalkKinematics$Subject)
WalkKinematics$Config <- as.factor(WalkKinematics$Config)
WalkKinematics <- subset(WalkKinematics, WalkKinematics$NL > 100)
WalkKinematics <- subset(WalkKinematics, WalkKinematics$VLR < 11000)

ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = NL, fill = Config)) + geom_boxplot()
ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = VLR, fill = Config)) + geom_boxplot() +
  ylab('Vertical loading rate (N/s)')

b <- ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = PkMed, fill = Config)) + geom_boxplot() +
  ylab('Peak Medial Force (N)')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

c <- ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = abs(PkLat), fill = Config)) + geom_boxplot() +
  ylab('Peak Lateral Force (N)')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

d <- ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = abs(peakBrake), fill = Config)) + geom_boxplot() +
  ylab('Braking Force (N)')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

f <- ggplot(data = WalkKinematics, mapping = aes(x = Subject, y = abs(brakeImpulse), fill = Config)) + geom_boxplot() +
  ylab('Braking Impulse (N)') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

(b + c) / (d + f)

testAnova('PkMed', WalkKinematics)
testAnova('PkLat', WalkKinematics)
testAnova('peakBrake', WalkKinematics)
testAnova('brakeImpulse', WalkKinematics)
testAnova('VLR', WalkKinematics)

# pressures ---------------------------------------------------------------
WalkPressure <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/Hike Work Research/Work Pilot 2021/pressuresComb.csv')
WalkPressure$Config <- as.factor(WalkPressure$Config)
WalkPressure$Subject <- as.factor(WalkPressure$Subject)
WalkPressure <- subset(WalkPressure, WalkPressure$sdRHeel > 10) #removing false steps

p1 <- ggplot(data = WalkPressure, mapping = aes(x = Subject, y = sdRHeel, fill = Config)) + geom_boxplot() +
  ylab('Heel Pressure Variation')
testAnova('sdRHeel', WalkPressure)

p2 <- ggplot(data = WalkPressure, mapping = aes(x = Subject, y = meanRToes, fill = Config)) + geom_boxplot() +
  ylab('Mean Toe Pressure (PSI)')
testAnova('meanRToes', WalkPressure)

p1 / p2

ggplot(data = WalkPressure, mapping = aes(x = Subject, y = meanRHeel, fill = Config)) + geom_boxplot() +
  ylab('Mean Heel Pressure')
testAnova('meanRHeel', WalkPressure)

ggplot(data = WalkPressure, mapping = aes(x = Subject, y = sdRLatFF, fill = Config)) + geom_boxplot()

ggplot(data = WalkPressure, mapping = aes(x = Subject, y = sdRMedFF, fill = Config)) + geom_boxplot()

ggplot(data = WalkPressure, mapping = aes(x = Subject, y = meanRLatFF, fill = Config)) + geom_boxplot()


