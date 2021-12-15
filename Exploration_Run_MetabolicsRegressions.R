rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(emmeans)
library(patchwork)
library(lmerTest)
library(ggpubr)

# functions

makeHist <- function(col, dfName){
  
  genPlot <- ggplot(data = dfName, mapping = aes(x = .data[[col]], color = Config)) +
    geom_density() + facet_wrap(~Subject) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_color_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))
  
  return(genPlot)
}


testRandSlopes <- function(metric, df) {
  
  myformula <- as.formula(paste0(metric," ~ Config", " + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (Config|Subject)"))
  
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit" )
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "SL") 
  
  
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "SL"))
  return(newList)
  
}


# load and clean data

kinDat <- read.csv(file.choose())

kinDat <- subset(kinDat, kinDat$AnkleNegWork < -75)
kinDat <- subset(kinDat, kinDat$AnkleNegWork > -250)
kinDat <- subset(kinDat, kinDat$AnklePosWork < 700)
kinDat <- subset(kinDat, kinDat$AnklePosWork > 100)
kinDat <- subset(kinDat, kinDat$HipAbdROM <25)
kinDat <- subset(kinDat, kinDat$pkAnkleMomY <500)
kinDat <- subset(kinDat, kinDat$minAnkleMomZ > -250)
kinDat <- subset(kinDat, kinDat$MinKneeMomY > -250)
kinDat <- subset(kinDat, kinDat$pkHipMomY < 500)
kinDat <- subset(kinDat, kinDat$pkHipMomZ < 300)


kinDat <- subset(kinDat, kinDat$Subject != 'S26')
kinDat <- subset(kinDat, kinDat$Subject != 'S29')

#makeHist("AnkleNegWork", kinDat)
#makeHist("AnklePosWork", kinDat)
#makeHist("KneeNegWork", kinDat)
#makeHist("KneePosWork", kinDat)
#makeHist("hipNegWork", kinDat)
#makeHist("hipPosWork", kinDat)
#makeHist("HipAbdROM", kinDat)
#makeHist("hipFlexROM", kinDat)
#makeHist("kneeZROM", kinDat)
#makeHist("kneeFlexROM", kinDat)
#makeHist("AnkleInvROM", kinDat)
#makeHist("AnkleFlexROM", kinDat)
#makeHist("pkAnkleMomY", kinDat)
#makeHist("minAnkleMomZ", kinDat)
#makeHist("MinKneeMomY", kinDat)
#makeHist("pkKneeMomZ", kinDat)
#makeHist("pkHipMomY", kinDat)
#makeHist("pkHipMomZ", kinDat)

metDat <- read.csv(file.choose())

subDat <- read_excel(file.choose())

forceDat <- read.csv(file.choose())

makeHist("CT", forceDat)

forceDat <- subset(forceDat, forceDat$CT > 175)



# average data for within subject / shoe condition combo

subjects <- unique(kinDat$Subject)
shoes <- unique(kinDat$Config)
AnkleNegWorkMean <- matrix(0, length(subjects)*length(shoes))
AnklePosWorkMean <- matrix(0, length(subjects)*length(shoes))
KneeNegWorkMean <- matrix(0, length(subjects)*length(shoes))
KneePosWorkMean <- matrix(0, length(subjects)*length(shoes))
HipNegWorkMean <- matrix(0, length(subjects)*length(shoes))
HipPosWorkMean <- matrix(0, length(subjects)*length(shoes))
HipAbdROMMean <- matrix(0, length(subjects)*length(shoes))
HipFlexROMMean <- matrix(0, length(subjects)*length(shoes))
KneeAbdROMMean <- matrix(0, length(subjects)*length(shoes))
KneeFlexROMMean <- matrix(0, length(subjects)*length(shoes))
AnkleInvROMMean <- matrix(0, length(subjects)*length(shoes))
AnkleFlexROMMean <- matrix(0, length(subjects)*length(shoes))
ankleMomXMean<- matrix(0, length(subjects)*length(shoes))
ankleMomYMean<- matrix(0, length(subjects)*length(shoes))
kneeMomXMean<- matrix(0, length(subjects)*length(shoes))
kneeMomYMean<- matrix(0, length(subjects)*length(shoes))
hipMomXMean<- matrix(0, length(subjects)*length(shoes))
hipMomYMean<- matrix(0, length(subjects)*length(shoes))

Sub <- rep(NA, length(subjects)*length(shoes))
Config <- rep(NA, length(subjects)*length(shoes))
r = 1

for (sub in subjects) {
  
  tmp_sub <- subset(kinDat, kinDat$Subject == sub)
  
  for (s in shoes) {
    tmp_shoe <- subset(tmp_sub, tmp_sub$Config == s)
    Sub[r] <- sub
    Config[r] <- s
    AnkleNegWorkMean[r] <- mean(tmp_shoe$AnkleNegWork, na.rm = TRUE ) 
    AnklePosWorkMean[r] <- mean(tmp_shoe$AnklePosWork, na.rm = TRUE )
    KneeNegWorkMean[r] <- mean(tmp_shoe$KneeNegWork, na.rm = TRUE ) 
    KneePosWorkMean[r] <- mean(tmp_shoe$KneePosWork, na.rm = TRUE )
    HipNegWorkMean[r] <- mean(tmp_shoe$hipNegWork, na.rm = TRUE ) 
    HipPosWorkMean[r] <- mean(tmp_shoe$hipPosWork, na.rm = TRUE )
    HipAbdROMMean[r] <- mean(tmp_shoe$HipAbdROM, na.rm = TRUE )
    HipFlexROMMean[r] <- mean(tmp_shoe$hipFlexROM, na.rm = TRUE )
    KneeAbdROMMean[r] <- mean(tmp_shoe$kneeZROM, na.rm = TRUE )
    KneeFlexROMMean[r] <- mean(tmp_shoe$kneeFlexROM, na.rm = TRUE )
    AnkleInvROMMean[r] <- mean(tmp_shoe$AnkleInvROM, na.rm = TRUE )
    AnkleFlexROMMean[r] <- mean(tmp_shoe$AnkleFlexROM, na.rm = TRUE )
    ankleMomXMean[r]<- mean(tmp_shoe$pkAnkleMomY, na.rm = TRUE )
    ankleMomYMean[r]<- (mean(tmp_shoe$minAnkleMomZ, na.rm = TRUE ))*-1
    kneeMomXMean[r]<- (mean(tmp_shoe$MinKneeMomY, na.rm = TRUE ))*-1
    kneeMomYMean[r]<- mean(tmp_shoe$pkKneeMomZ, na.rm = TRUE )
    hipMomXMean[r]<- mean(tmp_shoe$pkHipMomY, na.rm = TRUE )
    hipMomYMean[r]<- mean(tmp_shoe$pkHipMomZ, na.rm = TRUE )
    
    r = r+1
    
  }
}

meanKinDat <- cbind(AnkleNegWorkMean, AnklePosWorkMean, KneeNegWorkMean, KneePosWorkMean, HipNegWorkMean, HipPosWorkMean, 
                 HipAbdROMMean, HipFlexROMMean, KneeAbdROMMean, KneeFlexROMMean, AnkleInvROMMean, AnkleFlexROMMean, 
                 ankleMomXMean, ankleMomYMean, kneeMomXMean, kneeMomYMean, hipMomXMean, hipMomYMean)

meanKinDat <- as.data.frame(meanKinDat)

meanKinDat <- cbind(Sub, Config, meanKinDat)

colnames(meanKinDat) <- c('Subject', 'Config', 'AnkleNegWorkMean', 'AnklePosWorkMean', 'KneeNegWorkMean', 'KneePosWorkMean', 'HipNegWorkMean', 'HipPosWorkMean', 
                       'HipAbdROMMean', 'HipFlexROMMean', 'KneeAbdROMMean', 'KneeFlexROMMean', 'AnkleInvROMMean', 'AnkleFlexROMMean', 
                       'ankleMomXMean', 'ankleMomYMean', 'kneeMomXMean', 'kneeMomYMean', 'hipMomXMean', 'hipMomYMean')

######## averaging force data

subjects <- unique(forceDat$Subject)
shoes <- unique(forceDat$Config)
pBFmean <- matrix(0, length(subjects)*length(shoes))
brakeImpulseMean <- matrix(0, length(subjects)*length(shoes))
VALRmean <- matrix(0, length(subjects)*length(shoes))
pMFmean <- matrix(0, length(subjects)*length(shoes))
pLFmean <- matrix(0, length(subjects)*length(shoes))
CTmean <- matrix(0, length(subjects)*length(shoes))
pVGRFmean <- matrix(0, length(subjects)*length(shoes))


Sub <- rep(NA, length(subjects)*length(shoes))
Config <- rep(NA, length(subjects)*length(shoes))
r = 1

for (sub in subjects) {
  
  tmp_sub <- subset(forceDat, forceDat$Subject == sub)
  
  for (s in shoes) {
    tmp_shoe <- subset(tmp_sub, tmp_sub$Config == s)
    Sub[r] <- sub
    Config[r] <- s
    pBFmean[r] <- mean(tmp_shoe$pBF, na.rm = TRUE ) 
    brakeImpulseMean[r] <- mean(tmp_shoe$brakeImpulse, na.rm = TRUE )
    VALRmean[r] <- mean(tmp_shoe$VALR, na.rm = TRUE ) 
    pMFmean[r] <- mean(tmp_shoe$pMF, na.rm = TRUE )
    pLFmean[r] <- mean(tmp_shoe$pLF, na.rm = TRUE ) 
    CTmean[r] <- mean(tmp_shoe$CT, na.rm = TRUE )
    pVGRFmean[r] <- mean(tmp_shoe$pVGRF, na.rm = TRUE )
    
    r = r+1
    
  }
}

meanForceDat <- cbind(pBFmean, brakeImpulseMean, VALRmean, pMFmean, pLFmean, CTmean, pVGRFmean)

meanForceDat <- as.data.frame(meanForceDat)

meanForceDat <- cbind(Sub, Config, meanForceDat)

colnames(meanForceDat) <- c('Subject', 'Config', 'pBFMean', 'brakeImpulseMean', 'VALRmean', 'pMFmean', 'pLFmean', 'CTmean', 'pVGRFmean')


######### average metabolic data 

subjects <- unique(metDat$Subject)
shoes <- unique(metDat$Config)
EEmean <- matrix(0, length(subjects)*length(shoes))


Sub <- rep(NA, length(subjects)*length(shoes))
Config <- rep(NA, length(subjects)*length(shoes))
r = 1

for (sub in subjects) {
  
  tmp_sub <- subset(metDat, metDat$Subject == sub)
  
  for (s in shoes) {
    tmp_shoe <- subset(tmp_sub, tmp_sub$Config == s)
    Sub[r] <- sub
    Config[r] <- s
    EEmean[r] <- mean(tmp_shoe$EE, na.rm = TRUE ) 
    
    
    r = r+1
    
  }
}



meanMetDat <- as.data.frame(EEmean)

meanMetDat <- cbind(Sub, Config, meanMetDat)

colnames(meanMetDat) <- c('Subject', 'Config', 'EE')




# join metabolic and biomechanical data

allDat <- inner_join (meanKinDat, meanMetDat, by = c('Subject', 'Config'))

allDat <- inner_join (allDat, meanForceDat, by = c('Subject', 'Config'))

allDat <- inner_join(allDat, subDat, by = 'Subject')

EE_dist <- allDat$EE * allDat$Speed_minPerKm
EE_dist <- as.data.frame(EE_dist)

allDat <- cbind(allDat, EE_dist)

### normalize to mass
allDat$EE_dist <- allDat$EE_dist / allDat$Mass
allDat$AnkleNegWorkMean <- allDat$AnkleNegWorkMean / allDat$Mass
allDat$AnklePosWorkMean <- allDat$AnklePosWorkMean / allDat$Mass
allDat$KneeNegWorkMean <- allDat$KneeNegWorkMean / allDat$Mass
allDat$KneePosWorkMean <- allDat$KneePosWorkMean / allDat$Mass
allDat$HipNegWorkMean <- allDat$HipNegWorkMean / allDat$Mass
allDat$HipPosWorkMean <- allDat$HipPosWorkMean / allDat$Mass
allDat$ankleMomXMean <- allDat$ankleMomXMean / allDat$Mass
allDat$ankleMomYMean <- allDat$ankleMomYMean / allDat$Mass
allDat$kneeMomXMean <- allDat$kneeMomXMean / allDat$Mass
allDat$kneeMomYMean <- allDat$kneeMomYMean / allDat$Mass
allDat$hipMomXMean <- allDat$hipMomXMean / allDat$Mass
allDat$hipMomYMean <- allDat$hipMomYMean / allDat$Mass
allDat$pBFMean <- allDat$pBFMean / allDat$Mass
allDat$pLFmean <- allDat$pLFmean / allDat$Mass
allDat$pMFmean <- allDat$pMFmean / allDat$Mass
allDat$pVGRFmean <- allDat$pVGRFmean / allDat$Mass
allDat$VALRmean <- allDat$VALRmean / allDat$Mass


# build model

fullMod <- lmer(EE_dist ~ AnkleNegWorkMean + AnklePosWorkMean + KneeNegWorkMean + KneePosWorkMean + HipNegWorkMean + HipPosWorkMean + 
              HipAbdROMMean + HipFlexROMMean + KneeAbdROMMean + KneeFlexROMMean + AnkleInvROMMean + AnkleFlexROMMean + 
              ankleMomXMean + ankleMomYMean + kneeMomXMean + kneeMomYMean + hipMomXMean + hipMomYMean + pBFMean + 
              pLFmean + pMFmean + CTmean + (1|Subject), data = allDat)

summary(fullMod)

all_vifs <- car::vif(fullMod)
print(all_vifs)

signif_all <- names(all_vifs)

while(any(all_vifs > 4)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]
  myForm <- as.formula(paste("EE_dist ~",paste(signif_all, collapse=" + "), paste( "+ (1|Subject)"), sep = ""))
  fullMod <- lmer(myForm, data = allDat)
  all_vifs <- car::vif(fullMod)
  
}

summary(fullMod)

all_vifs <- car::vif(fullMod)
print(all_vifs)

redMod <- step(fullMod, reduce.fixed = TRUE, reduce.random = FALSE)

redMod <- lmer(EE_dist ~ AnkleInvROMMean + kneeMomXMean + (1|Subject), data = allDat)
all_vifs <- car::vif(redMod)
print(all_vifs)

summary(redMod)
get_model(redMod)

nullMod <- lmer(EE_dist ~ (1|Subject), data = allDat)

ggscatter(allDat, x = "AnkleInvROMMean", y = "EE_dist", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = 'Frontal Plane Ankle ROM (deg)', ylab = 'Energy Expenditure (W/kg/km)') 


############### Ankle/ground focued model

fullModShoe <- lmer(EE_dist ~ AnkleNegWorkMean + AnklePosWorkMean + AnkleInvROMMean + AnkleFlexROMMean + 
                  ankleMomXMean + ankleMomYMean + pBFMean + 
                  pLFmean + pMFmean + CTmean + (1|Subject), data = allDat)

summary(fullModShoe)

all_vifs <- car::vif(fullModShoe)
print(all_vifs)

signif_all <- names(all_vifs)

while(any(all_vifs > 4)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]
  myForm <- as.formula(paste("EE_dist ~",paste(signif_all, collapse=" + "), paste( "+ (1|Subject)"), sep = ""))
  fullModShoe <- lmer(myForm, data = allDat)
  all_vifs <- car::vif(fullModShoe)
  
}

summary(fullModShoe)

redModShoe <- step(fullModShoe, reduce.fixed = TRUE, reduce.random = FALSE)

 
redMod <- lmer(EE_dist ~ AnkleInvROMMean + kneeMomXMean + (1|Subject), data = allDat)
all_vifs <- car::vif(redMod)
print(all_vifs)

summary(redMod)
get_model(redMod)

nullMod <- lmer(EE_dist ~ (1|Subject), data = allDat)

################## On trail measures only (i.e. only 1D forces and kinematics)

# build model

fullMod <- lmer(EE_dist ~ HipAbdROMMean + HipFlexROMMean + KneeAbdROMMean + KneeFlexROMMean + AnkleInvROMMean + AnkleFlexROMMean + 
                  pVGRFmean + VALRmean + CTmean + (1|Subject), data = allDat)

summary(fullMod)

all_vifs <- car::vif(fullMod)
print(all_vifs)

signif_all <- names(all_vifs)

while(any(all_vifs > 4)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]
  myForm <- as.formula(paste("EE_dist ~",paste(signif_all, collapse=" + "), paste( "+ (1|Subject)"), sep = ""))
  fullMod <- lmer(myForm, data = allDat)
  all_vifs <- car::vif(fullMod)
  
}

summary(fullMod)

all_vifs <- car::vif(fullMod)
print(all_vifs)

redMod <- step(fullMod, reduce.fixed = TRUE, reduce.random = FALSE)

redMod <- lmer(EE_dist ~ AnkleInvROMMean + pVGRFmean + (1|Subject), data = allDat)
all_vifs <- car::vif(redMod)
print(all_vifs)

summary(redMod)
get_model(redMod)

nullMod <- lmer(EE_dist ~ (1|Subject), data = allDat)

anova(redMod, nullMod)

########

testRandSlopes('AnkleInvROM', kinDat)
testRandSlopes('pBF', forceDat)
testRandSlopes('AnkleNegWork', kinDat)
