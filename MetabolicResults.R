# EH Run Validation study analysis
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
  contrast(conditions.emm, "trt.vs.ctrl", ref = "SL") 
  
  
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "SL"))
  return(newList)
  
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


plotAndStore <- function(col, dfName){
  
  genPlot <- ggplot(data = dfName, mapping = aes(x = Subject, y = .data[[col]], fill = Config)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values=c("#000000", "#ECE81A", "#CAF0E4"))
  
  return(genPlot)
}

withinSubPlot <- function(col, dfName){
  ggplot(data = dfName, mapping = aes(x = as.factor(Config), y = .data[[col]], col = BestConfig, group = Subject)) + geom_point(size = 4) + 
    geom_line() + xlab('Configuration') + theme(text = element_text(size = 16)) + ylab('EE') 
}

# metbaolics --------------------------------------------------------------

metDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL - General/AgilityPerformanceData/BOA_InternalStrap_July2021/Metabolics/MetResults.csv')

plotAndStore('EE',metDat)
# met data averaging over 2 minutes instead of 1
metDat2 <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL - General/AgilityPerformanceData/BOA_InternalStrap_July2021/Metabolics/MetResults2.csv')
plotAndStore('EE',metDat2)


whichConfig <- metDat2 %>%
  group_by(Subject) %>%
  summarize(
    mm = min(EE),
    BestConfig = Config[which.min(EE)]
            )
metOut <- merge(metDat2, whichConfig)
withinSubPlot('EE',metOut)

