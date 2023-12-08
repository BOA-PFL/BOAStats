library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

# Functions:
testAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "Coefficients" = coef(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "lace"))
  return(newList)
}

SlopePlusAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + SetSlope + (Config|Subject) + (SetSlope|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ SetSlope + (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "SetSlope", lmer.df = "satterthwaite")
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = pairs(conditions.emm))
  return(newList)
}

SlopeAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ SetSlope + (SetSlope|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (SetSlope|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "SetSlope", lmer.df = "satterthwaite")
  newList <- list("randEffectMod" = summary(full.mod), "Coefficients" = coef(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = pairs(conditions.emm))
  return(newList)
}


withinSubPlot <- function(inputDF, colName, dir,ylabel) {
  # Specify ylabel in function or default to the original name
  if(missing(ylabel)){
    ylabel = paste0({{colName}})
  }
  
  # direction can be 'lower' or higher'. It is the direction of change that is better.
  # For example, for contact time lower is better. so we put 'lower'. for jump height, higher is better, so we put higher.
  meanDat <- inputDF %>%
    group_by(Subject, Config) %>%
    summarize(mean = mean(!! sym(colName)))
  
  if (dir == 'lower'){
    whichConfig <- meanDat %>%
      group_by(Subject) %>%
      summarize(
        BestConfig = Config[which.min(mean)]
      )
    
  } else if (dir == 'higher') {
    whichConfig <- meanDat %>%
      group_by(Subject) %>%
      summarize(
        BestConfig = Config[which.max(mean)]
      )
    
  }
  
  # "Best of" line plot code
  whichConfig <- merge(meanDat, whichConfig)
  
  ggplot(data = whichConfig, mapping = aes(x = as.factor(Config), y = mean, col = BestConfig, group = Subject)) + geom_point(size = 4) +
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 40)) + ylab(ylabel)
  
}

# Call the .csv file with the kinetics data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/InLabData/AnkleFootWork.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs','sock'))
dat$SetSlope <- factor(dat$SetSlope, c('n6','p2','p4'))

ggplot(data = dat, aes(x = NegFootWork, fill = SetSlope)) + geom_histogram() + facet_wrap(~Subject)

# subset the dataset for only examining the slope component
slopedat <- dat %>%
  filter(SetSpeed == 3, Config != 'sock')

dat1 <- slopedat %>% filter(SetSlope == 'p4')
dat2 <- slopedat %>% filter(SetSlope == 'p2')
dat3 <- slopedat %>% filter(SetSlope == 'n6')

SlopeAnova('NegFootWork',slopedat)
SlopeAnova('PosFootWork',slopedat)
SlopeAnova('MinFootPower',slopedat)
SlopeAnova('PosFootWork',slopedat)
SlopeAnova('MaxFootPower',slopedat)
SlopeAnova('NegAnkWork',slopedat)
SlopeAnova('PosAnkWork',slopedat)

SlopePlusAnova('NegFootWork',slopedat)
SlopePlusAnova('PosFootWork',slopedat)
SlopePlusAnova('NegAnkWork',slopedat)
SlopePlusAnova('PosAnkWork',slopedat)
SlopePlusAnova('NegCOMWork',slopedat)
SlopePlusAnova('PosCOMWork',slopedat)


testAnova('NegFootWork',dat1)
testAnova('NegFootWork',dat2)
testAnova('NegFootWork',dat3)

testAnova('PosFootWork',dat1)
testAnova('PosFootWork',dat2)
testAnova('PosFootWork',dat3)

testAnova('NegAnkWork',dat1)
testAnova('NegAnkWork',dat2)
testAnova('NegAnkWork',dat3)

testAnova('PosAnkWork',dat1)
testAnova('PosAnkWork',dat2)
testAnova('PosAnkWork',dat3)

testAnova('NegCOMWork',dat1)
testAnova('NegCOMWork',dat2)
testAnova('NegCOMWork',dat3)

testAnova('PosCOMWork',dat1)
testAnova('PosCOMWork',dat2)
testAnova('PosCOMWork',dat3)

summarydat <- slopedat %>%
  group_by(Subject,Config, SetSlope) %>%
  summarize(NegFootWork = mean(NegFootWork), PosFootWork = mean(PosFootWork),
            NegAnkWork = mean(NegAnkWork), PosAnkWork = mean(PosAnkWork))

avgsubdat <- summarydat %>%
  group_by(Config, SetSlope) %>%
  summarize(avgNegFootWork = mean(NegFootWork),stdNegFootWork = sd(NegFootWork),
            avgPosFootWork = mean(PosFootWork),stdPosFootWork = sd(PosFootWork),
            avgNegAnkWork = mean(NegAnkWork),stdNegAnkWork = sd(NegAnkWork),
            avgPosAnkWork = mean(PosAnkWork),stdPosAnkWork = sd(PosAnkWork))

ggplot(avgsubdat, aes(x = Config, y = avgNegFootWork, fill = Config)) + facet_grid(~SetSlope) + 
  geom_bar(stat = 'identity',color = 'black', position = position_dodge()) + 
  geom_errorbar(aes(ymin=avgNegFootWork-stdNegFootWork, ymax=avgNegFootWork+stdNegFootWork), width=.2,position=position_dodge(.9)) + 
  theme_classic()

ggplot(avgsubdat, aes(x = Config, y = avgPosFootWork, fill = Config)) + facet_grid(~SetSlope) + 
  geom_bar(stat = 'identity',color = 'black', position = position_dodge()) + 
  geom_errorbar(aes(ymin=avgPosFootWork-stdPosFootWork, ymax=avgPosFootWork+stdPosFootWork), width=.2,position=position_dodge(.9)) + 
  theme_classic()

ggplot(avgsubdat, aes(x = Config, y = avgNegAnkWork, fill = Config)) + facet_grid(~SetSlope) + 
  geom_bar(stat = 'identity',color = 'black', position = position_dodge()) + 
  geom_errorbar(aes(ymin=avgNegAnkWork-stdNegAnkWork, ymax=avgNegAnkWork+stdNegAnkWork), width=.2,position=position_dodge(.9)) + 
  theme_classic() + ylim(-60,0)

ggplot(avgsubdat, aes(x = Config, y = avgPosAnkWork, fill = Config)) + facet_grid(~SetSlope) + 
  geom_bar(stat = 'identity',color = 'black', position = position_dodge()) + 
  geom_errorbar(aes(ymin=avgPosAnkWork-stdPosAnkWork, ymax=avgPosAnkWork+stdPosAnkWork), width=.2,position=position_dodge(.9)) + 
  theme_classic() + ylim(0,60)

