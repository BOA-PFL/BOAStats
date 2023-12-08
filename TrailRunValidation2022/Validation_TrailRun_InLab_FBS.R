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
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
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

dat <- dat %>% filter(SetSpeed == 3) %>%
  filter(pAnkEvVel < 500)

# Look at the distributions

ggplot(data = dat, aes(x = NegFootWork, fill = Config)) + geom_histogram() + facet_wrap(~SetSlope)

ggplot(data = dat, aes(x = PosFootWork, fill = Config)) + geom_histogram() + facet_wrap(~SetSlope) + xlim(0,60)

ggplot(data = dat, aes(x = VALR, fill = Config)) + geom_histogram() + facet_wrap(~SetSlope)

ggplot(data = dat, aes(x = pAnkEvVel, fill = Config)) + geom_histogram() + facet_wrap(~SetSlope)


# Best of plots:
dat1 <- dat %>% filter(SetSlope == 'p4')
dat2 <- dat %>% filter(SetSlope == 'p2')
dat3 <- dat %>% filter(SetSlope == 'n6')

withinSubPlot(dat1, colName = 'VALR', dir = 'lower','VALR [N/s]')
withinSubPlot(dat2, colName = 'VALR', dir = 'lower','VALR [N/s]')
withinSubPlot(dat3, colName = 'VALR', dir = 'lower','VALR [N/s]')

withinSubPlot(dat1, colName = 'pAnkEvVel', dir = 'lower','Eversion Velocity [deg/s]')
withinSubPlot(dat2, colName = 'pAnkEvVel', dir = 'lower','Eversion Velocity [deg/s]')
withinSubPlot(dat3, colName = 'pAnkEvVel', dir = 'lower','Eversion Velocity [deg/s]')

withinSubPlot(dat3, colName = 'FootConAng', dir = 'lower','Foot Contact Angle (deg)') + 
  theme_classic()

testAnova('NegFootWork',dat1)
testAnova('NegFootWork',dat2)
testAnova('NegFootWork',dat3)

testAnova('FootConAng',dat1)
testAnova('FootConAng',dat2)
testAnova('FootConAng',dat3)

testAnova('AnkConAng',dat1)
testAnova('AnkConAng',dat2)
testAnova('AnkConAng',dat3)

testAnova('pAnkEvVel',dat1)
testAnova('pAnkEvVel',dat2)
testAnova('pAnkEvVel',dat3)

# Bar plots
labelnames <- c(
  '1' = "Uphill",
  '2' = "Top",
  '3' = "Downhill"
)

summarydat <- dat %>%
  group_by(Subject,Config, SetSlope) %>%
  summarize(NegFootWork = mean(NegFootWork), FootConAng = mean(FootConAng))

summarydat2 <- dat %>%
  filter(SetSlope == 'n6') %>%
  group_by(Subject,Config) %>%
  summarize(NegFootWork = mean(NegFootWork), FootConAng = mean(FootConAng))

avgsubdat <- summarydat %>%
  group_by(Config, SetSlope) %>%
  summarize(avgNegFootWork = mean(NegFootWork),stdNegFootWork = sd(NegFootWork), avgFootConAng = mean(FootConAng),stdFootConAng = sd(FootConAng))

ggplot(avgsubdat, aes(x = Config, y = avgNegFootWork, fill = Config)) + facet_grid(~SetSlope) + 
  geom_bar(stat = 'identity',color = 'black', position = position_dodge()) + 
  geom_errorbar(aes(ymin=avgNegFootWork-stdNegFootWork, ymax=avgNegFootWork+stdNegFootWork), width=.2,position=position_dodge(.9)) + 
  theme_classic()

ggplot(avgsubdat, aes(x = Config, y = avgFootConAng, fill = Config)) + facet_grid(~SetSlope) + 
  geom_bar(stat = 'identity',color = 'black', position = position_dodge()) + 
  geom_errorbar(aes(ymin=avgFootConAng-stdFootConAng, ymax=avgFootConAng+stdFootConAng), width=.2,position=position_dodge(.9)) + 
  theme_classic()

ggplot(data = summarydat2, mapping = aes(x = Config, y = FootConAng, group = Subject)) + geom_point(size = 4) +
  geom_line()
