library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

# Functions
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
    summarize(mean = mean(!! sym(colName)), na.rm = TRUE)
  
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
  
  ggplot(data = whichConfig, mapping = aes(x = as.factor(Config), y = mean, col = BestConfig, group = Subject, label = Subject)) + geom_point(size = 4) +
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 26)) + ylab(ylabel)
  
}

# Call the .csv file with the pressure data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/InLabData/InLabPressureOutcomes.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs','sock'))
dat$Label <- factor(dat$Label, c('1', '2','3'))

# Examine Histograms of Variables of Interest
ggplot(data = dat, aes(x = HeelCon, fill = Label)) + geom_histogram() + facet_wrap(~Subject)

# Stats on the Pressure Metrics
dat1 <- dat %>% filter(Label == 1, Speed == 'ss', Side == 'L')
dat2 <- dat %>% filter(Label == 2, Speed == 'ss', Side == 'L')
dat3 <- dat %>% filter(Label == 3, Speed == 'ss', Side == 'L')

withinSubPlot(dat1, colName = 'HeelCon', dir = 'higher','Heel Contact (%)') 
withinSubPlot(dat2, colName = 'HeelCon', dir = 'higher','Heel Contact (%)') 
withinSubPlot(dat3, colName = 'HeelCon', dir = 'higher','Heel Contact (%)')

testAnova('HeelCon',dat1)
testAnova('HeelCon',dat2)
testAnova('HeelCon',dat3)

labelnames <- c(
  '1' = "Uphill",
  '2' = "Top",
  '3' = "Downhill"
)

# Summary bar plots for FBS poster
summarydat <- dat %>%
  group_by(Subject,Config, Label) %>%
  summarize(HeelCon = mean(HeelCon))

avgsubdat <- summarydat %>%
  group_by(Config, Label) %>%
  summarize(avgHeelCon = mean(HeelCon),stdHeelCon = sd(HeelCon))

ggplot(avgsubdat, aes(x = Config, y = avgHeelCon, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + 
  geom_bar(stat = 'identity',color = 'black', position = position_dodge()) + 
  geom_errorbar(aes(ymin=avgHeelCon-stdHeelCon, ymax=avgHeelCon+stdHeelCon), width=.2,position=position_dodge(.9)) + 
  theme_classic()

