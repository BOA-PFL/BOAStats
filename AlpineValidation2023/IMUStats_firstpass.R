 library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

# Functions:
testAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + Side + TrialNo + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ Side + TrialNo + (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  newList <- list("randEffectMod" = summary(full.mod),"Coefficients" = coef(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "Buckle"))
  return(newList)
}

# Call the .csv file with the kinetics data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Snow Performance/SkiValidation_Dec2022/IMU/IMUOutcomes2.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('BOA', 'Buckle'))

# Examine Histograms of Variables of Interest
ggplot(data = dat, aes(x = edgeang_dwn_gyr, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = dat, aes(x = edgeang_up_gyr, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = dat, aes(x = RAD_dwn, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = dat, aes(x = RADt_dwn, fill = Config)) + geom_histogram() + facet_wrap(~Subject)
ggplot(data = dat, aes(x = freq50fft, fill = Config)) + geom_histogram() + facet_wrap(~Subject)

testAnova('edgeang_dwn_gyr',dat)
testAnova('edgeangt_dwn_gyr',dat)
testAnova('edgeang_up_gyr',dat)
testAnova('RAD_dwn',dat)
testAnova('RADt_dwn',dat)
testAnova('freq50fft',dat)


# Creating Plots
summarydat <- dat %>%
  group_by(Subject, Config) %>%
  summarize(edgeang_dwn = mean(edgeang_dwn_gyr), edgeang_up = mean(edgeang_up_gyr))

# point and error bar plot
ggplot(data = summarydat, mapping = aes(x = Config, y = edgeang_dwn, fill = Config)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#00966C", "#999999")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Edge Ang') + xlab('Configuration') + ylim(30,60)
  # theme(axis.text.y=element_blank())

ggplot(data = summarydat, mapping = aes(x = Config, y = edgeang_up, fill = Config)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#00966C", "#999999")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Edge Ang') + xlab('Configuration') + ylim(30,60)
# theme(axis.text.y=element_blank())

