library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

# Functions:

remove_outliers <- function(x, na.rm = TRUE, ...) {
  mn <- median(x, na.rm = TRUE)
  std <- IQR(x, na.rm = TRUE)
  y <- x
  y[x < (mn - 1.96*std)] <- NA
  y[x > (mn + 1.96*std)] <- NA
  y
}

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
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Snow Performance/SkiValidation_Dec2022/Loadsol/CompiledResultsTestBothSides.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('BOA', 'Buckle'))

dat <- subset(dat, dat$Config %in% c('BOA','Buckle'))
S18Good <- dat[dat$Subject == 'S18' & dat$TrialNo %in% c(3,4,5,6,7),]


cleanedDat <- dat %>%
  filter(Subject != 'S14')%>%
  filter(Subject != 'S18')%>%
  group_by(Subject) %>%
  mutate(
    Outforce = remove_outliers(OutTotMaxForce),
    RFD = remove_outliers(RFD),
    RFDTime = remove_outliers(RFDtime),
    OutTotAvgForce = remove_outliers(OutTotAvgForce),
    OutToeMaxForce = remove_outliers(OutToeMaxForce),
    OutMedMaxForce = remove_outliers(OutMedMaxForce)
  )
# S18 added back in below with only good trials
# S18 and trials 1 or 2
### need to remove S18 high force trials that are from a zero'ing issue ###
cleanedDat <- rbind(cleanedDat, S18Good)

ggplot(data = cleanedDat, aes(x = InsideTotMaxForce, fill = Config)) + geom_histogram() + facet_wrap(~Subject)


# Creating Plots
summarydat <- cleanedDat %>%
  group_by(Subject, Config) %>%
  summarize(OutTotMaxForce = mean(OutTotMaxForce), RFD = mean(RFD, na.rm = TRUE), InsideTotMaxForce = mean(InsideTotMaxForce, na.rm = TRUE), OutTotAvgForce = mean(OutTotAvgForce, na.rm = TRUE))

# point and error bar plot: size for plotting: 225x300
ggplot(data = summarydat, mapping = aes(x = Config, y = OutTotMaxForce, fill = Config)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#00966C", "#999999")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Max Force') + xlab('Configuration') + theme(axis.text.y=element_blank())

ggplot(data = summarydat, mapping = aes(x = Config, y = RFD, fill = Config)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#00966C", "#999999")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('RFD') + xlab('Configuration') + theme(axis.text.y=element_blank())

ggplot(data = summarydat, mapping = aes(x = Config, y = InsideTotMaxForce, fill = Config)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#00966C", "#999999")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Inside Max') + xlab('Configuration') + theme(axis.text.y=element_blank())

ggplot(data = summarydat, mapping = aes(x = Config, y = OutTotAvgForce, fill = Config)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#00966C", "#999999")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Outside Mean') + xlab('Configuration') + ylim() + theme(axis.text.y=element_blank())


