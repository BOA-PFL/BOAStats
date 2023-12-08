library(tidyverse)
library(readxl)
library(lme4)
library(ggplot2)

#Clearing the environment
rm(list=ls())

# Functions
testAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (Config|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "lace"))
  return(newList)
}

testAnova_small <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config", " + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (1|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "lace"))
  return(newList)
}

# Call the .csv file with the IMU speed data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/OutdoorData/IMUspeed.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs'))
dat$imuSpeed <- as.numeric(dat$Speed)

dat1 <- dat %>% filter(Label == 1)
dat2 <- dat %>% filter(Label == 2)
dat3 <- dat %>% filter(Label == 3)

testAnova('Speed',dat1)
testAnova('Speed',dat2)
testAnova('Speed',dat3)

ggplot(data = dat, aes(x = Speed)) + geom_histogram() + facet_wrap(~Subject) 


summarydat <- dat %>%
  group_by(Subject, Label) %>%
  filter(Label > 0) %>%
  summarize(avgSpeed = mean(Speed), sdSpeed = sd(Speed))

bardat <- summarydat %>%
  group_by(Config,Label) %>%
  summarize(sdSpeed = sd(avgSpeed), Speed = mean(avgSpeed))

summarydat1 <-summarydat %>% filter(Label == 1)
summarydat2 <-summarydat %>% filter(Label == 2)
summarydat3 <-summarydat %>% filter(Label == 3)

testAnova_small('varSpeed',summarydat1)
testAnova_small('varSpeed',summarydat2)
testAnova_small('varSpeed',summarydat3)

labelnames <- c(
  '1' = "Uphill",
  '2' = "Top",
  '3' = "Downhill"
)

# Plots for presentation
exdat <- dat2 %>%
  filter(Subject == 'S04', Config == 'pfs')

ggplot(exdat, aes(x = Config, y=Speed)) + geom_dotplot(binaxis='y',stackdir='center', dotsize=.25) + theme_minimal() +
  scale_fill_manual(values=c("#00966C")) + theme(text = element_text(size = 24), axis.text.x=element_blank()) + ylab('Speed (m/s)') + xlab('') + ylim(0,4)

# Point plot
ggplot(data = summarydat, mapping = aes(x = Config, y = avgSpeed, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Speed (m/s)') + xlab('Configuration') + 
  theme(axis.text.y=element_blank())

ggplot(data = bardat, mapping = aes(x = Config, y = Speed, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_bar(stat='identity') + theme_minimal() + 
  geom_errorbar(aes(ymin = Speed-sdSpeed, ymax = Speed+sdSpeed)) + theme(text = element_text(size = 24)) + ylab('Average Speed [m/s]') + xlab('Configuration') + 
  scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none")

ggplot(data = summarydat, mapping = aes(x = Config, y = avgSpeed, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Average Speed [m/s]') + xlab('Configuration')

ggplot(data = summarydat, mapping = aes(x = Config, y = varSpeed, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Speed Variation [m/s]') + xlab('Configuration')


