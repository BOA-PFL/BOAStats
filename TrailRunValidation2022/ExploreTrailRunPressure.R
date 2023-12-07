library(tidyverse)
library(readxl)
library(ggplot2)
library(lme4)
library(emmeans)

#Clearing the environment
rm(list=ls())

# Functions
testAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config", " + (Config|Subject)"))
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

# Call the .csv file with the IMU speed data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/PressureOutcomes.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs'))

dat1 <- dat %>% filter(Label == 1)
dat2 <- dat %>% filter(Label == 2)
dat3 <- dat %>% filter(Label == 3)


testAnova('HeelCon',dat1)
testAnova('HeelCon',dat2)
testAnova('HeelCon',dat3)

testAnova('ToeCon',dat1)
testAnova('ToeCon',dat2)
testAnova('ToeCon',dat3)

testAnova('ToeCon1',dat1)
testAnova('ToeCon1',dat2)
testAnova('ToeCon1',dat3)

# Examine the different variables
ggplot(data = dat, aes(x = HeelVar)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = ToePeak)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = HeelCon)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = HeelCon1)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = HeelCon2)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = ToeCon)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = ToeCon1)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = ToeCon2)) + geom_histogram() + facet_wrap(~Subject)


condat <- dat %>%
  group_by(Subject, Config, Label) %>%
  filter(Label > 0) %>%
  summarize(HeelCon = mean(HeelCon), HeelCon1 = mean(HeelCon1), HeelCon2 = mean(HeelCon2),
            ToeCon = mean(ToeCon), ToeCon1 = mean(ToeCon1))

pressdat <- dat %>%
  group_by(Subject, Config, Label) %>%
  filter(Label > 0) %>%
  summarize(ToePeak = mean(ToePeak)) 

labelnames <- c(
  '1' = "Uphill",
  '2' = "Top",
  '3' = "Downhill"
)


ggplot(data = condat, mapping = aes(x = Config, y = HeelCon, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Average Heel Contact throughout Stance [%]') + xlab('Configuration')

ggplot(data = condat, mapping = aes(x = Config, y = HeelCon1, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Average Heel Contact 1st Half of Stance [%]') + xlab('Configuration')

ggplot(data = condat, mapping = aes(x = Config, y = HeelCon2, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Average Heel Contact 2nd Half of Stance [%]') + xlab('Configuration')

ggplot(data = pressdat, mapping = aes(x = Config, y = ToePeak, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Peak Toe Pressure [kPa]') + xlab('Configuration')

ggplot(data = condat, mapping = aes(x = Config, y = ToeCon, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Average Toe Contact throughout Stance [%]') + xlab('Configuration')

ggplot(data = condat, mapping = aes(x = Config, y = ToeCon1, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Average Toe Contact 1st Half of Stance [%]') + xlab('Configuration')

