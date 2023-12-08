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
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "lace") 
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "lace"))
  return(newList)
}

# Call the .csv file with the pressure data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/PressureOutcomes.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs'))
dat <- dat %>% filter(Subject != "S19", Label > 0)


# Examine Histograms of Variables of Interest
ggplot(data = dat, aes(x = HeelVar)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = HeelCon)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = HeelCon1)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = HeelCon2)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = m_heelPP_lat)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = m_heelPP_med)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = m_heelPP)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = m_midPP_lat)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = m_metPP_lat)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = m_toePP_lat)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = m_toePP_med)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = avg_toe_med)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = ToeCon)) + geom_histogram() + facet_wrap(~Subject)



# Stats on the Pressure Metrics
dat1 <- dat %>% filter(Label == 1)
dat2 <- dat %>% filter(Label == 2)
dat3 <- dat %>% filter(Label == 3)

testAnova('HeelVar',dat1)
testAnova('HeelVar',dat2)
testAnova('HeelVar',dat3)

testAnova('HeelCon',dat1)
testAnova('HeelCon',dat2)
testAnova('HeelCon',dat3)

testAnova('ToeCon',dat1)
testAnova('ToeCon',dat2)
testAnova('ToeCon',dat3)

testAnova('m_heelPP_lat',dat1)
testAnova('m_heelPP_lat',dat2)
testAnova('m_heelPP_lat',dat3)

testAnova('m_heelPP_med',dat1)
testAnova('m_heelPP_med',dat2)
testAnova('m_heelPP_med',dat3)

testAnova('m_heelPP',dat1)
testAnova('m_heelPP',dat2)
testAnova('m_heelPP',dat3)

testAnova('m_midPP_lat',dat1)
testAnova('m_midPP_lat',dat2)
testAnova('m_midPP_lat',dat3)

testAnova('m_metPP_lat',dat1)
testAnova('m_metPP_lat',dat2)
testAnova('m_metPP_lat',dat3)

testAnova('m_toePP_lat',dat1)
testAnova('m_toePP_lat',dat2)
testAnova('m_toePP_lat',dat3)

testAnova('m_toePP_med',dat1)
testAnova('m_toePP_med',dat2)
testAnova('m_toePP_med',dat3)

testAnova('m_toePP',dat1)
testAnova('m_toePP',dat2)
testAnova('m_toePP',dat3)

testAnova('avg_toe_med',dat3)

labelnames <- c(
  '1' = "Uphill",
  '2' = "Top",
  '3' = "Downhill"
)

summarydat <- dat %>%
  group_by(Subject, Config, Label) %>%
  filter(Label > 0) %>%
  summarize(HeelCon = mean(HeelCon), ToeCon = mean(ToeCon), m_heelPP = mean(m_heelPP), m_toePP = mean(m_toePP))

# Plotting
ggplot(data = summarydat, mapping = aes(x = Config, y = HeelCon, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Heel Contact (%)') + xlab('Configuration') + 
  ylim(20,80) + theme(axis.text.y=element_blank())

ggplot(data = summarydat, mapping = aes(x = Config, y = ToeCon, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Toe Contact (%)') + xlab('Configuration') + 
  theme(axis.text.y=element_blank())

ggplot(data = summarydat, mapping = aes(x = Config, y = m_heelPP, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Heel Peak Pressure (kPa)') + xlab('Configuration') + 
  theme(axis.text.y=element_blank())

ggplot(data = summarydat, mapping = aes(x = Config, y = m_toePP, fill = Config)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=.75) + scale_fill_manual(values=c("#999999", "#00966C")) + 
  stat_summary(fun = 'mean',fun.args = list(mult=1), geom = 'point', color = 'black') + 
  stat_summary(geom = 'errorbar', color = 'black', fun.min = function(x) mean(x) - sd(x), fun.max=function(x) mean(x) + sd(x), width = 0.5) +
  theme_minimal() + theme(text = element_text(size = 24)) + theme(legend.position="none") + ylab('Toe Peak Pressure (kPa)') + xlab('Configuration') + 
  + theme(axis.text.y=element_blank())

#### Old Work

# ggplot(data = dat, aes(x = m_heel_ratio)) + geom_histogram() + facet_wrap(~Subject)+xlim(0,10)
# 
# ggplot(data = dat, aes(x = m_mid_ratio)) + geom_histogram() + facet_wrap(~Subject)+xlim(0,10)
# 
# ggplot(data = dat, aes(x = m_met_ratio)) + geom_histogram() + facet_wrap(~Subject)+xlim(0,10)
# 
# ggplot(data = dat, aes(x = m_toe_ratio)) + geom_histogram() + facet_wrap(~Subject)+xlim(0,10)

# testAnova('ToeCon1',dat1)
# testAnova('ToeCon1',dat2)
# testAnova('ToeCon1',dat3)
# 
# dat1 <- dat %>% filter(Label == 1, m_heel_ratio < 1000)
# dat2 <- dat %>% filter(Label == 2, m_heel_ratio < 1000)
# dat3 <- dat %>% filter(Label == 3, m_heel_ratio < 1000)
# 
# testAnova('m_heel_ratio',dat1)
# testAnova('m_heel_ratio',dat2)
# testAnova('m_heel_ratio',dat3)
# 
# dat1 <- dat %>% filter(Label == 1, m_mid_ratio < 1000)
# dat2 <- dat %>% filter(Label == 2, m_mid_ratio < 1000)
# dat3 <- dat %>% filter(Label == 3, m_mid_ratio < 1000)
# 
# testAnova('m_mid_ratio',dat1)
# testAnova('m_mid_ratio',dat2)
# testAnova('m_mid_ratio',dat3)
# 
# dat1 <- dat %>% filter(Label == 1, m_met_ratio < 1000)
# dat2 <- dat %>% filter(Label == 2, m_met_ratio < 1000)
# dat3 <- dat %>% filter(Label == 3, m_met_ratio < 1000)
# 
# testAnova('m_met_ratio',dat1)
# testAnova('m_met_ratio',dat2)
# testAnova('m_met_ratio',dat3)
# 
# dat1 <- dat %>% filter(Label == 1, m_toe_ratio < 1000)
# dat2 <- dat %>% filter(Label == 2, m_toe_ratio < 1000)
# dat3 <- dat %>% filter(Label == 3, m_toe_ratio < 1000)

# dat2 <- dat %>% 
#   filter(Label == 2) %>%
#   mutate(z_score = scale(m_metPP_lat)) %>%
#   filter(z_score > 2)

# condat <- dat %>%
#   group_by(Subject, Config, Label) %>%
#   filter(Label > 0) %>%
#   summarize(HeelCon = mean(HeelCon), HeelCon1 = mean(HeelCon1), HeelCon2 = mean(HeelCon2))
# 
# pressdat <- dat %>%
#   group_by(Subject, Config, Label) %>%
#   filter(Label > 0) %>%
#   summarize(MetPeak = mean(m_metPP_lat)) 
# 
# avgdat <- dat %>%
#   filter(Label > 0) %>%
#   group_by(Subject,Config,Label) %>% 
#   summarize(avgIE = mean(HeelCon)) %>% 
#   group_by(Config, Label) %>%
#   summarize(stdvar = sd(avgIE),avgvar = mean(avgIE))

# ggplot(data = avgdat, aes(x = Label, y = avgvar, fill = Config)) + geom_bar(stat = 'identity', position = position_dodge()) + theme_minimal() +
#   geom_errorbar(aes(ymin = avgvar-stdvar, ymax = avgvar+stdvar), width = 0.2, position=position_dodge(0.9)) + theme(text = element_text(size = 24)) + 
#   ylab('% Heel Contact') + scale_fill_manual(values=c("#999999", "#00966C")) + theme(legend.position="none") + ylim(0,100)
# 
# 
# ggplot(data = condat, mapping = aes(x = Config, y = HeelCon, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
#   geom_line() + theme(text = element_text(size = 16)) + ylab('Average Heel Contact throughout Stance [%]') + xlab('Configuration')
# 
# ggplot(data = condat, mapping = aes(x = Config, y = HeelCon1, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
#   geom_line() + theme(text = element_text(size = 16)) + ylab('Average Heel Contact 1st Half of Stance [%]') + xlab('Configuration')
# 
# ggplot(data = condat, mapping = aes(x = Config, y = HeelCon2, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
#   geom_line() + theme(text = element_text(size = 16)) + ylab('Average Heel Contact 2nd Half of Stance [%]') + xlab('Configuration')
# 
# ggplot(data = pressdat, mapping = aes(x = Config, y = MetPeak, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
#   geom_line() + theme(text = element_text(size = 16)) + ylab('Peak Lateral Metatarsal Pressure [kPa]') + xlab('Configuration')
# 
# ggplot(data = condat, mapping = aes(x = Config, y = ToeCon, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
#   geom_line() + theme(text = element_text(size = 16)) + ylab('Average Toe Contact throughout Stance [%]') + xlab('Configuration')
# 
# ggplot(data = condat, mapping = aes(x = Config, y = ToeCon1, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
#   geom_line() + theme(text = element_text(size = 16)) + ylab('Average Toe Contact 1st Half of Stance [%]') + xlab('Configuration')
