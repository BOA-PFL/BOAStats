library(tidyverse)
library(readxl)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(readxl)
library(brms)
library(patchwork)
library(lme4)
library(emmeans)


#Clearing the environment
rm(list=ls())

###############

testAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (1|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "Lace") 
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "Lace"))
  return(newList)
}

################

#Load in Compiled Qualitative Sheet
qualDat <- read_xlsx('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/TrailRunQual.xlsx')
qualDat <- qualDat %>% filter(Subject != "S19")

#Defining our baseline and shoes being tested agaisnt the baseline
qualDat$Config <- factor(qualDat$Config, c('Lace', 'PFS')) #List baseline first then shoes you want to test against

# Call the .csv file with the pressure data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/PressureOutcomes.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs'))
dat <- dat %>% filter(Subject != "S19", Label > 0)

avg_dat <- dat %>%
  group_by(Subject, Config) %>%
  summarize(avg_heel = mean(HeelCon))

cmp_dat <- data.frame(matrix(ncol = 2, nrow = length(avg_dat$avg_heel)/2))
colnames(cmp_dat) <- c('diff_press','diff_sub')

for (ii in 1:(length(avg_dat$avg_heel)/2)) {
  cmp_dat$diff_press[ii] = avg_dat$avg_heel[ii*2] - avg_dat$avg_heel[ii*2-1]
  cmp_dat$diff_sub[ii] = qualDat$Heel[ii*2] - qualDat$Heel[ii*2-1] 
}

ggplot(cmp_dat, mapping = aes(x = diff_press, y = diff_sub)) + geom_point()


  


