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
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 26)) + ylab(ylabel)
  
}


# Call the .csv file with the IMU speed data
dat <- read.csv(file = 'C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/InLabIMUmetrics.csv')

dat <- as_tibble(dat) # creating the data frame
dat$Config <- factor(dat$Config, c('lace', 'pfs','sock'))
dat$Label <- factor(dat$Label, c('1', '2','3'))

# Examine the histograms of the different variables
ggplot(data = dat, aes(x = pJerk, color = Config)) + geom_histogram() + facet_wrap(~Subject) + xlim(0,100000)

ggplot(data = dat, aes(x = rMLacc, color = Config)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = pGyr, fill = Label)) + geom_histogram() + facet_wrap(~Subject)

ggplot(data = dat, aes(x = imuSpeed, color = Config)) + geom_histogram() + facet_wrap(~Subject)

dat1 <- dat %>% filter(Label == 1, Speed == 'ps')
dat2 <- dat %>% filter(Label == 2, Speed == 'ps')
dat3 <- dat %>% filter(Label == 3, Speed == 'ps')

withinSubPlot(dat1, colName = 'pGyr', dir = 'lower','Max Ev. Vel. (deg/s)')
withinSubPlot(dat2, colName = 'pGyr', dir = 'lower','Max Ev. Vel. (deg/s)')
withinSubPlot(dat3, colName = 'pGyr', dir = 'lower','Max Ev. Vel. (deg/s)')



testAnova('pJerk',dat1)
testAnova('pJerk',dat2)
testAnova('pJerk',dat3)

testAnova('rMLacc',dat1)
testAnova('rMLacc',dat2)
testAnova('rMLacc',dat3)

testAnova('pGyr',dat1)
testAnova('pGyr',dat2)
testAnova('pGyr',dat3)

testAnova('imuSpeed',dat1)
testAnova('imuSpeed',dat2)
testAnova('imuSpeed',dat3)


ggplot(data = dfvar, aes(x = rMLacc, color = Config)) + geom_histogram() + facet_wrap(~Subject)

dfvar1 <- dfvar %>% filter(Label == 1)
dfvar2 <- dfvar %>% filter(Label == 2)
dfvar3 <- dfvar %>% filter(Label == 3)

testAnova('rMLacc',dfvar1)
testAnova('rMLacc',dfvar2)
testAnova('rMLacc',dfvar3)

summarydat <- dat %>%
  group_by(Subject, Config, Label) %>%
  filter(Label > 0) %>%
  filter(Speed == 'ps') %>%
  summarize(pJerk = mean(pJerk), avgML = mean(rMLacc), sdML = var(rMLacc), rIEgyro = mean(rIEgyro))

summarydat1 <-summarydat %>% filter(Label == 1)
summarydat2 <-summarydat %>% filter(Label == 2)
summarydat3 <-summarydat %>% filter(Label == 3)

testAnova_small('sdML',summarydat1)
testAnova_small('sdML',summarydat2)
testAnova_small('sdML',summarydat3)

labelnames <- c(
  '1' = "Uphill",
  '2' = "Top",
  '3' = "Downhill"
)

# Bar plot for presentation
exdat <- dat2 %>%
  filter(Subject == 'IS05', Config == 'pfs')

ggplot(exdat, aes(x = Config, y=imuSpeed)) + geom_dotplot(binaxis='y',stackdir='center', dotsize=.25) + theme_minimal() +
  scale_fill_manual(values=c("#00966C")) + theme(text = element_text(size = 24), axis.text.x=element_blank()) + ylab('Speed (m/s)') + xlab('') + ylim(0,4)



  
  # fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color="red", width=0.2) +
  # stat_summary(fun.y=mean, geom="point", color="red")

print(p)

# 

# Overall plots
ggplot(data = summarydat, mapping = aes(x = Config, y = pJerk, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Peak Jerk [m/s^3]') + xlab('Configuration')

ggplot(data = summarydat, mapping = aes(x = Config, y = avgML, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('M/L Acc. Range [m/s^2]') + xlab('Configuration')

ggplot(data = summarydat, mapping = aes(x = Config, y = sdML, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('SD M/L Acc. Range [m/s^2]') + xlab('Configuration')

ggplot(data = summarydat, mapping = aes(x = Config, y = rIEgyro, group = Subject)) + facet_grid(~Label,labeller = as_labeller(labelnames)) + geom_point(size = 4) + 
  geom_line() + theme(text = element_text(size = 16)) + ylab('Inv/Ever Gyro Range [rad/sec]') + xlab('Configuration')

