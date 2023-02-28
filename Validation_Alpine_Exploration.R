library(tidyverse)
library(lme4)
library(brms)
library(emmeans)

rm(list=ls())

dat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/Testing Segments/Snow Performance/SkiValidation_Dec2022/Loadsol/CompiledResultsTestBothSides.csv')
dat$Config <- as.factor(dat$Config)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  mn <- median(x, na.rm = TRUE)
  std <- IQR(x, na.rm = TRUE)
  y <- x
  y[x < (mn - 1.96*std)] <- NA
  y[x > (mn + 1.96*std)] <- NA
  y
}


expPlot <- function(inputDF, colName) {
  # Specify ylabel in function or default to the original name
  ggplot(data = inputDF, aes(x = .data[[colName]], fill = Config)) +
    geom_histogram() + facet_wrap(~Subject + TurnDir)
}

testAnovaSimp <- function(metric, df) {
  
  myformula <- as.formula(paste0(metric," ~ Config", "+ TurnDir", " + TrialNo", " + (Config|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ TurnDir", " + TrialNo", " + (Config|Subject)"))
  
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit" )
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "asymptotic")
  
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod), "Coefs" = coef(full.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "Buckle"))
  return(newList)
  
}

testAnova <- function(metric, df) {
  
  myformula <- as.formula(paste0(metric," ~ Config", "+ TurnDir", " + TrialNo", " + (Config|Subject)", "+(TurnDir|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ TurnDir", " + TrialNo", " + (Config|Subject)", "+(TurnDir|Subject)"))
  
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit" )
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )

  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "asymptotic")

  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod), "Coefs" = coef(full.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "Buckle"))
  return(newList)
  
}
## this is a janky way to verify we do not have any misspelled config names and
## to ensure trials 1 and 2 for subject 18 are excluded based on a zero'ing issue
## they are added back in below
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



expPlot(cleanedDat, 'OutTotMaxForce')
testAnovaSimp('OutTotMaxForce', cleanedDat)

expPlot(cleanedDat, 'RFD')
testAnovaSimp('RFD', cleanedDat)

expPlot(cleanedDat, 'RFDtime')
testAnova('RFDtime',cleanedDat)

expPlot(cleanedDat, 'OutTotAvgForce')
testAnovaSimp('OutTotAvgForce', cleanedDat)

expPlot(cleanedDat, 'OutToeMaxForce')
testAnovaSimp('OutToeMaxForce', cleanedDat)

expPlot(cleanedDat, 'OutMedMaxForce')
testAnovaSimp('OutMedMaxForce', cleanedDat)

expPlot(dat, 'OutToeFracImpulseEarly')
testAnovaSimp('OutToeFracImpulseEarly', cleanedDat)

expPlot(cleanedDat, 'InsideTotMaxForce')
testAnovaSimp('InsideTotMaxForce', cleanedDat)


# bayes -------------------------------------------------------------------

cleanedDat <- cleanedDat %>% 
  group_by(Subject) %>%
  mutate(z_rfd = scale(RFD))%>%
  mutate(z_toeforce = scale(OutToeMaxForce))%>%
  mutate(z_force = scale(OutTotMaxForce))


rfdMod <- brm(data = cleanedDat, 
              family = gaussian,
              z_rfd ~ Config + TurnDir + TrialNo + (Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 5), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 5), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 5), class = sd)),#overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20), sample_prior = TRUE,
              seed = 190831)
summary(rfdMod)
post <- posterior_samples(rfdMod)
length(post$b_ConfigBuckle[post$b_ConfigBuckle < 0])/length(post$b_ConfigBuckle)
ss <- posterior_predict(rfdMod)
str(ss)


testMod <- brm(data = cleanedDat, 
               family = gaussian,
               z_force ~ Config + TurnDir + TrialNo + (Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
               prior = c(prior(normal(0, 2), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                         prior(normal(0, 2), class = b), #beta for the intercept for the change in loading rate for each configuration
                         prior(cauchy(0, 2), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                         prior(cauchy(0, 100), class = sigma)), #overall variability that is left unexplained 
               iter = 2000, warmup = 1000, chains = 4, cores = 4,
               control = list(adapt_delta = .975, max_treedepth = 20), sample_prior = TRUE,
               seed = 190831)
summary(testMod)
post <- posterior_samples(testMod)
length(post$b_ConfigBuckle[post$b_ConfigBuckle < 0])/length(post$b_ConfigBuckle)


toeFMod <- brm(data = dat, 
              family = gaussian,
              OutToeMaxForce ~ Config + TurnDir + TrialNo + (Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(760, 100), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 100), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 100), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 100), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20), sample_prior = TRUE,
              seed = 190831)
summary(toeFMod)
post <- posterior_samples(toeFMod)
length(post$b_ConfigBuckle[post$b_ConfigBuckle < 0])/length(post$b_ConfigBuckle)

avgFMod <- brm(data = dat, 
               family = gaussian,
               OutTotAvgForce ~ Config + TurnDir + TrialNo + (Config|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
               prior = c(prior(normal(760, 100), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                         prior(normal(0, 100), class = b), #beta for the intercept for the change in loading rate for each configuration
                         prior(cauchy(0, 100), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                         prior(cauchy(0, 100), class = sigma)), #overall variability that is left unexplained 
               iter = 2000, warmup = 1000, chains = 4, cores = 4,
               control = list(adapt_delta = .975, max_treedepth = 20), sample_prior = TRUE,
               seed = 190831)
summary(avgFMod)
post <- posterior_samples(avgFMod)
length(post$b_ConfigBuckle[post$b_ConfigBuckle < 0])/length(post$b_ConfigBuckle)

# in lab pressures --------------------------------------------------------

pDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/Testing Segments/Snow Performance/SkiValidation_Dec2022/InLabPressure/CompiledResults2.csv')
pDat$CVDorsal <- pDat$sdDorsalpressure / pDat$meanDorsalPressure

pDat$Config <- str_replace(pDat$Config, "BuckP", "BuckleP")
pDat <- pDat %>%
  filter(Config != 'FootCal')

expPlot2 <- function(inputDF, colName) {
  # Specify ylabel in function or default to the original name
  ggplot(data = inputDF, aes(x = Config, y = .data[[colName]], color = Config)) +
    geom_point() + facet_wrap(~Subject)
}

pAnova <- function(metric, df) {
  
  myformula <- as.formula(paste0(metric," ~ Config", " + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (1|Subject)"))
  
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit" )
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "asymptotic")
  
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod), "Coefs" = coef(full.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "BuckleP"))
  return(newList)
  
}

### Dorsal Pressures ###
expPlot2(pDat, 'meanDorsalPressure') #3/4 lower in BOA. People leave BOA a little less tight
pAnova('meanDorsalPressure',pDat)

expPlot2(pDat, 'maxDorsalPressure') #Similar story
pAnova('maxDorsalPressure',pDat)

expPlot2(pDat, 'sdDorsalpressure') #Lower deviation in BOA
pAnova('sdDorsalpressure',pDat)

expPlot2(pDat, 'totalDorsalPressure') #greatest in Buckles in 3/4 and the 1/4 did not tighten buckles as much
pAnova('totalDorsalPressure',pDat)

expPlot2(pDat, 'DorsalContact') #greatest in Buckles in 3/4 and the 1/4 did not tighten buckles as much
pAnova('DorsalContact',pDat)

## Heel ##
expPlot2(pDat, 'avgMHeel')
expPlot2(pDat, 'pkMHeel')
expPlot2(pDat, 'conMHeel')
expPlot2(pDat, 'avgLHeel')
expPlot2(pDat, 'pkLHeel')
expPlot2(pDat, 'conLHeel')

## Midfoot ##
expPlot2(pDat, 'avgMMid')
expPlot2(pDat, 'pkMMid')
expPlot2(pDat, 'conMmid')
expPlot2(pDat, 'avgLMid')
expPlot2(pDat, 'pkLMid')
expPlot2(pDat, 'conLMid')

## Mets ##
expPlot2(pDat, 'avgMMets')
expPlot2(pDat, 'pkMMid')
expPlot2(pDat, 'conMmid')
expPlot2(pDat, 'avgLMets')
expPlot2(pDat, 'pkLMets')
expPlot2(pDat, 'conLMets')

## Toes ##
expPlot2(pDat, 'avgMToes')
expPlot2(pDat, 'pkMToes')
expPlot2(pDat, 'conMToes')
expPlot2(pDat, 'avgLToes')
expPlot2(pDat, 'pkLToes')
expPlot2(pDat, 'conLToes')



