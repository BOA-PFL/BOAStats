## Bayesian analysis of run data ##
#loading required packages, etc. If you are less familiar with R, you may need to install these packages first
rm(list=ls())
library(readxl)
library(tidyverse)
library(brms)
library(tidybayes)
library(lme4)


dat <- read.csv(file.choose())
sizes <- read_excel(file.choose())
dat$TimePeriod <- as.factor(dat$TimePeriod)


dat <- merge(dat, sizes, "Subject")

dat$Subject <- as.factor(dat$Subject)

dat <- dat %>%
  rename(
    Mass = Mass..kg.,
    Ht = Ht..cm.,
    VLR2 = X.VLR2.,
    VLR = X.VLR.,
    pVGRF = X.pVGRF.,
    pBF = X.pBF.,
    pPF = X.pPF.,
    pMF = X.pMF.,
    pLF = X.pLF.,
    Bimp = X.Bimp.,
    Pimp = X.Pimp.,
    Mimp = X.Mimp.,
    Limp = X.Limp.
  )
head(dat)

# Create body-mass normalized VLR -----------------------------------------

dat['VLRnorm'] <- dat['VLR2'] / 9.81 / dat$Mass
dat['VLRnormPk'] <- dat['VLR'] / 9.81 / dat$Mass

# look at speed of each participant across sessions. Expect even speeds
ggplot(data = dat, mapping = aes(x = Shoe, y = Speed)) + geom_point() + facet_wrap(~Subject)
# look at distributions for each subject ----------------------------------

ggplot(data = dat, mapping = aes(x = as.factor(Subject), y = VLRnormPk, fill = Shoe)) + geom_boxplot() 

ggplot(data = dat, mapping = aes(x = VLRnormPk, fill = Shoe)) + geom_density() +facet_wrap(~Subject)

# These shoe a lot of non-physiologically plausible values that I think we should remove
# Try token outlier removal -----------------------------------------------

### Outlier removal based on 25 quantile - 1.5* IQR or 75 + 1.5*IQR ###
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Level
dt2 <- dat %>%
  group_by(Subject, Shoe) %>%
  mutate(
    VLRpk = remove_outliers(VLRnormPk)
    )
#need to drop speed column since it is incomplete so far
dt2 <- subset(dt2, select = c(-Speed))
dt2 <- na.omit(dt2) #Drop outlier rows from above

#Plots look more clear
ggplot(data = dt2, mapping = aes(x = as.factor(Subject), y = VLRpk, fill = Shoe)) + geom_boxplot() 
ggplot(data = dt2, mapping = aes(x = VLRpk, fill = Shoe)) + geom_density() +facet_wrap(~Subject)


# larger model ------------------------------------------------------------
# probably the most concise model that keeps it maximal while being interpretable
fullMod <- lmer(VLRnormPk ~ Shoe * TimePeriod + (1|Subject), data = dt2)
summary(fullMod)
((92.85-4.99) - (92.85 - 0.62)) / (92.85 - 0.62) #pct change scratch work. Will change as data changes TODO write function
90.36 + 1.149 #more scratch work. 
ggplot(data = dt2, mapping = aes(x = VLRnormPk, fill = Shoe)) + geom_density() +facet_wrap(~Subject)

# Braking impulse analysis --------------------------------------------------
dt2$Bimp <- -1 * dt2$Bimp #Flip to positive, easier to interpret. 

bimpmod <- lmer(Bimp ~ Shoe * TimePeriod + (1 | Subject), data = dt2)
summary(bimpmod)

ggplot(data = dt2, mapping = aes(x = Bimp, fill = Shoe)) + geom_density() +facet_wrap(~Subject)


# Braking force analysis --------------------------------------------------
dt2$pBF <- -1 * dt2$pBF #again positive, easier for me

BFmod <- lmer(pBF ~ Shoe * TimePeriod + (1 | Subject), data = dt2)
summary(BFmod)

ggplot(data = dt2, mapping = aes(x = pBF, fill = Shoe)) + geom_density() +facet_wrap(~Subject)


# Medial force ------------------------------------------------------------
PkMdmod <- lmer(pMF ~ Shoe * TimePeriod + (1 | Subject), data = dt2)
summary(PkMdmod)
(((164.76-4.04)-(164.76-13.9))/(164.76-13.9)) * 100 # 6.5% improvment of SD over lace
((4.04)/(164.76)) * 100 # 1.02% decriment of DD to laces

ggplot(data = dt2, mapping = aes(x = pMF, fill = Shoe)) + geom_density() +facet_wrap(~Subject)

# Lateral force ------------------------------------------------------------
PkLtmod <- lmer(pLF ~ Shoe * TimePeriod + (1 | Subject), data = dt2)
summary(PkLtmod)
((22.9)/(187)) * 100 # 6.5% improvment of SD over lace
((10.43)/(187)) * 100 # 5.57% improvement DD to lace

ggplot(data = dt2, mapping = aes(x = pLF, fill = Shoe)) + geom_density() +facet_wrap(~Subject)


# longitudinal plots ------------------------------------------------------
# with error bars: hard to see the lines through the noise
dt2 %>%
  group_by(Subject, TimePeriod, Shoe) %>%
  summarize(
    AvgLR = mean(VLRpk),
    SDRate = sd(VLRpk)) %>%
  ggplot(mapping=aes(x = TimePeriod, y = AvgLR, color = Shoe, group = Shoe)) +
  geom_point() + geom_line() + geom_errorbar(aes(ymin=AvgLR-0.5*SDRate, ymax=AvgLR+0.5*SDRate), width = 0.2) +
  facet_wrap(~Subject, scales = "free") + scale_x_discrete(limits = c("'5'","'15'","'30'","'45'")) #+

# no error bars
dt2 %>%
  group_by(Subject, TimePeriod, Shoe) %>%
  summarize(
    AvgLR = mean(VLRpk),
    SDRate = sd(VLRpk)) %>%
  ggplot(mapping=aes(x = TimePeriod, y = AvgLR, color = Shoe, group = Shoe)) +
  geom_point() + geom_line() + 
  facet_wrap(~Subject, scales = "free") + scale_x_discrete(limits = c("'5'","'15'","'30'","'45'")) #+

# no error bars
dt2 %>%
  group_by(Subject, TimePeriod, Shoe) %>%
  summarize(
    AvgBF = -1 * mean(pBF),
    SDRate = sd(pBF)) %>%
  ggplot(mapping=aes(x = TimePeriod, y = AvgBF, color = Shoe, group = Shoe)) +
  geom_point() + geom_line() + 
  facet_wrap(~Subject, scales = "free") + scale_x_discrete(limits = c("'5'","'15'","'30'","'45'")) #+


# Look at 45 min ----------------------------------------------

lateDat <- subset(dt2, dt2$TimePeriod == "'45'")

#peak instantaneous
pkVLRmodLate <- lmer(VLRpk ~ Shoe + (1 | Subject), data = lateDat)
summary(pkVLRmodLate)

# Braking Force
bimpmodLate <- lmer(Bimp ~ Shoe + (1 | Subject), data = lateDat)
summary(bimpmodLate)

# Medial force
PkMdmodLate <- lmer(pMF ~ Shoe + (1 | Subject), data = lateDat)
summary(PkMdmodLate)

# Lateral force
PkLtmodLate <- lmer(pLF ~ Shoe + (1 | Subject), data = lateDat)
summary(PkLtmodLate)


# Size artifacts? ---------------------------------------------------------
# only two size 11s at this point
size11 <- subset(dt2, dt2$Size == 11)
#peak instantaneous
pkVLRmod11 <- lmer(VLRpk ~ Shoe + (1 | Subject), data = size11)
summary(pkVLRmod11)

# Braking Force
bimpmod11 <- lmer(Bimp ~ Shoe + (1 | Subject), data = size11)
summary(bimpmod11)

# Medial force
PkMdmod11 <- lmer(pMF ~ Shoe + (1 | Subject), data = size11)
summary(PkMdmod11)

# Lateral force
PkLtmodLate <- lmer(pLF ~ Shoe + (1 | Subject), data = size11)
summary(PkLtmod1)


# metabolics --------------------------------------------------------------
dat['MetNorm'] <- dat$MetEffic / dat$Mass
dat %>%
  group_by(Subject, Shoe, TimePeriod) %>%
  summarize(
    MetCost = mean(MetNorm),
    HR = mean(AvgHR)) %>%
ggplot(mapping=aes(x = TimePeriod, y = MetCost, color = Shoe, group = Shoe)) +
  geom_point() + geom_line() + 
  facet_wrap(~Subject, scales = "free") + scale_x_discrete(limits = c("'5'","'15'","'30'","'45'"))  

dat %>%
  group_by(Subject, Shoe, TimePeriod) %>%
  summarize(
    MetCost = mean(MetNorm),
    HR = mean(AvgHR)) %>%
  ggplot(mapping=aes(x = TimePeriod, y = HR, color = Shoe, group = Shoe)) +
  geom_point() + geom_line() + 
  facet_wrap(~Subject, scales = "free") + scale_x_discrete(limits = c("'5'","'15'","'30'","'45'"))  

MetDat <- dat %>%
  group_by(Subject, Shoe, TimePeriod) %>%
  summarize(
    MetCost = mean(MetNorm),
    HR = mean(AvgHR))
MetDat <- na.omit(MetDat)

MetMod <- lmer(MetCost ~ Shoe  *TimePeriod + (1 | Subject), data = MetDat)
summary(MetMod)
(.41/13.6) * 100
(.3525+.4372) / 13.8 * 100

HRmod <- lmer(HR ~ Shoe * TimePeriod + (1 | Subject), data = MetDat)
summary(HRmod)

# at time point 45 mins only
Met45 <- subset(MetDat, MetDat$TimePeriod == "'45'")
MetMod45 <- lmer(MetCost ~ Shoe + (1 | Subject), data = Met45)
summary(MetMod45)
  

# Plots for 10/1 meeting. All customized and representative ONLY. Must  --------
# be changed in future for any meetings/papers and based on updated models #
# This is for placement only and should NOT be run 'as is' without #
# updating numbres or models. This should be automated but is not #
# currently for the sake of time. 
## TODO: automate group-average plots ## 

RFD <- c(28.9, 28.7, 29.0, 33.2, 28.4, 29.5)
Cond <- c(rep('pre',3), rep('post',3))
Shoe <- c('DD','SD','SL','DD','SD','SL')

summaryRFDdat <- as.data.frame(cbind(RFD, Cond, Shoe))

ggplot(data = summaryRFDdat, mapping = aes(x = Cond, y = RFD, color = Shoe, group = Shoe)) +
         geom_point() + geom_line() + scale_x_discrete(limits = c("pre","post")) + 
        xlab('Condition') + ylab('Rate Force Dev (kN/s)')

#summary VLR dat (Taken from model, numbers must be updated as new subjects appear)
VLR <- c(91, 88.0,88.2, 92.85, 87.86, 92.23, 91.2, 90, 91.5, 92.4, 91.1, 92.0)
Cond <- c(rep(5,3), rep(15,3), rep(30,3), rep(45,3))
Shoe <-  c('DD','SD','SL','DD','SD','SL','DD','SD','SL','DD','SD','SL')
summaryVLRdat <- as.data.frame(cbind(RFD, Cond, Shoe))

ggplot(data = summaryVLRdat, mapping = aes(x = Cond, y = VLR, color = Shoe, group = Shoe)) +
  geom_point() + geom_line()  + scale_x_discrete(limits = c('5','15','30','45')) +
  xlab('Condition') + ylab('Loading Rate (BW/s)')

#summary braking force dat (Taken from model, numbers must be updated as new subjects appear)
BimpP <- c(12.4, 11.9, 12.5, 12.53, 12.63, 12.2, 12.2, 12.3, 12.5, 12.7, 12.3, 12.1)
Cond <- c(rep(5,3), rep(15,3), rep(30,3), rep(45,3))
Shoe <-  c('DD','SD','SL','DD','SD','SL','DD','SD','SL','DD','SD','SL')
summaryBrakedat <- as.data.frame(cbind(BimpP, Cond, Shoe))

ggplot(data = summaryBrakedat, mapping = aes(x = Cond, y = BimpP, color = Shoe, group = Shoe)) +
  geom_point() + geom_line() + scale_x_discrete(limits = c('5','15','30','45')) +
  xlab('Condition') + ylab('Braking Impulse (N*s)')

#Summary metabolic cost plot
MetCost <- c(13.5, 12.55, 13.7,13.76, 12.5, 13.75,13.6, 14, 14, 13.7, 13.4, 13.6)
summaryMet <- as.data.frame(cbind(MetCost, Cond, Shoe))
ggplot(data = summaryMet, mapping = aes(x = as.factor(Cond), y = MetCost, color = Shoe, group = Shoe)) +
  geom_point() + geom_line()  + scale_x_discrete(limits = c('5','15','30','45')) +
  xlab('Condition') + ylab('Metabolic Cost (W/kg)')

#S1 braking force plot
s1 <- subset(dt2, dt2$Subject == 1)
s1 %>%
  group_by(Subject, TimePeriod, Shoe) %>%
  summarize(
    AvgBF = -1 * mean(pBF),
    SDRate = sd(pBF)) %>%
  ggplot(mapping=aes(x = TimePeriod, y = AvgBF, color = Shoe, group = Shoe)) +
  geom_point() + geom_line() + 
  facet_wrap(~Subject, scales = "free") + scale_x_discrete(limits = c("'5'","'15'","'30'","'45'")) #+
