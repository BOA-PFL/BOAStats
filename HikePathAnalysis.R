rm(list=ls())
library(ggplot2)
library(lme4)
library(tidyr)
library(dplyr)

dat <- read.csv(file.choose())
bv <- subset(dat, dat$SubName == 'BV')
## outlier removal
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


dt2 <- dat %>%
  group_by(dat$SubName) %>%
  mutate(FTI = remove_outliers(FTI))
dt2 <- na.omit(dt2)

#split into subjects
bv <- subset(dt2, dt2$SubName == 'BV')
df <- subset(dt2, dt2$SubName == 'DF')
head(bv)

#not enough data to look at all steps. 
FTImod <- lmer(dat$FTI ~ dat$Config + (1|dat$StepNo))
summary(FTImod)

hist(bv$FTI)
hist(df$FTI)

## look where differences is greatest between steps ##
bv <- subset(bv, bv$StepNo <= 24)
bv <- subset(bv, bv$Config != 'SD')
diffToLace <- bv %>% 
  arrange(StepNo, Config) %>% 
  group_by(StepNo) %>% 
  mutate(diff = c(diff(X)))

diffToLace <- subset(diffToLace, diffToLace$diff < 50)
plot(diffToLace$StepNo, diffToLace$diff) 
abline(h=0, col="blue")
#Aggregation functions
FTIavg <- aggregate(FTI ~ Config + SubName , data = dt2, FUN = mean)
names(FTIavg) <- c('Subject', 'Config', 'FTI')
FTIavg

Favg <- aggregate(force ~ Config + SubName , data = dt2, FUN = mean)
names(Favg) <- c('Subject', 'Config', 'Force')
Favg

Pavg <- aggregate(meanPressure ~ Config + SubName , data = dt2, FUN = mean)
names(Pavg) <- c('Subject', 'Config', 'MeanPressure')
Pavg

#plots
ggplot(data = bv, aes(x=as.factor(Config), y = FTI)) + geom_boxplot() + 
  geom_point() + theme_bw(base_size = 16) + xlab('Config') +
  ylab('Force-Time Integral') + labs(fill = 'Configuration')
