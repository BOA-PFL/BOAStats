rm(list=ls())
library(tidyverse)
library(lme4)
library(lsmeans)


# Read in and clean data --------------------------------------------------


dat <- read.csv('C:/Users/bethany.kilpatrick/OneDrive - Boa Technology Inc/Documents/PFL/DU_Qualitative_Summer2021.csv')
#dat <- read.csv('QualitativeSmall.csv')
dat$Shoe <- gsub(" ","",dat$Shoe) #Some shoes were coded with a space in front of S, gsub eliminates that

dat$Condition <- factor(dat$Condition, c( 'SL', 'DD', 'SD')) # Put baseline condition first followed by tested conditions

dat$Condition <- as.factor(dat$Condition)
dat$Subject <- as.factor(dat$Subject)



# Plot overview  ----------------------------------------------------------


dat %>%
  group_by(Condition) %>%
  summarize(
    avgScore = median(Overall, na.rm = TRUE) # "Performance.Score" to "Overall
  )


dat %>%
  group_by(Subject, Condition) %>%
  ggplot(mapping = aes(x = Condition, y = as.numeric(Overall))) + geom_point() + facet_wrap(~ Subject) 


dat %>%
  group_by(Condition) %>%
  ggplot(mapping = aes(x = Performance)) + geom_histogram(binwidth = 1) + facet_wrap(~ Condition) +
  theme(text = element_text(size=20)) + ylab('Count') + xlab('Performance Rating') + theme_bw() 
  




# Bayesian Models ---------------------------------------------------------
library(brms)
runmod <- brm(data = dat,
              family = gaussian,
              Overall ~ Condition + (1|Subject), #fixed effect of occasion, random interecept  slope for each subject
              prior = c(prior(normal(5, 10), class = Intercept),
                        prior(normal(0, 1), class = b),
                        prior(cauchy(0, 1), class = sd),
                        prior(cauchy(0, 1), class = sigma)),
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)
print(runmod)
plot(runmod)

posterior <- posterior_samples(runmod) #This extracts the posterior (grabs samples in a proportion to the probability they would be observed)
## What percent of data lies above 0?
sum(posterior$b_ConditionSD > 0) / length(posterior$b_ConditionSD) #92% of density lives above 0
mean(posterior$b_ConditionSD) #0.58 is MAP gong from lace to LR
hist(posterior$b_ConditionSD)
## What percent lies above 0 for tri
sum(posterior$b_ConditionSD > 0) / length(posterior$b_ConditionSD) # 73% of density lives above 0
mean(posterior$b_ConditionSD) #0.25 is MAP gong from lace to LR
# Y Wrap
sum(posterior$b_ConditionYWrap > 0) / length(posterior$b_ConditionYWrap) # 73% of density lives above 0
mean(posterior$b_ConditionYWrap) #0.25 is MAP gong from lace to LR

# Run models --------------------------------------------------------------
kruskal.test(Overall ~ Condition, data = dat)

mod1 <- lmer(as.numeric(Overall) ~ as.factor(Condition) + (1|Subject), data = dat)
summary(mod1)


redMod <- lmer(as.numeric(Overall) ~ (1|Subject), data = dat)
anova(mod1, redMod)

lsmeans(mod1, pairwise~Condition, adjust = "Tukey")



# LMER model --------------------------------------------------------------

# Task Time

preference.mod = lmer(Overall ~ Condition + (1|Subject), data = dat, REML = TRUE, na.action = "na.omit" )
#preference.mod = lmer(Overall ~ Condition + (1|Subject), data = dat, REML = TRUE, na.action = "na.omit" )
summary(preference.mod)
Overall = lmer(Overall ~ (1|Subject), data = dat, REML = TRUE, na.action = "na.omit" )
anova(preference.mod, preference.null)
conditions.emm <- emmeans(SkaterTaskTime.mod, "Condition")
conditions.emm
contrast(conditions.emm, "trt.vs.ctrl", ref = "SL")


