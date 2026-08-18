rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(patchwork)
library(effsize)
library(ggplot2)
library(performance)
library(broom.mixed)
library(glmmTMB)
library(dplyr)
library(bayestestR)
library(brms)
library(bayestestR)

#-------------------------------------------------------------------------------
# The purpose of this code is to examine A&S databases and compare metrics
# against one another
#-------------------------------------------------------------------------------



InLab <- read.csv('C:/Users/max.ferguson/OneDrive - BOA Technology Inc/Documents/TrailAnkleMetrics/InLab/InLabIMUmetrics_wInversionInLab.csv',       fileEncoding = "UTF-8-BOM")
Trail <- read.csv('C:/Users/max.ferguson/OneDrive - BOA Technology Inc/Documents/TrailAnkleMetrics/Trail/IMUmetrics_app.csv',       fileEncoding = "UTF-8-BOM")





GYRO_THRESHOLD <- -300

burden <- function(df, Config, Subject, Label) {
  df %>%
    group_by(Config, Subject,Label) %>%
    summarise(
      n_risky = sum(pIgyro < -300),
      n_total = n(),
      pct_risky = n_risky/n_total,
      burden  = (sum(pIgyro[pIgyro < -300]) / n())*-1,
      .groups = "drop"
    )
}


InLab <- InLab %>%
  mutate(risky = as.integer(pIgyro < GYRO_THRESHOLD))

Trail <- Trail %>%
  filter(!Subject %in% "S22")%>%
  filter(!Label %in% "0")%>%
  mutate(risky = as.integer(pIgyro < GYRO_THRESHOLD))

InLabBurden <- burden(InLab,Config)
TrailBurden <- burden(Trail,Config)


# number of participants with less risky steps/n steps
BestConfig <- TrailBurden %>%
  group_by(Subject) %>%
  slice_min(pct_risky, n = 1, with_ties = FALSE) %>%
  ungroup()

BestCounts <- count(BestConfig, Config, name = "n_subjects")


#LMM for risky steps  

TrailMod <- glmmTMB(risky ~ Config + (1|Subject), data = Trail, family = binomial)
summary(TrailMod)$coefficients 
tidy(TrailMod, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)

#burdenpaired <- t.test(TrailBurden$burden[TrailBurden$Config == "lace"],
 #      TrailBurden$burden[TrailBurden$Config == "pfs"],
  #     paired = TRUE)


tt <- tidy(TrailMod, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)")           # OR of intercept isn't interpretable here



library(lmerTest)
my_mod <- lmer(burden ~ Config + (1|Subject), data = TrailBurden,
               REML = TRUE, na.action = "na.omit")
summary(my_mod)
tidy(my_mod, effects = "fixed", conf.int = TRUE)
#------------------------------------------------
# Baysian

# Burden

#priors <- c(
 # prior(normal(0, 1),      class = Intercept),
#  prior(normal(0, 1),      class = b),       # config effect
 # prior(exponential(0.15), class = sd),
#  prior(exponential(0.15), class = sigma)
#)



#bmod <- brm(burden ~ Config + (1|Subject),
 #           data = TrailBurden, prior = priors,
  #          chains = 4, iter = 4000, warmup = 1000,
   #         cores = 1, seed = 1)


# Risky steps

TrailAgg <- Trail %>%
  group_by(Subject, Config) %>%
  summarise(n_risky = sum(risky), n_total = n(), .groups = "drop")

priors_bin <- c(
  prior(normal(-4, 1.5), class = Intercept),   # log-odds; -4 ≈ 1.8% baseline
  prior(normal(0, 0.5),  class = b),           # OR mostly within 0.37–2.7
  prior(exponential(1),  class = sd)
)



bmod_bin <- brm(n_risky | trials(n_total) ~ Config + (1|Subject),
                data = TrailAgg, family = binomial(),
                prior = priors_bin, chains = 4, iter = 4000,
                warmup = 1000, cores = 1, seed = 1,
                file = "bmod_bin")

#summary(bmod)

summary(bmod_bin)


describe_posterior(bmod_bin, ci = 0.95, rope_range = c(-0.81, 0.81), rope_ci = 1)
p_direction(bmod_bin)



#------------------------------------------------
# Visualizations


ggplot(tt, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.8) +
  labs(x = "Odds ratio (risky step vs. lace)", y = NULL,
       title = "Effect of Config on odds of a high-inversion step") +
  theme_minimal(base_size = 12)

# Trail (right foot only, as you filtered it)
p_trail <- ggplot(Trail, aes(pIgyro)) +
  geom_histogram(bins = 80, fill = "darkorange", colour = "white", linewidth = .2) +
  geom_vline(xintercept = -300, linetype = "dashed") +
  labs(x = "Inversion velocity (deg/s)", y = "Stride count",
       title = sprintf("Trail (n = %d)", nrow(Trail))) +
  theme_minimal(base_size = 12)

p_inlab <- ggplot(InLab, aes(pIgyro)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "white", linewidth = .2) +
  geom_vline(xintercept = -300, linetype = "dashed") +
  labs(x = "Inversion velocity (deg/s)", y = "Stride count",
       title = sprintf("In-Lab (n = %d)", nrow(InLab))) +
  theme_minimal(base_size = 12)

p_trail; p_inlab