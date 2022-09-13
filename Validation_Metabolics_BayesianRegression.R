## Bayesain Regression

rm(list=ls())
#load libraries
library(rstan)
rstan_options(auto_write = TRUE)
library(dplyr)




## fake data
x <- rnorm(40, 10, 5)
z <- rnorm(40, 0, 1)



pred <- data.frame(x = x, z = z)
noise <- rnorm(40,0,1)
y <- x*.5 + z * 0.25 + noise
data <- list( x= as.matrix(pred), y = y, N = length(x), K = 2L)

mult_linear_regression <- stan_model("C:/Users/kate.harrison/Documents/BOAGitHub/BOAStats/stan_multiple_linear_regression.stan")


fit1 <- sampling(mult_linear_regression, data = data, chains = 2, iter = 2000, refresh = 0)

print(fit1, pars = "beta", probs = c(0.025, 0.5, 0.975))

traceplot(fit1)

pairs(fit1)


#real data

regDat <- read.csv(file.choose())

regDat <- subset(regDat, select = -c(Config, Mass, Sex))

pred <- regDat[1:19]

pred <- pred [2:19] # Dropping Subject. Will build hierarchical model later. 

data <- list(x = as.matrix(pred), y = regDat$EE_dist, N = nrow(pred), K = 18L)

mult_linear_regression <- stan_model("C:/Users/kate.harrison/Documents/BOAGitHub/BOAStats/stan_multiple_linear_regression.stan")

realDataFit <- sampling(mult_linear_regression, data = data, chains = 2, iter = 2000, refresh = 0, warmup = 1000, control = list(adapt_delta = .975, max_treedepth = 20))

print(realDataFit, pars = "beta", probs = c(0.025, 0.975))

traceplot(realDataFit, pars = "theta")

traceplot(realDataFit, pars = "beta")

pairs(realDataFit, pars = "theta")
