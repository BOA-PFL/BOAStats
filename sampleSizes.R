rm(list=ls())


# football field example --------------------------------------------------
set.seed(5280)

# Heads or tails with 3 people doing 10 flips each
noSubs <- 3
noReps <- 100
noTosses <- 6
p3 <- matrix(nrow = noSubs, ncol = noTosses)
meansArray <- numeric(noReps)
for (repli in 1:noReps) {
  for (i in 1:noSubs) {
    p3[i,] <- sample(c(0,1), size = noTosses, prob = c(0.5,0.5), replace = TRUE)
  }
  meansArray[repli] <- mean(rowSums(p3))
}

hist(meansArray)
length(meansArray[meansArray <= 2 | meansArray >= 4])
length(meansArray[meansArray <= 1 | meansArray >= 5])
length(meansArray[meansArray == 3 | meansArray == 3])

# 10 people

noSubs <- 10
noReps <- 100
noTosses <- 6
p3 <- matrix(nrow = noSubs, ncol = noTosses)
meansArray <- numeric(noReps)
for (repli in 1:noReps) {
  for (i in 1:noSubs) {
    p3[i,] <- sample(c(0,1), size = noTosses, prob = c(0.5,0.5), replace = TRUE)
  }
  meansArray[repli] <- mean(rowSums(p3))
}

hist(meansArray)
length(meansArray[meansArray <= 2 | meansArray >= 4])
length(meansArray[meansArray <= 1 | meansArray >= 5])
length(meansArray[meansArray == 3 | meansArray == 3])

# 30 people
noSubs <- 30
noReps <- 100
noTosses <- 6
p3 <- matrix(nrow = noSubs, ncol = noTosses)
meansArray <- numeric(noReps)
for (repli in 1:noReps) {
  for (i in 1:noSubs) {
    p3[i,] <- sample(c(0,1), size = noTosses, prob = c(0.5,0.5), replace = TRUE)
  }
  meansArray[repli] <- mean(rowSums(p3))
}

hist(meansArray)
length(meansArray[meansArray <= 2 | meansArray >= 4])
length(meansArray[meansArray <= 1 | meansArray >= 5])
length(meansArray[meansArray == 3 | meansArray == 3])
