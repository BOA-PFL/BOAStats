library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(lme4)
library(readxl)
library(ggpubr)

rm(list=ls())

#load compiled outcomes

testDat <- read_csv(file.choose())


footDat <- read_excel('C:/Users/kate.harrison/Boa Technology Inc/PFL - Documents/General/BigData2021/MasterSubjectSizes.xlsx')


# split foot size data into left and right 



#testDat <- subset(dat, dat$Movement == 'CMJ')

testDat <- testDat %>% 
   #filter(ContactTime > 10) %>% #remove values with impossible contact time
   #filter(ContactTime < 100) %>%
   group_by(Subject) %>%
   mutate(z_score = scale(VLR)) # Change to the variable you want to test

testDat <- subset(testDat, testDat$z_score < 3)
testDat <- subset(testDat, testDat$z_score > -3)


ggplot(data = testDat, aes(x = VLR)) + geom_histogram() + facet_wrap(~Subject)

RfootDat <- subset(footDat, footDat$Side == 'R')
LfootDat <- subset(footDat, footDat$Side == 'L')
 
baselineDat <- subset(testDat, testDat$Config == 'Lace') # Change to baseline shoe name

newShoeDat <- subset(testDat, testDat$Config == 'SD') # Change to shoe you want to compare to baseline

Subject <- rep(NA, 2)
diff <- rep(0, 2)

r = 1

for (sub in unique(testDat$Subject)) {
   
   Subject[r] <- sub
   tempbaseDat <- subset(baselineDat, baselineDat$Subject == sub)
   tempnewDat <- subset(newShoeDat, newShoeDat$Subject == sub)
   
   diff[r] <- mean(tempnewDat$z_score) - mean(tempbaseDat$z_score)
   
   r = r+1
   
}

diff <- as.numeric(diff)
diffDat <- cbind(Subject, diff)
diffDat <- as.data.frame(diffDat)


# join dataframes

corrDat <- inner_join (diffDat, RfootDat, by = 'Subject')

 
# Find correlations between foot characteristics and outcomes

corrDat[,2] <- as.numeric(corrDat[,2])
 
cor.test(corrDat$`Length (cm)`, corrDat$diff, method = 'pearson')

ggscatter(corrDat, x = "Length (cm)", y = "diff", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = 'Foot Length', ylab = 'Change in performance from Lace to SD') 



######### For golf ####################################

rm(list=ls())

#load compiled outcomes

testDat <- read_excel(file.choose())


footDat <- read_excel('C:/Users/kate.harrison/Boa Technology Inc/PFL - Documents/General/BigData2021/MasterSubjectSizes.xlsx')




RfootDat <- subset(footDat, footDat$Side == 'R')
LfootDat <- subset(footDat, footDat$Side == 'L')

baselineDat <- subset(testDat, testDat$Shoe == 'Lace') # Change to baseline shoe name

newShoeDat <- subset(testDat, testDat$Shoe == 'A') # Change to shoe you want to compare to baseline

Subject <- rep(NA, 2)
diff <- rep(0, 2)

r = 1

for (sub in unique(baselineDat$Subject)) {
   
   Subject[r] <- sub
   tempbaseDat <- subset(baselineDat, baselineDat$Subject == sub)
   tempnewDat <- subset(newShoeDat, newShoeDat$Subject == sub)
   
   diff[r] <- mean(tempnewDat$Overall) - mean(tempbaseDat$Overall)
   
   r = r+1
   
}

diff <- as.numeric(diff)
diffDat <- cbind(Subject, diff)
diffDat <- as.data.frame(diffDat)


# join dataframes

corrDat <- inner_join (diffDat, RfootDat, by = 'Subject')


# Find correlations between foot characteristics and outcomes

corrDat[,2] <- as.numeric(corrDat[,2])

ggscatter(corrDat, x = "Length (cm)", y = "diff", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = 'Foot Length', ylab = 'Change in performance from Lace to SD')