library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(lme4)
library(readxl)
library(ggpubr)

rm(list=ls())

#loading in test data
testDat <- read_csv(file.choose())

#Loading in the BigData Footvolume data set
footDat <- read_excel('C:/Users/kate.harrison/Boa Technology Inc/PFL - Documents/General/BigData2021/MasterSubjectSizes.xlsx')

# Defining the movement we're looking at
testDat <- subset(dat, dat$Movement == 'CMJ')

#Filtering impossible contact times
testDat <- testDat %>%
   filter(ContactTime > 10) %>% 
   filter(ContactTime < 100) %>%
   group_by(Subject) %>%
   mutate(z_score = scale(CT)) # Change to the variable you want to test

# Filtering any scores that are 2sds above or below the mean
testDat <- subset(testDat, testDat$z_score < 2)
testDat <- subset(testDat, testDat$z_score > -2)

#Defining which sides we are looking at
RfootDat <- subset(footDat, footDat$Side == 'R')
LfootDat <- subset(footDat, footDat$Side == 'L') 


#Defining the shoes we tested  
baselineDat <- subset(testDat, testDat$Config == 'Single') # Change to baseline shoe name

newShoeDat <- subset(testDat, testDat$Config == 'Paired') # Change to shoe you want to compare to baseline

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
corrDat$diff <- as.numeric(corrDat$diff)
 
# Find correlations between foot characteristics and outcomes

ggscatter(corrDat, x = "Length (cm)", y = "diff", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = 'Foot Length', ylab = 'Change in performance from Single to Paired') 

ggscatter(corrDat, x = "Width (cm)", y = "diff", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = 'Foot Width', ylab = 'Change in performance from Single to Paired') 

ggscatter(corrDat, x = "Instep (cm)", y = "diff", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = 'Instep height', ylab = 'Change in performance from Single to Paired')




######### For golf ####################################

#Clearing the environment
rm(list=ls())

#loading in CompiledTrackMan Data
testDat <- read_excel(file.choose())

#Loading in foot volume BigData sheet
footDat <- read_excel('C:/Users/kate.harrison/Boa Technology Inc/PFL - Documents/General/BigData2021/MasterSubjectSizes.xlsx')



#Defining which sides we are looking at
RfootDat <- subset(footDat, footDat$Side == 'R')
LfootDat <- subset(footDat, footDat$Side == 'L') 


#Defining the shoes we tested
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


# join foot volume and trackman dataframes
corrDat <- inner_join (diffDat, RfootDat, by = 'Subject')


# Find correlations between foot characteristics and outcomes
corrDat[,2] <- as.numeric(corrDat[,2])

ggscatter(corrDat, x = "Length (cm)", y = "diff", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = 'Foot Length', ylab = 'Change in performance from Lace to SD')
