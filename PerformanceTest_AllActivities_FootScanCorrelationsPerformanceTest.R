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

<<<<<<< Updated upstream:PerformanceTest_AllActivities_FootScanCorrelationsPerformanceTest.R
#Filtering impossible contact times
testDat <- testDat %>%
   filter(ContactTime > 10) %>% 
   filter(ContactTime < 100) %>%
=======
testDat <- subset(testDat, testDat$Movement == 'CMJ')

testDat <- testDat %>% 
   filter(CT > 10) %>% #remove values with impossible contact time
   filter(CT < 120) %>%
>>>>>>> Stashed changes:footScanPerformanceTest.R
   group_by(Subject) %>%
   mutate(z_score = scale(CT)) # Change to the variable you want to test
   mutate(z_score = scale(propImpulse)) # Change to the variable you want to test

# Filtering any scores that are 2sds above or below the mean
testDat <- subset(testDat, testDat$z_score < 2)
testDat <- subset(testDat, testDat$z_score > -2)

#Defining which sides we are looking at
RfootDat <- subset(footDat, footDat$Side == 'R')
<<<<<<< Updated upstream:PerformanceTest_AllActivities_FootScanCorrelationsPerformanceTest.R
LfootDat <- subset(footDat, footDat$Side == 'L') 


#Defining the shoes we tested  
baselineDat <- subset(testDat, testDat$Config == 'Single') # Change to baseline shoe name
=======
LfootDat <- subset(footDat, footDat$Side == 'L')
 
baselineDat <- subset(testDat, testDat$Config == 'V1') # Change to baseline shoe name
>>>>>>> Stashed changes:footScanPerformanceTest.R

newShoeDat <- subset(testDat, testDat$Config == 'V2') # Change to shoe you want to compare to baseline


#  This purpose of this For Loop is finding difference for each subject between configs, and finding the relationship between performance and foot size
# Defining the subject and difference as indices
Subject <- rep(NA, 2)
diff <- rep(0, 2)

r = 1 # representing the subject

for (sub in unique(testDat$Subject)) { # Finding the uniqe names of the subjects
   
   Subject[r] <- sub #temp subject name 
   
   #Subsetting that data into baeline and new congigs
   tempbaseDat <- subset(baselineDat, baselineDat$Subject == sub) 
   tempnewDat <- subset(newShoeDat, newShoeDat$Subject == sub)
   
   diff[r] <- mean(tempnewDat$z_score) - mean(tempbaseDat$z_score) # finding the differences in variation between the two configs
   
   r = r+1 # Moving onto the next subject
   
}
# This section is binding the code into a single data frame comparing performance differences between the configs
diff <- as.numeric(diff) 
diffDat <- cbind(Subject, diff) 
diffDat <- as.data.frame(diffDat)



# join dataframes

corrDat <- inner_join (diffDat, RfootDat, by = 'Subject')
corrDat$diff <- as.numeric(corrDat$diff)
 
# Plots - Looking for correlations between foot characteristics and outcomes

ggscatter(corrDat, x = "Length (cm)", y = "diff", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = 'Foot Length', ylab = 'Change in performance from baseline') 

ggscatter(corrDat, x = "Width (cm)", y = "diff", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = 'Foot Width', ylab = 'Change in performance from baseline') 

ggscatter(corrDat, x = "Instep (cm)", y = "diff", 
          add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = 'Instep height', ylab = 'Change in performance from baseline')




######### For qualtitative Data ####################################

#Clearing the environment
rm(list=ls())

<<<<<<< Updated upstream:PerformanceTest_AllActivities_FootScanCorrelationsPerformanceTest.R
#loading in CompiledTrackMan Data
testDat <- read_excel(file.choose())
=======

testDat <- read_excel(file.choose()) # load compiled qualitative data
>>>>>>> Stashed changes:footScanPerformanceTest.R

#Loading in foot volume BigData sheet
footDat <- read_excel('C:/Users/kate.harrison/Boa Technology Inc/PFL - Documents/General/BigData2021/MasterSubjectSizes.xlsx')



#Defining which sides we are looking at
RfootDat <- subset(footDat, footDat$Side == 'R')
LfootDat <- subset(footDat, footDat$Side == 'L') 


<<<<<<< Updated upstream:PerformanceTest_AllActivities_FootScanCorrelationsPerformanceTest.R
#Defining the shoes we tested
baselineDat <- subset(testDat, testDat$Shoe == 'Lace') # Change to baseline shoe name
=======
baselineDat <- subset(testDat, testDat$Shoe == 'V1') # Change to baseline shoe name
>>>>>>> Stashed changes:footScanPerformanceTest.R

newShoeDat <- subset(testDat, testDat$Shoe == 'V2') # Change to shoe you want to compare to baseline


# finding difference for each sub between configs, finding relationship between performance and foot size
Subject <- rep(NA, 2)
diff <- rep(0, 2)

r = 1

for (sub in unique(baselineDat$Subject)) {
   
   Subject[r] <- sub
   tempbaseDat <- subset(baselineDat, baselineDat$Subject == sub)
   tempnewDat <- subset(newShoeDat, newShoeDat$Subject == sub)
   
   diff[r] <- mean(tempnewDat$OverallFit) - mean(tempbaseDat$OverallFit)
   
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
<<<<<<< Updated upstream:PerformanceTest_AllActivities_FootScanCorrelationsPerformanceTest.R
          xlab = 'Foot Length', ylab = 'Change in performance from Lace to SD')
=======
          xlab = 'Foot Length', ylab = 'Change in rating from V1 to V2')


>>>>>>> Stashed changes:footScanPerformanceTest.R
