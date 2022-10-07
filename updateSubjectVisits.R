#### Updating Subjects Visits ###
library(tidyverse)
rm(list=ls())

# Set path to your directory
subVisits <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/DB_V2/MasterSubjectVisits.csv')
subVisits <- subVisits %>%
  rename('Subject' = ï..Subject)

# Add Subject name brand, year, month, shoe name

Subject <- 'TestSub'
Year <- '2022'
Month <- 'November'
Benefit <- 'E/H'
Brand <- 'TestBrand'
Model <- 'TestShoe'
Name.of.Test <- 'Cycling_HL_test'
Type <- 'Performance'
Speed.run. <- 3.0
Mass <- 68
Resistance <- NA


dat_to_append <- data.frame(Subject, Year, Month, Benefit, Brand, Model,
                            Name.of.Test, Type, Speed.run., Mass, Resistance)

subVisits <- rbind(subVisits, dat_to_append)

# write output. add a 1 to the end if you are at all unsure of ourput!!!
a <- winDialog(type = 'yesno', message = 'about to overwrite DB')
if (a == 'YES'){
  write.table(subVisits, "C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/DB_V2/MasterSubjectVisits.csv", sep=',')
  
}
