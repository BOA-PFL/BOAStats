library(tidyverse)
## Function
## prints name of unmatched subject referencing the master list
## reason for no match include:
# a.) incorrect spelling, b.) subject not in master visit sheet
Match = function(masterN, testingList)
  { M <- match(tolower(testingList$Subject), masterN)
    N <- which(is.na(M))
    print(testingList$Subject[N]) 
}

## code to look at data frames and identify unique subjects
## making sure spelling is correct

subVisits <- read.csv('Z:/BigData/DB_V2/MasterSubjectVisits.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = gsub(" ", "", Subject))%>%
  count()

master_Name <- tolower(subVisits$Subject)

testName <- read.csv('Z:/BigData/DB_V2/MasterSubjectVisits.csv')%>%
  select('Name.of.Test')%>%
  group_by(Name.of.Test) %>%
  mutate(Name.of.Test = gsub(" ", "", Name.of.Test))%>%
  count()

master_testName <- tolower(testName$Name.of.Test)

configs <- read.csv('Z:/BigData/DB_V2/ConfigDB.csv')%>%
  select('Name.of.Test', 'Year', 'Config')
  
## agility pressure DB
ASPress <- read.csv('Z:/BigData/DB_V2/AgilityPressureDB.csv') %>%
          select('Subject') %>%
          mutate(Subject = gsub(" ", "", Subject))%>%
          group_by(Subject) %>%
          count()

ASPress_CONFIGS <- read.csv('Z:/BigData/DB_V2/AgilityPressureDB.csv') %>%
  group_by(Year, Config, Month, Brand, Model) %>%
  count()

Match(master_Name, ASPress)

## agility speed DB
A_S <- read.csv('Z:/BigData/DB_V2/AgilitySpeedDB.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = gsub(" ", "", Subject))%>%
  count()

ASP_CONFIGS <- read.csv('Z:/BigData/DB_V2/AgilitySpeedDB.csv') %>%
  group_by(Year, Config, Month, Brand, Model) %>%
  count()

Match(master_Name, A_S)

## cycling power DB
CycleP <- read.csv('Z:/BigData/DB_V2/CyclingPowerDB_V2.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = gsub(" ", "", Subject))%>%
  count()

CycleP_CONFIGS <- read.csv('Z:/BigData/DB_V2/CyclingPowerDB_V2.csv') %>%
  group_by(Year, Config, Month, Brand, Model) %>%
  count()

Match(master_Name, CycleP)

## Knee Brace DB
KneeDB <- read.csv('Z:/BigData/DB_V2/KneeBraceDB.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = gsub(" ", "", Subject))%>%
  count()

KneeDB_CONFIGS <- read.csv('Z:/BigData/DB_V2/KneeBraceDB.csv') %>%
  group_by(Year, Config, Month, Brand, Model) %>%
  count()

Match(master_Name, KneeDB)

# Landing DB
LandingDB <- read.csv('Z:/BigData/DB_V2/LandingDB.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = tolower(gsub(" ", "", Subject)))%>%
  count()


LandingDB_CONFIGS <- read.csv('Z:/BigData/DB_V2/LandingDB.csv') %>%
  group_by(Year, Config, Month, Brand, Model) %>%
  count()

Match(master_Name, LandingDB)

# Trail DB
TrailDB <- read.csv('Z:/BigData/DB_V2/TrailDB.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = tolower(gsub(" ", "", Subject)))%>%
  count()

TrailDB_CONFIGS <- read.csv('Z:/BigData/DB_V2/TrailDB.csv') %>%
  group_by(Year, Config, Month, Brand, Model) %>%
  count()

Match(master_Name, TrailDB)

# Walk Run DB
WalkRDB <- read.csv('Z:/BigData/DB_V2/WalkRunDB.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = gsub(" ", "", Subject))%>%
  count()

WalkRDB_CONFIGS <- read.csv('Z:/BigData/DB_V2/WalkRunDB.csv') %>%
  group_by(Year, Config, Month, Brand, Model) %>%
  count()

Match(master_Name, WalkRDB)

##### counting who came in last year (2023)
subVisits <- read.csv('Z:/BigData/DB_V2/MasterSubjectVisits.csv')%>%
  select('Subject', 'Year')%>%
  group_by(Subject) %>%
  mutate(Subject = tolower(gsub(" ", "", Subject)))%>%
  filter( Year == '2023')%>%
  count()

# use to write csv locally
#write.csv(subVisits, "C:/Users/milena.singletary/Boa Technology Inc/Documents/Subvisits23.csv", row.names = FALSE)
