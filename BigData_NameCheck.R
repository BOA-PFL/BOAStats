library(tidyverse)

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

## agility pressure DB
ASPress <- read.csv('Z:/BigData/DB_V2/AgilityPressureDB.csv') %>%
          select('Subject') %>%
          group_by(Subject) %>%
          count()
        

## agility speed DB
A_S <- read.csv('Z:/BigData/DB_V2/AgilitySpeedDB.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = gsub(" ", "", Subject))%>%
  count()


## cycling power DB
CycleP <- read.csv('Z:/BigData/DB_V2/CyclingPowerDB_V2.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = gsub(" ", "", Subject))%>%
  count()


## Knee Brace DB
KneeDB <- read.csv('Z:/BigData/DB_V2/KneeBraceDB.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = gsub(" ", "", Subject))%>%
  count()


# Landing DB
LandingDB <- read.csv('Z:/BigData/DB_V2/LandingDB.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = tolower(gsub(" ", "", Subject)))%>%
  count()


# Trail DB
TrailDB <- read.csv('Z:/BigData/DB_V2/TrailDB.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = tolower(gsub(" ", "", Subject)))%>%
  count()


# Walk Run DB
WalkRDB <- read.csv('Z:/BigData/DB_V2/WalkRunDB.csv')%>%
  select('Subject')%>%
  group_by(Subject) %>%
  mutate(Subject = gsub(" ", "", Subject))%>%
  count()


##### counting who came in last year (2022)
subVisits <- read.csv('Z:/BigData/DB_V2/MasterSubjectVisits.csv')%>%
  select('Subject', 'Year')%>%
  group_by(Subject) %>%
  mutate(Subject = tolower(gsub(" ", "", Subject)))%>%
  filter( Year == '2023')%>%
  count()


write.csv(subVisits, "C:/Users/milena.singletary/Boa Technology Inc/Documents/Subvisits23.csv", row.names = FALSE)
