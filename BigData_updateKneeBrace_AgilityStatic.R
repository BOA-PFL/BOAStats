### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())


replaceName <- function(DF, toReplace, newName){ 
  
  # replace incorrect subject names with new name
  DF <- DF %>% 
    mutate(Trial = replace(Trial, Trial == toReplace, newName))
  return(DF)
} 


# Read the existing database: Only to get column name order
ParentDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/BigData/DB_V2/KneeBraceDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read and summarize the overground data:
AgilityDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/EndurancePerformance/EH_KneeBracePilotII_Aug23/Overground/CompiledAgilityData_3.csv') 


CMJDat <- AgilityDat %>%
  filter(Movement == 'CMJ') %>%
  group_by(Subject, Config, Movement, Trial) %>%
  summarise(ContactTime = mean(CT), PeakAnklePFMoment = mean(peakPFmom), PropForce = mean(peakGRF_Z),
            PeakAnkleInMoment = mean(peakINVmom), KneeAbAdROM = mean(kneeABDrom),PeakKneeAbMoment = mean(PeakKneeAbMoment), 
            kneeFLEXrom = mean(kneeFLEXrom),peakKneeEXTmom = mean(peakKneeEXTmom), COMEccWork = mean(eccWork), COMConWork = mean(conWork))

SKTDat <- AgilityDat %>%
  filter(Movement == 'Skater') %>%
  group_by(Subject, Config, Movement, Trial) %>%
  summarise(ContactTime = mean(CT), PeakAnklePFMoment = mean(peakPFmom), PropForce = mean(peakGRF_X),
            PeakAnkleInMoment = mean(peakINVmom), KneeAbAdROM = mean(kneeABDrom), PeakKneeAbMoment = mean(PeakKneeAbMoment),
            kneeFLEXrom = mean(kneeFLEXrom),peakKneeEXTmom = mean(peakKneeEXTmom), COMEccWork = mean(eccWork), COMConWork = mean(conWork)) 


ChildDat <- list(CMJDat,SKTDat) %>%
  reduce(full_join)
ChildDat <- merge(x=CMJDat,y=SKTDat,all=TRUE)

#Static Brace data
braceDat <- read.csv('C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/Testing Segments/EndurancePerformance/EH_KneeBracePilotII_Aug23/Overground/Static/AllStaticBraceDat.csv')

# Renaming the delta trials 
braceDat <- braceDat %>% 
  rename(deltaOne = One)%>% 
  rename(deltaTwo = Two)%>%
  rename(deltaThree = Three)%>%
  rename(deltaFour = Four)
  

ChildDat <- merge(x=ChildDat,y=braceDat,all=TRUE)


ChildDat$Year <- rep(2023, dim(ChildDat)[1])
ChildDat$Month <- rep('August', dim(ChildDat)[1])
ChildDat$Brand <- rep('Breg', dim(ChildDat)[1])
ChildDat$Model <- rep('Fusion', dim(ChildDat)[1])

#_______________________________________________________________________________
# Place NaNs for missing data
# ChildDat$COMConWork <- rep('NA', dim(ChildDat)[1])
# ChildDat$PeakKneeAbMoment <- rep('NA', dim(ChildDat)[1])
# ChildDat$PeakAnklePFMoment <- rep('NA', dim(ChildDat)[1])
# ChildDat$PropForce <- rep('NA', dim(ChildDat)[1])
# ChildDat$PeakAnkleInMoment <- rep('NA', dim(ChildDat)[1])
# ChildDat$KneeAbAdROM <- rep('NA', dim(ChildDat)[1])
# ChildDat$COMEccWork <- rep('NA', dim(ChildDat)[1])
#_______________________________________________________________________________

# Sort the DataFrame columns into the right order (from the Parent)
ChildDat <- ChildDat[,name_order]

a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "C:/Users/bethany.kilpatrick/Boa Technology Inc/PFL - General/BigData/DB_V2/KneeBraceDB_Current.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
}
