### Appending new biomech data to big data ###
library(tidyverse)
rm(list=ls())


replaceName <- function(DF, toReplace, newName){ 
  
  # replace incorrect subject names with new name
  DF <- DF %>% 
    mutate(Trial = replace(Trial, Trial == toReplace, newName))
  return(DF)
} 




Year <- '2025'
Month <- 'August'
Brand <- 'STOKO'
Model <- 'SupportiveTight'
TestName <- '2025_Performance_CompressionKneeStability_Stoko'
Benefit <- 'S&C'
Type <- 'Mechanistic' 



# Read the existing database: Only to get column name order
ParentDat <- read.csv('\\\\boa-srv10\\PFL-DATA\\BigData\\DB_V2\\KneeBraceDB.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)

# Read and summarize the overground data:
AgilityDat <- read.csv('C:\\Users\\bethany.kilpatrick\\BOA Technology Inc\\PFL Team - General\\Testing Segments\\AgilityPerformanceData\\2025_Performance_CompressionKneeStability_Stoko\\Overground\\0_CompiledAgilityData.csv') 


CMJDat <- AgilityDat %>%
  filter(Movement == 'CMJ') %>%
  group_by(Subject, Config, Movement, Order) %>%
  summarise(ContactTime = mean(CT), PeakAnklePFMoment = mean(peakPFmom), PropForce = mean(peakGRF_Z),
            PeakAnkleInMoment = mean(peakINVmom), KneeAbAdROM = mean(kneeABDrom),PeakKneeAbMoment = mean(PeakKneeAbMoment), 
            kneeFLEXrom = mean(kneeFLEXrom),peakKneeEXTmom = mean(peakKneeEXTmom), COMEccWork = mean(eccWork), COMConWork = mean(conWork))

SKTDat <- AgilityDat %>%
  filter(Movement == 'Skater') %>%
  group_by(Subject, Config, Movement, Order) %>%
  summarise(ContactTime = mean(CT), PeakAnklePFMoment = mean(peakPFmom), PropForce = mean(peakGRF_X),
            PeakAnkleInMoment = mean(peakINVmom), KneeAbAdROM = mean(kneeABDrom), PeakKneeAbMoment = mean(PeakKneeAbMoment),
            kneeFLEXrom = mean(kneeFLEXrom),peakKneeEXTmom = mean(peakKneeEXTmom), COMEccWork = mean(eccWork), COMConWork = mean(conWork)) 


ChildDat <- list(CMJDat,SKTDat) %>%
  reduce(full_join)
ChildDat <- merge(x=CMJDat,y=SKTDat,all=TRUE)

#Static Brace data
# braceDat <- read.csv('Z:/Testing Segments/EndurancePerformance/EH_KneeBrace_ThighCalfImportance_Oct23/Overground/Static/AllStaticBraceDat.csv')

# Renaming the delta trials 
# braceDat <- braceDat %>% 
#   rename(deltaOne = One)%>% 
#   rename(deltaTwo = Two)%>%
#   rename(deltaThree = Three)%>%
#   rename(deltaFour = Four)


ChildDat$deltaOne <- rep('NA', dim(ChildDat)[1])
ChildDat$deltaTwo  <- rep('NA', dim(ChildDat)[1])
ChildDat$deltaThree <- rep('NA', dim(ChildDat)[1])
ChildDat$deltaFour <- rep('NA', dim(ChildDat)[1])
ChildDat$s_One <- rep('NA', dim(ChildDat)[1])
ChildDat$s_Two <- rep('NA', dim(ChildDat)[1])
ChildDat$s_Three <- rep('NA', dim(ChildDat)[1])
ChildDat$s_Four <- rep('NA', dim(ChildDat)[1])




# ChildDat <- merge(x=ChildDat,y=braceDat,all=TRUE)
# 
# 
# ChildDat$Year <- rep(2023, dim(ChildDat)[1])
# ChildDat$Month <- rep('October', dim(ChildDat)[1])
# ChildDat$Brand <- rep('Breg', dim(ChildDat)[1])
# ChildDat$Model <- rep('Fusion', dim(ChildDat)[1])

ChildDat$Year <- rep(Year, dim(ChildDat)[1])
ChildDat$Month <- rep(Month, dim(ChildDat)[1])
ChildDat$Brand <- rep(Brand, dim(ChildDat)[1])
ChildDat$Model <- rep(Model, dim(ChildDat)[1])


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
  write.table(ChildDat, "Z:/BigData/DB_V2/KneeBraceDB.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
} 


######### Sub Visits BD ############

# Read the existing database: Only to get column name order
ParentDat <- read.csv('Z:/BigData/DB_V2/MasterSubjectVisits.csv',nrows=1)
# ParentDat <- ParentDat %>%
#   rename('Subject' = ?..Subject)
name_order = colnames(ParentDat)
# Read in qual sheet to reference names
ChildDat <- read_xlsx('C:\\Users\\bethany.kilpatrick\\Boa Technology Inc\\PFL - General\\Testing Segments\\EndurancePerformance\\EH_KneeBrace_ThighCalfImportance_Oct23\\CompiledQualData_KneeBrace_Oct23.xlsx',sheet = 'Sheet4')
ChildDat <- subset(ChildDat,select = -c(FootScan,Ht,Age,THICirc,ShankCirc,Swag,BraceSize))
# ChildDat <- ChildDat %>% rename(Speed.run. = RunSpeed)
noSub <- length(ChildDat$Subject)
ChildDat$Year <- rep(Year, each = noSub)
ChildDat$Month <- rep(Month, each = noSub)
ChildDat$Brand <- rep(Brand, each = noSub)
ChildDat$Model <- rep(Model, each = noSub)
ChildDat$Name.of.Test <- rep(TestName, each = noSub)
ChildDat$Benefit <- rep(Benefit, each = noSub)
ChildDat$Type <- rep(Type, each = noSub)
ChildDat$Resistance <- rep('NA', each = noSub)
ChildDat$Speed.run. <- rep('1', each = noSub)

# Sort the data into the correct order
ChildDat <- ChildDat[,name_order]

# write output. add a 1 to the end if you are at all unsure of output!!!
a <- winDialog(type = 'yesno', message = 'Have you checked the Child Dataframe?')
if (a == 'YES'){
  write.table(ChildDat, "Z:/BigData/DB_V2/MasterSubjectVisits.csv", sep=',', 
              append = TRUE,col.names = FALSE, row.names = FALSE)
  
}
rm(ParentDat,ChildDat,noSub,name_order,a)




