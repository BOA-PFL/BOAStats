rm(list=ls())

library(tidyverse)
library(lme4)

dat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/FootScan Data/DataPortal_data.csv')
dat <- dat %>%
  rename(Location = 'ï..Location')

dat$Location <- as.factor(dat$Location)
dat$Gender <- as.factor(dat$Gender)

ggplot(data = dat, aes(x = Size, Avg..Length, color = Location)) + geom_point() +
  geom_jitter() + facet_wrap(~Gender) + scale_x_continuous(limits=c(5,13)) +
  ylab('Length') + xlab('Size')
## too hard to see things in the above plot ###
dat <- dat %>%
  mutate(Region = ifelse(Location %in% c("Brazil","Mexico","Puerto Rico","Canada","United States"),"Americas", 
                         ifelse(Location %in% c("China","Indonesia","Japan","Thailand"),"Asia",
                         ifelse(Location %in% c("Denmark","Germany","Netherlands","United Kingdom"),"Europe","Other")
                         )))
### too reduced plot below ##
dat %>%
  group_by(Region, Size, Gender) %>%
  summarize(avgLength = mean(Avg..Length))%>%
ggplot(aes(x = Size, avgLength, color = Region)) + geom_point() +
  geom_jitter() + facet_wrap(~Gender) + scale_x_continuous(limits=c(5,13)) +
  ylab('Length') + xlab('Size')

##### Final plots ###
createPlot <- function(df, sex, col, title){
  if (sex == 'Male'){
    df %>%
      filter(Gender == 'Male')%>%
      filter(Size > 6.5 & Size < 14.5)%>%
      ggplot(aes(x = as.factor(Size), .data[[col]], color = Region)) + geom_boxplot() +
      facet_wrap(~Gender) + ylab(paste0(title)) + xlab('Size') 
  }
  else if (sex == 'Female'){
    dat %>%
      filter(Gender == 'Female')%>%
      filter(Size > 5.5 & Size < 11.5)%>%
      ggplot(aes(x = as.factor(Size), .data[[col]], color = Region)) + geom_boxplot() +
      facet_wrap(~Gender) + ylab(paste0(title)) + xlab('Size')
  }
}

lmer(Avg..Length ~ Size + Region + (1|Gender), data = dat)

createPlot(dat, 'Male','Avg..Length','Length')
createPlot(dat, 'Female','Avg..Length','Length')

createPlot(dat, 'Male','Avg..Width','Width')
createPlot(dat, 'Female','Avg..Width','Width')
lmer(Avg..Width ~ Size + Region + (1|Gender), data = dat)

createPlot(dat, 'Male','Avg..DorsalHeight','DorsalHeight (cm)')
createPlot(dat, 'Female','Avg..DorsalHeight','DorsalHeight (cm)')
lmer(Avg..DorsalHeight ~ Size + Region + (1|Gender), data = dat)

createPlot(dat, 'Male','Avg..InstepWidth','InstepWidth (cm)')
createPlot(dat, 'Female','Avg..InstepWidth','InstepWidth (cm)')

createPlot(dat, 'Male','Avg..Girth','Girth (cm)')
createPlot(dat, 'Female','Avg..Girth','Girth (cm)')

createPlot(dat, 'Male','Avg..HeelWidth','Heel Width (cm)')
createPlot(dat, 'Female','Avg..Width','Heel Width (cm)')


# average values ----------------------------------------------------------
boaDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/FootScan Data/MasterSubjectSizes_Male.csv')

sumDat <- dat %>%
  group_by(Region, Size, Gender) %>%
  summarize(
    meanLen = mean(Avg..Length),
    sdLen = mean(Std..Dev..Length)
  )


createLayeredPlot <- function(aetrexDat, boaDat, Sex, Size, Region){

  ## Regions: Americas, Asia, Europe, Other
  ## Sex: Male or Female
  ## Size: numeric with 1 decimal (e.g. 10.0)

  tmpDat <- filter(aetrexDat, Region == 'Americas' & Gender == 'Male' & Size == 10.5) 
  #tmpDat <- filter(aetrexDat, Region == Region & Gender == Sex & Size == Size) 

  
  boa10s <- boaDat %>%
    group_by(ShoeSize, Sex)%>%
    filter(ShoeSize == Size)%>%
    summarize(
      meanLen = mean(Length),
      sdLen = sd(Length)
    )
  
  boaSimp <- boaDat %>%
    group_by(ShoeSize, Sex)%>%
    filter(ShoeSize == Size)
  
  ggplot() + 
    stat_function(fun = ~ dnorm(.x, tmpDat$meanLen, tmpDat$sdLen), geom = "area",
                  fill = "deepskyblue4", alpha = 0.5, color = "black") +
    theme_bw(base_size = 16) + xlim(c(25,28)) +
    
    stat_function(fun = ~ dnorm(.x, boa10s$meanLen, boa10s$sdLen), geom = "area",
                  fill = "red", alpha = 0.5, color = "black") +
    theme_bw(base_size = 16) 
}


createLayeredPlot(sumDat, boaDat, 'Male', 10.5, 'Americas')

## male widths ###

sumWidthDat <- dat %>%
  group_by(Region, Size, Gender) %>%
  summarize(
    meanLen = mean(Avg..Width),
    sdLen = mean(Std..Dev..Width)
  )


createLayeredWidthPlot <- function(aetrexDat, boaDat, Sex, Size, Region){
  
  ## Regions: Americas, Asia, Europe, Other
  ## Sex: Male or Female
  ## Size: numeric with 1 decimal (e.g. 10.0)
  
  tmpDat <- aetrexDat %>%
    filter(Region == 'Americas' & Gender == 'Male' & Size == 10) 
  #filter(Region == Region & Gender == Sex & Size == Size) 
  
  boaSum <- boaDat %>%
    group_by(ShoeSize)%>%
    filter(ShoeSize == Size)%>%
    summarize(
      meanLen = mean(Width),
      sdLen = sd(Width)
    )

  
  ggplot() + 
    stat_function(fun = ~ dnorm(.x, tmpDat$meanLen, tmpDat$sdLen), geom = "area",
                  fill = "deepskyblue4", alpha = 0.5, color = "black") +
    theme_bw(base_size = 16) +
    
    stat_function(fun = ~ dnorm(.x, boaSum$meanLen, boaSum$sdLen), geom = "area",
                  fill = "red", alpha = 0.5, color = "black") +
    theme_bw(base_size = 16) + 
    xlim(c(5, 15)) 
}


createLayeredWidthPlot(sumWidthDat, boaDat, 'Male', 10, 'Americas')


### Female Dat ###
boaFDat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL Team - General/BigData/FootScan Data/MasterSubjectSizes_Female.csv')

sumDat <- dat %>%
  group_by(Region, Size, Gender) %>%
  summarize(
    meanLen = mean(Avg..Length),
    sdLen = mean(Std..Dev..Length)
  )


createLayeredPlot2 <- function(aetrexDat, boaDat, Sex, Size, Region){
  
  ## Regions: Americas, Asia, Europe, Other
  ## Sex: Male or Female
  ## Size: numeric with 1 decimal (e.g. 10.0)
  
  tmpDat <- aetrexDat %>%
    filter(Region == 'Americas' & Gender == 'Female' & Size == 9) 
    #filter(Region == Region & Gender == Sex & Size == Size) 
  
  boa10s <- boaDat %>%
    group_by(ShoeSize, Sex)%>%
    filter(ShoeSize == Size)%>%
    summarize(
      meanLen = mean(Length),
      sdLen = sd(Length)
    )
  
  boaSimp <- boaDat %>%
    group_by(ShoeSize, Sex)%>%
    filter(ShoeSize == Size)
  
  ggplot() + 
    stat_function(fun = ~ dnorm(.x, tmpDat$meanLen, tmpDat$sdLen), geom = "area",
                  fill = "deepskyblue4", alpha = 0.5, color = "black") +
    theme_bw(base_size = 16) + xlim(c(23,28)) +
    
    stat_function(fun = ~ dnorm(.x, boa10s$meanLen, boa10s$sdLen), geom = "area",
                  fill = "red", alpha = 0.5, color = "black") +
    theme_bw(base_size = 16) 
}

createLayeredPlot2(sumDat, boaDat, 'Female', 9, 'Americas')

## Female Widths ##

sumWidthDatFem <- dat %>%
  group_by(Region, Size, Gender) %>%
  summarize(
    meanLen = mean(Avg..Width),
    sdLen = mean(Std..Dev..Width)
  )


createLayeredWidthPlot2 <- function(aetrexDat, boaDat, Sex, Size, Region){
  
  ## Regions: Americas, Asia, Europe, Other
  ## Sex: Male or Female
  ## Size: numeric with 1 decimal (e.g. 10.0)
  
  tmpDat <- aetrexDat %>%
    filter(Region == 'Americas' & Gender == 'Female' & Size == 8.5) 
  #filter(Region == Region & Gender == Sex & Size == Size) 
  
  boaSum <- boaDat %>%
    group_by(ShoeSize)%>%
    filter(ShoeSize == Size)%>%
    summarize(
      meanLen = mean(Width),
      sdLen = sd(Width)
    )
  
  
  ggplot() + 
    stat_function(fun = ~ dnorm(.x, tmpDat$meanLen, tmpDat$sdLen), geom = "area",
                  fill = "deepskyblue4", alpha = 0.5, color = "black") +
    theme_bw(base_size = 16) +
    
    stat_function(fun = ~ dnorm(.x, boaSum$meanLen, boaSum$sdLen), geom = "area",
                  fill = "red", alpha = 0.5, color = "black") +
    theme_bw(base_size = 16) + 
    xlim(c(5, 15)) 
}


createLayeredWidthPlot2(sumWidthDatFem, boaFDat, 'Female', 8.5, 'Americas')

# Create distributions ----------------------------------------------------

lenDat <- dat[,1:21]
lenDat <- lenDat %>%
  mutate(Region = ifelse(Location %in% c("Brazil","Mexico","Puerto Rico","Canada","United States"),"Americas", 
                         ifelse(Location %in% c("China","Indonesia","Japan","Thailand"),"Asia",
                                ifelse(Location %in% c("Denmark","Germany","Netherlands","United Kingdom"),"Europe","Other")
                         )))
lenDat <- subset(lenDat, select=-c(Avg..Length, Std..Dev..Length))

lenDat <- lenDat %>%
  pivot_longer(
              cols = Length.10th:Length.95th,
              names_to=c("char","Decile"),
              names_sep = "//.",
              values_to="Length"
    )%>%
  separate(col = Decile, into = c('numDec', 'b'), sep = 't')
lenDat <- subset(lenDat, select=-c(char,b))
lenDat$numDec <- as.numeric(lenDat$numDec)

## Trying to create density plot... not working 
male10 <- subset(lenDat, lenDat$Gender == 'Male' & lenDat$Size == 10 & lenDat$Region == 'Americas')
m10Q <- male10 %>%
  group_by(numDec)%>%
  summarize(mean = mean(Length))

lengthOfDat <- dim(m10Q)[1]
x <- seq(m10Q$mean[1], m10Q$mean[lengthOfDat], length.out=100)
y <- dnorm(x, 26.7, 2)
plot(x, y, col = "red")
