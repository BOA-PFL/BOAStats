library(tidyverse)
library(readxl)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(readxl)
library(brms)
library(patchwork)
library(tidyr)


#Clearing the environment
rm(list=ls())

###############

# Setting up  "Best Of" line plots 
withinSubQualPlot <- function(inputDF) {
  
  # direction can be 'lower' or higher'. It is the direction of change that is better. 
  # For example, for contact time lower is better. so we put 'lower'. for jump height, higher is better, so we put higher. 
  
  whichConfig <- inputDF %>%
    group_by(Subject) %>%
    summarize(
      BestConfig = Config[which.max(OverallFit)]
    )
  
  whichConfig <- merge(inputDF, whichConfig)
  
  ggplot(data = whichConfig, mapping = aes(x = as.factor(Config), y = OverallFit, col = BestConfig, group = Subject)) + geom_point(size = 4) + 
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 40)) + ylab('Rating') 
  
}


extractVals <- function(dat, mod, configNames, var, dir) {
  
  #configNames = otherConfigs
  #mod = runmod
  #dir = 'higher'
  #var = 'CarryFlatLength'
  
  Config = rep(NA, length(configNames))
  ProbImp = matrix(0, length(configNames))
  lowCI = matrix(0, length(configNames))
  highCI = matrix(0, length(configNames))
  
  for (i in 1:length(configNames)) {
    # This function takes the original dataframe (dat, same one entered into runmod), the Bayesian model from brms (runmod), 
    # the configuration Name, and the variable you are testing. It returns:
    # [1] the probabality the variable was better in the test config vs. the baseline config
    # [3] the lower bound of the bayesian 95% posterior interval (as percent change from baseline) 
    # [4] the upper bound of the bayesian 95% posterior interval (as percent change from baseline)
    #i = 1
    
    configName = configNames[i]
    configColName <- paste('b_Config', configName, sep = "")
    posterior <- as_draws_matrix(mod)
    
    if (dir == 'lower'){
      prob <- sum(posterior[,configColName] < 0) / length(posterior[,configColName])
      
    } else if (dir == 'higher') {
      
      prob <- sum(posterior[,configColName] > 0) / length(posterior[,configColName])
    }
    
    ci <- posterior_interval(mod, prob = 0.95)
    ciLow <- ci[configColName,1] 
    ciHigh <- ci[configColName,2]
    
    SDdat <- dat %>%
      group_by(Subject) %>%
      summarize(sd = sd(!! sym(var), na.rm = TRUE), mean = mean(!! sym(var), na.rm = TRUE))
    
    meanSD = mean(SDdat$sd)
    mean = mean(SDdat$mean)
    ci_LowPct <- meanSD*ciLow/mean*100
    ci_HighPct <- meanSD*ciHigh/mean*100
    
    output = list('Config:', configName, 'Probability of Improvement:', prob, 'Worse end of CI:', ci_LowPct, 'Best end of CI:', ci_HighPct)
    Config[i] = configName
    ProbImp[i] = prob
    lowCI[i] = ci_LowPct
    highCI[i] = ci_HighPct
  }
  ProbImp = round(ProbImp, 2)
  lowCI = round(lowCI, 1)
  highCI = round(highCI,1)
  output = cbind(Config, ProbImp, lowCI, highCI)
  
  colnames(output) = c('Config', 'Probability of Improvement', 'Low end of CI', 'High end of CI')
  return(output)
}

################

#Load in Compiled Qualitative Sheet
qualDat <- read_xlsx(file.choose())


base <- 'TP' # baseline configuration

otherConfigs <- c('OP') # other configurations tesed against base

allConfigs <- c(base, otherConfigs)

qualDat$Config <- factor(qualDat$Config, allConfigs)


#Defining our baseline and shoes being tested agaisnt the baseline
qualDat$Config <- factor(qualDat$Config, c('TP','OP')) #List baseline first then shoes you want to test against

# Making a summary table of the average, and median ratings of fit for the different shoe sections
qualDat %>%
  pivot_longer(cols = OverallFit:Heel, 
               names_to = "Location", values_to = "Rating") %>%
  group_by(Location, Config) %>%
  summarize(
    avg = mean(Rating, na.rm = TRUE),
    medAvg = median(Rating, na.rm = TRUE)
  )

### For High Cut
# Making a summary table of the average, and median ratings of fit for the different shoe sections
qualDat %>%
  pivot_longer(cols = OverallFit:Cuff,
               names_to = "Location", values_to = "Rating") %>%
  group_by(Location, Config) %>%
  summarize(
    avg = mean(Rating, na.rm=TRUE),
    medAvg = median(Rating, na.rm=TRUE)
  )

### Probability of higher overall score (for radar plot)
qualDat <- qualDat %>% 
  mutate(z_score = scale(OverallFit))
  

runmod <- brm(data = qualDat,
              family = gaussian,
              z_score ~ Config, #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                      
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

extractVals(qualDat, runmod, otherConfigs, 'OverallFit', 'higher') 

## Score for forefoot fit

qualDat <- qualDat %>% 
  mutate(ffCent = Forefoot - 5) %>%
  mutate(ffCentAbs = abs(ffCent)) %>%
  mutate(z_score = scale(ffCentAbs))# Change to the variable you want to test


runmod <- brm(data = qualDat,
              family = gaussian,
              z_score ~ Config, #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

extractVals(qualDat, runmod, otherConfigs, 'Forefoot', 'lower') 


## Score for midfoot fit

qualDat <- qualDat %>% 
  mutate(mfCent = Midfoot - 5) %>%
  mutate(mfCentAbs = abs(mfCent)) %>%
  mutate(z_score = scale(mfCentAbs))# Change to the variable you want to test


runmod <- brm(data = qualDat,
              family = gaussian,
              z_score ~ Config, #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

extractVals(qualDat, runmod, otherConfigs, 'Midfoot', 'lower')

## Score for heel fit

qualDat <- qualDat %>% 
  mutate(heelCent = Heel - 5) %>%
  mutate(heelCentAbs = abs(heelCent)) %>%
  mutate(z_score = scale(heelCentAbs))# Change to the variable you want to test


runmod <- brm(data = qualDat,
              family = gaussian,
              z_score ~ Config, #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

extractVals(qualDat, runmod, otherConfigs, 'Heel', 'lower') 


#### Plots------------------------------------------------------------

# Best of Line plot for overall ratings of the shoe
withinSubQualPlot(qualDat)
#Defining the rating for the location 
#Density plots for fit ratings of shoe locations
qualDat <- pivot_longer(qualDat, cols = Forefoot:Heel, names_to = 'Location', values_to = 'Rating')

FF <- qualDat %>% 
  filter(Location=="Forefoot")

ggplot(FF,mapping = aes(x = Rating, fill = Config, ..count.. )) + geom_density(alpha = 0.5)
  
qualDat$Location <- factor(qualDat$Location, c('Forefoot', 'Midfoot', 'Heel')) 


ggplot(qualDat, mapping = aes(x = Rating, fill = Config)) + geom_density(aes(y = ..density..*(nrow(qualDat)/3)*0.1), alpha = 0.5) + facet_wrap(~Location) + scale_fill_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) +
ylab('Responses') + theme(text=element_text(size=20)) + geom_vline(xintercept = 5, size = 1)

ggplot(qualDat, mapping = aes(x = Rating, fill = Config)) + 
  geom_histogram(position = 'dodge', binwidth = 1) + facet_wrap(~Location) + scale_fill_manual(values=c("#999999", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) +
  ylab('Responses') + theme(text=element_text(size=40)) + geom_vline(xintercept = 5, size = 1)


### For high cut  
#Defining the rating for the location
#Density plots for fit ratings of shoe locations
qualDat <- pivot_longer(qualDat, cols = Forefoot:Cuff, names_to = 'Location', values_to = 'Rating')

qualDat$Location <- factor(qualDat$Location, c('Forefoot', 'Midfoot', 'Heel', 'Cuff')) 

ggplot(qualDat, mapping = aes(x = Rating, fill = Config)) + geom_density(alpha = 0.5) + facet_wrap(~Location) + scale_fill_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))
+ theme(text=element_text(size=20)) + geom_vline(xintercept = 5, size = 1)


# making word clouds ------------------------------------------------------

#Defining the configs for their respective word couds 
#Define the config for the cloud  - In the quotes , use the origional name if the config from the csv



ED <- subset(qualDat, qualDat$Config == 'ED', GoodComments:BadComments)
PD <- subset(qualDat, qualDat$Config == 'PD', GoodComments:BadComments) 



#Defining the word clouds 
# Leaving out numbers, and extra unneeded words
replacePunctuation <- content_transformer(function(x) {return (gsub("[[:punct:]]", " ", x))})

makeWordCloud <- function(inputText) {
  
  docs <- Corpus(VectorSource(inputText))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(replacePunctuation)
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("like", "feel","feels","lace","bottom","steel","replacement","toe.","toe",
                                      "felt","tri", "na","shoe","can","everything","noticable", "else","kind")) 
  
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  
  # Defining "good" comments from "bad" in the csv
  vGood <- m[,1]
  vBad <- m[,2]
  vBad <- sort(vBad, decreasing = TRUE)
  vGood <- sort(vGood, decreasing = TRUE)
  vGood <- as.data.frame(vGood)
  vBad <- as.data.frame(vBad)
  
  # Positive words = green, negative words = grey
  colorList <- c(rep('dark green', nrow(vGood)), rep('grey', nrow(vBad)))
  
  # Word frequency
  #Making popular words large and less used words smaller
  GoodWords <- rownames(vGood)
  GoodFrq<- vGood[,1]
  Good <- cbind(GoodWords, GoodFrq)
  BadWords <- rownames(vBad)
  BadFrq <- vBad[,1]
  Bad <- cbind(BadWords, BadFrq)
  
  #Combining the positive words with the negative words in a single word cloud
  d <- rbind(Good, Bad)
  d <- cbind(d, colorList)
  d <- as.data.frame(d)
  colnames(d) <- c('Word', 'Freq', 'Color')
  d <- d[order(d$Freq, decreasing = TRUE),]
  d <- d[d$Freq > 0, ]
 
  d$Freq <- as.numeric(d$Freq)
  
  #Combining the word frequency, connotation and order into the word cloud
  set.seed(1234)
  wordcloud(d$Word, d$Freq, min.freq = 1, max.words = nrow(d),
            random.order=FALSE, rot.per=0.35, 
            colors=d$Color, ordered.colors = TRUE)
  
}


makeWordCloud(HED)
makeWordCloud(PD)

