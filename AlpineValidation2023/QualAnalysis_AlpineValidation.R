library(tidyverse)
library(readxl)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(readxl)
library(brms)
library(patchwork)
library(lme4)
library(emmeans)


#Clearing the environment
rm(list=ls())

###############

testAnova <- function(metric, df) {
  myformula <- as.formula(paste0(metric," ~ Config + (1|Subject)"))
  myformula2 <- as.formula(paste0(metric, " ~ (1|Subject)"))
  full.mod = lmer(myformula, data = df, REML = TRUE, na.action = "na.omit")
  red.mod = lmer(myformula2, data = df, REML = TRUE, na.action = "na.omit" )
  conditions.emm <- emmeans(full.mod, "Config", lmer.df = "satterthwaite")
  #conditions.emm
  contrast(conditions.emm, "trt.vs.ctrl", ref = "Buckle") 
  newList <- list("randEffectMod" = summary(full.mod), "anovaBetweenMods" = anova(full.mod, red.mod),
                  "contrasts" = conditions.emm, "Contrasts2" = contrast(conditions.emm, "trt.vs.ctrl", ref = "Buckle"))
  return(newList)
}

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
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 26)) + ylab('Rating') 
  
}

withinSubPlot <- function(inputDF, colName, dir,ylabel) {
  # Specify ylabel in function or default to the original name
  if(missing(ylabel)){
    ylabel = paste0({{colName}})
  }
  
  # direction can be 'lower' or higher'. It is the direction of change that is better.
  # For example, for contact time lower is better. so we put 'lower'. for jump height, higher is better, so we put higher.
  meanDat <- inputDF %>%
    group_by(Subject, Config) %>%
    summarize(mean = mean(!! sym(colName)))
  
  if (dir == 'lower'){
    whichConfig <- meanDat %>%
      group_by(Subject) %>%
      summarize(
        BestConfig = Config[which.min(mean)]
      )
    
  } else if (dir == 'higher') {
    whichConfig <- meanDat %>%
      group_by(Subject) %>%
      summarize(
        BestConfig = Config[which.max(mean)]
      )
    
  }
  
  # "Best of" line plot code
  whichConfig <- merge(meanDat, whichConfig)
  
  ggplot(data = whichConfig, mapping = aes(x = as.factor(Config), y = mean, col = BestConfig, group = Subject)) + geom_point(size = 4) +
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 26)) + ylab(ylabel)
  
}

################

#Load in Compiled Qualitative Sheet
# qualDat <- read_xlsx('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/EndurancePerformance/TrailRun_2022/TrailRunQual.xlsx')
qualDat <- read_xlsx('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/Testing Segments/Snow Performance/SkiValidation_Dec2022/SkiValidationQual.xlsx')

qualDat <- qualDat %>% filter(Subject != "S14")

qualDat$Forefoot <- abs(qualDat$Forefoot-5)
qualDat$Midfoot <- abs(qualDat$Midfoot-5)
qualDat$Heel <- abs(qualDat$Heel-5)
qualDat$Cuff <- abs(qualDat$Cuff-5)

#Defining our baseline and shoes being tested agaisnt the baseline
qualDat$Config <- factor(qualDat$Config, c('Buckle', 'BOA')) #List baseline first then shoes you want to test against

testAnova('Exertion',qualDat)
testAnova('Carving',qualDat)
testAnova('Confidence',qualDat)
testAnova('OverallFit',qualDat)
testAnova('Forefoot',qualDat)
testAnova('Midfoot',qualDat)
testAnova('Heel',qualDat)
testAnova('Cuff',qualDat)

# Examine different types of statistical tests (Reviewer 3)
BOAdat <- qualDat %>%
  filter(Config == 'BOA')
Buckdat <- qualDat %>%
  filter(Config == 'Buckle')

t.test(BOAdat$Exertion,Buckdat$Exertion)
t.test(BOAdat$Exertion,Buckdat$Exertion, paired = TRUE)
wilcox.test(BOAdat$Exertion,Buckdat$Exertion)
wilcox.test(BOAdat$Exertion,Buckdat$Exertion, paired = TRUE)

t.test(BOAdat$Carving,Buckdat$Carving)
t.test(BOAdat$Carving,Buckdat$Carving, paired = TRUE)
wilcox.test(BOAdat$Carving,Buckdat$Carving)
wilcox.test(BOAdat$Carving,Buckdat$Carving, paired = TRUE)

t.test(BOAdat$Confidence,Buckdat$Confidence)
t.test(BOAdat$Confidence,Buckdat$Confidence, paired = TRUE)
wilcox.test(BOAdat$Confidence,Buckdat$Confidence)
wilcox.test(BOAdat$Confidence,Buckdat$Confidence, paired = TRUE)

t.test(BOAdat$OverallFit,Buckdat$OverallFit)
t.test(BOAdat$OverallFit,Buckdat$OverallFit, paired = TRUE)
wilcox.test(BOAdat$OverallFit,Buckdat$OverallFit)
wilcox.test(BOAdat$OverallFit,Buckdat$OverallFit, paired = TRUE)

t.test(BOAdat$Forefoot,Buckdat$Forefoot)
t.test(BOAdat$Forefoot,Buckdat$Forefoot, paired = TRUE)
wilcox.test(BOAdat$Forefoot,Buckdat$Forefoot)
wilcox.test(BOAdat$Forefoot,Buckdat$Forefoot, paired = TRUE)

t.test(BOAdat$Midfoot,Buckdat$Midfoot)
t.test(BOAdat$Midfoot,Buckdat$Midfoot, paired = TRUE)
wilcox.test(BOAdat$Midfoot,Buckdat$Midfoot)
wilcox.test(BOAdat$Midfoot,Buckdat$Midfoot, paired = TRUE)

t.test(BOAdat$Heel,Buckdat$Heel)
t.test(BOAdat$Heel,Buckdat$Heel, paired = TRUE)
wilcox.test(BOAdat$Heel,Buckdat$Heel)
wilcox.test(BOAdat$Heel,Buckdat$Heel, paired = TRUE)

t.test(BOAdat$Cuff,Buckdat$Cuff)
t.test(BOAdat$Cuff,Buckdat$Cuff, paired = TRUE)
wilcox.test(BOAdat$Cuff,Buckdat$Cuff)
wilcox.test(BOAdat$Cuff,Buckdat$Cuff, paired = TRUE)

# Making a summary table of the average, and median ratings of fit for the different shoe sections
qualDat %>%
  pivot_longer(cols = OverallFit:Heel,
               names_to = "Location", values_to = "Rating") %>%
  group_by(Location, Config) %>%
  summarize(
    avg = mean(Rating, na.rm = TRUE),
    medAvg = median(Rating, na.rm = TRUE)
  )

### Probability of higher overall score (for radar plot)
qualDat <- qualDat %>% 
  
  mutate(z_score = scale(OverallFit))# Change to the variable you want to test
  

runmod <- brm(data = qualDat,
              family = gaussian,
              z_score ~ Config + (1|Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

posterior <- posterior_samples(runmod)

sum(posterior[,3] > 0) / length(posterior[,3]) 


#### Plots------------------------------------------------------------

# Best of Line plot for overall ratings of the shoe
withinSubQualPlot(qualDat) + ylab('Overall Rating')

#Defining the rating for the location 
#Density plots for fit ratings of shoe locations
qualDatL <- pivot_longer(qualDat, cols = Forefoot:Heel, names_to = 'Location', values_to = 'Rating')

qualDatL$Location <- factor(qualDatL$Location, c('Forefoot', 'Midfoot', 'Heel')) 

ggplot(qualDatL, mapping = aes(x = Rating, fill = Config)) + geom_density(aes(y = ..density..*(nrow(qualDat)/3)*0.1), alpha = 0.5) + facet_wrap(~Location) + scale_fill_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) +
  ylab('Responses') + theme(text=element_text(size=20))

ggplot(qualDatL, mapping = aes(x = Rating, fill = Config)) + geom_density(binwidth = 1, alpha = 0.5) + facet_wrap(~Location) + scale_fill_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) +
  ylab('Responses') + theme(text=element_text(size=20)) #+ geom_vline(xintercept = 5, size = 0.5)

ggplot(qualDatL, mapping = aes(x = Rating, fill = Config)) + geom_histogram(binwidth=1,position="identity",alpha=0.5) + facet_wrap(~Location) + scale_fill_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) +
  ylab('Responses') + theme(text=element_text(size=20)) + scale_x_continuous(breaks=seq(1, 9, 2))

ggplot(qualDat, mapping = aes(x = Heel, fill = Config)) + geom_histogram(binwidth=1,position="dodge")


# Looking at different grades
withinSubPlot(qualDat, colName = 'Uphill', dir = 'higher','Uphill Rating')
withinSubPlot(qualDat, colName = 'Level', dir = 'higher','Level Rating')
withinSubPlot(qualDat, colName = 'Downhill', dir = 'higher','Downhill Rating')
withinSubPlot(qualDat, colName = 'Confidence', dir = 'higher','Confidence Rating')
withinSubPlot(qualDat, colName = 'Heel', dir = 'lower','Heel Rating')

ggplot(qualDat, mapping = aes(x = Confidence, fill = Config)) + geom_histogram(binwidth=1,position="identity",alpha=0.5) +
  scale_fill_manual(values=c("#000000", "#00966C"))+ theme(text=element_text(size=20)) + scale_x_continuous(breaks=seq(1, 10, 1))

ggplot(qualDat, mapping = aes(x = Uphill, fill = Config, ..count..)) + geom_density(alpha = 0.5) + scale_fill_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))

ggplot(qualDat, mapping = aes(x = Level, fill = Config, ..count..)) + geom_density(alpha = 0.5) + scale_fill_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))

ggplot(qualDat, mapping = aes(x = Downhill, fill = Config, ..count..)) + geom_density(alpha = 0.5) + scale_fill_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))

ggplot(qualDat, mapping = aes(x = Confidence, fill = Config, ..count..)) + geom_density(alpha = 0.5) + scale_fill_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))

# making word clouds ------------------------------------------------------

#Defining the configs for their respective word couds 
#Define the config for the cloud  - In the quotes , use the origional name if the config from the csv

Lace <- subset(qualDat, qualDat$Config == 'Lace', GoodComments:BadComments)
PFS <- subset(qualDat, qualDat$Config == 'PFS', GoodComments:BadComments) 


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
                                      "felt","tri", "na")) 
  
  
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

# Output of the word cloud 


makeWordCloud(Lace)
makeWordCloud(PFS)

