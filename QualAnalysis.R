library(readxl)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(readxl)
library(brms)
library(tidyverse)


rm(list=ls())

###############


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
    geom_line() + xlab('Configuration') + scale_color_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(text = element_text(size = 16)) + ylab('Rating') 
  
}





qualDat$Config <- factor(qualDat$Config, c('SL','LRSD','4guide'))

qualDat %>%
  pivot_longer(cols = OverallFit:Heel,
               names_to = "Location", values_to = "Rating") %>%
  group_by(Location, Config) %>%
  summarize(
    avg = mean(Rating),
    medAvg = median(Rating)
  )




qualDat <- read_xlsx(file.choose())

qualDat$Config <- factor(qualDat$Config, c('SL', 'LRSD','4guide')) #List baseline first

qualDat %>%
  pivot_longer(cols = OverallFit:Heel,
               names_to = "Location", values_to = "Rating") %>%
  group_by(Location, Config) %>%
  summarize(
    avg = mean(Rating),
    medAvg = median(Rating)
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


####
#qualDat <- pivot_longer(qualDat, cols = OverallFit:Heel, names_to = "Location", values_to = "Rating")
#qualDat$Location <- factor(qualDat$Location, c("OverallFit", "Forefoot", "Midfoot", "Heel"))


withinSubQualPlot(qualDat)




qualDat <- pivot_longer(qualDat, cols = Forefoot:Heel, names_to = 'Location', values_to = 'Rating')
  
qualDat$Location <- factor(qualDat$Location, c('Forefoot', 'Midfoot', 'Heel')) 

ggplot(qualDat, mapping = aes(x = Rating, fill = Config)) + geom_density(alpha = 0.5) + facet_wrap(~Location) + scale_fill_manual(values=c("#000000", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))





  

# making word clouds ------------------------------------------------------


SL <- subset(qualDat, qualDat$Config == 'SL', GoodComments:BadComments)
guide <- subset(qualDat, qualDat$Config == '4guide', GoodComments:BadComments)

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
  
  vGood <- m[,1]
  vBad <- m[,2]
  vBad <- sort(vBad, decreasing = TRUE)
  vGood <- sort(vGood, decreasing = TRUE)
  vGood <- as.data.frame(vGood)
  vBad <- as.data.frame(vBad)
  
  colorList <- c(rep('dark green', nrow(vGood)), rep('grey', nrow(vBad)))
  
  GoodWords <- rownames(vGood)
  GoodFrq<- vGood[,1]
  Good <- cbind(GoodWords, GoodFrq)
  BadWords <- rownames(vBad)
  BadFrq <- vBad[,1]
  Bad <- cbind(BadWords, BadFrq)
  
  d <- rbind(Good, Bad)
  d <- cbind(d, colorList)
  d <- as.data.frame(d)
  colnames(d) <- c('Word', 'Freq', 'Color')
  d <- d[order(d$Freq, decreasing = TRUE),]
  d <- d[d$Freq > 0, ]
 
  d$Freq <- as.numeric(d$Freq)
  
  set.seed(1234)
  wordcloud(d$Word, d$Freq, min.freq = 1, max.words = nrow(d),
            random.order=FALSE, rot.per=0.35, 
            colors=d$Color, ordered.colors = TRUE)
  
}


makeWordCloud(SL)


