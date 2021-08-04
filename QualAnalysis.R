library(readxl)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(readxl)
library(ggplot2)
library(tidyverse)

rm(list=ls())

qualDat <- read_xlsx(file.choose())

qualDat$Shoe <- factor(qualDat$Shoe, c('Lace', 'A','B', 'C'))

qualDat %>%
  pivot_longer(cols = Overall:Heel,
               names_to = "Location", values_to = "Rating") %>%
  group_by(Location, Shoe) %>%
  summarize(
    avg = mean(Rating),
    medAvg = median(Rating)
  )


### Probability of higher overall score (for radar plot)

qualDat <- qualDat %>% 
  group_by(Subject) %>%
  mutate(z_score = scale(Overall)) %>% # Change to the variable you want to test
  group_by(Shoe)

runmod <- brm(data = qualDat,
              family = gaussian,
              z_score ~ Shoe + (1 | Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

posterior <- posterior_samples(runmod)

sum(posterior[,4] < 0) / length(posterior[,4]) 


####
qualDat <- pivot_longer(qualDat, cols = Overall:Heel, names_to = "Location", values_to = "Rating")
qualDat$Location <- factor(qualDat$Location, c("Overall", "Forefoot", "Midfoot", "Heel"))

qualDat %>%
  filter(Location == 'Overall') %>%
  ggplot(mapping = aes(x = Shoe, y = Rating, group = Subject)) + geom_line(aes(color = Subject)) + geom_point(aes(color = Subject)) 
  


qualDat %>%
  filter(Location != 'Overall') %>%
    ggplot(mapping = aes(x = Rating, fill = Shoe)) + geom_density(alpha = 0.5) + facet_wrap(~Location) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))





  group_by(Subject) %>% 
  mutate(z_score = scale(Distance)) %>%
  group_by(Shoe)

datDist <- subset(datDist, datDist$z_score < 2)
datDist <- subset(datDist, datDist$z_score > -2)

ggplot(data = datDist) + geom_boxplot(mapping = aes(x = Subject, y = Distance, fill = Shoe)) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

runmod <- brm(data = datDist,
              family = gaussian,
              z_score ~ Shoe + (1 + Shoe | Subject), #fixed effect of configuration and time period with a different intercept and slope for each subject
              prior = c(prior(normal(0, 1), class = Intercept), #The intercept prior is set as a mean of 25 with an SD of 5 This may be interpreted as the average loading rate (but average is again modified by the subject-specific betas)
                        prior(normal(0, 1), class = b), #beta for the intercept for the change in loading rate for each configuration
                        prior(cauchy(0, 1), class = sd), #This is a regularizing prior, meaning we will allow the SD of the betas to vary across subjects
                        prior(cauchy(0, 1), class = sigma)), #overall variability that is left unexplained 
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = .975, max_treedepth = 20),
              seed = 190831)

# making word clouds ------------------------------------------------------

A <- subset(qualDat, qualDat$Shoe == 'A', GoodComments:BadComments)
B <- subset(qualDat, qualDat$Shoe == 'B', GoodComments:BadComments)
C <- subset(qualDat, qualDat$Shoe == 'C', GoodComments:BadComments)
Lace <- subset(qualDat, qualDat$Shoe == 'Lace', GoodComments:BadComments)

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
  
  colorList <- c(rep('green', nrow(vGood)), rep('red', nrow(vBad)))
  
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

#makeWordCloud(triNotes)
makeWordCloud(Lace)


