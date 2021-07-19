library(readxl)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(readxl)
library(ggplot2)

rm(list=ls())

qualDat <- read_xlsx(file.choose())

qualDat$Shoe <- factor(qualDat$Shoe, c('Tri', 'Asym'))

qualDat %>%
  pivot_longer(cols = Overall:Heel,
               names_to = "Location", values_to = "Rating") %>%
  group_by(Location, Shoe) %>%
  summarize(
    avg = mean(Rating),
    medAvg = median(Rating)
  )

qualDat %>%
  pivot_longer(cols = Overall:Heel,
               names_to = "Location", values_to = "Rating") %>%
  filter(Location == 'Overall') %>%
  ggplot(mapping = aes(x = Shoe, y = Rating, group = SubjectName)) + geom_line(aes(color = SubjectName)) + geom_point(aes(color = SubjectName)) 
  


qualDat %>%
  pivot_longer(cols = Forefoot:Heel,
               names_to = "Location", values_to = "Rating") %>%
  filter(Location != 'Performance') %>%
  ggplot(mapping = aes(x = Rating, fill = Shoe)) + geom_density(alpha = 0.5) + facet_wrap(~Location) + scale_fill_manual(values=c("#003D4C", "#00966C", "#ECE81A","#DC582A","#CAF0E4"))


# making word clouds ------------------------------------------------------

#tri <- subset(qualDat, qualDat$Shoe == 'Tri')
BOA <- subset(qualDat, qualDat$Shoe == 'BOA')
lace <- subset(qualDat, qualDat$Shoe == 'Lace')
#triNotes <- tri$Comments
BOAnotes <- BOA$Comments
laceNotes <- lace$Comments


makeWordCloud <- function(inputText) {
  
  docs <- Corpus(VectorSource(inputText))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation)
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("like", "feel","feels","lace","bottom","steel","replacement","toe.","toe",
                                      "felt","tri")) 
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=25, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}

#makeWordCloud(triNotes)
makeWordCloud(BOAnotes)
makeWordCloud(laceNotes)

