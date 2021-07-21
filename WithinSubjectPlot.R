rm(list=ls())
library(tidyverse)
library(readxl)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

dat <- read_xlsx('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/Qualitative Feedback Forms/Altra Running Test/CompiledQualData.xlsx')

dat <- dat %>% 
  mutate(Subject = replace(Subject, Subject == 'Amanda Bashum', 'Amanda Basham'))


ggplot(data = dat, mapping = aes(x = as.factor(Shoe), y = Overall, col = Subject, group = Subject)) + geom_point(size = 4) + 
  geom_line() + xlab('Configuration') + theme(text = element_text(size = 20)) + ylab('Overall Rating of Shoe') +
  scale_y_continuous(limits=c(5,10))


dat %>%
  pivot_longer(cols = Overall:Heel,
               names_to = "Location", values_to = "Rating") %>%
  filter(Location != 'Overall') %>%
  ggplot(mapping = aes(x = Rating, fill = Shoe)) + geom_density() + 
    facet_wrap(~Location) + scale_fill_manual(values=c("#CAF0E4", "#ECE81A")) +
    theme(text = element_text(size = 20)) + ylab('Density') +
    scale_x_continuous(limits=c(1,10))


BOA <- subset(dat, dat$Shoe == 'BOA')
lace <- subset(dat, dat$Shoe == 'Lace')
BOA <- BOA$Comments
lace <- lace$Comments



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
                                      "felt","tri","blah")) 
  
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

makeWordCloud(BOA)
makeWordCloud(lace)



# testing section ---------------------------------------------------------

dat <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/EndurancePerformance/Altra_MontBlanc_June2021/Kinetics/CompiledKineticData.csv')

ggplot(data = dat, mapping = aes(x = SubjectName, y = pAnklePower, fill = ShoeCondition)) + geom_boxplot()
ggplot(data = dat, mapping = aes(x = SubjectName, y = pAnkleFrontalMoment, fill = ShoeCondition)) + geom_boxplot()
ggplot(data = dat, mapping = aes(x = SubjectName, y = abs(pKneeFrontalMoment), fill = ShoeCondition)) + geom_boxplot()

