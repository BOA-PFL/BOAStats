rm(list=ls())
library(tidyverse)
library(readxl)

dat <- read_xlsx('C:/Users/Daniel.Feeney/Boa Technology Inc/PFL - General/Qualitative Feedback Forms/Altra Running Test/CompiledQualData.xlsx')

ggplot(data = dat, mapping = aes(x = as.factor(Shoe), y = Overall, col = Subject, group = Subject)) + geom_point(size = 4) + 
  geom_line() + xlab('Configuration')
