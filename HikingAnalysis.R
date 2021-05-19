rm(list=ls())

dat <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/Hike Work Research/Data/HikingPathR.csv')
names(dat)[1] <- 'StepNo'

#

ggplot(data = dat) + 
  geom_point(mapping = aes(x = as.factor(StepNo), y = MeanPressure, colour = Condition)) +
  ylab('Mean Pressure') + xlab('Step Number')
# mean pressure is greater for all steps for tension except 5, 8, and 9. This could be due to the increased
# force pushing down on the foot. 

ggplot(data = dat) + 
  geom_point(mapping = aes(x = as.factor(StepNo), y = PkPressure, colour = Condition)) +
  ylab('Peak Pressure') + xlab('Step Number')
#Peak pressure is greater for tension in all steps except 1 and 4

ggplot(data = dat) + 
  geom_point(mapping = aes(x = as.factor(StepNo), y = PkForce, colour = Condition)) +
  ylab('Peak Force') + xlab('Step Number')
#Peak force greater in no tension for all except 5,6,7,10. 

ggplot(data = dat) + 
  geom_point(mapping = aes(x = as.factor(StepNo), y = ContactArea, colour = Condition)) +
  ylab('Peak Contact Area') + xlab('Step Number')

# Peak contact area, which is GREATER in the no tension condition 
# for 8/10 steps (steps 5 and 7 were flipped. )
