### balance measurements analysis hike/work ####

#######
# How-to use. Import data

######

rm(list=ls())
balanceDat <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/Hike Work Research/Data/Balance_ProtocolR.csv')
names(balanceDat)[1] <- 'Subject'

balance <- subset(balanceDat, balanceDat$Movement == 'Balance')
singleLeg <- subset(balanceDat, balanceDat$Movement == 'SingleLeg')

# Balance measurements 
#### Trends: ####
#There is less motion in the Y direction in the BOA configurations for both subjects during balance movements
#X direction has no systematic differences.

# DF had lowest velocity in DD, but no changes otherwise
XVel<- aggregate(AvgVX ~ Config + Subject , data = balance, FUN = mean)
names(XVel) <- c('Config', 'Subject', 'AvgVelX')
XVel
ggplot(XVel, aes(fill=Config, y= AvgVelX, x=Subject)) + 
  geom_bar(position="dodge", stat="identity") + ylab('Average ML Velocity') + xlab('Subject')

#Both subjects had less Y movement in SD and DD
YVel<- aggregate(AvgVY ~ Config + Subject , data = balance, FUN = mean)
names(YVel) <- c('Config', 'Subject', 'AvgVelY')
YVel


ggplot(YVel, aes(fill=Config, y= AvgVelY, x=Subject)) + 
  geom_bar(position="dodge", stat="identity") + ylab('Average AP Velocity') + xlab('Subject')

# Average distance COP moves is lowest in DD and SD for DF
DistX <- aggregate(DistX ~ Config + Subject , data = balance, FUN = mean)
names(DistX) <- c('Config','Subject','AvgVelX')
DistX

#Average distance is lower in SD and DD than lace for both subjects
DistY <- aggregate(DistY ~ Config + Subject , data = balance, FUN = mean)
names(DistY) <- c('Config','Subject','AvgVelX')
DistY

# Single leg landing measurements 
#Single leg landing has no obvious systematic differences. This could be due to the task being more difficult
#and requiring more practice. Or each trial is more variable, so we need more data from each subject. 
# No systematic differences for time to stabilie
stabTime<- aggregate(TimeToStab ~ Config + Subject , data = singleLeg, FUN = mean)
names(stabTime) <- c('Config', 'Subject', 'TimeToStabilize')
stabTime
ggplot(stabTime, aes(fill=Config, y= stabTime$TimeToStabilize, x=Subject)) + 
  geom_bar(position="dodge", stat="identity") + ylab('Average Time to Stabilize') + xlab('Subject')

# Minimzied SD for BV and Lace for DF
XVel<- aggregate(AvgVX ~ Config + Subject , data = singleLeg, FUN = mean)
names(XVel) <- c('Config', 'Subject', 'AvgVelX')
XVel

ggplot(XVel, aes(fill=Config, y= XVel$AvgVelX, x=Subject)) + 
  geom_bar(position="dodge", stat="identity") + ylab('Average ML Velocity') + xlab('Subject')


# Minimized in SD for BV and DD for DF but most were similar for DF
YVel<- aggregate(AvgVY ~ Config + Subject , data = singleLeg, FUN = mean)
names(YVel) <- c('Config', 'Subject', 'AvgVelY')
YVel

ggplot(YVel, aes(fill=Config, y= YVel$AvgVelY, x=Subject)) + 
  geom_bar(position="dodge", stat="identity") + ylab('Average ML Velocity') + xlab('Subject')


# No systematic trends 
DistX <- aggregate(DistX ~ Config + Subject , data = singleLeg, FUN = mean)
names(DistX) <- c('Config','Subject','DistX')
DistX

# No systematic trends
DistY <- aggregate(DistY ~ Config + Subject , data = singleLeg, FUN = mean)
names(DistY) <- c('Config','Subject','DistY')
DistY
