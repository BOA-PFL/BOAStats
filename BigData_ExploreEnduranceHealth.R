rm(list=ls())
library(tidyverse)
library(readxl)
library(lme4)
library(patchwork)
library(effsize)
library(effects)
library(ggstatsplot)
library(performance)

#-------------------------------------------------------------------------------
# The purpose of this code is to examine A&S databases and compare metrics
# against one another
#-------------------------------------------------------------------------------
RunDB <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/WalkRunDB.csv')
subDB <- read.csv('C:/Users/eric.honert/Boa Technology Inc/PFL Team - General/BigData/DB_V2/MasterSubjectVisits.csv')

subDB$Subject <- gsub(" ", "", subDB$Subject) # Remove spaces in config names
subDB <- subDB %>%
  select(-c(Benefit,Name.of.Test,Type,Speed.run.,Resistance))

RunDB <- RunDB %>%
  rename('Subject' = ï..Subject) %>%
  filter(Speed == 3) %>%
  filter(Slope == 0) %>%
  filter(PosCOMWork != 'NA', HeelContact != 'NA')

combdat <- left_join(RunDB,subDB, 
                     by = c("Subject","Brand", "Year","Month", "Model"))

combdat$PosCOMWork <- combdat$PosCOMWork/combdat$Mass

combdat$NegCOMWork <- combdat$NegCOMWork/combdat$Mass

combdat$NetCOMWork <- combdat$PosCOMWork+combdat$NegCOMWork

combdat$TotalCOMWork <- combdat$PosCOMWork+abs(combdat$NegCOMWork)

### Look at positive COM Work
my_mod = lmer('PosCOMWork ~ HeelContact + (1|Model)', data = combdat, REML = TRUE, na.action = "na.omit")
summary(my_mod)
coef(my_mod)
performance::model_performance(my_mod)
cor.test(combdat$HeelContact,combdat$PosCOMWork,use='complete.obs')

# Setting up plotting
newdf <- as.data.frame(effects::effect(term='HeelContact',mod=my_mod))
ggplot() + geom_point(data=combdat, aes(x=HeelContact,y=PosCOMWork),size=2) + 
  geom_line(data=newdf, aes(x=HeelContact,y=fit), color='blue') + 
  geom_ribbon(data= newdf, aes(x=HeelContact, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") + 
  labs(x='Heel Contact (%)', y='Positive COM Work (J/kg)') + theme(text = element_text(size = 25))

### Look at negative COM Work
my_mod = lmer('NegCOMWork ~ HeelContact + (1|Model)', data = combdat, REML = TRUE, na.action = "na.omit")
summary(my_mod)
coef(my_mod)
performance::model_performance(my_mod)
cor.test(combdat$HeelContact,combdat$NegCOMWork,use='complete.obs')

# Setting up plotting
newdf <- as.data.frame(effects::effect(term='HeelContact',mod=my_mod))
ggplot() + geom_point(data=combdat, aes(x=HeelContact,y=NegCOMWork),size=2) + 
  geom_line(data=newdf, aes(x=HeelContact,y=fit), color='blue') + 
  geom_ribbon(data= newdf, aes(x=HeelContact, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") + 
  labs(x='Heel Contact (%)', y='Negative COM Work (J/kg)') + theme(text = element_text(size = 25))

### Look at Total COM Work
my_mod = lmer('TotalCOMWork ~ HeelContact + (1|Model)', data = combdat, REML = TRUE, na.action = "na.omit")
summary(my_mod)
summary(my_mod)
coef(my_mod)
performance::model_performance(my_mod)
cor.test(combdat$HeelContact,combdat$TotalCOMWork,use='complete.obs')

# Setting up plotting
newdf <- as.data.frame(effects::effect(term='HeelContact',mod=my_mod))
ggplot() + geom_point(data=combdat, aes(x=HeelContact,y=TotalCOMWork),size=2) + 
  geom_line(data=newdf, aes(x=HeelContact,y=fit), color='blue') + 
  geom_ribbon(data= newdf, aes(x=HeelContact, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") + 
  labs(x='Heel Contact (%)', y='Total COM Work (J/kg)') + theme(text = element_text(size = 25))


### Look at negative COM Work
my_mod = lmer('NetCOMWork ~ HeelContact + (1|Model)', data = combdat, REML = TRUE, na.action = "na.omit")
summary(my_mod)
# extract coefficients
coefs <- data.frame(coef(summary(my_mod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coef(my_mod)
effects_HH <- effects::effect(term='HeelContact',mod=my_mod)

newdf <- as.data.frame(effects_HH)
ggplot() + geom_point(data=combdat, aes(x=HeelContact,y=TotalCOMWork),size=2) + 
  geom_line(data=newdf, aes(x=HeelContact,y=fit), color='blue') + 
  geom_ribbon(data= newdf, aes(x=HeelContact, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") + 
  labs(x='Heel Contact (%)', y='Total COM Work (J/kg)') + theme(text = element_text(size = 25))

ggscatterstats(data = combdat, x=HeelContact, y=TotalCOMWork, marginal = FALSE, bf.message = FALSE, 
               xlab = 'Heel Contact Area (%)', ylab = 'Work [J/kg]')
