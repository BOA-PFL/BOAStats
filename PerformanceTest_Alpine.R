######## Alpine Ski Analysis #############

library(tidyverse)


# Loading data ------------------------------------------------------------


dat <- read.csv('C:/Users/daniel.feeney/Boa Technology Inc/PFL - General/Snow Performance/Alpine_V1vsV2_Internal_Feb2022/testresults.csv')
dat$Subject <- as.factor(dat$Subject)
dat$Configuration <- as.factor(dat$Configuration)


# Plotting ----------------------------------------------------------------

plotDV <- function(col, dfName){
  
  genPlot <- ggplot(data = dfName, mapping = aes(x = Configuration, y = .data[[col]], fill = Configuration)) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_wrap(~Subject + TurnType)
  
  return(genPlot)
}
plotDV('PeakForce',dat)
plotDV('PkLatForceEarly',dat)
plotDV('CVForce',dat)
plotDV('PkHeelLate',dat)

