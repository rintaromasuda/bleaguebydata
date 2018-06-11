setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)

View(b1.2nd)
rivals <- b1.2nd[c(24,31),]
View(rivals)
