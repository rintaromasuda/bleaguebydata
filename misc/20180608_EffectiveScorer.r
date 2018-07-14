setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)

b1.2nd$PPM <- round(b1.2nd$PTS / b1.2nd$MIN, 2)
ppm.top10 <- b1.2nd[order(b1.2nd$PPM, decreasing = TRUE),] %>%
  filter(MIN > 100) %>%
  select(c("TEAM", "PLAYER", "G", "PTS", "MIN", "PPM")) %>%
  head(10)
kable(ppm.top10)

b1.2nd$EFGP <- round((b1.2nd$FGM + 0.5 * b1.2nd$TPGM) / b1.2nd$FGA, 2)
b1.2nd$TSP <- round(b1.2nd$PTS / (2 * (b1.2nd$FGA + (0.44 * b1.2nd$FTA))) , 2)
efgts.top <- b1.2nd[order(b1.2nd$PPG, decreasing = TRUE),] %>%
  select(c("TEAM", "PLAYER", "G", "PTS", "PPG", "FGP", "EFGP", "TSP")) %>%
  filter(G > 30) %>%
  head(30)
View(efgts.top)

b1.2nd$FTR <- round(b1.2nd$FTM / b1.2nd$PTS, 2)
ftppts.top <- b1.2nd[order(b1.2nd$FTR, decreasing = TRUE),] %>%
  filter(PTS > 100) %>%
  select(c("TEAM", "PLAYER", "G", "PTS", "FTM", "FTR")) %>%
  head(10)
kable(ftppts.top)
