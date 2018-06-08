setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)

rookies <- b1.2nd[c(11,53,74,87,90,94,104),]
rownames(rookies) <- NULL
rookies %>% 
  select(c("TEAM", "PLAYER", "G", "GS", "MIN")) %>%
  kable()

rookies$PPM <- round(rookies$PTS / rookies$MIN, 2)
rookies$EFGP <- round((rookies$FGM + 0.5 * rookies$TPGM) / rookies$FGA, 2)
rookies$TSP <- round(rookies$PTS / (2 * (rookies$FGA + (0.44 * rookies$FTA))) , 2)

rookies %>% 
  select(c("TEAM", "PLAYER", "PTS", "PPG", "PPM", "FGP", "EFGP", "TSP")) %>%
  kable()

rookies %>% 
  select(c("TEAM", "PLAYER", "TPGA", "TPGP", "FTA", "FTP")) %>%
  kable()

rookies$TOPG <- round(rookies$TO / rookies$G, 2)
rookies$ORPG <- round(rookies$OR / rookies$G, 2)
rookies$DRPG <- round(rookies$DR / rookies$G, 2)

rookies %>% 
  select(c("PLAYER", "APG", "TOPG", "ORPG", "DRPG")) %>%
  kable()

rookies$STPG <- round(rookies$ST / rookies$G, 2)
rookies$BSPG <- round(rookies$BS / rookies$G, 2)

rookies %>% 
  select(c("PLAYER", "STPG", "BSPG")) %>%
  kable()
