setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)

pgs.b1.2nd <- b1.2nd[grepl("PG", b1.2nd$PO),]
pgs.b1.2nd[order(pgs.b1.2nd$PPG, decreasing = TRUE),] %>%
  filter(MIN > 100) %>%
  select("TEAM", "PLAYER","G", "MIN", "PTS", "PPG", "FGP", "EFGP", "TSP") %>%
  View()

pgs.b1.2nd[order(pgs.b1.2nd$TOPG, decreasing = FALSE),] %>%
  filter(MIN > 100) %>%
  select("TEAM", "PLAYER","G", "MIN", "TOPG") %>%
  View()


