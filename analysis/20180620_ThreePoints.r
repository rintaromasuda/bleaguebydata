setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)
library(ggplot2)

df <- subset(b1.2nd, TPGM >= 90)

df$TPGP2 <- round(df$TPGM / df$TPGA, 3)
df$TPGP_STR <- paste(df$TPGP2 * 100, "%")
df$TPGMPG <- round(df$TPGM / df$G, 2)
df$TPGMPM <- round(df$TPGM / df$MIN,2)
rownames(df) <- NULL

df[order(df$TPGP2, decreasing = TRUE),] %>%
  select(c("TEAM", "PLAYER", "G", "MIN", "TPGP_STR", "TPGM", "TPGA", "TPGMPG", "TPGMPM")) %>%
  head(10) %>%
  kable()
  