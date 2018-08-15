setwd('C:/git/bleaguebydata/misc')
source('playerstats.r')

library(knitr)
library(dplyr)
library(ggplot2)

df_brex <- b1.2nd[b1.2nd$TEAM == "“È–Ø", c("PLAYER", "PTS", "FGP", "EFGP", "TSP")]
df_hiejima <- b1.2nd[b1.2nd$PLAYER == "”ä]“‡ T", c("PLAYER", "PTS", "FGP", "EFGP", "TSP")]
df <- rbind(df_brex, df_hiejima)
df <- df[order(df$PTS, decreasing = TRUE), ]
df$FGP_STR <- paste(as.character((df$FGP * 100)), "%", sep = "")
df$EFGP_STR <- paste(as.character(round(df$EFGP * 100, 1)), "%", sep = "")
df$TSP_STR <- paste(as.character(round(df$TSP * 100, 1)), "%", sep = "")
dd <- df[1:11, c("PLAYER", "FGP_STR", "EFGP_STR", "TSP_STR")]
rownames(dd) <- NULL
View(dd)
kable(dd)
