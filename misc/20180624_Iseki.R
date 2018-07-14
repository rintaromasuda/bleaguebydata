setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)
library(ggplot2)

b1.1st <- rbind(b1.1st, b2.1st)

b1.1st$PLAYER <- gsub(" ", "", b1.1st$PLAYER)
b1.1st$PLAYER <- gsub("@", "", b1.1st$PLAYER)
b1.2nd$PLAYER <- gsub(" ", "", b1.2nd$PLAYER)
b1.2nd$PLAYER <- gsub("@", "", b1.2nd$PLAYER)
df <- merge(b1.2nd, b1.1st, by = "PLAYER", all.x = TRUE)

df$MIN_DLT <- df$MIN.x - df$MIN.y
df$PTS_DLT <- df$PTS.x - df$PTS.y
df$PPG_DLT <- df$PPG.x - df$PPG.y
df$PPM_DLT <- df$PPM.x - df$PPM.y
rownames(df) <- NULL

df[order(df$MIN_DLT, decreasing = TRUE),] %>%
  select(c("PLAYER", "TEAM.x", "TEAM.y", "G.x", "G.y", "MIN.x", "MIN.y", "PTS.x", "PTS.y", "MIN_DLT", "PTS_DLT")) %>%
  filter(TEAM.x != TEAM.y) %>%
  View()

df[order(df$MIN_DLT, decreasing = TRUE),] %>%
  select(c("PLAYER", "TEAM.x","TEAM.y", "MIN_DLT")) %>%
  filter(TEAM.x != TEAM.y) %>%
  head(10) %>%
  kable()

df[order(df$PPG_DLT, decreasing = TRUE),] %>%
  select(c("PLAYER", "TEAM.x","TEAM.y", "PPG.x", "PPG.y", "PPG_DLT")) %>%
  filter(TEAM.x != TEAM.y) %>%
  head(10) %>%
  kable()

df[order(df$PPM_DLT, decreasing = TRUE),] %>%
  select(c("PLAYER", "TEAM.x","TEAM.y", "PPM.x", "PPM.y", "PPM_DLT")) %>%
  filter(TEAM.x != TEAM.y) %>%
  head(10) %>%
  kable()
