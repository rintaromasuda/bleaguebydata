setwd('C:/git/bleaguebydata/analysis')

library(knitr)
library(dplyr)
library(ggplot2)
library(stringr)

df <- read.csv("201718_b1_gamebygame.csv")

df$POS <- df$F2GA + df$F3GA + (0.44 * df$FTA) + df$TO - df$OR
df$PPP <- df$PTS / df$POS
df$PPP100 <- df$PPP * 100

gsub(df$VS, "vs", "")
df$VS_STR <- as.character(df$VS)
df$VS_STR <- gsub("vs", "", df$VS_STR)
df$ISAVR <- grepl("A東京", df$VS_STR)
df[df$ISAVR, ]
df$ISBREX <- grepl("栃木", df$VS_STR)
summary(df$TEAM)

df <- df[order(df$PPP100, decreasing = TRUE),]

df.mikawa <- subset(df, TEAM == "三河")
#ggplot(df.mikawa, aes(x = DAY, y = PPP100)) +
#  geom_bar(stat = "identity") +
#  geom_bar(stat = "Identity", data = subset(df.mikawa, ISAVR), fill = "red")
ggplot(df.mikawa, aes(y = PPP100)) +
  geom_histogram(stat = "identity")
