setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)
library(ggplot2)
library(moments)
library(ggrepel)

df2 <- b2.2nd %>%
  filter(G >= 51) %>%
  filter(TPGM >= 90)

ggplot(df, aes(x = TPGA, y = TPGP, label = paste(PLAYER,"(", as.character(TPGP * 100), "%)", sep = ""))) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  geom_point(aes(alpha = 0.2), col = "red") +
  geom_text_repel(direction = "x", nudge_y = - 0.01) +
  ylab("3Pシュート成功率") +
  xlab("3Pシュート試投数") +
  ggtitle("B2の3Pシューターの成績（2017-18シーズン）") +
  scale_y_continuous(labels = scales::percent)

df <- b1.2nd %>%
  filter(G >= 51) %>%
  filter(TPGM >= 90)

df <- rbind(df, df2)

ggplot(df, aes(x = TPGA, y = TPGP, label = paste(PLAYER,"(", as.character(TPGP * 100), "%)", sep = ""))) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  geom_point(aes(alpha = 0.2), col = "blue") +
  geom_text_repel(direction = "x", nudge_y = - 0.01) +
  ylab("3Pシュート成功率") +
  xlab("3Pシュート試投数") +
  ggtitle("B1の3Pシューターの成績（2017-18シーズン）") +
  scale_y_continuous(labels = scales::percent)

View(b2.2nd)
