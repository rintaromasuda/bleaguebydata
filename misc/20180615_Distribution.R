setwd('C:/git/bleaguebydata/analysis')
source('playerstats.r')

library(knitr)
library(dplyr)
library(ggplot2)
require(scales)

b1.2nd$PSTSZN_STR <- "中位"
b1.2nd[b1.2nd$TEAM %in% c("千葉", "A東京", "川崎", "栃木", "三河", "名古屋D", "琉球", "京都"),]$PSTSZN_STR <- "上位"
b1.2nd[b1.2nd$TEAM %in% c("横浜", "富山", "西宮", "島根"),]$PSTSZN_STR <- "下位"
b1.2nd$PSTSZN <- factor(b1.2nd$PSTSZN_STR, labels <- c("上位", "中位", "下位"), levels <- c("上位", "中位", "下位"))

str(b1.2nd)

b1.2nd %>%
  group_by(PSTSZN) %>%
  summarise(Count = n()) %>%
  as.data.frame() %>%
  kable()

b1.2nd %>%
  group_by(TEAM) %>%
  summarise(Count = n()) %>%
  as.data.frame() %>%
  kable()

ggplot(b1.2nd, aes(G, fill = PSTSZN)) +
  geom_density(alpha = 0.2) +
  labs(fill='グループ') +
  ylab("") +
  xlab("出場試合数") +
  scale_y_continuous(labels=percent)

ggplot(b1.2nd, aes(MIN, fill = PSTSZN)) +
  geom_density(alpha = 0.2) +
  labs(fill='グループ') +
  ylab("") +
  xlab("出場時間（分）") +
  scale_y_continuous(labels=percent)

ggplot(b1.2nd, aes(GS, fill = PSTSZN)) +
  geom_density(alpha = 0.2) +
  labs(fill='グループ') +
  ylab("") +
  xlab("スタメン出場数") +
  scale_y_continuous(labels=percent)

ggplot(b1.2nd, aes(x = G, y = MIN, size = GS)) +
  geom_point(data = b1.2nd, alpha = 0.5) +
  geom_point(data = subset(b1.2nd, G > 55 & MIN > 1000 & GS < 15), alpha = 0.5, col = "red") +
  theme(legend.position="none") +
  ylab("出場時間（分）") +
  xlab("出場試合数")

dd <- subset(b1.2nd, G > 55 & MIN > 750 & GS < 15) %>%
  select(c("TEAM", "PLAYER", "PO", "G", "GS", "MIN", "PPM"))
rownames(dd) <- NULL
kable(dd)

b1.2nd[50, c("TEAM", "PLAYER", "G", "GS", "MIN")] %>%
  kable()
