setwd('C:/git/bleaguebydata/analysis')

library(knitr)
library(dplyr)
library(ggplot2)
library(moments)
library(ggrepel)

clubs.2nd.b2 <- read.csv("201718_b2_clubs.csv",
                         header = TRUE,
                         sep = ",",
                         quote = "",
                         stringsAsFactors = FALSE
)

clubs.2nd.b2$POS <- round(clubs.2nd.b2$FGA - clubs.2nd.b2$OR + clubs.2nd.b2$TO + (clubs.2nd.b2$FTA * 0.44), 0)
clubs.2nd.b2$PPP <- round(clubs.2nd.b2$PTS / clubs.2nd.b2$POS, 4)
clubs.2nd.b2$PPP100 <- clubs.2nd.b2$PPP * 100

clubs.2nd.b2$WINRATE <- round(clubs.2nd.b2$WIN / (clubs.2nd.b2$WIN + clubs.2nd.b2$LOSE), 3)

ggplot(clubs.2nd.b2, aes(PPP100, WINRATE, label = TEAM)) +
  geom_point(col = "red") +
  geom_text_repel(direction = "x", nudge_y = 0.02) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  ylab("シーズン勝率") +
  xlab("攻撃100回あたりの得点力") +
  ggtitle("B2チームの得点力と勝率の関係（2017-18シーズン）") +
  scale_y_continuous(labels = scales::percent)

clubs.2nd.b2[order(clubs.2nd.b2$WINRATE, decreasing = TRUE),] %>%
  select("TEAM", "WINRATE", "PPP100", "PTS", "POS") %>%
  kable()
