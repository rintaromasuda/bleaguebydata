library(bleaguer)
library(dplyr)
library(ggplot2)
library(ggrepel)

targetSeason <- b.current.season
targetLeague <- "B1"

df <- subset(GetGameSummary(),
             Season == targetSeason &
             Category == "Regular" &
             League == targetLeague)
dim(df)

df.boxscore <- subset(b.games.boxscore, ScheduleKey %in% df$ScheduleKey)
df.boxscore %<>% # In case a player has two different names
  group_by(PlayerId) %>%
  mutate(Player = last(Player),
         TeamId = last(TeamId),
         Counter = 1)

df.box.agg <-
  df.boxscore %>%
  group_by(PlayerId, Player, TeamId) %>%
  summarise(GP = sum(Counter[MIN > 0]),
            PTS = sum(PTS),
            FGA = sum(FGA),
            FGM = sum(FGM),
            F3GA = sum(F3GA),
            F3GM = sum(F3GM),
            FTA = sum(FTA),
            FTM = sum(FTM)) %>%
  mutate(PPG = (PTS / GP),
         FGR = (FGM / FGA),
         F3GR = (F3GM / F3GA),
         FTR = (FTM / FTA))

df.box.agg$PPG_SD <- scale(df.box.agg$PPG)
df.box.agg$F3GA_SD <- scale(df.box.agg$F3GA)
df.box.agg$F3GM_SD <- scale(df.box.agg$F3GM)
df.box.agg$FTA_SD <- scale(df.box.agg$FTA)
df.box.agg$FTM_SD <- scale(df.box.agg$FTM)

FGR_Avg <- sum(df.boxscore$FGM) / sum(df.boxscore$FGA)
F3GR_Avg <- sum(df.boxscore$F3GM) / sum(df.boxscore$F3GA)
FTR_Avg <- sum(df.boxscore$FTM) / sum(df.boxscore$FTA)

df.box.agg$F3GR_IS_GTE_AVG <- (df.box.agg$F3GR >= F3GR_Avg)
df.box.agg$F3GR_NUDGE <- ifelse(df.box.agg$F3GR_IS_GTE_AVG, 0.4, -0.2)
df.box.agg$F3GR_IS_TARGET <- (df.box.agg$TeamId == 727)
df.box.agg$FTR_IS_GTE_AVG <- (df.box.agg$FTR >= FTR_Avg)
df.box.agg$FTR_NUDGE <- ifelse(df.box.agg$FTR_IS_GTE_AVG, 0.4, -0.2)
df.box.agg$FTR_IS_TARGET <- (df.box.agg$FTA_SD >= 1.7 |
                                df.box.agg$FTM_SD >= 1.7 |
                                (df.box.agg$FTA >= 75 & df.box.agg$FTR >= 0.8))

# Visualize

ggplot() +
  geom_point(data = df.box.agg,
             aes(x = F3GA,
                 y = F3GR,
                 size = PPG),
             alpha = 0.5) +
  geom_hline(yintercept = F3GR_Avg, linetype = 2, size = 1, color = "purple") +
  geom_text_repel(data = subset(df.box.agg, F3GR_IS_TARGET),
                   aes(x = F3GA,
                       y = F3GR,
                       label = Player),
                  nudge_y = subset(df.box.agg, F3GR_IS_TARGET)$F3GR_NUDGE) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste(targetSeason, "レギュラシーズン", "3ポイントシュート (", targetLeague, ")"),
       subtitle = "点線はリーグ平均%、点の大きさはその選手の1試合平均得点を表示。",
       x = "試投数",
       y = "成功率") +
  theme_gray() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggplot() +
  geom_point(data = df.box.agg,
             aes(x = FTA,
                 y = FTR,
                 size = PPG),
             alpha = 0.5) +
  geom_hline(yintercept = FTR_Avg, linetype = 2, size = 1, color = "purple") +
  geom_text_repel(data = subset(df.box.agg, FTR_IS_TARGET),
                  aes(x = FTA,
                      y = FTR,
                      label = Player),
                  nudge_y = subset(df.box.agg, FTR_IS_TARGET)$FTR_NUDGE) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.4)) +
  labs(title = paste(targetSeason, "レギュラシーズン", "フリースロー (", targetLeague, ")"),
       subtitle = "点線はリーグ平均%、点の大きさはその選手の1試合平均得点を表示。",
       x = "試投数",
       y = "成功率") +
  theme_gray() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
ggsave("FT_B1.jpg", width = 9, height = 6)
