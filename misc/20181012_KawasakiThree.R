library(ggplot2)
library(ggrepel)

df_1st_Kawasaki <- read.csv("all_games_201617_Kawasaki.csv")
df_1st_Kawasaki <- subset(df_1st_Kawasaki, X != 30)
df_1st_Kawasaki$SEASON <- "2016-17"

df_2nd <- read.csv("all_games_201718.csv")
df_2nd_Kawasaki <- df_2nd[df_2nd$TEAM.A == "川崎",]
df_2nd_Kawasaki$SEASON <- "2017-18"

df <- rbind(df_1st_Kawasaki, df_2nd_Kawasaki)

ggplot() +
  geom_histogram(data = subset(df, SEASON == "2016-17"), aes(x = X3POINTSFGM.A, fill = "2016-17"), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = subset(df, SEASON == "2017-18"), aes(x = X3POINTSFGM.A, fill = "2017-18"), alpha = 0.5, binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw() +
  xlab("スリーポイント成功数") +
  ylab("試合数") +
  ggtitle("川崎ブレイブサンダースの1試合ごとスリーポイント成功数の分布") +
  scale_fill_discrete(name = "") +
  theme(legend.position = c(0.9, 0.85))

ggsave(filename = "Kawasaki3FGM_Histogram_1st2nd.jpeg", width = 7, height = 5)

ggplot() +
  geom_histogram(data = subset(df, SEASON == "2016-17"), aes(x = X3POINTSFGA.A, fill = "2016-17"), alpha = 0.5, binwidth = 2) +
  geom_histogram(data = subset(df, SEASON == "2017-18"), aes(x = X3POINTSFGA.A, fill = "2017-18"), alpha = 0.5, binwidth = 2) +
  scale_x_continuous(breaks = seq(0, 50, by = 2)) +
  scale_y_continuous(breaks = seq(0, 30, by = 1)) +
  theme_bw() +
  xlab("スリーポイント試投数") +
  ylab("試合数") +
  ggtitle("川崎ブレイブサンダースの1試合ごとスリーポイント試投数の分布") +
  scale_fill_discrete(name = "") +
  theme(legend.position = c(0.9, 0.85))

ggsave(filename = "Kawasaki3FGA_Histogram_1st2nd.jpeg", width = 7, height = 5)

# This season

df_3rd <- read.csv("B1_201819_11setsu_Summary.csv")

# POS/PPP
df_3rd$POS.A <-
  df_3rd$X2POINTSFGA.A +
  df_3rd$X3POINTSFGA.A -
  df_3rd$OFFENSIVEREBOUNDS.A +
  df_3rd$TURNOVER.A +
  (df_3rd$FREE.THROWSA.A * 0.44)
df_3rd$PPP.A <- df_3rd$F.A / df_3rd$POS.A

df_3rd_Kawasaki <- df_3rd[df_3rd$TEAM.A == "川崎",]
df_3rd_Kawasaki$SEASON <- "2018-19"

dim(df_3rd_Kawasaki)

ggplot() +
  geom_label_repel(data = subset(df_3rd_Kawasaki, X3POINTSFGM.A < 5),
                   aes(x = GAMEIDX,
                       y = X3POINTSFGM.A,
                       label =paste(TEAM.B, "戦", "\n", X3POINTSFGR.A, sep = "")),
                   nudge_y = 13 - subset(df_3rd_Kawasaki, X3POINTSFGM.A < 5)$X3POINTSFGM.A,
                   color ="darkred",
                   segment.size  = 0.1,
                   size = 3) +
  geom_label_repel(data = subset(df_3rd_Kawasaki, X3POINTSFGM.A >= 5),
                   aes(x = GAMEIDX,
                       y = X3POINTSFGM.A,
                       label = paste(TEAM.B, "戦", "\n", X3POINTSFGR.A, sep = "")),
                   nudge_y = 0 - subset(df_3rd_Kawasaki, X3POINTSFGM.A >= 5)$X3POINTSFGM.A,
                   color ="darkred",
                   segment.size  = 0.1,
                   size = 3) +
  geom_line(data = df_3rd_Kawasaki, aes(x = GAMEIDX, y = X3POINTSFGM.A), size = 2, alpha = 0.7) +
  geom_point(data = df_3rd_Kawasaki, aes(x = GAMEIDX, y = X3POINTSFGM.A), size = 4, color = "darkred") +
  ylim(c(0,13)) +
#  scale_y_continuous(breaks = seq(0, 13, by = 1)) +
#  scale_x_continuous(breaks = seq(0, 15, by = 2)) +
  ylab("スリーポイント成功数") +
  xlab("x 試合目") +
  ggtitle("川崎ブレイブサンダースの試合ごとスリーポイント成功数の推移（2018-19シーズン）") +
  theme_bw()

ggsave(filename = "Kawasaki3FGM_201819.jpeg", width = 7, height = 5)

ggplot() +
  geom_label_repel(data = df_3rd_Kawasaki,
                   aes(x = GAMEIDX,
                       y = PPP.A,
                       label =paste(TEAM.B, "戦", sep = "")),
                   nudge_y = 0 - df_3rd_Kawasaki$PPP.A,
                   color ="darkred",
                   segment.size  = 0.1,
                   size = 3) +
  geom_line(data = df_3rd_Kawasaki, aes(x = GAMEIDX, y = PPP.A), size = 2, alpha = 0.7) +
  geom_point(data = df_3rd_Kawasaki, aes(x = GAMEIDX, y = PPP.A), size = 4, color = "darkred") +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.3, by = 0.2)) +
  #  scale_x_continuous(breaks = seq(0, 15, by = 2)) +
  ylab("ポゼッション当たりの得点") +
  xlab("x 試合目") +
  ggtitle("川崎ブレイブサンダースのポゼッション当たりの得点の推移（2018-19シーズン）") +
  theme_bw()

ggsave(filename = "KawasakiPPP_201819_19Games.jpeg", width = 7, height = 5)
