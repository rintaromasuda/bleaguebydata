setwd("C:\\git\\bleaguebydata\\misc")

df_1st_Kawasaki <- read.csv("all_games_201617_Kawasaki.csv")
df_1st_Kawasaki <- subset(df_1st_Kawasaki, X != 30)
df_1st_Kawasaki$SEASON <- "2016-17"

df_2nd <- read.csv("all_games_201718.csv")
df_2nd_Kawasaki <- df_2nd[df_2nd$TEAM.A == "川崎",]
df_2nd_Kawasaki$SEASON <- "2017-18"

df <- rbind(df_1st_Kawasaki, df_2nd_Kawasaki)
View(df)
library(ggplot2)

ggplot(df, aes(x = X3POINTSFGM.A)) +
  geom_density(aes(fill = SEASON, alpha = 0.7)) +
  xlab("1試合のスリーポイント成功数") +
  ylab("密度") +
  ggtitle("川崎ブレイブサンダース 2016-17/2017-18 レギュラーシーズン各60試合") +
  guides(alpha=FALSE)

ggplot(NULL, aes(x = GAMEIDX, y = X3POINTSFGM.A)) +
  geom_line(data = subset(df, SEASON == "2017-18"), colour = "red") +
  geom_point(data = subset(df, SEASON == "2017-18" & TEAM.B == "三河"), colour = "blue") +
  xlab("x 試合目") +
  ylab("スリーポイント成功数") +
  ggtitle("川崎ブレイブサンダース 2017-18 レギュラーシーズン60試合（青は三河戦）") +
  scale_y_continuous(breaks = seq(0, 15, by = 2)) 

df_2nd_Mikawa <- df_2nd[df_2nd$TEAM.A == "三河",]
ggplot(NULL, aes(x = GAMEIDX, y = X3POINTSFGM.B)) +
  geom_line(data = df_2nd_Mikawa, colour = "blue") +
  geom_point(data = subset(df_2nd_Mikawa, TEAM.B == "川崎"), colour = "red") +
  xlab("x 試合目") +
  ylab("被スリーポイント成功数") +
  ggtitle("シーホース三河 2017-18 レギュラーシーズン60試合（赤は川崎戦）") +
  scale_y_continuous(breaks = seq(0, 15, by = 2))
