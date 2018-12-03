df <- read.csv("B1_201819_9setsu_Summary.csv")

library(dplyr)
library(ggplot2)

df %>%
  group_by(TEAM.A) %>%
  summarise(N = n())

df$POS.A <- df$X2POINTSFGA.A +
  df$X3POINTSFGA.A +
  df$TURNOVER.A +
  (df$FREE.THROWSA.A * 0.44) -
  df$OFFENSIVEREBOUNDS.A

df$POS.B <- df$X2POINTSFGA.B +
  df$X3POINTSFGA.B +
  df$TURNOVER.B +
  (df$FREE.THROWSA.B * 0.44) -
  df$OFFENSIVEREBOUNDS.B

df$PPP.A <- df$F.A / df$POS.A
df$PPP.B <- df$F.B / df$POS.B

df$RESULT.A <- ifelse(df$F.A < df$F.B, "負け", "勝ち")
df$RESULT.A <- factor(df$RESULT.A, levels = c("負け", "勝ち"))

#####################
# Fouls
#####################

ggplot() +
  geom_boxplot(data = df, aes(x = TEAM.A, y = FOULS.A, fill = RESULT.A)) +
  xlab("") +
  ylab("チームファウル数") +
  ggtitle("2018-19シーズン B1 15ゲームまで") +
  guides(fill=guide_legend(title="結果")) +
  theme_bw()

#ggsave("teamfouls.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = df, aes(x = RESULT.A, y = FOULS.A, fill = RESULT.A), size = 1) +
  xlab("結果") +
  ylab("チームファウル数") +
  ggtitle("2018-19シーズン B1 15ゲームまで") +
  theme_bw() +
  scale_fill_discrete(guide=FALSE)

#ggsave("teamfouls_total.jpeg", width = 8, height = 5)

#####################
# 3 points
#####################

ggplot() +
  geom_boxplot(data = df, aes(x = TEAM.A, y = X3POINTSFGM.A, fill = RESULT.A)) +
  xlab("") +
  ylab("スリーポイント成功数") +
  ggtitle("2018-19シーズン B1 15ゲームまで") +
  guides(fill=guide_legend(title="結果")) +
  ylim(c(0,20)) +
  theme_bw()

ggsave("3FGM.jpeg", width = 8, height = 5)

ggplot() +
  geom_box(data = df, aes(x = RESULT.A, y = X3POINTSFGM.A, color = RESULT.A), size = 1) +
  xlab("結果") +
  ylab("スリーポイント成功数") +
  ggtitle("2018-19シーズン B1 15ゲームまで") +
  #guides(color=guide_legend(title="結果")) +
  theme_bw() +
  scale_color_discrete(guide=FALSE)

#ggsave("3FGM_Total.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = df, aes(x = TEAM.A, y = X3POINTSFGA.A, fill = RESULT.A)) +
  xlab("") +
  ylab("スリーポイント試投数") +
  ggtitle("2018-19シーズン B1 15ゲームまで") +
  guides(fill=guide_legend(title="結果")) +
  ylim(c(0,40)) +
  theme_bw()

ggsave("3FGA.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = df, aes(x = RESULT.A, y = X3POINTSFGA.A, color = RESULT.A), size = 1) +
  xlab("結果") +
  ylab("スリーポイント試功数") +
  ggtitle("2018-19シーズン B1 15ゲームまで") +
  #guides(color=guide_legend(title="結果")) +
  theme_bw() +
  scale_color_discrete(guide=FALSE)

#ggsave("3FGA_Total.jpeg", width = 8, height = 5)
