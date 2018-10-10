df <- read.csv("all_games_201718.csv")

library(dplyr)

df %>%
  group_by(TEAM.B) %>%
  summarise(N = n())

df$POS.B <- df$X2POINTSFGA.B + df$X3POINTSFGA.B - df$OFFENSIVEREBOUNDS.B + (0.44 * df$FREE.THROWSA.B)
df$PPP100.B <- (df$F.B / df$POS.B) * 100

df$X2POINTSFGR_RAW.B <- df$X2POINTSFGR.B
df$X3POINTSFGR_RAW.B <- df$X3POINTSFGR.B
df$FREE.THROWSR_RAW.B <- df$FREE.THROWSR.B

df$FGR.B <- (df$X2POINTSFGM.B + df$X3POINTSFGM.B) / (df$X2POINTSFGA.B + df$X3POINTSFGA.B)
df$X2POINTSFGR.B <- (df$X2POINTSFGM.B) / (df$X2POINTSFGA.B)
df$X3POINTSFGR.B <- (df$X3POINTSFGM.B) / (df$X3POINTSFGA.B)

library(ggplot2)

ggplot(df, aes(x = TEAM.A, y = F.B)) +
  geom_boxplot(color="darkblue", alpha=0.7) +
  xlab("") +
  ylab("失点") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = FGR.B)) +
  geom_boxplot(color="darkred", alpha=0.7) +
  xlab("") +
  ylab("相手のフィールドゴール%") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent)

ggplot(df, aes(x = TEAM.A, y = POINTSINTHEPAINT.B, color = TEAM.A)) +
  geom_boxplot(color="darkgreen", alpha=0.7) +
  xlab("") +
  ylab("相手のペイントエリア内での得点") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = PPP100.B, color = TEAM.A)) +
  geom_boxplot(color="purple", alpha=0.7) +
  xlab("") +
  ylab("相手のオフェンス効率") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = X3POINTSFGM.B * 3, color = TEAM.A)) +
  geom_boxplot(color="orange", alpha=0.7) +
  xlab("") +
  ylab("相手のスリーポイントによる得点") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = FASTBREAKPOINTS.B)) +
  geom_boxplot(color="black", alpha=0.7) +
  xlab("") +
  ylab("相手のファストブレイクでの得点") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = TURNOVER.B / POS.B)) +
  geom_boxplot(color="blue", alpha=0.7) +
  xlab("") +
  ylab("相手のターンオーバー") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = FGR.B, fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("相手のフィールドゴール%") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = FGR.B, fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("相手のフィールドゴール%") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = X2POINTSFGR.B, fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("相手の2Pフィールドゴール%") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = X3POINTSFGR.B, fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("相手の3Pフィールドゴール%") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = X3POINTSFGA.B, fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("相手の3PA") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")

View(df)
median(df$X3POINTSFGA.A)

df.ryukyu <- df %>%
  filter(TEAM.A == "琉球") 

ggplot(df.ryukyu, aes(x = TEAM.B, y = F.B)) +
  geom_point(color="darkblue", alpha=0.7) +
  xlab("対戦相手") +
  ylab("失点") +
  ggtitle("琉球の2017-18 レギュラーシーズン60試合分") +
  theme(legend.position="none")
