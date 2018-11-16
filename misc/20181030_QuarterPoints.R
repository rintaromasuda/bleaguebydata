df <- read.csv("B1_201718_AllGames_Summary_SJIS.csv")

library(dplyr)
library(ggplot2)

df %>%
  group_by(TEAM.B) %>%
  summarise(N = n())

df$WIN.A <- df$F.A > df$F.B
df$X2QTTL.A <- (df$X1Q.A + df$X2Q.A)
df$X2QTTL.B <- (df$X1Q.B + df$X2Q.B)
df$X3QTTL.A <- (df$X1Q.A + df$X2Q.A + df$X3Q.A)
df$X3QTTL.B <- (df$X1Q.B + df$X2Q.B + df$X3Q.B)

ggplot() + 
  geom_histogram(data = subset(df, WIN.A), aes(x = (X1Q.A - X1Q.B)), binwidth = 2, alpha = 0.7) +
  geom_vline(xintercept=0, color="red", linetype="dashed", size=0.3) +
  scale_x_continuous(breaks = seq(-50, 50, 5)) +
  ylab("") +
  xlab("ゲームに勝ったチームのQ1終了時点での得失点差") +
  ggtitle("B1 2017-18レギュラーシーズン全試合分") +
  guides(fill=guide_legend(title=""))

ggsave("Q1.png", width = 8, height = 5)

ggplot() + 
  geom_histogram(data = subset(df, WIN.A), aes(x = (X2QTTL.A - X2QTTL.B)), binwidth = 2, alpha = 0.7) +
  geom_vline(xintercept=0, color="red", linetype="dashed", size=0.3) +
  scale_x_continuous(breaks = seq(-50, 50, 5)) +
  ylab("") +
  xlab("ゲームに勝ったチームのQ2終了時点での得失点差") +
  ggtitle("B1 2017-18レギュラーシーズン全試合分") +
  guides(fill=guide_legend(title=""))

ggsave("Q2.png", width = 8, height = 5)

ggplot() + 
  geom_histogram(data = subset(df, WIN.A), aes(x = (X3QTTL.A - X3QTTL.B)), binwidth = 2, alpha = 0.7) +
  geom_vline(xintercept=0, color="red", linetype="dashed", size=0.3) +
  scale_x_continuous(breaks = seq(-50, 50, 5)) +
  ylab("") +
  xlab("ゲームに勝ったチームのQ3終了時点での得失点差") +
  ggtitle("B1 2017-18レギュラーシーズン全試合分") +
  guides(fill=guide_legend(title=""))

ggsave("Q3.png", width = 8, height = 5)

ggplot() + 
  geom_histogram(data = subset(df, WIN.A), aes(x = (X1Q.A - X1Q.B), fill = "Q1終了時点"), binwidth = 2, alpha = 0.5) +
  geom_histogram(data = subset(df, WIN.A), aes(x = (X2QTTL.A - X2QTTL.B), fill = "Q2終了時点"), binwidth = 2, alpha = 0.5) +
  geom_histogram(data = subset(df, WIN.A), aes(x = (X3QTTL.A - X3QTTL.B), fill = "Q3終了時点"), binwidth = 2, alpha = 0.5) +
  geom_vline(xintercept=0, color="red", linetype="dashed", size=0.3) +
  scale_x_continuous(breaks = seq(-50, 50, 5)) +
  ylab("") +
  xlab("ゲームに勝ったチームの各Q終了時点での得失点差") +
  ggtitle("B1 2017-18レギュラーシーズン全試合分") +
  guides(fill=guide_legend(title=""))

ggsave("Q4.png", width = 8, height = 5)

nrow(subset(df, WIN.A & (X1Q.A < X1Q.B))) / nrow(subset(df, WIN.A))
nrow(subset(df, WIN.A & (X2QTTL.A < X2QTTL.B))) / nrow(subset(df, WIN.A))
nrow(subset(df, WIN.A & (X3QTTL.A < X3QTTL.B))) / nrow(subset(df, WIN.A))

View(subset(df, WIN.A & (X1Q.A < X1Q.B)))

df$xQ2DIFF.A <- (df$X2QTTL.A - df$X2QTTL.B)
View(df)
