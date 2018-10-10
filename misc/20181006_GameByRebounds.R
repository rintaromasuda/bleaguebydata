df <- read.csv("all_games_201718.csv")

library(dplyr)

df %>%
  group_by(TEAM.B) %>%
  summarise(N = n())

df$POS.B <- df$X2POINTSFGA.B + df$X3POINTSFGA.B - df$OFFENSIVEREBOUNDS.B + (0.44 * df$FREE.THROWSA.B)
df$PPP100.B <- (df$F.B / df$POS.B) * 100
df$WON.A <- df$F.A > df$F.B

library(ggplot2)

ggplot(df, aes(x = TEAM.A, y = PPP100.B, fill = TEAM.A)) +
  geom_boxplot()

ggplot(df, aes(x = TEAM.A, y = (TOTALREBOUNDS.A / (TOTALREBOUNDS.A + TOTALREBOUNDS.B)), fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("リバウンド支配力") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = (DEFENSIVEREBOUNDS.A / (DEFENSIVEREBOUNDS.A + OFFENSIVEREBOUNDS.B)), fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("ディフェンスリバウンド取得率") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none")

ggplot(subset(df, TEAM.A == "三遠"), aes(x = WON.A, y = (DEFENSIVEREBOUNDS.A / (DEFENSIVEREBOUNDS.A + OFFENSIVEREBOUNDS.B)), fill = WON.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("ディフェンスリバウンド取得率") +
  ggtitle("三遠 2017-18 レギュラーシーズン60試合分") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = (OFFENSIVEREBOUNDS.A / (OFFENSIVEREBOUNDS.A + DEFENSIVEREBOUNDS.B)), fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("オフェンスリバウンド取得率") +
  ggtitle("2017-18 レギュラーシーズン60試合分") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none")

ggplot(subset(df, TEAM.A == "三遠"), aes(x = WON.A, y = (OFFENSIVEREBOUNDS.A / (OFFENSIVEREBOUNDS.A + DEFENSIVEREBOUNDS.B)), fill = WON.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("オフェンスリバウンド取得率") +
  ggtitle("三遠 2017-18 レギュラーシーズン60試合分") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none")

ggplot(df,
       aes(x = (F.A > F.B),
           y = (TOTALREBOUNDS.A / (TOTALREBOUNDS.A + TOTALREBOUNDS.B)))) +
  geom_boxplot()

plot <- function(team) {
  ggplot(subset(df, TEAM.A == team),
         aes(x = (F.A > F.B),
             y = (TOTALREBOUNDS.A / (TOTALREBOUNDS.A + TOTALREBOUNDS.B)))) +
    geom_boxplot() +
    xlab("試合に勝ったか?") +
    ylab("リバウンド支配力") +
    ggtitle(paste(team, "2017-18 レギュラーシーズン60試合分")) +
    scale_y_continuous(labels = scales::percent, limits = c(0.25,0.75))
}

plot("A東京")
plot("横浜")
plot("三河")
plot("西宮")
plot("川崎")
plot("名古屋D")
plot("琉球")
