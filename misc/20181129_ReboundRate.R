library(dplyr)
library(ggplot2)

df <- read.csv("B1_201819_11setsu_Summary.csv")

df %>%
  group_by(TEAM.B) %>%
  summarise(N = n())

df$ORR.A <- (df$OFFENSIVEREBOUNDS.A / (df$OFFENSIVEREBOUNDS.A + df$DEFENSIVEREBOUNDS.B))
df$DRR.A <- (df$DEFENSIVEREBOUNDS.A / (df$DEFENSIVEREBOUNDS.A + df$OFFENSIVEREBOUNDS.B))
                                   
ggplot() +
  geom_boxplot(data = subset(df, TEAM.A != "秋田" & TEAM.A != "栃木"), aes(x = TEAM.A, y = ORR.A))+
  geom_boxplot(data = subset(df, TEAM.A == "秋田" | TEAM.A == "栃木"), aes(x = TEAM.A, y = ORR.A), color = "red") +
  xlab("") +
  ylab("オフェンスリバウンド取得率") +
  ggtitle("2018-19 B1 19ゲーム終了時点のオフェンスリバウンド取得率") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("ORR.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, TEAM.A != "A東京"),
               aes(x = TEAM.A, y = DRR.A)) +
  geom_boxplot(data = subset(df, TEAM.A == "A東京"),
               aes(x = TEAM.A, y = DRR.A),
               color = "red") +
  xlab("") +
  ylab("ディフェンスリバウンド取得率") +
  ggtitle("2018-19 B1 19ゲーム終了時点のディフェンスリバウンド取得率") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("DRR.jpeg", width = 8, height = 5)
