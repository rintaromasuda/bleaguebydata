library(dplyr)
library(ggplot2)
library(knitr)

df <- read.csv("Fazekas_20181205.csv")

dd <- df %>%
  filter(TYPE == "RS") %>%
  group_by(OPP_TEAM) %>%
  summarise(N = n())
kable(dd[order(dd$N, decreasing = TRUE),])

ggplot() +
  geom_boxplot(data = subset(df, TYPE = "RS"),
               aes(x = SEASON, y = MIN),
               color = "darkred") +
  ggtitle("ニック・ファジーカスの各ゲームでの出場時間（分）\n（2016-17、2017-18レギュラーシーズン全試合と2018-19レギュラーシーズン19試合分）") +
  xlab("") +
  ylab("出場時間（分）") +
  theme_bw()

ggsave("Fazekas_MINoverSeasons.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, TYPE = "RS"),
               aes(x = SEASON, y = PTS),
               color = "darkred") +
  ggtitle("ニック・ファジーカスの各ゲームでの得点\n（2016-17、2017-18レギュラーシーズン全試合と2018-19レギュラーシーズン19試合分）") +
  xlab("") +
  ylab("得点") +
  theme_bw()

ggsave("Fazekas_PTSoverSeasons.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, TYPE = "RS"),
               aes(x = SEASON, y = FGM/FGA),
               color = "darkred") +
  ggtitle("ニック・ファジーカスの各ゲームでのFG%\n（2016-17、2017-18レギュラーシーズン全試合と2018-19レギュラーシーズン19試合分）") +
  xlab("") +
  ylab("FG%") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggsave("Fazekas_FGRoverSeasons.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, TYPE == "RS" & OPP_TEAM == "SR渋谷"),
               aes(x = SEASON, y = FGM/FGA),
               color = "darkred") +
  ggtitle("ニック・ファジーカスの各ゲーaaムでのFG%\n（2016-17、2017-18レギュラーシーズン全試合と2018-19レギュラーシーズン19試合分）") +
  xlab("") +
  ylab("FG%") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggsave("Fazekas_FGRoverSeasons.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, TYPE == "RS"),
               aes(x = SEASON, y = PTS/MIN),
               color = "darkred") +
  ggtitle("ニック・ファジーカスの分平均得点\n（2016-17、2017-18レギュラーシーズン全試合と2018-19レギュラーシーズン19試合分）") +
  xlab("") +
  ylab("分平均得点") +
  theme_bw()

ggsave("Fazekas_PPMoverSeasons.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, TYPE == "RS"),
               aes(x = OPP_TEAM, y = PTS/MIN),
               color = "darkred") +
  ggtitle("ニック・ファジーカスの分平均得点\n（2016-17、2017-18レギュラーシーズン全試合と2018-19レギュラーシーズン19試合分）") +
  xlab("") +
  ylab("分平均得点") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Fazekas_PPMoverTeams.jpeg", width = 8, height = 5)


ggplot() +
  geom_boxplot(data = subset(df, TYPE == "RS"),
               aes(x = OPP_TEAM, y = FGM/FGA),
               color = "darkred") +
  ggtitle("ニック・ファジーカスのFG%\n（2016-17、2017-18レギュラーシーズン全試合と2018-19レギュラーシーズン19試合分）") +
  xlab("") +
  ylab("FG%") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Fazekas_FGRoverTeams.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, TYPE == "RS"),
               aes(x = OPP_TEAM, y = FGA),
               color = "darkred") +
  ggtitle("ニック・ファジーカスのフィールドゴール試投数\n（2016-17、2017-18レギュラーシーズン全試合と2018-19レギュラーシーズン19試合分）") +
  xlab("") +
  ylab("フィールドゴール試投数") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Fazekas_FGAoverTeams.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, TYPE = "RS"),
               aes(x = SEASON, y = FGA),
               color = "darkred") +
  ggtitle("ニック・ファジーカスの各ゲームでのフィールドゴール試投数\n（2016-17、2017-18レギュラーシーズン全試合と2018-19レギュラーシーズン19試合分）") +
  xlab("") +
  ylab("フィールドゴール試投数") +
  theme_bw()

ggsave("Fazekas_FGARoverSeasons.jpeg", width = 8, height = 5)
