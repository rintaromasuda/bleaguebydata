devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)
library(dplyr)
library(ggplot2)

df <- GetGameSummary()
df.avg <- df %>%
            filter(Category == "Regular" &
                     Season == "2018-19" &
                     League == "B1") %>%
            group_by(TeamName) %>%
            summarize(Avg = mean(F3GA)) %>%
            as.data.frame()
ggplot() +
  geom_boxplot(data = subset(df, Category == "Regular" &
                               Season == "2018-19" &
                               League == "B1"),
               aes(x = TeamName, y = F3GA)) +
  geom_point(data = df.avg, aes(x = TeamName, y = Avg), color = "red", shape = 3, size = 2) +
  ylab("") +
  xlab("") +
  ggtitle("B1各チームのスリーポイントシュート試投数\n(2018-19レギュラーシーズン31試合終了時点。赤+は試合平均値。)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))# +
  #scale_y_continuous(labels = scales::percent)

ggsave("B1_F3GA.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, Category == "Regular" &
                               TeamName == "名古屋D"),
               aes(x = Season, y = F3GA), color = "red") +
  ylab("") +
  xlab("") +
  ggtitle("名古屋ダイヤモンドドルフィンズのスリーポイントシュート試投数\n(2018-19レギュラーシーズンは31試合終了時点)") +
  theme_bw()

ggsave("名古屋D.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, Category == "Regular" &
                               TeamName == "新潟"),
               aes(x = Season, y = F3GA), color = "darkorange") +
  ylab("") +
  xlab("") +
  ggtitle("新潟アルビレックスBBのスリーポイントシュート試投数\n(2018-19レギュラーシーズンは31試合終了時点)") +
  theme_bw()

ggsave("新潟.jpeg", width = 8, height = 5)

df.avg <- df %>%
  filter(Category == "Regular" &
           Season == "2018-19" &
           League == "B2") %>%
  group_by(TeamName) %>%
  summarize(Avg = mean(F3GA)) %>%
  as.data.frame()

ggplot() +
  geom_boxplot(data = subset(df, Category == "Regular" &
                               Season == "2018-19" &
                               League == "B2"),
               aes(x = TeamName, y = F3GA)) +
  geom_point(data = df.avg, aes(x = TeamName, y = Avg), color = "red", shape = 3, size = 2) +
  ylab("") +
  xlab("") +
  ggtitle("B2各チームのスリーポイントシュート試投数\n(2018-19レギュラーシーズン31試合終了時点。赤+は試合平均値。)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("B2_F3GA.jpeg", width = 8, height = 5)

ggplot() +
  geom_boxplot(data = subset(df, Category == "Regular" &
                               TeamName == "信州"),
               aes(x = Season, y = F3GA), color = "navy") +
  ylab("") +
  xlab("") +
  ggtitle("信州ブレイブウォリアーズのスリーポイントシュート試投数\n(2018-19レギュラーシーズンは31試合終了時点)") +
  theme_bw()

ggsave("信州.jpeg", width = 8, height = 5)
