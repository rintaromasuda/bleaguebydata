devtools::install_github("rintaromasuda/bleaguer", force = TRUE)
library(bleaguer)
library(dplyr)
library(ggplot2)

df <- GetGameSummary()
ggplot() +
  geom_boxplot(data = subset(df, Category == "Regular" & Season == "2018-19" & League == "B1"),
               aes(x = TeamName, y = (F3GM * 3) / PTS), color = "red") +
  ylab("") +
  xlab("") +
  ggtitle("スリーポイントシュートによる得点が総得点に占める割合\n(2018-19レギュラーシーズン24試合終了時点)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)

ggsave("B1.jpeg", width = 8, height = 5)

df %>%
  filter(Category == "Regular" & Season == "2018-19" & League == "B2") %>%
  group_by(TeamName) %>%
  summarize(N = n())

ggplot() +
  geom_boxplot(data = subset(df, Category == "Regular" & Season == "2018-19" & League == "B2"),
               aes(x = TeamName, y = (F3GM * 3) / PTS), color = "blue") +
  ylab("") +
  xlab("") +
  ggtitle("スリーポイントシュートによる得点が総得点に占める割合\n(2018-19レギュラーシーズン24試合終了時点)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)

ggsave("B2.jpeg", width = 8, height = 5)
