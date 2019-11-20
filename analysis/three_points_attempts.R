devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(ggrepel)) {
  install.packages("ggrepel")
  library(ggrepel)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

df <- GetGameSummary()
df %<>%
  group_by(TeamId) %>%
  mutate(AxisTeamName = last(TeamName))

df <- subset(df, Category == "Regular" &
                 Season %in% c("2018-19", "2019-20") &
                 TeamId %in% subset(df, Season == "2019-20" & League == "B2")$TeamId)

ggplot() +
  geom_boxplot(data = df,
               aes(x = AxisTeamName,
                   y = F3GA,
                   fill = Season)) +
  scale_fill_brewer(palette = "Set1") +
  xlab("") +
  ylab("1試合当たりのスリーポイント試投数") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("B2_3FGA.jpeg", width = 8, height = 5)
