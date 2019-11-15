Sys.setlocale(locale = 'Japanese')

devtools::install_github("rintaromasuda/bleaguer", force = TRUE)
library(bleaguer)
library(dplyr)
library(ggplot2)
library(ineq)

df_games <- subset(GetGameSummary(),
                   Season %in% c("2018-19", "2019-20") &
                   Category == "Regular")

df_box_agg <-
  b.games.boxscore %>%
  group_by(ScheduleKey, TeamId) %>%
  summarize(
    Gini.Index = ineq(MIN)
  )

df <- merge(df_games, df_box_agg, by = c("ScheduleKey", "TeamId"))
df <-
  df %>%
  group_by(TeamId) %>%
  mutate(LatestTeamName = last(TeamName))

plot <- function(league){
  teams <- subset(b.teams, Season == "2019-20" & League == league)

  ggplot() +
    geom_boxplot(data = subset(df, TeamId %in% teams$TeamId),
                 aes(x = reorder(LatestTeamName, as.integer(TeamDivision)),
                     y = Gini.Index,
                     fill = Season)) +
    ylim(c(0, 0.6)) +
    xlab("") +
    ylab("") +
    guides(fill=guide_legend(title="シーズン")) +
    scale_fill_brewer(palette="Set1") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
}
plot("B1")
ggsave("Gini_B1.jpeg", width = 8, height = 5)

plot("B2")
ggsave("Gini_B2.jpeg", width = 8, height = 5)
