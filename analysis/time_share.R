Sys.setlocale(locale = 'Japanese')

devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)
library(dplyr)
library(ggplot2)
library(ineq)

df_games <- subset(GetGameSummary(),
                   Season %in% c("2017-18", "2018-19", "2019-20") &
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
                 aes(x = LatestTeamName,
                     y = Gini.Index,
                     fill = Season)) +
    ylim(c(0, 0.6))
  
}
plot("B1")
