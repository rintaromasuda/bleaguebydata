Sys.setlocale(locale = 'Japanese')

devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)
library(dplyr)
library(ggplot2)

df_games <- subset(GetGameSummary(),
                   TeamId == 727 &
                   Season %in% c("2018-19", "2019-20") &
                   Category == "Regular")

df_box_agg <-
  b.games.boxscore %>%
  group_by(ScheduleKey, TeamId) %>%
  summarize(
    PTS_Total = sum(PTS),
    PTS_Starters = sum(PTS[StarterBench == "Starter"]),
    PTS_Bench = sum(PTS[StarterBench =="Bench"]),
    MIN_Total = sum(MIN),
    MIN_Starters = sum(MIN[StarterBench == "Starter"]),
    MIN_Bench = sum(MIN[StarterBench == "Bench"]),
    FGA_Starters = sum(FGA[StarterBench == "Starter"]),
    FGA_Bench = sum(FGA[StarterBench =="Bench"]),
    FGM_Starters = sum(FGM[StarterBench == "Starter"]),
    FGM_Bench = sum(FGM[StarterBench =="Bench"])
  )

df <- merge(df_games, df_box_agg, by = c("ScheduleKey", "TeamId"))

ggplot() +
  geom_boxplot(data = df,
               aes(x = Season,
                   y = PTS_Bench / PTS_Total))

ggplot() +
  geom_line(data = df,
            aes(x = Game.Index,
                y = PTS_Bench / PTS_Total,
                color = Season),
            size = 2)

ggplot() +
  geom_line(data = df,
            aes(x = Game.Index,
                y = MIN_Bench / MIN_Total,
                color = Season),
            size = 2)

ggplot() +
  geom_boxplot(data = df,
               aes(x = Season,
                   y = FGM_Bench / FGA_Bench))

plotOneTeam <- function(teamName) {
  df <- GetGameSummary()
  df_box <-
    b.games.boxscore %>%
    group_by(PlayerId) %>%
    mutate(LatestPlayerName = last(Player)) %>%
    as.data.frame()
  df <- merge(df, df_box, by = c("ScheduleKey", "TeamId"))
  df <- merge(df, b.teams[, c("Season", "TeamId", "NameLong")], by = c("Season", "TeamId"))
  
  ggplot() +
    geom_tile(data = subset(df, TeamName == teamName & Category == "Regular"),
              aes(x = Game.Index,
                  y = reorder(LatestPlayerName, as.integer(Position)),
                  fill = StarterBench),
              width = 0.5,
              height = 0.8,
              size = 2) +
    ylab("") +
    xlab("") +
    #scale_fill_continuous(high = "blue", low = "white") +
    scale_x_continuous(breaks = seq(5, 60, by = 5)) +
    theme(plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(hjust = 1, size = 8),
          axis.text.x = element_text(size = 8),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "white", colour = "white")
    ) +
    facet_grid(~Season)
}
plotOneTeam("川崎")
