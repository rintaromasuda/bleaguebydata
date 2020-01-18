Sys.setlocale(locale = 'Japanese')

library(bleaguer)
library(dplyr)
library(ggplot2)

targetSeason <- b.current.season

df_games <- subset(GetGameSummary(),
             Season == targetSeason &
               Category == "Regular")
df.teams <- subset(b.teams, Season == b.current.season)[c("TeamId", "NameShort")]
df_games <- merge(df_games, df.teams, by = "TeamId")
df_games$TeamName <- df_games$NameShort
df_games$NameShort <- NULL

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

df_games_and_boxagg <- merge(df_games, df_box_agg, by = c("ScheduleKey", "TeamId"))
df_games_and_boxagg$BenchPTSRate <- df_games_and_boxagg$PTS_Bench / df_games_and_boxagg$PTS_Total
df_games_and_boxagg$BenchMINRate <- df_games_and_boxagg$MIN_Bench / df_games_and_boxagg$MIN_Total

plotBenchMinRate <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(BenchMINRate),
           Avg = (sum(MIN_Bench) / sum(MIN_Starters + MIN_Bench)),
           TeamNameN = paste0(TeamName, " (", n(), ")"))

  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, Med),
                     y = BenchMINRate)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, Med),
                   y = Avg),
               shape = 4,
               color = "blue") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste(targetSeason, "レギュラシーズン", "ベンチ選手の出場時間割合"),
         subtitle = "中央値順での並び。xは累計ベースの値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotBenchMinRate(df_games_and_boxagg)
ggsave("BenchMinRate_B1.jpg", width = 6, height = 9)


