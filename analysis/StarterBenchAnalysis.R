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
  geom_boxplot(data = df,
               aes(x = Season,
                   y = FGM_Bench / FGA_Bench))

ggplot() +
  geom_boxplot(data = df,
               aes(x = Season,
                   y = PTS_Bench,
                   fill = "S")) +
  geom_boxplot(data = df,
               aes(x = Season,
                   y = PTS_Starters,
                   fill = "B"))
  