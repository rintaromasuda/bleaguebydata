Sys.setlocale(locale = 'Japanese')

devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)
library(dplyr)
library(ggplot2)

teamId <- 727

plotMemberHistory <- function() {
  df <- GetGameSummary()
  df_box <-
    b.games.boxscore %>%
    group_by(PlayerId) %>%
    mutate(LatestPlayerName = last(Player),
           TotalMIN = sum(MIN[TeamId == teamId])) %>%
    as.data.frame()
  df <- merge(df, df_box, by = c("ScheduleKey", "TeamId"))
  df <- merge(df, b.teams[, c("Season", "TeamId", "NameLong")], by = c("Season", "TeamId"))
  
  ggplot() +
    geom_tile(data = subset(df, TeamId == teamId & Category == "Regular"),
              aes(x = Game.Index,
                  y = reorder(LatestPlayerName, TotalMIN),
                  fill = StarterBench),
              width = 0.6,
              height = 0.8) +
    ylab("") +
    xlab("") +
    scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60)) +
    theme(plot.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(hjust = 1, size = 8),
          axis.text.x = element_text(size = 8),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "white", colour = "white"),
          legend.position="bottom",
          legend.title = element_blank()
    ) +
    facet_grid(~Season)
}
plotMemberHistory()
ggsave(paste0("MemberHistory_", teamId, ".jpg"), width = 16, height = 9)

df_games <- subset(GetGameSummary(),
                   TeamId == teamId &
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

df_games_and_boxagg <- merge(df_games, df_box_agg, by = c("ScheduleKey", "TeamId"))
df_games_and_boxagg$BenchPTSRate <- df_games_and_boxagg$PTS_Bench / df_games_and_boxagg$PTS_Total
df_games_and_boxagg$BenchMINRate <- df_games_and_boxagg$MIN_Bench / df_games_and_boxagg$MIN_Total

ggplot() +
  geom_text(data = df_games_and_boxagg %>%
              group_by(Season) %>%
              summarize(Label = paste0("N = ", n(), "\n",
                                       "Median = ", round(median(BenchPTSRate) * 100, 1), "%\n",
                                       "Total = ", round((sum(PTS_Bench) / sum(PTS_Total)) * 100, 1), "%")),
            aes(x = Season,
                y = 0.95,
                label = Label)) +
  geom_boxplot(data = df_games_and_boxagg,
               aes(x = Season,
                   y = BenchPTSRate),
               color = "darkred",
               size = 1) +
  xlab("") +
  ylab("ベンチメンバー得点比率") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_bw()
ggsave(paste0("BenchPTSRate_", teamId, ".jpg"), width = 8, height = 5)

ggplot() +
  geom_text(data = df_games_and_boxagg %>%
              group_by(Season) %>%
              summarize(Label = paste0("N = ", n(), "\n",
                                       "Median = ", round(median(BenchMINRate) * 100, 1), "%\n",
                                       "Total = ", round((sum(MIN_Bench) / sum(MIN_Total)) * 100, 1), "%")),
            aes(x = Season,
                y = 0.95,
                label = Label)) +
  geom_boxplot(data = df_games_and_boxagg,
               aes(x = Season,
                   y = BenchMINRate),
               color = "darkred",
               size = 1) +
  xlab("") +
  ylab("ベンチメンバー出場時間比率") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_bw()
ggsave(paste0("BenchMINRate_", teamId, ".jpg"), width = 8, height = 5)

