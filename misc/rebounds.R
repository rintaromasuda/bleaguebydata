library(dplyr)
library(bleaguer)
library(knitr)
library(ggplot2)
library(ggrepel)

dt <- readr::read_csv("C:\\git\\bleaguebydata\\misc\\play_by_play_20200222.csv")
#dt <- readr::read_csv("C:\\git\\bleaguebydata\\misc\\play_by_play_b2_20200226.csv")
df <- data.frame(dt)

str(df)

df %>%
  group_by(ActionCD1, ActionCD2) %>%
  summarize(F = first(PlayText)) %>%
  View()

#1 3P-made
#2 3P-miss
#3 2P-outside-paint-made
#4 2P-inside-paint-made
#5 2P-outside-paint-miss
#6 2P-inside-paint-miss
#7 FT-made
#16 Basket Count

#9 DR
#10 OR

df %<>%
  filter(ActionCD1 %in% c(2, 5, 6, 8, 9, 10)) %>%
  group_by(ScheduleKey, Period) %>%
  arrange(ScheduleKey, Period, No) %>%
  mutate(PrevActionCD1 = lag(ActionCD1, n = 1, default = NA),
         PrevPlayerID1 = lag(PlayerID1, n = 1, default = NA),
         PrevPlayerNameJ1 = lag(PlayerNameJ1, n = 1, default = NA),
         PrevTeamID = lag(TeamID, n = 1, default = NA),
         PrevTeamNameJ = lag(TeamNameJ, n = 1, default = NA),
         NextActionCD1 = lead(ActionCD1, n = 1, default = NA),
         NextPlayerID1 = lead(PlayerID1, n = 1, default = NA),
         NextPlayerNameJ1 = lead(PlayerNameJ1, n = 1, default = NA),
         NextTeamID = lead(TeamID, n = 1, default = NA),
         NextTeamNameJ = lead(TeamNameJ, n = 1, default = NA),
         Next2ActionCD1 = lead(ActionCD1, n = 2, default = NA),
         Next2PlayerID1 = lead(PlayerID1, n = 2, default = NA),
         Next2PlayerNameJ1 = lead(PlayerNameJ1, n = 2, default = NA),
         Next2TeamID = lead(TeamID, n = 2, default = NA),
         Next2TeamNameJ = lead(TeamNameJ, n = 2, default = NA)
         ) %>%
  as.data.frame()

df %>%
  filter(ActionCD1 %in% c(9, 10)) %>%
  group_by(ActionCD1, PrevActionCD1) %>%
  summarize(N = n()) %>%
  kable()

df %>%
  filter(ActionCD1 %in% c(10)) %>%
  filter(PrevActionCD1 %in% c(2, 5, 6, 8)) %>%
  filter(PlayerID1 == PrevPlayerID1) %>%
  group_by(PlayerNameJ1) %>%
  summarize(N = n()) %>%
  View()

####################
# OR Analysis
####################
dt <- readr::read_csv("C:\\git\\bleaguebydata\\misc\\play_by_play_20200222.csv")
#dt <- readr::read_csv("C:\\git\\bleaguebydata\\misc\\play_by_play_b2_20200226.csv")
df <- data.frame(dt)

df %<>%
  filter(!ActionCD1 %in% c(88, 89)) %>%
  group_by(ScheduleKey, Period) %>%
  arrange(ScheduleKey, Period, No) %>%
  mutate(PrevActionCD1 = lag(ActionCD1, n = 1, default = NA),
         PrevPlayerID1 = lag(PlayerID1, n = 1, default = NA),
         PrevPlayerNameJ1 = lag(PlayerNameJ1, n = 1, default = NA),
         PrevTeamID = lag(TeamID, n = 1, default = NA),
         PrevTeamNameJ = lag(TeamNameJ, n = 1, default = NA),
         NextActionCD1 = lead(ActionCD1, n = 1, default = NA),
         NextActionCD3 = lead(ActionCD3, n = 1, default = NA),
         NextPlayerID1 = lead(PlayerID1, n = 1, default = NA),
         NextPlayerNameJ1 = lead(PlayerNameJ1, n = 1, default = NA),
         NextTeamID = lead(TeamID, n = 1, default = NA),
         NextTeamNameJ = lead(TeamNameJ, n = 1, default = NA),
         NextPlayText = lead(PlayText, n = 1, default = NA),
         Next2ActionCD1 = lead(ActionCD1, n = 2, default = NA),
         Next2PlayerID1 = lead(PlayerID1, n = 2, default = NA),
         Next2PlayerNameJ1 = lead(PlayerNameJ1, n = 2, default = NA),
         Next2TeamID = lead(TeamID, n = 2, default = NA),
         Next2TeamNameJ = lead(TeamNameJ, n = 2, default = NA)
  ) %>%
  as.data.frame()

df$IsNextSameTeam = (df$TeamID == df$NextTeamID)

df %>%
  filter(ActionCD1 %in% c(10, 18)) %>%
  group_by(IsNextSameTeam, NextActionCD1, NextActionCD3) %>%
  summarize(N = n(),
            F = first(NextPlayText)) %>%
  View()
