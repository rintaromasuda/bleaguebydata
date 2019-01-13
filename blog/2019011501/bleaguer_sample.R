devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

df <- GetGameSummary()

# 富山グラウジーズの2018-19レギュラーシーズンの全得点を取得する
subset(df, TeamName == "富山" & Season == "2018-19" & Category == "Regular")$PTS

# 富山グラウジーズの2017-18ポストシーズンの全得点を取得する
subset(df, TeamName == "富山" & Season == "2017-18" & Category == "Post")$PTS

# 2018-19レギュラーシーズンのB1各チームの平均点と平均失点を取得する
df %>%
  filter(Season == "2018-19" & Category == "Regular" & League == "B1") %>%
  group_by(TeamName) %>%
  summarise(Points = mean(PTS),
            OppPoints = mean(Opp.PTS)) %>%
  as.data.frame()

# 栃木ブレックスのシーズンごと、アリーナごとの平均観客動員数と試合数を取得する
df %>%
  filter(Category == "Regular" & TeamName == "栃木" & HomeAway == "Home") %>%
  group_by(Season, Arena) %>%
  summarize(MeanAttendance = mean(Attendance),
            NumGames = n()) %>%
  as.data.frame()
