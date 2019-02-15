devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)  
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)  
}

if (!require(ggrepel)) {
  install.packages("ggrepel")
  library(ggrepel)
}

# Data Preparation
df <- GetGameSummary()

df$WinLose <-ifelse(df$PTS > df$Opp.PTS, "Win", "Lose")
df$WinLoseF <- factor(df$WinLose, levels = c("Lose", "Win"))


season <- "2018-19"
league <- "B1"
game.index <- 35

# Win rate and point diff
df.target <- df %>%
  filter(Season == season &
           Category == "Regular" &
           League == league &
           Game.Index <= game.index) %>%
  group_by(TeamName, TeamDivision) %>%
  summarise(WinRate = sum(WinLoseF == "Win") / n(),
            PtsDiff = sum(PTS) - sum(Opp.PTS)) %>%
  as.data.frame()

# Ranking overall
df.target <- df.target %>%
  mutate(Rank = row_number()) %>%
  as.data.frame()

# Ranking by division
df.target <- df.target %>%
  arrange(desc(WinRate), desc(PtsDiff)) %>%
  group_by(TeamDivision) %>%
  mutate(DivisionRank = row_number()) %>%
  as.data.frame()

