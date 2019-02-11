devtools::install_github("rintaromasuda/bleaguer")
library(bleaguer)
library(dplyr)
library(ggplot2)

df <- GetGameSummary()

# Data preparation
df <- df %>%
  arrange(Season, Category, Date) %>%
  group_by(Season, Category, TeamName) %>%
  mutate(DaysFromLastGame = as.integer(Date - lag(Date)),
         IsLastGameSameOpp = Opp.TeamId == lag(Opp.TeamId),
         DaysToNextGame = as.integer(lead(Date) - Date),
         IsNextGameSameOpp = Opp.TeamId == lead(Opp.TeamId))

df.target <- subset(df, Category == "Regular" &
                        ((DaysFromLastGame == 1 & IsLastGameSameOpp) |
                           (DaysToNextGame == 1 & IsNextGameSameOpp)))

df.target$GameSeq <- ifelse(!is.na(df.target$DaysToNextGame) & !is.na(df.target$IsNextGameSameOpp) &
                              df.target$DaysToNextGame == 1 & df.target$IsNextGameSameOpp,
                            "ゲーム1",
                     ifelse(!is.na(df.target$DaysFromLastGame) & !is.na(df.target$IsLastGameSameOpp) &
                              df.target$DaysFromLastGame == 1 & df.target$IsLastGameSameOpp,
                            "ゲーム2",
                            NA))

df.target$GameSeq <- factor(df.target$GameSeq)
summary(df.target$GameSeq)

# Visualize
ggplot() +
  geom_boxplot(data = df.target,
               aes(x = Season,
                   y = PTS,
                   fill = GameSeq)) +
  ylab("得点") +
  xlab("") +
  ggtitle("2日連続ゲームのときの得点") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    legend.title = element_blank()
  )

ggplot() +
  geom_boxplot(data = subset(df.target, Season == "2018-19" & League == "B1"),
               aes(x = GameSeq,
                   y = PTS,
                   fill = GameSeq)) +
  ylab("得点") +
  xlab("") +
  ggtitle("2日連続ゲームのときの得点（2018-19シーズン38試合終了時点）") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    legend.title = element_blank()
  ) +
  facet_wrap(~TeamName, ncol = 6)

df.target$eFGR <- (df.target$F2GM + df.target$F3GM * 1.5) / (df.target$F2GA + df.target$F3GA)

ggplot() +
  geom_boxplot(data = df.target,
               aes(x = Season,
                   y = eFGR,
                   fill = GameSeq)) +
  ylab("eFG%") +
  xlab("") +
  ggtitle("2日連続ゲームのときのeFG%") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    legend.title = element_blank()
  )

ggplot() +
  geom_boxplot(data = subset(df.target, Season == "2018-19" & League == "B2"),
               aes(x = Season,
                   y = eFGR,
                   fill = GameSeq)) +
  ylab("eFG%") +
  xlab("") +
  ggtitle("2日連続ゲームのときのeFG%（2018-19シーズン38試合終了時点）") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    legend.title = element_blank()
  ) +
  facet_wrap(~TeamName, ncol = 6)

df.target$POS <- df.target$F2GA + df.target$F3GA + df.target$TO + (df.target$FTA * 0.44) - df.target$OR
  
ggplot() +
  geom_boxplot(data = df.target,
               aes(x = Season,
                   y = POS,
                   fill = GameSeq)) +
  ylab("ポゼッション数") +
  xlab("") +
  ggtitle("2日連続ゲームのときのポゼッション数") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    legend.title = element_blank()
  )

ggplot() +
  geom_boxplot(data = subset(df.target, Season == "2018-19" & League == "B1"),
               aes(x = GameSeq,
                   y = POS,
                   fill = GameSeq)) +
  ylab("ポゼッション数") +
  xlab("") +
  ggtitle("2日連続ゲームのときのポゼッション数（2018-19シーズン38試合終了時点）") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    legend.title = element_blank()
  ) +
  facet_wrap(~TeamName, ncol = 6)

ggplot() +
  geom_boxplot(data = df.target,
               aes(x = Season,
                   y = F,
                   fill = GameSeq)) +
  ylab("ファウル数") +
  xlab("") +
  ggtitle("2日連続ゲームのときのファウル数") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    legend.title = element_blank()
  )

ggplot() +
  geom_boxplot(data = subset(df.target, Season == "2018-19" & League == "B1"),
               aes(x = GameSeq,
                   y = F,
                   fill = GameSeq)) +
  ylab("ファウル数") +
  xlab("") +
  ggtitle("2日連続ゲームのときのファウル数（2018-19シーズン38試合終了時点）") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    legend.title = element_blank()
  ) +
  facet_wrap(~TeamName, ncol = 6)

ggplot() +
  geom_boxplot(data = subset(df.target, Season == "2018-19" & League == "B1"),
               aes(x = GameSeq,
                   y = PTS - Opp.PTS,
                   fill = GameSeq)) +
  ylab("得失点差") +
  xlab("") +
  ggtitle("2日連続ゲームのときの得失点差（2018-19シーズン38試合終了時点）") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    legend.title = element_blank()
  ) +
  facet_wrap(~TeamName, ncol = 6)
