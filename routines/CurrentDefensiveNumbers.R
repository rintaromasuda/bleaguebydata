library(bleaguer)
library(dplyr)
library(ggplot2)

targetSeason <- b.current.season

df <- subset(GetGameSummary(),
             Season == targetSeason &
             Category == "Regular")
df.teams <- subset(b.teams, Season == b.current.season)[c("TeamId", "NameShort")]
df <- merge(df, df.teams, by = "TeamId")
df$TeamName <- df$NameShort
df$NameShort <- NULL

###
# Opp.PtsSecondChance
###

plotOpp2ndPts <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(Opp.PtsSecondChance),
           Avg = mean(Opp.PtsSecondChance),
           TeamNameN = paste0(TeamName, " (", n(), ")"))
  
  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, desc(Med)),
                     y = Opp.PtsSecondChance)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, desc(Med)),
                   y = Avg),
               shape = 4,
               color = "blue") +
    labs(title = paste(targetSeason, "レギュラシーズン", "相手のセカンドチャンス得点"),
         subtitle = "中央値順での並び。xは1試合平均値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotOpp2ndPts(df)
ggsave("Opp2ndPts_B1.jpg", width = 6, height = 9)
plotOpp2ndPts(df, "B2")
ggsave("Opp2ndPts_B2.jpg", width = 6, height = 9)

###
# Opp.PtsInPaint
###

plotOppPaintPts <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(Opp.PtsInPaint),
           Avg = mean(Opp.PtsInPaint),
           TeamNameN = paste0(TeamName, " (", n(), ")"))
  
  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, desc(Med)),
                     y = Opp.PtsInPaint)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, desc(Med)),
                   y = Avg),
               shape = 4,
               color = "blue") +
    labs(title = paste(targetSeason, "レギュラシーズン", "相手のペイント内得点"),
         subtitle = "中央値順での並び。xは1試合平均値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotOppPaintPts(df)
ggsave("OppPaintPts_B1.jpg", width = 6, height = 9)
plotOppPaintPts(df, "B2")
ggsave("OppPaintPts_B2.jpg", width = 6, height = 9)

###
# Opp.EFG
###
plotOppEFG <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(Opp.EFG),
           Avg = ((sum(Opp.F3GM) * 1.5) + sum(Opp.F2GM)) / (sum(Opp.F3GA) + sum(Opp.F2GA)),
           TeamNameN = paste0(TeamName, " (", n(), ")"))
  
  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, desc(Med)),
                     y = Opp.EFG)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, desc(Med)),
                   y = Avg),
               shape = 4,
               color = "blue") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste(targetSeason, "レギュラシーズン", "相手のeFG%"),
         subtitle = "中央値順での並び。xは累計ベースの値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotOppEFG(df)
ggsave("OppEFG_B1.jpg", width = 6, height = 9)
plotOppEFG(df, "B2")
ggsave("OppEFG_B2.jpg", width = 6, height = 9)

###
# Opp.TO
###

plotOppTO <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(Opp.TO),
           Avg = mean(Opp.TO),
           TeamNameN = paste0(TeamName, " (", n(), ")"))
  
  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, Med),
                     y = Opp.TO)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, Med),
                   y = Avg),
               shape = 4,
               color = "blue") +
    labs(title = paste(targetSeason, "レギュラシーズン", "相手のターンオーバー"),
         subtitle = "中央値順での並び。xは1試合平均値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotOppTO(df)
ggsave("OppTO_B1.jpg", width = 6, height = 9)
plotOppTO(df, "B2")
ggsave("OppTO_B2.jpg", width = 6, height = 9)
