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
# PtsSecondChance
###

plot2ndPts <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(PtsSecondChance),
           Avg = mean(PtsSecondChance),
           TeamNameN = paste0(TeamName, " (", n(), ")"))

  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, Med),
                     y = PtsSecondChance)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, Med),
                   y = Avg),
               shape = 4,
               color = "blue") +
    labs(title = paste(targetSeason, "レギュラシーズン", "セカンドチャンス得点"),
         subtitle = "中央値順での並び。xは1試合平均値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plot2ndPts(df)
ggsave("2ndPts_B1.jpg", width = 6, height = 9)
plot2ndPts(df, "B2")
ggsave("2ndPts_B2.jpg", width = 6, height = 9)

###
# PtsInPaint
###

plotPaintPts <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(PtsInPaint),
           Avg = mean(PtsInPaint),
           TeamNameN = paste0(TeamName, " (", n(), ")"))

  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, Med),
                     y = PtsInPaint)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, Med),
                   y = Avg),
               shape = 4,
               color = "blue") +
    labs(title = paste(targetSeason, "レギュラシーズン", "ペイント内得点"),
         subtitle = "中央値順での並び。xは1試合平均値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotPaintPts(df)
ggsave("PaintPts_B1.jpg", width = 6, height = 9)
plotPaintPts(df, "B2")
ggsave("PaintPts_B2.jpg", width = 6, height = 9)

###
# Fast Break
###

plotFastBreakPts <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(PtsFastBreak),
           Avg = mean(PtsFastBreak),
           TeamNameN = paste0(TeamName, " (", n(), ")"))
  
  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, Med),
                     y = PtsFastBreak)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, Med),
                   y = Avg),
               shape = 4,
               color = "blue") +
    labs(title = paste(targetSeason, "レギュラシーズン", "ファストブレイク得点"),
         subtitle = "中央値順での並び。xは1試合平均値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotFastBreakPts(df)
ggsave("FastBreakPts_B1.jpg", width = 6, height = 9)
plotFastBreakPts(df, "B2")
ggsave("FastBreakPts_B2.jpg", width = 6, height = 9)

###
# .EFG
###
plotEFG <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(EFG),
           Avg = ((sum(F3GM) * 1.5) + sum(F2GM)) / (sum(F3GA) + sum(F2GA)),
           TeamNameN = paste0(TeamName, " (", n(), ")"))

  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, Med),
                     y = EFG)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, Med),
                   y = Avg),
               shape = 4,
               color = "blue") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste(targetSeason, "レギュラシーズン", "eFG%"),
         subtitle = "中央値順での並び。xは累計ベースの値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotEFG(df)
ggsave("EFG_B1.jpg", width = 6, height = 9)
plotEFG(df, "B2")
ggsave("EFG_B2.jpg", width = 6, height = 9)

###
# TO
###

plotTO <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(TO),
           Avg = mean(TO),
           TeamNameN = paste0(TeamName, " (", n(), ")"))

  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, desc(Med)),
                     y = TO)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, desc(Med)),
                   y = Avg),
               shape = 4,
               color = "blue") +
    labs(title = paste(targetSeason, "レギュラシーズン", "ターンオーバー"),
         subtitle = "中央値順での並び。xは1試合平均値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotTO(df)
ggsave("TO_B1.jpg", width = 6, height = 9)
plotTO(df, "B2")
ggsave("TO_B2.jpg", width = 6, height = 9)
