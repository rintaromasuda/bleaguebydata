library(bleaguer)
library(dplyr)
library(ggplot2)

df <- subset(GetGameSummary(),
             Season == b.current.season &
             Category == "Regular")
df.teams <- subset(b.teams, Season == b.current.season)[c("TeamId", "NameShort")]
df <- merge(df, df.teams, by = "TeamId")
df$TeamName <- df$NameShort
df$NameShort <- NULL

plotORR <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(ORR),
           Avg = sum(OR) / (sum(OR) + sum(Opp.DR)),
           TeamNameN = paste0(TeamName, " (", n(), ")"))
  
  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, Med),
                     y = ORR)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, Med),
                   y = Avg),
               shape = 4,
               color = "blue") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste(b.current.season, "レギュラシーズン", "オフェンスリバウンド取得率"),
         subtitle = "中央値順での並び。xは累計ベースの値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotORR(df)
ggsave("ORR_B1.jpg", width = 6, height = 9)
plotORR(df, "B2")
ggsave("ORR_B2.jpg", width = 6, height = 9)

plotDRR <- function(df, league = "B1"){
  df <- subset(df, League == league)
  df %<>%
    group_by(TeamId, TeamName) %>%
    mutate(Med = median(DRR),
           Avg = sum(DR) / (sum(DR) + sum(Opp.OR)),
           TeamNameN = paste0(TeamName, " (", n(), ")"))
  
  ggplot() +
    geom_boxplot(data = df,
                 aes(x = reorder(TeamNameN, Med),
                     y = DRR)) +
    geom_point(data = df,
               aes(x = reorder(TeamNameN, Med),
                   y = Avg),
               shape = 4,
               color = "blue") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste(b.current.season, "レギュラシーズン", "ディフェンスリバウンド取得率"),
         subtitle = "中央値順での並び。xは累計ベースの値。()内はデータ内の経過試合数。",
         x = "",
         y = "") +
    theme_gray() +
    theme(
      axis.text.y = element_text(size = 12)
    ) +
    coord_flip()
}
plotDRR(df)
ggsave("DRR_B1.jpg", width = 6, height = 9)
plotDRR(df, "B2")
ggsave("DRR_B2.jpg", width = 6, height = 9)
