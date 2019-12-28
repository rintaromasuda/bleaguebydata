#devtools::install_github("rintaromasuda/bleaguer")

library(bleaguer)
library(dplyr)
library(ggplot2)

plot <- function(div){
  df <- subset(GetGameSummary(), Category == "Regular" &
                 HomeAway == "Home" &
                 League == "B1" &
                 TeamDivision == div &
                 Opp.TeamDivision == div)
  df$PtsRatio <- ifelse(df$PTS > df$Opp.PTS,
                          df$PTS / (df$PTS + df$Opp.PTS),
                          df$Opp.PTS / (df$PTS + df$Opp.PTS))
  df$PtsDiff<- ifelse(df$PTS > df$Opp.PTS,
                        df$PTS - df$Opp.PTS,
                        df$Opp.PTS - df$PTS)

  ggplot() +
    geom_text(data = df %>%
                group_by(Season) %>%
                summarize(Med = median(PtsDiff),
                          Mean = mean(PtsDiff),
                          N = n()) %>%
                mutate(Label = paste0("試合数: ",
                                      N,
                                      "\n",
                                      "平均値: ",
                                      round(Mean, 2),
                                      "\n",
                                      "中央値: ",
                                      Med)),
              aes(x = Season,
                  y = 70,
                  label = Label),
              size = 4.5) +
    coord_cartesian(ylim = c(0, 75)) +
    geom_boxplot(data = df,
                 aes(x = Season,
                     y = PtsDiff)) +
    labs(y = "試合の得失点差",
         x = "",
         title = paste0(div, "（B1）チーム同士のゲームの得失点差の推移")) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 15)
    )

  ggsave(paste0(div, ".jpg"), width = 6, height = 9)
}

plot("東地区")
plot("西地区")
plot("中地区")

