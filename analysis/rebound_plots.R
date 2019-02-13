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

df$ORR <- df$OR / (df$Opp.DR + df$OR)
df$DRR <- df$DR / (df$Opp.OR + df$DR)

# Visualize
plotReboundTrend <- function(season = "2018-19", league = "B1") {
  df.target <- df %>%
    filter(Season == season & Category == "Regular" & League == league)
  
  plotTitle <- paste0("リバウンド取得率の推移（",
                      league,
                      " ",
                      season,
                      "シーズン）")
  
  ggplot() +
    geom_point(data = df.target,
               size = 2,
               aes(x = Game.Index,
                   y = ORR,
                   color = "オフェンスリバウンド")) +
    geom_smooth(data = df.target,
                method = "lm",
                se = FALSE,
                aes(x = Game.Index,
                    y = ORR,
                    color = "オフェンスリバウンド")) +
    geom_point(data = df.target,
               size = 2,
               aes(x = Game.Index,
                   y = DRR,
                   color = "ディフェンスリバウンド")) +
    geom_smooth(data = df.target,
                method = "lm",
                se = FALSE,
                aes(x = Game.Index,
                    y = DRR,
                    color = "ディフェンスリバウンド")) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_hue(breaks = c("ディフェンスリバウンド", "オフェンスリバウンド")) +
    xlab("n 試合目") +
    ylab("リバウンド取得率") +
    ggtitle(plotTitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank()
    ) +
    facet_wrap(~TeamName, ncol = 6)
}

plotReboundTrend(league = "B1")
ggsave("Rebound_Trend_B1.jpeg", width = 8, height = 5)
plotReboundTrend(league = "B2")
ggsave("Rebound_Trend_B2.jpeg", width = 8, height = 5)

plotReboundRate <- function(league = "B1", season = "2018-19") {
  df.target <- df %>%
    filter(Season == season & Category == "Regular" & League == league) %>%
    group_by(League, TeamName, TeamDivision) %>%
    summarise(OR = sum(OR),
              DR = sum(DR),
              Opp.OR = sum(Opp.OR),
              Opp.DR = sum(Opp.DR),
              NumGame = n_distinct(ScheduleKey)) %>%
    as.data.frame()

  numGames <- max(df.target$NumGame)

  plotTitle <- paste0("リバウンド取得率（",
                      league,
                      " ",
                      season,
                      "シーズン",
                      " ",
                      numGames,
                      "試合終了時点）")

  df.target$ORR <- df.target$OR / (df.target$Opp.DR + df.target$OR)
  df.target$DRR <- df.target$DR / (df.target$Opp.OR + df.target$DR)
  
  ggplot() +
    geom_point(data = subset(df.target, League == league),
               aes(x = DRR,
                   y = ORR,
                   color = TeamDivision),
               size = 4) +
    geom_label_repel(data = subset(df.target, League == league),
                     aes(x = DRR,
                         y = ORR,
                         label = TeamName)) +
    scale_color_discrete(guide = FALSE) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    xlab("ディフェンスリバウンド取得率") +
    ylab("オフェンスリバウンド取得率") +
    ggtitle(plotTitle) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5)
    )
}

plotReboundRate(league = "B1")
ggsave("Rebound_Total_B1.jpeg", width = 8, height = 5)
plotReboundRate(league = "B2")
ggsave("Rebound_Total_B2.jpeg", width = 8, height = 5)
